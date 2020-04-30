module read_envConditions

use environ_conditions_mod, only: environ_conditions_create, environ_conditions
use ccpp_kinds, only: kind_phys
use const_props_mod,   only: const_props_type
use calc_density, only: calc_density_run
use input_file, only: MAX_ATT_LEN

implicit none

type(environ_conditions),allocatable :: theEnvConds(:)
type(environ_conditions),allocatable :: colEnvConds(:) 

contains

 subroutine  read_envConditions_init(nbox, nSpecies, env_conds_file, env_lat, env_lon, env_lev, user_begin_time, &
             user_end_time, user_dtime, cnst_info, gas_number_density_boxes__num_m3, dt, sim_beg_time, sim_end_time, &
             nlayers, photo_lev)

   real, parameter :: NOT_SET = -huge(1.0)

   integer, intent(in)                        :: nbox, nSpecies
   character(len=120), intent(in)             :: env_conds_file

   ! These are dimensioned as plain reals to interface with the reading routins
   real, dimension(:), intent(in)       :: env_lat, env_lon, env_lev  
   real, intent(in)                     :: user_begin_time, user_end_time, user_dtime

   type(const_props_type), intent(in)   :: cnst_info(:)
   ! Gas species number density for each grid cell (#/m3) (n_species, n_boxes)
   real(kind=kind_phys), intent(inout)  :: gas_number_density_boxes__num_m3(:,:)
   

   real(kind_phys), intent(out)        :: dt
   real(kind_phys), intent(out)        :: sim_beg_time, sim_end_time
   integer, intent(out)                :: nlayers, photo_lev

   real(kind_phys), allocatable     :: file_times(:)


    character(len=512)              :: errmsg
    integer                         :: errflg

  integer,parameter  :: nbox_param=1    ! Need to read this in from namelist and then allocate arrays
  
  integer            :: i,n
  real(kind=kind_phys), allocatable :: wghts(:)

  character(len=16)  :: cnst_name
  character(len=255) :: model_name

  integer :: file_ntimes
  integer :: ibox

  character(len=MAX_ATT_LEN) :: concentration_units
  real(kind=kind_phys) :: air_density__num_m3
  real(kind=kind_phys) :: temperature__K
  real(kind=kind_phys), allocatable :: pressure__Pa(:)

  !---------------------------
  ! allocate host model arrays

  allocate(theEnvConds(nbox))
  allocate(colEnvConds(nbox))

  !---------------------------
  ! Read in the first time slice of data.  
  ! "theEnvConds" contains the time slice at the requested lat/lon/level
  ! "colEnvConds" contains the column time slice at the requested lat/lon

  do ibox=1,nbox
     theEnvConds(ibox) = environ_conditions_create( env_conds_file, lat=env_lat(ibox), lon=env_lon(ibox), lev=env_lev(ibox) )
     colEnvConds(ibox)= environ_conditions_create( env_conds_file, lat=env_lat(ibox), lon=env_lon(ibox) )
  end do

  !---------------------------
  ! Assign the times based on the user provided namelist values or use the
  ! values provided in the environmental conditions file

  if (user_dtime>0.) then
     dt = user_dtime
  else
     dt = theEnvConds(1)%dtime()
  end if

  if (user_begin_time == NOT_SET .or. user_end_time == NOT_SET) then
     file_ntimes= theEnvConds(1)%ntimes()
     allocate(file_times(file_ntimes))
     file_times = theEnvConds(1)%get_times()
  end if
  if (user_begin_time /= NOT_SET) then
     sim_beg_time = user_begin_time
  else
     sim_beg_time = file_times(1)
  end if
  if (user_end_time /= NOT_SET) then
     sim_end_time = user_end_time
  else
     sim_end_time = file_times(file_ntimes)
  end if

  nlayers = colEnvConds(1)%nlayers()
  photo_lev = theEnvConds(1)%levnum()

  !---------------------------
  ! Retrieve the vmr for all species and boxes

  allocate( pressure__Pa( nlayers ) )

  do n = 1,nSpecies
    call cnst_info(n)%print()
    cnst_name = cnst_info(n)%get_name()
    concentration_units = theEnvConds(1)%getunits( cnst_name )
    if( trim( concentration_units ) .eq. 'molecules/m3' ) then
      do ibox=1,nbox
        gas_number_density_boxes__num_m3(n,ibox) = theEnvConds(ibox)%getvar(cnst_name,default_value=0.00_kind_phys)
        write(*,fmt="(' cnst name : ',a20,' init value : ',e13.6,' molecules/m3')") &
          cnst_name, gas_number_density_boxes__num_m3(n,ibox)
        if (cnst_name == 'CL2') then
          wghts(n) = 2._kind_phys
        end if
      enddo
    else if( trim( concentration_units ) .eq. 'molec/molec' .or. &
             trim( concentration_units ) .eq. 'mol/mol' ) then
      do ibox=1,nbox
        pressure__Pa( :nlayers ) = colEnvConds(ibox)%press_mid( nlayers )
        temperature__K           = theEnvConds(ibox)%getvar( "T" )
        call calc_density_run( pressure__Pa( theEnvConds(ibox)%levnum() ), temperature__K, &
                               air_density__num_m3, errmsg, errflg )
        gas_number_density_boxes__num_m3(n,ibox) = air_density__num_m3 * &
                                             theEnvConds(ibox)%getvar(cnst_name,default_value=0.00_kind_phys)
        write(*,fmt="(' cnst name : ',a20,' init value : ',e13.6,' molecules/m3')") &
          cnst_name, gas_number_density_boxes__num_m3(n,ibox)
        if (cnst_name == 'CL2') then
          wghts(n) = 2._kind_phys
        end if
      enddo
    else
      write(*,*) "Invalid input concentration units: '"//trim( concentration_units )//"'"
      stop 3
    end if
  enddo

  deallocate( file_times     )

 end subroutine read_envConditions_init

 subroutine  read_envConditions_timestep(TimeStart,ibox, nlayers, photo_lev, vmrboxes, zenith, albedo, &
               press_mid, press_int, alt, &
               temp, o2vmrcol, o3vmrcol, so2vmrcol, no2vmrcol, vmr, box_h2o, box_temp, box_press, box_aer_sad, box_aer_diam, box_o2)

   real(kind_phys), intent(in)                :: TimeStart
   integer,         intent(in)                :: ibox, nlayers
   integer,         intent(in)                :: photo_lev
   real(kind=kind_phys), intent(in)           :: vmrboxes(:,:)   ! vmr for all boxes
   real(kind_phys), intent(out)               :: zenith, albedo, box_h2o, box_temp, box_press, box_o2
   real(kind_phys), dimension(:), intent(out) :: press_mid, press_int, alt, temp, o2vmrcol, o3vmrcol
   real(kind_phys), dimension(:), intent(out) :: so2vmrcol, no2vmrcol, vmr
   real(kind_phys), dimension(:), intent(out) :: box_aer_sad, box_aer_diam

   character(len=32) :: fld_name
   integer :: nmodes, n
   
   !---------------------------
   ! Update the time

   call colEnvConds(ibox)%update(TimeStart)
   call theEnvConds(ibox)%update(TimeStart)

   !---------------------------
   ! Read in the species information
 
   zenith              = colEnvConds(ibox)%getsrf('SZA')
   albedo              = colEnvConds(ibox)%getsrf('ASDIR')
   press_mid(:nlayers) = colEnvConds(ibox)%press_mid(nlayers)
   press_int(:nlayers) = colEnvConds(ibox)%press_int(nlayers)
   alt(:nlayers)       = colEnvConds(ibox)%getcol('Z3',nlayers)
   temp(:nlayers)      = colEnvConds(ibox)%getcol('T',nlayers)
   o2vmrcol(:nlayers)  = colEnvConds(ibox)%getcol('O2',nlayers)
   o3vmrcol(:nlayers)  = colEnvConds(ibox)%getcol('O3',nlayers)
   so2vmrcol(:nlayers) = colEnvConds(ibox)%getcol('SO2',nlayers)
   no2vmrcol(:nlayers) = colEnvConds(ibox)%getcol('NO2',nlayers)

   box_h2o             = theEnvConds(ibox)%getvar('H2O')
   box_o2             = theEnvConds(ibox)%getvar('O2')

   nmodes = size(box_aer_sad)
   do n = 1,nmodes
      fld_name = ' '
      write(fld_name,'(a,i1)') 'SFC_HET_',n
      box_aer_sad(n) = theEnvConds(ibox)%getvar(trim(fld_name))
      fld_name = ' '
      write(fld_name,'(a,i1)') 'DM_HET_',n
      box_aer_diam(n) = theEnvConds(ibox)%getvar(trim(fld_name))
   end do
   
   vmr(:)              = vmrboxes(:,ibox)
   box_temp            = temp(photo_lev)
   box_press           = press_mid(photo_lev)

end subroutine  read_envConditions_timestep


 subroutine  read_envConditions_update_timestep(ibox, vmr, vmrboxes)

   integer, intent(in)                       :: ibox
   real(kind_phys), dimension(:), intent(in) :: vmr
   real(kind=kind_phys), intent(out)         :: vmrboxes(:,:)   ! vmr for all boxes


   vmrboxes(:,ibox) = vmr(:)

end subroutine  read_envConditions_update_timestep

end module read_envConditions
