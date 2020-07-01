module read_envConditions

use environ_conditions_mod, only: environ_conditions_create, environ_conditions
use ccpp_kinds, only: rk=>kind_phys
use const_props_mod, only: const_props_type

use input_file, only: MAX_ATT_LEN

implicit none

type(environ_conditions),allocatable :: theEnvConds(:)
type(environ_conditions),allocatable :: colEnvConds(:) 

contains

subroutine read_envConditions_init(nbox, nSpecies, init_conds_file, env_conds_file, env_lat, env_lon, env_lev, user_begin_time, &
       user_end_time, user_dtime, cnst_info, vmrboxes, dt, sim_beg_time, sim_end_time, nlayers, photo_lev)
  
  integer,          intent(in) :: nbox, nSpecies
  character(len=*), intent(in) :: env_conds_file
  character(len=*), intent(in) :: init_conds_file

  ! These are dimensioned as plain reals to interface with the reading routins
  real, dimension(:), intent(in)       :: env_lat, env_lon, env_lev  
  real, intent(in)                     :: user_begin_time, user_end_time, user_dtime

  type(const_props_type), intent(in)   :: cnst_info(:)
  real(rk), intent(inout)  :: vmrboxes(:,:)   ! vmr for all boxes


  real(rk), intent(out)        :: dt
  real(rk), intent(out)        :: sim_beg_time, sim_end_time
  integer, intent(out)                :: nlayers, photo_lev

  real(rk), allocatable     :: file_times(:)


  character(len=512)              :: errmsg
  integer                         :: errflg

  real(rk), parameter :: NOT_SET = -huge(1.0)

  integer            :: i,n

  character(len=16)  :: cnst_name
  character(len=255) :: model_name

  integer :: file_ntimes
  integer :: ibox

  type(environ_conditions) :: initialConds(nbox)
  character(len=MAX_ATT_LEN) :: ic_units(nSpecies)
  logical :: need_to_convert
  integer :: o2_ndx, n2_ndx, ar_ndx, o_ndx, h_ndx
  real(rk) :: n2_mmr(nbox)
  real(rk) :: x(nbox)
  real(rk) :: mbar(nbox)
  real(rk) :: mass(nspecies)
  real(rk), parameter :: mass_n2 = 28.02 ! grams/mole

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
     colEnvConds(ibox) = environ_conditions_create( env_conds_file, lat=env_lat(ibox), lon=env_lon(ibox) )
     initialConds(ibox) = environ_conditions_create( init_conds_file, lat=env_lat(ibox), lon=env_lon(ibox), lev=env_lev(ibox) )
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

  o2_ndx = -1
  n2_ndx = -1
  ar_ndx = -1
  o_ndx = -1
  h_ndx = -1
  need_to_convert=.false.

  !---------------------------
  ! Retrieve the vmr for all species and boxes
  do n = 1,nSpecies
     call cnst_info(n)%print()
     cnst_name = cnst_info(n)%get_name()
     if (cnst_name=='H2O') cnst_name='Q'
     ic_units(n) = initialConds(1)%get_units(cnst_name,abort=.false.)
     mass(n) = cnst_info(n)%get_wght()
     if (ic_units(n)=='kg/kg') need_to_convert=.true.
     if (cnst_name == 'N2' .and. ic_units(n)/='NONE') n2_ndx = n
     if (cnst_name == 'O2' .and. ic_units(n)/='NONE') o2_ndx = n
     if (cnst_name == 'Ar' .and. ic_units(n)/='NONE') ar_ndx = n
     if (cnst_name == 'O'  .and. ic_units(n)/='NONE')  o_ndx = n
     if (cnst_name == 'H'  .and. ic_units(n)/='NONE')  h_ndx = n
     do ibox=1,nbox
        vmrboxes(n,ibox) = initialConds(ibox)%getvar(cnst_name,default_value=1.e-36_rk)
        write(*,fmt="(' cnst name : ',a20,' init value : ',e13.6,' ',a)") cnst_name, vmrboxes(n,ibox), trim(ic_units(n))
     enddo
  enddo
  
  if (need_to_convert) then
     if (n2_ndx<1 .and. o2_ndx<1) then
        mbar(:) = 28.9644_rk
     else if (n2_ndx<1) then
        n2_mmr(:) = 1._rk - vmrboxes(o2_ndx,:)
        if (ar_ndx>0) n2_mmr(:) = n2_mmr(:) - vmrboxes(ar_ndx,:)
        if ( o_ndx>0) n2_mmr(:) = n2_mmr(:) - vmrboxes( o_ndx,:)
        if ( h_ndx>0) n2_mmr(:) = n2_mmr(:) - vmrboxes( h_ndx,:)
        x(:) = n2_mmr(:)/mass_n2 + vmrboxes(o2_ndx,:)/mass(o2_ndx)

        if (ar_ndx>0) x(:) = x(:) + vmrboxes(ar_ndx,:)/mass(ar_ndx)
        if ( o_ndx>0) x(:) = x(:) + vmrboxes( o_ndx,:)/mass( o_ndx)
        if ( h_ndx>0) x(:) = x(:) + vmrboxes( h_ndx,:)/mass( h_ndx)
        mbar(:) = 1._rk/x(:)
     else
        x(:) = vmrboxes(n2_ndx,:)/mass(n2_ndx) + vmrboxes(o2_ndx,:)/mass(o2_ndx)
        if (ar_ndx>0) x(:) = x(:) + vmrboxes(ar_ndx,:)/mass(ar_ndx)
        if ( o_ndx>0) x(:) = x(:) + vmrboxes( o_ndx,:)/mass( o_ndx)
        if ( h_ndx>0) x(:) = x(:) + vmrboxes( h_ndx,:)/mass( h_ndx)
        mbar(:) = 1._rk/x(:)
     endif
     
     do n = 1,nSpecies
        if (ic_units(n)=='kg/kg') then
           print*,'convert '//trim(cnst_info(n)%get_name())//' mol-mass: ',mass(n)
           vmrboxes(n,:) = vmrboxes(n,:)*mbar(:)/mass(n)
           do ibox=1,nbox
              write(*,fmt="(' cnst name : ',a20,' init value : ',e13.6,' ',a)") &
                   cnst_info(n)%get_name(), vmrboxes(n,ibox), 'mole/mole'
           enddo
        endif
     end do

  endif


  nlayers = colEnvConds(1)%nlayers()
  photo_lev = theEnvConds(1)%levnum()

  if (allocated(file_times)) deallocate(file_times)

 end subroutine read_envConditions_init

 subroutine  read_envConditions_timestep(TimeStart,ibox, nlayers, photo_lev, vmrboxes, zenith, albedo, &
               press_mid, press_int, alt, &
               temp, o2vmrcol, o3vmrcol, so2vmrcol, no2vmrcol, vmr, box_h2o, box_temp, box_press, box_aer_sad, box_aer_diam, box_o2)

   real(rk), intent(in)                :: TimeStart
   integer,  intent(in)                :: ibox, nlayers
   integer,  intent(in)                :: photo_lev
   real(rk), intent(in)                :: vmrboxes(:,:)   ! vmr for all boxes
   real(rk), intent(out)               :: zenith, albedo, box_h2o, box_temp, box_press, box_o2
   real(rk), dimension(:), intent(out) :: press_mid, press_int, alt, temp, o2vmrcol, o3vmrcol
   real(rk), dimension(:), intent(out) :: so2vmrcol, no2vmrcol, vmr
   real(rk), dimension(:), intent(out) :: box_aer_sad, box_aer_diam

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
   box_o2              = theEnvConds(ibox)%getvar('O2')

   nmodes = size(box_aer_sad)
   box_aer_sad = 0._rk
   box_aer_diam = 0._rk

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

end module read_envConditions
