!> \file MusicBox_var_defs.f90
!!  Contains type definitions for MICM variables and physics-related variables

module MusicBox_var_defs

use :: machine,           only: r8 => kind_phys
use :: solver_var_defs,   only: Solver_type

 implicit none
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------
! The following definition sets up the variables for use within MusicBox
!-----------------------------------------------------------------------
!-----------------------------------------------------------------------

! Filter with CPP for PGI compiler
!> \section arg_table_MusicBox_var_defs
!! | local_name | standard_name                                    | long_name                               | units     | rank | type        |    kind   | intent | optional |
!! |------------|--------------------------------------------------|-----------------------------------------|---------- |------|-------------|-----------|--------|----------|
!! | vmr        | concentration                                    | species concentration                   | mole/mole |    1 | real        | kind_phys | none   | F        |
!! | k_rateConst| gasphase_rate_constants                          | gas phase rates constants               | s-1       |    1 | real        | kind_phys | none   | F        |
!! | j_rateConst| photo_rate_constants                             | photochemical rates constants           | s-1       |    1 | real        | kind_phys | none   | F        |
!! | ODE_obj    | ODE_ddt                                          | ODE derived data type                   | DDT       |    0 | Solver_type |           | none   | F        |
!! | dt         | time_step_for_physics                            | physics time step                       | s         |    0 | real        | kind_phys | in     | F        |
!! | ncol       | horizontal_loop_extent                           | horizontal dimension                    | count     |    0 | integer     |           | in     | F        |
!! | nlev       | adjusted_vertical_layer_dimension_for_radiation  | number of vertical layers for radiation | count     |    0 | integer     |           | in     | F        |
!! | errmsg     | error_message                                    | CCPP error message                      | none      |    0 | character   | len=*     | out    | F        | 
!! | errflg     | error_flag                                       | CCPP error flag                         | flag      |    0 | integer     |           | out    | F        |
!! | icntrl     | ODE_icontrol                                     | ODE integer controls                    | flag      |    1 | integer     |           | in     | F        |
!! | rcntrl     | ODE_rcontrol                                     | ODE real controls                       | none      |    1 | real        | kind_phys | in     | F        |
!! | AbsTol     | abs_trunc_error                                  | ODE absolute step truncation error      | none      |    1 | real        | kind_phys | in     | F        |
!! | RelTol     | rel_trunc_error                                  | ODE relative step truncation error      | none      |    1 | real        | kind_phys | in     | F        |
!! | TimeStart  | chem_step_start_time                             | Chem step start time                    | s         |    0 | real        | kind_phys | in     | F        |
!! | TimeEnd    | chem_step_end_time                               | Chem step end time                      | s         |    0 | real        | kind_phys | in     | F        |
!! | Time       | Simulation_time                                  | Present simulation time                 | s         |    0 | real        | kind_phys | in     | F        |
!!

end module MusicBox_var_defs