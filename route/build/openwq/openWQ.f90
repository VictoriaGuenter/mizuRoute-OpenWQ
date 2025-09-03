module openwq
   
 USE, intrinsic :: iso_c_binding
 USE nrtype
 private
 public :: CLASSWQ_openwq

 include "openWQInterface.f90"

 type CLASSWQ_openwq
    private
    type(c_ptr) :: ptr ! pointer to openWQ class

 contains
    procedure :: decl => openWQ_init
    procedure :: openwq_run_time_start => openwq_run_time_start
    procedure :: openwq_run_space => openwq_run_space
    procedure :: openwq_run_space_in => openwq_run_space_in
    procedure :: openwq_run_time_end => openwq_run_time_end

 end type

 interface CLASSWQ_openwq
    procedure create_openwq
 end interface
 contains
    function create_openwq()
        implicit none
        type(CLASSWQ_openwq) :: create_openwq
        create_openwq%ptr = create_openwq_c()
    end function

    ! supposed to be decl but needed to openWQ_decl in the interface file
    ! returns integer of either a failure(-1) or success(0)
    integer function openWQ_init(   &
      this,                         &
      nRch)                
      
      implicit none
      class(CLASSWQ_openwq) :: this
      integer(i4b), intent(in) :: nRch

      openWQ_init = openwq_decl_c(  &
         this%ptr,                  &
         nRch)                 ! num of layers in y-dir (set to 1 because not used in summa)

    end function

    integer function openwq_run_time_start(   &
      this,                                   &
      simtime,                                &
      nRch_2openwq,                           &
      REACH_VOL_0)
    
      implicit none
      class(CLASSWQ_openwq)      :: this
      integer(i4b), intent(in)   :: nRch_2openwq
      integer(i4b), intent(in)   :: simtime(6) ! 5 is the number of timevars
      real(dp),     intent(in)   :: REACH_VOL_0(nRch_2openwq)

      openwq_run_time_start = openwq_run_time_start_c(   &
         this%ptr,                                       &
         simtime,                                        &
         nRch_2openwq,                                   &
         REACH_VOL_0)
   
   end function

   integer function openwq_run_space(  &
      this,                            &
      simtime,                         &
      source,ix_s,iy_s,iz_s,           &
      recipient,ix_r,iy_r,iz_r,        &
      wflux_s2r,wmass_source)

      implicit none
      class(CLASSWQ_openwq)      :: this
      integer(i4b), intent(in)   :: simtime(6) ! 5 is the number of timevars
      integer(i4b), intent(in)   :: source
      integer(i4b), intent(in)   :: ix_s
      integer(i4b), intent(in)   :: iy_s
      integer(i4b), intent(in)   :: iz_s
      integer(i4b), intent(in)   :: recipient
      integer(i4b), intent(in)   :: ix_r
      integer(i4b), intent(in)   :: iy_r
      integer(i4b), intent(in)   :: iz_r
      real(dp),     intent(in)   :: wflux_s2r
      real(dp),     intent(in)   :: wmass_source

      openwq_run_space = openwq_run_space_c( &
         this%ptr,                           &
         simtime,                            &
         source,ix_s,iy_s,iz_s,              &
         recipient,ix_r,iy_r,iz_r,           &
         wflux_s2r,wmass_source)
   
   end function

   integer function openwq_run_space_in(  &
      this,                               &
      simtime,                            &
      source_EWF_name,                    &
      recipient,ix_r,iy_r,iz_r,           &
      wflux_s2r)

      implicit none
      class(CLASSWQ_openwq)      :: this
      integer(i4b), intent(in)   :: simtime(5) ! 5 is the number of timevars
      integer(i4b), intent(in)   :: recipient
      integer(i4b), intent(in)   :: ix_r
      integer(i4b), intent(in)   :: iy_r
      integer(i4b), intent(in)   :: iz_r
      real(dp),  intent(in)      :: wflux_s2r
      character(*), intent(in)   :: source_EWF_name

      openwq_run_space_in = openwq_run_space_in_c( &
         this%ptr,                                 &
         simtime,                                  &
         source_EWF_name,                          &
         recipient,ix_r,iy_r,iz_r,                 &
         wflux_s2r)

   end function


   integer function openwq_run_time_end(  &
      this,                               &
      simtime)

      implicit none
      class(CLASSWQ_openwq)      :: this
      integer(i4b), intent(in)   :: simtime(6) ! 5 is the number of timevars

      openwq_run_time_end = openwq_run_time_end_c( &
         this%ptr,                                 &
         simtime)

   end function

end module openwq