module mizuroute_openwq

  USE nrtype
  USE openWQ, only:CLASSWQ_openwq
  !USE data_types, only:gru_hru_doubleVec
  implicit none
  private
  ! Subroutines
  public :: openwq_init
  public :: openwq_run_time_start
  !public :: openwq_run_time_start_go
  public :: openwq_run_space_step_basin_in
  public :: openwq_run_space_step
  public :: openwq_run_time_end
  public :: openwq_handle_run_space_step

  ! Global Data for prognostic Variables of HRUs
  !type(gru_hru_doubleVec),save,public   :: progStruct_timestep_start ! copy of progStruct at the start of timestep for passing fluxes

  integer,save,public :: openwq_run_space_type_mpi

  type openwq_run_space_step_type
      integer(i4b) :: ix_s, ix_r
      real(dp) :: wflux_s2r, wmass_source
  end type openwq_run_space_step_type

  type(openwq_run_space_step_type), save, public,allocatable :: data_to_send(:)
  contains

! ####################
! OpenWQ: openwq init
! ####################
subroutine openwq_init(err, message)

      USE globalData, ONLY:openwq_obj
      USE globalData, ONLY: nRch             ! number of reaches in the whoel river network
      USE globalData,       ONLY : pid
      use globalData, only: reachID
      use mpi

      implicit none

      ! Local variables
      integer(i4b),intent(inout)    :: err
      character(*),intent(inout)    :: message 

      type(openwq_run_space_step_type) :: dummy(2)
      integer(kind=MPI_ADDRESS_KIND) :: offsets(4)
      integer :: ierr
      integer :: i
      integer :: oldtypes(4), lengths(4)
      integer(kind=MPI_ADDRESS_KIND) :: extent
      integer :: openwq_run_space_type_mpi0

      print*, pid, 'init_nRch', nRch, size(reachID)

      if (pid == 0) then
      ! initalize openWQ object
      openwq_obj = CLASSWQ_openwq() 

      ! Call openwq_init
      err=openwq_obj%decl(nRch)
      end if

      
      call MPI_GET_ADDRESS(dummy(1)%ix_s,  offsets(1), ierr)
      call MPI_GET_ADDRESS(dummy(1)%ix_r,  offsets(2), ierr)
      call MPI_GET_ADDRESS(dummy(1)%wflux_s2r,  offsets(3), ierr)
      call MPI_GET_ADDRESS(dummy(1)%wmass_source,  offsets(4), ierr)
      do i=2,4
            offsets(i) = offsets(i) - offsets(1)
      end do
      offsets(1) = 0
      ! dummy%ix_s
      oldtypes(1) = MPI_INTEGER
      lengths(1) = 1
      ! dummy%ix_r
      oldtypes(2) = MPI_INTEGER
      lengths(2) = 1
      ! dummy%wflux_s2r
      oldtypes(3) = MPI_DOUBLE_PRECISION
      lengths(3) = 1
      ! dummy%wmass_source
      oldtypes(4) = MPI_DOUBLE_PRECISION
      lengths(4) = 1

      call MPI_TYPE_CREATE_STRUCT(4, lengths, offsets, oldtypes, openwq_run_space_type_mpi0, ierr)
      ! Reuse the offsets array
      call MPI_GET_ADDRESS(dummy(1)%ix_s, offsets(1), ierr)
      call MPI_GET_ADDRESS(dummy(2)%ix_s, offsets(2), ierr)

      extent = offsets(2) - offsets(1)

      
      call MPI_TYPE_CREATE_RESIZED(openwq_run_space_type_mpi0, 0_MPI_ADDRESS_KIND, extent, &
                             openwq_run_space_type_mpi, ierr)
      call MPI_TYPE_COMMIT(openwq_run_space_type_mpi, ierr)

      allocate(data_to_send(nRch))

  
end subroutine openwq_init


! ####################
! OpenWQ: run_time_start
! ####################
subroutine openwq_run_time_start(openwq_obj)

      USE globalData,       ONLY : simDatetime        ! previous and current model time
      USE globalData,       ONLY : nRch            ! number of reaches in the whoel river network
      USE globalData,       ONLY : RCHFLX,RCHFLX_trib, domains_mpi, rch_per_proc, pid, nrch_mainstem, reachID, netopo, NETOPO_trib, NETOPO_main, masterproc, nTribOutlet
      use mpi_utils

      implicit none

      ! Local variables
      class(CLASSWQ_openwq), intent(in)   :: openwq_obj
      integer(i4b)                        :: iRch, iProc
      integer(i4b)                        :: simtime(6) ! 5 time values yy-mm-dd-hh-min
      real(dp),allocatable                :: REACH_VOL_local(:)
      real(dp),allocatable                :: REACH_VOL_0(:)
      integer(i4b)                        :: err
      character(strLen) :: message           ! error message

      integer(i4b) :: ndata_per_proc(0:size(rch_per_proc)-2)

      message = "openwq_run_time_start/"

      ! print*, 'run time start'
      ! print*, nRch
      do iProc = 0, size(ndata_per_proc)-1
            ndata_per_proc(iProc) = rch_per_proc(iProc)
      end do
      ndata_per_proc(0) = ndata_per_proc(0) + rch_per_proc(-1)
      ! print*, 'ndata', ndata_per_proc
      ! print*, sum(ndata_per_proc), nRch
      ! print*, 'mainstem reach', nRch_mainstem, rch_per_proc(-1)
      !return
      allocate(REACH_VOL_local(ndata_per_proc(pid)))
      ! Getting reach volume to update openwq:waterVol_hydromodel
      if (allocated(RCHFLX_trib)) then
            ! associate(segIndexSub => domains_mpi%segIndex)
            !print*, size(RCHFLX_trib)
            !print*, 'reachID', size(reachID)
            if (masterproc) then
                  do iRch = 1, nRch_mainstem
                        REACH_VOL_local(iRch) = RCHFLX_trib(iRch)%ROUTE(1)%REACH_VOL(0)
                  end do
                  do iRch = 1,rch_per_proc(0)
                        REACH_VOL_local(iRch + nRch_mainstem) = RCHFLX_trib(iRch + nRch_mainstem + nTribOutlet)%ROUTE(1)%REACH_VOL(0)
                  end do
            else
                  do iRch = 1, size(RCHFLX_trib)
                        !print*, iRch, pid, RCHFLX_trib(iRch)%ROUTE(1)%REACH_VOL(0)
                        !print*, domains_mpi%segIndex(iRch)
                        REACH_VOL_local(iRch) = RCHFLX_trib(iRch)%ROUTE(1)%REACH_VOL(0)
                  end do
            end if
      else
            do iRch = 1, nRch
                  REACH_VOL_local(iRch) = RCHFLX(iRch)%ROUTE(1)%REACH_VOL(0)
            end do
      end if
           ! call shr_mpi_abort('Evaluate domain decomposition. Terminate program', 20)

      call shr_mpi_gatherV(REACH_VOL_local, ndata_per_proc, REACH_VOL_0, err, message)

      if (masterproc) then
            ! add the time values to the array
            simtime(1) = simDatetime(1)%year()
            simtime(2) = simDatetime(1)%month()
            simtime(3) = simDatetime(1)%day()
            simtime(4) = simDatetime(1)%hour()
            simtime(5) = simDatetime(1)%minute()
            simtime(6) = simDatetime(1)%sec()

            ! Call openwq_run_time_start
            err=openwq_obj%openwq_run_time_start(     &
            simtime,                                  &
            nRch,                                     &
            REACH_VOL_0)
      end if
  
end subroutine openwq_run_time_start

!  OpenWQ space basin/summa
subroutine openwq_run_space_step_basin_in()

      USE globalData,   only: openwq_obj
      USE globalData,       ONLY: simDatetime        ! previous and current model time
      USE globalData,       ONLY : nRch
      USE globalData,       only : NETOPO
      USE globalData,       only : RCHFLX
      USE globalData,       only : TSEC

      implicit none

      ! Local variables
      integer(i4b)                           :: iRch      ! variable needed for looping through reaches
      integer(i4b)                           :: simtime(6) ! 5 time values yy-mm-dd-hh-min
      real(dp)                               :: mizuroute_timestep
      integer(i4b)                           :: err
      integer(i4b)                           :: river_network_reaches    = 0
      integer(i4b)                           :: index_s_openwq
      integer(i4b)                           :: index_r_openwq
      integer(i4b)                           :: ix_s_openwq
      integer(i4b)                           :: ix_r_openwq
      integer(i4b)                           :: iy_s_openwq
      integer(i4b)                           :: iy_r_openwq
      integer(i4b)                           :: iz_s_openwq
      integer(i4b)                           :: iz_r_openwq
      real(dp)                               :: wmass_source_openwq
      real(dp)                               :: compt_vol_m3
      real(dp)                               :: flux_m3_sec
      real(dp)                               :: flux_m3_timestep

      ! return
      ! Getting time
      simtime(1) = simDatetime(1)%year()
      simtime(2) = simDatetime(1)%month()
      simtime(3) = simDatetime(1)%day()
      simtime(4) = simDatetime(1)%hour()
      simtime(5) = simDatetime(1)%minute()
      simtime(6) = simDatetime(1)%sec()

      ! Mizuroute is 1D
      iy_r_openwq = 1
      iz_r_openwq = 1 

      print*, 'openwq_run_space_step_basin_in'
      ! Time step
      mizuroute_timestep = TSEC(2) - TSEC(1)  

      ! ####################################################################
      ! Apply Fluxes
      ! Call RunSpaceStep
      ! ####################################################################

      ! ====================================================
      ! 1 Basin routing: SUMMA to MizuRoute
      ! ====================================================
 
      do iRch = 1, nRch 
            ! *Source*:
            ! PRECIP (external flux, so need call openwq_run_space_in) 
            ! *Recipient*: canopy (only 1 z layer)
            index_r_openwq = river_network_reaches
            ix_r_openwq          = iRch 
            ! *Flux*: the portion of rainfall and snowfall not throughfall
            flux_m3_sec = RCHFLX(iRch)%BASIN_QR(0) 
            flux_m3_timestep = flux_m3_sec * mizuroute_timestep
            ! *Call openwq_run_space_in* if wflux_s2r not 0
            err=openwq_obj%openwq_run_space_in(                    &
            simtime,                                               &
            'SUMMA_RUNOFF',                                        &
            index_r_openwq, ix_r_openwq, iy_r_openwq, iz_r_openwq, &
            flux_m3_timestep)
      end do

end subroutine openwq_run_space_step_basin_in

! OpenWQ space (wihtin mizuroute)
subroutine openwq_run_space_step(segIndex,      & ! index
      NETOPO_in, &
      REACH_VOL_segIndex,                       & ! Volume
      Qlocal_in,                                & ! flow in
      Qlocal_out)                                 ! flow out

      USE globalData,       ONLY : openwq_obj
      USE globalData,       ONLY : simDatetime       
      USE globalData,       ONLY : nRch
      USE globalData,       ONLY : NETOPO
      USE globalData,       ONLY : RCHFLX
      USE globalData,       ONLY : TSEC
      use globalData, only : pid
      USE dataTypes,            ONLY: RCHTOPO         ! Network topology
      use mpi_utils
      use mpi
      USE globalData, ONLY: mpicom_route
      USE globalData, ONLY: reachID

      implicit none

      ! Local variables
      integer(i4b)       :: iRch                      ! variable needed for looping through reaches
      integer(i4b)       :: simtime(6)                ! 5 time values yy-mm-dd-hh-min
      real(dp)           :: mizuroute_timestep
      integer(i4b)       :: err
      integer(i4b)       :: segIndex                  ! index
      type(RCHTOPO),      intent(in)    :: NETOPO_in(:)         ! River Network topology
      real(dp)           :: REACH_VOL_segIndex        ! Volume
      real(dp)           :: Qlocal_in                 ! flow in
      real(dp)           :: Qlocal_out                ! flow out
      integer(i4b)       :: river_network_reaches = 0
      integer(i4b)       :: index_s_openwq
      integer(i4b)       :: index_r_openwq
      integer(i4b)       :: ix_s_openwq
      integer(i4b)       :: ix_r_openwq
      integer(i4b)       :: iy_s_openwq
      integer(i4b)       :: iy_r_openwq
      integer(i4b)       :: iz_s_openwq
      integer(i4b)       :: iz_r_openwq
      real(dp)           :: wflux_s2r_openwq
      real(dp)           :: wmass_source_openwq
      real(dp)           :: compt_vol_m3
      real(dp)           :: flux_m3_sec
      real(dp)           :: flux_m3_timestep
      type(openwq_run_space_step_type) :: buff
      integer(i4b) :: ierr
      integer(i4b) :: request
      integer(i4b) :: i
! return
      ! print*, 'run_space_step', pid, segIndex
      ! Get time from mizuroute
      simtime(1) = simDatetime(1)%year()
      simtime(2) = simDatetime(1)%month()
      simtime(3) = simDatetime(1)%day()
      simtime(4) = simDatetime(1)%hour()
      simtime(5) = simDatetime(1)%minute()
      simtime(6) = simDatetime(1)%sec()

      ! Mizuroute does not have a y-direction, 
      ! so the dimension will always be 1
      iy_s_openwq = 1
      iy_r_openwq = 1
      iz_s_openwq = 1
      iz_r_openwq = 1 
      ! print*, 'openwq_run_space_step'

      ! Time step
      mizuroute_timestep = TSEC(2) - TSEC(1)  

      ! ####################################################################
      ! Apply Fluxes
      ! Call RunSpaceStep
      ! ####################################################################

      ! ====================================================
      ! 1 River routing
      ! ====================================================
      ! segIndex to segIndex+1
      index_s_openwq = river_network_reaches
      ! print*, 'run_step', segIndex
      ! print*, lbound(reachID), ubound(reachID)
      do i=1,size(reachID)
            if (NETOPO_in(segIndex)%REACHID == reachID(i)) then
                  ix_s_openwq = i
                  exit
            end if
      end do
      ! ix_s_openwq          = reachID(segIndex)
      ! print*, ix_s_openwq
      compt_vol_m3         = REACH_VOL_segIndex + Qlocal_in ! That's what is received previous iteraction
      wmass_source_openwq  = compt_vol_m3
      ! *Recipient*: 
      index_r_openwq       = river_network_reaches
      do i=1,size(reachID)
            if (NETOPO_in(segIndex)%DREACHK == reachID(i)) then
                  ix_r_openwq = i
                  exit
            end if
      end do
      print*, pid, ix_r_openwq, NETOPO_in(segIndex)%DREACHK, ix_s_openwq, NETOPO_in(segIndex)%REACHID

      ! print*, NETOPO_in(segIndex)%DREACHI
      ! print*, size(reachID)
      ! ix_r_openwq          = reachID(NETOPO_in(segIndex)%DREACHI)
      !ix_r_openwq          = segIndex + 1
      if(ix_r_openwq.eq.-1) return
      ! flux
      flux_m3_timestep = Qlocal_out
      wflux_s2r_openwq = flux_m3_timestep
      ! *Call openwq_run_space* if wflux_s2r_openwq not 0


      ! this is incorrect, but useful for testing:
      ! if (pid .ne. 0) return


      if (pid .eq. 0) then
      err=openwq_obj%openwq_run_space(                          &
      simtime,                                                  &
      index_s_openwq, ix_s_openwq, iy_s_openwq, iz_s_openwq,    &
      index_r_openwq, ix_r_openwq, iy_r_openwq, iz_r_openwq,    &
      wflux_s2r_openwq,                                         &
      wmass_source_openwq)
            ! print*, simtime(3), simtime(4), simtime(5), ix_s_openwq, ix_r_openwq

      else
            data_to_send(ix_s_openwq)%ix_r = ix_r_openwq
            data_to_send(ix_s_openwq)%ix_s = ix_s_openwq
            data_to_send(ix_s_openwq)%wflux_s2r = wflux_s2r_openwq
            data_to_send(ix_s_openwq)%wmass_source = wmass_source_openwq
            call MPI_Isend(data_to_send(ix_s_openwq), 1, openwq_run_space_type_mpi, 0, openwq_tag, mpicom_route, request, ierr)
            ! print*, 'sent', simtime(3), simtime(4), simtime(5), ix_s_openwq, ix_r_openwq, wflux_s2r_openwq, wmass_source_openwq

      end if


end subroutine openwq_run_space_step

subroutine openwq_handle_run_space_step

      USE globalData,       ONLY : openwq_obj
      USE globalData,       ONLY : simDatetime       
      USE globalData,       ONLY : nRch
      USE globalData,       ONLY : NETOPO
      USE globalData,       ONLY : RCHFLX
      USE globalData,       ONLY : TSEC
      use globalData, only : pid
      USE dataTypes,            ONLY: RCHTOPO         ! Network topology
      use mpi_utils
      use mpi
      USE globalData, ONLY: mpicom_route

      implicit none

      ! Local variables
      ! integer(i4b)       :: iRch                      ! variable needed for looping through reaches
      integer(i4b)       :: simtime(6)                ! 5 time values yy-mm-dd-hh-min
      real(dp)           :: mizuroute_timestep
      integer(i4b)       :: err
      ! integer(i4b)       :: segIndex                  ! index
      ! type(RCHTOPO),      intent(in)    :: NETOPO_in(:)         ! River Network topology
      ! real(dp)           :: REACH_VOL_segIndex        ! Volume
      ! real(dp)           :: Qlocal_in                 ! flow in
      ! real(dp)           :: Qlocal_out                ! flow out
      integer(i4b)       :: river_network_reaches = 0
      integer(i4b)       :: index_s_openwq
      integer(i4b)       :: index_r_openwq
      integer(i4b)       :: ix_s_openwq
      integer(i4b)       :: ix_r_openwq
      integer(i4b)       :: iy_s_openwq
      integer(i4b)       :: iy_r_openwq
      integer(i4b)       :: iz_s_openwq
      integer(i4b)       :: iz_r_openwq
      real(dp)           :: wflux_s2r_openwq
      real(dp)           :: wmass_source_openwq
      ! real(dp)           :: compt_vol_m3
      ! real(dp)           :: flux_m3_sec
      ! real(dp)           :: flux_m3_timestep
      type(openwq_run_space_step_type) :: buff
      integer(i4b) :: ierr
      integer(i4b) :: status(MPI_STATUS_SIZE)
      logical(lgt) :: flag

      call MPI_Iprobe(MPI_ANY_SOURCE,openwq_tag, mpicom_route, flag, status, ierr)
      print*, 'handle', flag
      do 
            if (.not. flag) return

            call MPI_recv(buff, 1, openwq_run_space_type_mpi, MPI_ANY_SOURCE, openwq_tag, mpicom_route, status, ierr)


            ! Get time from mizuroute
            simtime(1) = simDatetime(1)%year()
            simtime(2) = simDatetime(1)%month()
            simtime(3) = simDatetime(1)%day()
            simtime(4) = simDatetime(1)%hour()
            simtime(5) = simDatetime(1)%minute()
            simtime(6) = simDatetime(1)%sec()

            ! Mizuroute does not have a y-direction, 
            ! so the dimension will always be 1
            iy_s_openwq = 1
            iy_r_openwq = 1
            iz_s_openwq = 1
            iz_r_openwq = 1 
            ! print*, 'openwq_run_space_step'

            ! Time step
            mizuroute_timestep = TSEC(2) - TSEC(1)  

            ! ####################################################################
            ! Apply Fluxes
            ! Call RunSpaceStep
            ! ####################################################################

            ! ====================================================
            ! 1 River routing
            ! ====================================================
            ! segIndex to segIndex+1
            index_s_openwq = river_network_reaches
            ix_s_openwq          = buff%ix_s
            wmass_source_openwq  = buff%wmass_source
            ! *Recipient*: 
            index_r_openwq       = river_network_reaches
            ix_r_openwq          = buff%ix_r
            !ix_r_openwq          = segIndex + 1
            if(ix_r_openwq.eq.-1) return
            ! flux
            wflux_s2r_openwq = buff%wflux_s2r
            ! *Call openwq_run_space* if wflux_s2r_openwq not 0

            err=openwq_obj%openwq_run_space(                          &
            simtime,                                                  &
            index_s_openwq, ix_s_openwq, iy_s_openwq, iz_s_openwq,    &
            index_r_openwq, ix_r_openwq, iy_r_openwq, iz_r_openwq,    &
            wflux_s2r_openwq,                                         &
            wmass_source_openwq)
                  ! print*, 'handled', simtime(3), simtime(4), simtime(5), ix_s_openwq, ix_r_openwq, wflux_s2r_openwq, wmass_source_openwq

            call MPI_Iprobe(MPI_ANY_SOURCE,openwq_tag, mpicom_route,  flag, status, ierr)

      end do

end subroutine openwq_handle_run_space_step

! ####################
! OpenWQ: run_time_end
! ####################
subroutine openwq_run_time_end(openwq_obj)

      USE globalData,        ONLY: simDatetime, pid             ! previous and current model time
      implicit none
      class(CLASSWQ_openwq), intent(in)  :: openwq_obj

      ! Local Variables
      integer(i4b)                       :: simtime(6) ! 5 time values yy-mm-dd-hh-min
      integer(i4b)                       :: err        ! error control
! return
      print*, 'run time end', pid, simDatetime(1)%hour(), simDatetime(1)%minute()
      ! Get time
      simtime(1) = simDatetime(1)%year()
      simtime(2) = simDatetime(1)%month()
      simtime(3) = simDatetime(1)%day()
      simtime(4) = simDatetime(1)%hour()
      simtime(5) = simDatetime(1)%minute()
      simtime(6) = simDatetime(1)%sec()

      ! Call openwq_run_time_end
      err=openwq_obj%openwq_run_time_end(simtime)

end subroutine openwq_run_time_end

end module mizuroute_openwq