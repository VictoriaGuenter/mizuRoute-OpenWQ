interface
    function create_openwq_c() bind(C, name="create_openwq")

        use iso_c_binding
        implicit none
        type(c_ptr) :: create_openwq_c

    end function

    function openwq_decl_c(openWQ,      &
        nRch) bind(C, name="openwq_decl")

        use iso_c_binding
        implicit none
        integer(c_int) :: openwq_decl_c ! returns a return value of 0 (success) or -1 (failure)
        type(c_ptr), intent(in), value :: openWQ
        integer(c_int), intent(in), value  :: nRch

    end function

    function openwq_run_time_start_c(   &
        openWQ,                         &
        simtime_mizuroute,              &
        nRch_2openwq,                   &
        REACH_VOL_0) bind(C, name="openwq_run_time_start")

        use iso_c_binding
        implicit none
        integer(c_int)                       :: openwq_run_time_start_c ! returns 0 (success) or -1 (failure)
        type(c_ptr),    intent(in), value    :: openWQ
        integer(c_int), intent(in), value    :: nRch_2openwq
        integer(c_int), intent(in)           :: simtime_mizuroute(6)
        real(c_double), intent(in)           :: REACH_VOL_0(nRch_2openwq)

    end function

    function openwq_run_space_c(    &
        openWQ,                     &
        simtime,                    &
        source,ix_s,iy_s,iz_s,      &
        recipient,ix_r,iy_r,iz_r,   &
        wflux_s2r,                  &
        wmass_source) bind(C, name="openwq_run_space")

        use iso_c_binding
        implicit none
        integer(c_int)                         :: openwq_run_space_c ! returns 0 (success) or -1 (failure)
        type(c_ptr),    intent(in), value      :: openWQ
        integer(c_int), intent(in)             :: simtime(5)
        integer(c_int), intent(in), value      :: source
        integer(c_int), intent(in), value      :: ix_s
        integer(c_int), intent(in), value      :: iy_s 
        integer(c_int), intent(in), value      :: iz_s
        integer(c_int), intent(in), value      :: recipient
        integer(c_int), intent(in), value      :: ix_r
        integer(c_int), intent(in), value      :: iy_r
        integer(c_int), intent(in), value      :: iz_r
        real(c_double), intent(in), value      :: wflux_s2r
        real(c_double), intent(in), value      :: wmass_source

    end function

    function openwq_run_space_in_c( &
        openWQ,                     &
        simtime,                    &
        source_EWF_name,            &
        recipient,ix_r,iy_r,iz_r,   &
        wflux_s2r) bind(C, name="openwq_run_space_in")

        USE iso_c_binding
        implicit none
        integer(c_int)                         :: openwq_run_space_in_c
        type(c_ptr), intent(in), value         :: openWQ
        integer(c_int), intent(in)             :: simtime(5)
        integer(c_int), intent(in), value      :: recipient
        integer(c_int), intent(in), value      :: ix_r
        integer(c_int), intent(in), value      :: iy_r
        integer(c_int), intent(in), value      :: iz_r
        real(c_double), intent(in), value      :: wflux_s2r
        character(c_char), intent(in)          :: source_EWF_name

    end function

    function openwq_run_time_end_c( &
        openWQ,                     &
        simtime) bind(C, name="openwq_run_time_end")

        USE iso_c_binding
        implicit none
        integer(c_int)                      :: openwq_run_time_end_c ! returns 0 (success) or -1 (failure)
        type(c_ptr),    intent(in), value   :: openWQ
        integer(c_int), intent(in)          :: simtime(6)

    end function

end interface