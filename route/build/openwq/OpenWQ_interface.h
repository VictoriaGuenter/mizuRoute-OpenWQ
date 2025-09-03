/**
 * This is the C interface for SUMMA, these are the functions that are called 
 * by SUMMA and the iso bindings. 
 * These are only their definition and their actual implementation is in
 * OpenWQ_hydrolink.cpp 
 */

#ifdef __cplusplus
extern "C" { 
    class CLASSWQ_openwq;
    typedef CLASSWQ_openwq CLASSWQ_openwq;
    #else
    typedef struct CLASSWQ_openwq CLASSWQ_openwq;
    #endif

    // Create OpenWQ Object
    CLASSWQ_openwq* create_openwq();

    // Delete OpenWQ Object
    void delete_openwq(CLASSWQ_openwq* openWQ);

    // OpenWQ initalization method
    int openwq_decl(
        CLASSWQ_openwq *openWQ,
        int nRch
        );           // num of layers in y-dir (set to 1 because not used in summa)

    int openwq_run_time_start(
        CLASSWQ_openwq *openWQ,
        int simtime_mizuroute[],
        int nRch_2openwq,
        double REACH_VOL_0[]
        );

    int openwq_run_space(
        CLASSWQ_openwq *openWQ, 
        int simtime_summa[], 
        int source, int ix_s, int iy_s, int iz_s,
        int recipient, int ix_r, int iy_r, int iz_r, 
        double wflux_s2r, double wmass_source);

    int openwq_run_space_in(
        CLASSWQ_openwq *openWQ, 
        int simtime_summa[],
        char* source_EWF_name,
        int recipient, int ix_r, int iy_r, int iz_r, 
        double wflux_s2r);

    int openwq_run_time_end(
        CLASSWQ_openwq *openWQ, 
        int simtime_mizuroute[]);

    #ifdef __cplusplus
}
#endif