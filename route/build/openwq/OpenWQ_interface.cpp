#include "OpenWQ_hydrolink.h"
#include "OpenWQ_interface.h"
/**
 * Below is the implementation of the C interface for SUMMA. When Summa calls a function 
 * the functions below are the ones that are invoked first. 
 * The openWQ object is then passed from Fortran to these functions so that the OpenWQ object
 * can be called. The openWQ object methods are defined above.
 */
// Interface functions to create Object
CLASSWQ_openwq* create_openwq() {
    return new CLASSWQ_openwq();
}

void delete_openwq(CLASSWQ_openwq* openWQ) {
    delete openWQ;
}

int openwq_decl(
    CLASSWQ_openwq *openWQ,
    int nRch
    ){            

    return openWQ->decl(nRch);

}

int openwq_run_time_start(
    CLASSWQ_openwq *openWQ,
      int simtime_mizuroute[],
      int nRch_2openwq,
      double REACH_VOL_0[]
) {
    
    return openWQ->openwq_run_time_start(
          simtime_mizuroute,
          nRch_2openwq,
          REACH_VOL_0
    );   
}

int openwq_run_space(
    CLASSWQ_openwq *openWQ, 
    int simtime_summa[], 
    int source, int ix_s, int iy_s, int iz_s,
    int recipient, int ix_r, int iy_r, int iz_r, 
    double wflux_s2r, double wmass_source) {

    return openWQ->openwq_run_space(
        simtime_summa, 
        source, ix_s, iy_s, iz_s,
        recipient, ix_r, iy_r, iz_r, 
        wflux_s2r, wmass_source);
}

int openwq_run_space_in(
    CLASSWQ_openwq *openWQ, 
    int simtime_summa[],
    char* source_EWF_name,
    int recipient, int ix_r, int iy_r, int iz_r, 
    double wflux_s2r) {
    
    // convert source_EWF_name to string
    std::string source_EWF_name_str(source_EWF_name);

    return openWQ->openwq_run_space_in(
        simtime_summa,
        source_EWF_name_str,
        recipient, ix_r, iy_r, iz_r, 
        wflux_s2r);
}

int openwq_run_time_end(
    CLASSWQ_openwq *openWQ, 
    int simtime_mizuroute[]) {

    return openWQ->openwq_run_time_end(
        simtime_mizuroute);
}
