// Copyright 2020, Diogo Costa, diogo.pinhodacosta@canada.ca
// This file is part of OpenWQ model.

// This program, openWQ, is free software: you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) aNCOLS later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.

// You should have received a copy of the GNU General Public License
// along with this program.  If not, see <http://www.gnu.org/licenses/>.
#include "OpenWQ_hydrolink.h"
#include "OpenWQ_interface.h"
// #include "OpenWQ_units.h"

// Constructor
// initalize numHRUs value
CLASSWQ_openwq::CLASSWQ_openwq() {}

// Deconstructor
CLASSWQ_openwq::~CLASSWQ_openwq() {}

int CLASSWQ_openwq::decl(
    int nRch
    ){           
    

    OpenWQ_hostModelconfig_ref = new OpenWQ_hostModelconfig();
    OpenWQ_couplercalls_ref = new OpenWQ_couplercalls();
    OpenWQ_json_ref = new OpenWQ_json();
    OpenWQ_wqconfig_ref = new OpenWQ_wqconfig();
    OpenWQ_units_ref = new OpenWQ_units();
    OpenWQ_utils_ref = new OpenWQ_utils();
    OpenWQ_readjson_ref = new OpenWQ_readjson();
    OpenWQ_initiate_ref = new OpenWQ_initiate();
    OpenWQ_transp_ref = new OpenWQ_TD_model();
    OpenWQ_LE_ref = new OpenWQ_LE_model();
    OpenWQ_SI_ref = new OpenWQ_SI_model();
    OpenWQ_TS_ref = new OpenWQ_TS_model();
    OpenWQ_chem_ref = new OpenWQ_CH_model();
    OpenWQ_extwatflux_ss_ref = new OpenWQ_extwatflux_ss();
    OpenWQ_solver_ref = new OpenWQ_compute();
    OpenWQ_output_ref = new OpenWQ_output();
    
    //this->num_HRU = num_HRU;
    this->nRch = nRch;

    if (OpenWQ_hostModelconfig_ref->get_num_HydroComp()==0) {

        // Compartment names
        // Make sure to use capital letters for compartment names
        OpenWQ_hostModelconfig_ref->add_HydroComp(rivernetwork_nRch_openwq,"RIVER_NETWORK_REACHES", nRch, 1, 1);

        // External fluxes
        // Make sure to use capital letters for external fluxes
        OpenWQ_hostModelconfig_ref->add_HydroExtFlux(summaEWF_runoff_openwq,"SUMMA_RUNOFF", nRch, 1, 1);

        OpenWQ_vars_ref = new OpenWQ_vars(OpenWQ_hostModelconfig_ref->get_num_HydroComp(),
                                          OpenWQ_hostModelconfig_ref->get_num_HydroExtFlux());

        // Dependencies
        // to expand BGC modelling options
        // OpenWQ_hostModelconfig_ref->HydroDepend.push_back(OpenWQ_hostModelconfig::hydroTuple(0,"SM",        num_HRU,nYdirec_2openwq, nSnow_2openwq + nSoil_2openwq));

        // Master Json
        OpenWQ_wqconfig_ref->set_OpenWQ_masterjson("openWQ_master.json");


        OpenWQ_couplercalls_ref->InitialConfig(
            *OpenWQ_hostModelconfig_ref,
            *OpenWQ_json_ref,                // create OpenWQ_json object
            *OpenWQ_wqconfig_ref,            // create OpenWQ_wqconfig object
            *OpenWQ_units_ref,               // functions for unit conversion
            *OpenWQ_utils_ref,                // utility methods/functions
            *OpenWQ_readjson_ref,            // read json files
            *OpenWQ_vars_ref,
            *OpenWQ_initiate_ref,            // initiate modules
            *OpenWQ_transp_ref,         // transport modules
            *OpenWQ_LE_ref,
            *OpenWQ_chem_ref,                // biochemistry modules
            *OpenWQ_SI_ref,
            *OpenWQ_TS_ref,
            *OpenWQ_extwatflux_ss_ref,       // sink and source modules)
            *OpenWQ_output_ref);
            
    }
    return 0;
}

int CLASSWQ_openwq::openwq_run_time_start(
    int simtime_mizuroute[],
    int nRch_2openwq,
    double REACH_VOL_0[]
    ) {
    // printf("99\n");
    time_t simtime = OpenWQ_units_ref->convertTime_ints2time_t(
        *OpenWQ_wqconfig_ref,
        simtime_mizuroute[0], 
        simtime_mizuroute[1], 
        simtime_mizuroute[2], 
        simtime_mizuroute[3], 
        simtime_mizuroute[4],
        simtime_mizuroute[5]);
    
    // update Vars that rely on Snow
    for (int x = 0; x < nRch_2openwq; x++) {
        OpenWQ_hostModelconfig_ref->set_waterVol_hydromodel_at(rivernetwork_nRch_openwq,x,0,0,REACH_VOL_0[x]);
        // (*OpenWQ_hostModelconfig_ref->waterVol_hydromodel)[rivernetwork_nRch_openwq](x,0,0) = REACH_VOL_0[x];   // snow
    }


    // *OpenWQ_hostModelconfig_ref.time_step = 5;

    //if (last_hru_flag) {
        OpenWQ_couplercalls_ref->RunTimeLoopStart(
            *OpenWQ_hostModelconfig_ref,
            *OpenWQ_json_ref,
            *OpenWQ_wqconfig_ref,            // create OpenWQ_wqconfig object
            *OpenWQ_units_ref,               // functions for unit conversion
            *OpenWQ_utils_ref,                // utility methods/functions
            *OpenWQ_readjson_ref,            // read json files
            *OpenWQ_vars_ref,
            *OpenWQ_initiate_ref,            // initiate modules
            *OpenWQ_transp_ref,         // transport modules
            *OpenWQ_LE_ref,
            *OpenWQ_chem_ref,                // biochemistry modules
            *OpenWQ_SI_ref,
            *OpenWQ_TS_ref,
            *OpenWQ_extwatflux_ss_ref,          // sink and source modules)
            *OpenWQ_solver_ref,
            *OpenWQ_output_ref,
            simtime);
    //}

    return 0;
}

int CLASSWQ_openwq::openwq_run_space(
    int simtime_summa[], 
    int source, int ix_s, int iy_s, int iz_s,
    int recipient, int ix_r, int iy_r, int iz_r, 
    double wflux_s2r, double wmass_source) {

    // Convert Fortran Index to C++ index
    ix_s -= 1; iy_s -= 1; iz_s -= 1;
    ix_r -= 1; iy_r -= 1; iz_r -= 1;
   
    time_t simtime = OpenWQ_units_ref->convertTime_ints2time_t(
        *OpenWQ_wqconfig_ref,
        simtime_summa[0], 
        simtime_summa[1], 
        simtime_summa[2], 
        simtime_summa[3], 
        simtime_summa[4],
        0);

    // Updating waterVol_hydromodel
    OpenWQ_hostModelconfig_ref->set_waterVol_hydromodel_at(source,ix_s,iy_s,iz_s,wmass_source);
    
    OpenWQ_couplercalls_ref->RunSpaceStep(
        *OpenWQ_hostModelconfig_ref,
        *OpenWQ_json_ref,
        *OpenWQ_wqconfig_ref,            // create OpenWQ_wqconfig object
        *OpenWQ_units_ref,               // functions for unit conversion
        *OpenWQ_utils_ref,                // utility methods/functions
        *OpenWQ_readjson_ref,            // read json files
        *OpenWQ_vars_ref,
        *OpenWQ_initiate_ref,            // initiate modules
        *OpenWQ_transp_ref,         // transport modules
        *OpenWQ_TS_ref,
        *OpenWQ_LE_ref,
        *OpenWQ_chem_ref,                // biochemistry modules
        *OpenWQ_SI_ref,
        *OpenWQ_extwatflux_ss_ref,       // sink and source modules
        *OpenWQ_solver_ref,
        *OpenWQ_output_ref,
        simtime,
        source, ix_s, iy_s, iz_s,
        recipient, ix_r, iy_r, iz_r,
        wflux_s2r, wmass_source);
    return 0;
}

int CLASSWQ_openwq::openwq_run_space_in(
    int simtime_summa[],
    std::string source_EWF_name,
    int recipient, int ix_r, int iy_r, int iz_r, 
    double wflux_s2r) {

    // Convert Fortran Index to C++ index
    ix_r -= 1; iy_r -= 1; iz_r -= 1;
    
    // printf("199\n");
    time_t simtime = OpenWQ_units_ref->convertTime_ints2time_t(
        *OpenWQ_wqconfig_ref,
        simtime_summa[0], 
        simtime_summa[1], 
        simtime_summa[2], 
        simtime_summa[3], 
        simtime_summa[4],
        0);

     OpenWQ_couplercalls_ref->RunSpaceStep_IN(
        *OpenWQ_hostModelconfig_ref,
        *OpenWQ_json_ref,
        *OpenWQ_wqconfig_ref,
        *OpenWQ_units_ref,
        *OpenWQ_utils_ref,
        *OpenWQ_readjson_ref,
        *OpenWQ_vars_ref,
        *OpenWQ_initiate_ref,
        *OpenWQ_transp_ref,
        *OpenWQ_chem_ref,
        *OpenWQ_TS_ref,
        *OpenWQ_extwatflux_ss_ref,
        *OpenWQ_solver_ref,
        *OpenWQ_output_ref,
        simtime,
        source_EWF_name,
        recipient, ix_r, iy_r, iz_r,
        wflux_s2r);

    return 0;
}

int CLASSWQ_openwq::openwq_run_time_end(
    int simtime_mizuroute[]) {
//   printf("234\n");  
    time_t simtime = OpenWQ_units_ref->convertTime_ints2time_t(
        *OpenWQ_wqconfig_ref,
        simtime_mizuroute[0], 
        simtime_mizuroute[1], 
        simtime_mizuroute[2], 
        simtime_mizuroute[3], 
        simtime_mizuroute[4],
        0);


    OpenWQ_couplercalls_ref->RunTimeLoopEnd(
        *OpenWQ_hostModelconfig_ref,
        *OpenWQ_json_ref,
        *OpenWQ_wqconfig_ref,            // create OpenWQ_wqconfig object
        *OpenWQ_units_ref,               // functions for unit conversion
        *OpenWQ_utils_ref,                // utility methods/functions
        *OpenWQ_readjson_ref,            // read json files
        *OpenWQ_vars_ref,
        *OpenWQ_initiate_ref,            // initiate modules
        *OpenWQ_transp_ref,         // transport modules
        *OpenWQ_LE_ref,
        *OpenWQ_chem_ref,                // biochemistry modules
        *OpenWQ_SI_ref,
        *OpenWQ_TS_ref,
        *OpenWQ_extwatflux_ss_ref,          // sink and source modules)
        *OpenWQ_solver_ref,
        *OpenWQ_output_ref,
        simtime);

    return 0;
}

