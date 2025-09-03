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

#ifndef OPENWQ_HYDROLINK_INCLUDED
#define OPENWQ_HYDROLINK_INCLUDED

#include "couplercalls/headerfile_CC.hpp"
#include "global/OpenWQ_hostModelConfig.hpp"
#include "global/OpenWQ_json.hpp"
#include "global/OpenWQ_wqconfig.hpp"
#include "global/OpenWQ_vars.hpp"
#include "readjson/headerfile_RJSON.hpp"
#include "initiate/headerfile_INIT.hpp"
#include "models_CH/headerfile_CH.hpp"
#include "models_TD/headerfile_td.hpp"
#include "models_LE/headerfile_le.hpp"
#include "extwatflux_ss/headerfile_EWF_SS.hpp"
#include "units/headerfile_units.hpp"
#include "utils/headerfile_UTILS.hpp"
#include "compute/headerfile_compute.hpp"
#include "output/headerfile_OUT.hpp"
#include <iostream>
#include <time.h>
#include <vector>

// Global Indexes for Compartments
  inline int rivernetwork_nRch_openwq = 0;
  //inline int canopy_index_openwq    = 0;
  //inline int snow_index_openwq      = 1;
  //inline int runoff_index_openwq    = 2;
  //inline int soil_index_openwq      = 3;
  //inline int aquifer_index_openwq   = 4;
  //inline int max_snow_layers        = 5;

// Global Indexes for EWF
  inline int summaEWF_runoff_openwq = 0;

class CLASSWQ_openwq
{

    // Instance Variables
    private:

        OpenWQ_couplercalls *OpenWQ_couplercalls_ref;
        OpenWQ_hostModelconfig *OpenWQ_hostModelconfig_ref;
        OpenWQ_json *OpenWQ_json_ref;
        OpenWQ_wqconfig *OpenWQ_wqconfig_ref;
        OpenWQ_units *OpenWQ_units_ref;
        OpenWQ_utils *OpenWQ_utils_ref;
        OpenWQ_readjson *OpenWQ_readjson_ref;
        OpenWQ_vars *OpenWQ_vars_ref;
        OpenWQ_initiate *OpenWQ_initiate_ref;
        OpenWQ_TD_model *OpenWQ_transp_ref;
        OpenWQ_LE_model *OpenWQ_LE_ref;
        OpenWQ_SI_model *OpenWQ_SI_ref;
        OpenWQ_TS_model *OpenWQ_TS_ref;
        OpenWQ_CH_model *OpenWQ_chem_ref;            
        OpenWQ_extwatflux_ss *OpenWQ_extwatflux_ss_ref;
        OpenWQ_compute *OpenWQ_solver_ref;
        OpenWQ_output *OpenWQ_output_ref;

        int nRch;
        //const float *hru_area;

    // Constructor
    public:
        CLASSWQ_openwq();
        ~CLASSWQ_openwq();
    
    // Methods
    void printNum() {
        std::cout << "num = " << this->nRch << std::endl;
    }

    int decl(
        int nRch
        );           // num of layers in y-dir (set to 1 because not used in summa)

    int openwq_run_time_start(
        int simtime_mizuroute[],
        int nRch_2openwq,
        double REACH_VOL_0[]
        );

    int openwq_run_space(
        int simtime_summa[], 
        int source, int ix_s, int iy_s, int iz_s,
        int recipient, int ix_r, int iy_r, int iz_r, 
        double wflux_s2r, double wmass_source);

    int openwq_run_space_in(
        int simtime_summa[],
        std::string source_EWF_name,
        int recipient, int ix_r, int iy_r, int iz_r, 
        double wflux_s2r);

    int openwq_run_time_end(
        int simtime_summa[]);

};
#endif