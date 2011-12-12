/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_se.h
* Desc: Soil Exp Simulation
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                         SE  - Soil Exp Input Data (non-duff)              */
typedef struct {
  char cr_Name[50];   /* Soil Type Name                                      */
  int  i_starttime;   /* starttime-time from simulation start to fire on - min */
  int  i_burntime;    /* burntime-time from sim start to fire off - min  */
  int  i_cooltime;    /* cooltime-time from start to end of simulation   */
  REAL r_maxRabs;     /* maxrabs-maximum radiant heat input to surface   */
  int  i_heatconst;   /* heatconst-time constant for heating, minutes    */
  int  i_coolconst;   /* coolconst-time constant for cooling, minutes    */
  REAL r_bd;          /* bd-soil bulk density - g/m3                     */
  REAL r_pd;          /* pd-soil particle density - g/m3                 */
  REAL r_xo;          /* xo-extrapolated water cont. at -1 J/kg          */
  REAL r_ls;          /* ls-thermal conductivity of mineral fraction     */
  REAL r_ga;          /* ga-de Vries shape factor                        */
  REAL r_xwo;         /* xwo-water content for liquid recirculation      */
  REAL r_cop;         /* cop-power for recirculation function            */
  int  i_dt;          /* dt-time step - s                                */
  REAL r_startwc;     /* startwc-starting soil water content - m3/m3     */
  REAL r_starttemp;   /* starttemp-starting soil temperatue - C          */

/* Layer and Display arrays,                                                 */
/* This get initalized before going into simulation, they tell the simulatn  */
/*  they define what the layers are and if it is to be outputed              */
/*  See where they get iniialized,                                           */
    REAL rr_z[e_mplus1+1];              /* Layer                             */
    REAL rr_node[e_mplus1+1];           /* 1 = Display, 0 = No Display       */

} d_SE ;




int  SE_Init (d_SI *a_SI, d_SE *a_SE, char cr_ErrMes[]);
int  SE_Mngr (d_SE *a_SE, char cr_TmpFN[], char cr_ErrMes[]);
void SE_Disp (d_SE *a_SE);
