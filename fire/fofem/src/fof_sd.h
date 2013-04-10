//
// $Id$
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fofs_sd.c
* Desc: Soil Duff Simulation
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/

/*...........................................................................*/
/*Consumed Duff Dept Limits...                                               */
#define e_DufDepLim  50.0

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* This amount will be used in Duff Sim when there has been no duff consumed */
/* so we have duff but none was consumed which is not the same as having     */
/* no duff to start with and thus running Exp Heat                           */
/* This amount is how much duff we'll say gets consumed just to get the      */
/* Duff Sim to do somthing, creating a little bit of heat for report/graph   */
#define  e_MinDufCon  0.1  /* in inches */


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                 SD - Soil Duff Simulation Input Parameters                */
typedef struct {
    char cr_Name[50];
    int  i_midburn;        /* midburn 720  1680 900 1100        1000  900  */
    int  i_burntime;       /* burntime 240  180  300  300        400  400  */
    int  i_stoptime;       /* stoptime 1200 2400 1800 1800      1500  1500 */
    REAL r_ConDufDep;      /* consumed duffdepth 0.08 0.06 0.05 0.04 0.07  */
    REAL r_duffheat;       /* duff heat content - J/m3                     */
    int  i_duffdensity;    /* duff density - kg/m3                         */
    REAL r_bd;             /* soil bulk density - g/m3                     */
    REAL r_pd;             /* soil particle density - g/m3                 */
    REAL r_xo;             /* extrapolated water cont. at -1 J/kg          */
    REAL r_ls;             /* thermal conductivity of mineral fraction     */
    REAL r_ga;             /* de Vries shape factor                        */
    REAL r_xwo;            /* water content for liquid recirculation       */
    REAL r_cop;            /* power for recirculation function             */
    int  i_dt;             /* time step - s                                */
    REAL r_startwc;        /* starting soil water content - m3/m3          */
    REAL r_starttemp;      /* starting soil temperatue - C                 */

/* Layer and Display arrays,                                                 */
/* This get initalized before going into simulation, they tell the simulatn  */
/*  they define what the layers are and if it is to be outputed              */
/*  See where they get iniialized,                                           */
    REAL rr_z[e_mplus1+1];              /* Layer                             */
    REAL rr_node[e_mplus1+1];           /* 1 = Display, 0 = No Display       */

} d_SD;


int  SD_Mngr (d_SD *a_SD, char cr_TmpFN[], char cr_ErrMes[]);
int  SD_Init (d_SD *a_SD,  d_SI *a_SI,  char cr_ErrMes[]);
int  SD_ChkSoiDuf (float f_DufDepPre, float f_DufDepPos, char cr_ErrMes[]);

REAL   SD_HeatAdj (REAL r_Post);
float  SD_Heatpercent(void);
void SD_Disp (d_SD *a_SD);
