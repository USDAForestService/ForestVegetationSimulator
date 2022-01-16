//
// $Id$
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_se.c
* Desc: Soil Exp Simulation
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
#ifdef ANSI
#define WINAPI
#else
#include <windows.h>
#endif

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

#ifdef wow
#define printf xprintf
void far xprintf (char *Format, ...);
#endif

#include "fof_sh.h"
#include "fof_sh2.h"
#include "fof_se.h"
#include "fof_se2.h"
#include "fof_ansi.h"

#include "fof_sha.h"
#include "fof_soi.h"


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SE_Mngt
* Desc: Exp Heat, this run when there is no Duff Depth
*       This will put out and output file and load the Heat Array use
*        to make graph.
* NOTE: output from the soil simulation gets returned indirectly thru
*        the SHA_* table and functions
* Note-1: The orginal soil code read this inputs from an input file, so
*          I left alot of the variables in place and just made assignments
*          to them from the inputs I send in now.
*   In: a_SE......see the SE_Init()
*                 also the the comments in the fof_sh.h in the d_SI struct
* Ret: 1 OK
*      0 Error - Couldn't open file, OR
*                Soil Simulation was unable to be done with the set
*                condtions, see notes in fof_soi.c soiltemp_step ()
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
int   SE_Mngr (d_SE *a_SE, char cr_TmpFN[], char cr_ErrMes[])
{
int  i, i_success, i_Tim;
int  i_starttime, i_burntime, i_cooltime, i_heatconst, i_coolconst,i_dt;
REAL r_bd,r_pd,r_xo,r_ls,r_ga,r_xwo,r_cop,r_startwc,r_starttemp,r_maxRabs;
REAL r_Rabs, r_time, r_exh, r_exc, r_Rabsub, r_simdt;
REAL rr_node [e_mplus1+1], rr_z [e_mplus1+1], rr_w [e_mplus1+1];
REAL rr_t [e_mplus1+1];
FILE  *fh_Out;

  strcpy (cr_ErrMes,"");

  if ( !strcmp (cr_TmpFN,"") ) {             /* Tmp output File Name         */
     fh_Out = NULL;                          /*  Not doing                   */
     goto A; }

  fh_Out = fopen (cr_TmpFN,"w");
  if ( fh_Out == NULL ) {
    sprintf (cr_ErrMes, "Can't open output file: %s\n", cr_TmpFN);
    return 0;  }

A:
    i_starttime = a_SE->i_starttime;         /* see note-1 above             */
    i_burntime  = a_SE->i_burntime;
    i_cooltime  = a_SE->i_cooltime;
    i_heatconst = a_SE->i_heatconst;
    i_coolconst = a_SE->i_coolconst;
    i_dt        = a_SE->i_dt;
    r_bd        = a_SE->r_bd;
    r_pd        = a_SE->r_pd;
    r_xo        = a_SE->r_xo;
    r_ls        = a_SE->r_ls;
    r_ga        = a_SE->r_ga;
    r_xwo       = a_SE->r_xwo;
    r_cop       = a_SE->r_cop;
    r_startwc   = a_SE->r_startwc;
    r_starttemp = a_SE->r_starttemp;
    r_maxRabs   = a_SE->r_maxRabs;

   r_Rabs = 5.67e-8 * sqr ( sqr (r_starttemp + 273.0));
   soiltemp_initconsts(r_bd,r_pd,r_ls,r_ga,r_xwo,r_cop,r_xo,a_SE->rr_z);
   soiltemp_getdepths(rr_z);

   for ( i = 0; i <= e_mplus1; i++ ) {
     rr_w[i] = r_startwc;
     rr_t[i] = r_starttemp; }

   soiltemp_initprofile(rr_w,rr_t);
   r_time = 0;


  while ( r_time < i_cooltime ) {

    if ( r_time < i_starttime ) {
      r_exh =0; r_exc = 1; }

    if ( r_time > i_starttime &&  r_time < i_burntime)               /* during the burn */
      r_exh = ( 1 - exp ( -(r_time - i_starttime) / i_heatconst));   /* exp. heating and */

    if ( r_time > i_burntime)                      /* cooling during */
      r_exc = exp(-(r_time - i_burntime) /i_coolconst);    /*and after the burn  */

    r_Rabsub = r_maxRabs * r_exh * r_exc + r_Rabs;    /* set abs. rad. at up. bnd */

    r_simdt = i_dt;

    while (1) {
      if ( !soiltemp_step(r_Rabsub, i_dt,&i_success,r_time)){ /* simulate a time step  } */
        strcpy (cr_ErrMes, e_SoiSimFail);         /* Unable to do Soil Sim   */
        goto Close; }                             /*  under thes coditions   */


      if ( !i_success ) {                         /* if not successful then} */
         r_simdt = r_simdt / 2;
         soiltemp_initprofile (rr_w,rr_t);}       /* cut time step in half } */
      if ( i_success )
         break;  }

    r_time = r_time + r_simdt / 60;
    soiltemp_getwater (rr_w);
    soiltemp_gettemps (rr_t);

    if (fh_Out != NULL )                          /* tmp file if requested   */
       fprintf (fh_Out, "%6.1f",r_time);
    for ( i = 1; i <= e_mplus1; i++ ) {           /* save heat & time        */
       i_Tim = r_time;
       SHA_Put (i,i_Tim, rr_t[i]);
       if ( a_SE->rr_node[i] == 1  ) {
         if (fh_Out != NULL )
           fprintf (fh_Out,"%6.1f", rr_t[i]); }  }

    if (fh_Out != NULL )
      fprintf (fh_Out, "\n");

  } /* while end */

Close:
  if (fh_Out != NULL ) {
    fclose (fh_Out);
    fh_Out = NULL; }

  if ( strcmp (cr_ErrMes,"") )          /* Error Occured                     */
    return 0;

  return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SE_Init
* Desc: Load the SE (soil exp) input struct. Most inputs comes from
*        the sr_SE table based on the soil type.
* Note-1: radiant heat coming from burnup is in Kilowatts per Meter Sq.
*         so we multiply by 1000 to gets watts sq.
* Note-2: Remember Burnup should always be run when we do Exp Heat (run
*         when there is 0 duff depth) But burnup may not have been run
*         because all the fuel loadings maybe 0, so there is no heat or
*         time data to get back from burnup, so here I stuff in some small
*         amounts so that Exp Heat can run and produce 0/no/min heating to
*         soil, then the Report and/or Graph can be run by rest of program
*         and show 0/no results, The idea is let the program run as usual
*         but with it showing 0s on report and an empty graph
* Note-3: Need to check this, if a 0 ever did come in it will cause a
*          divide by 0 later in the simulation.
*   In: a_EID_......a EID_ to load
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
int  SE_Init (d_SI *a_SI, d_SE *a_SE, char cr_ErrMes[])
{
int i,  i_time;
float   f_Heat;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/* Find entry in table, based on soil type                                   */
   for ( i = 0;  i < 100; i++ ) {            /* find match in EID Tbl   */
     if ( !strcmp (sr_SE[i].cr_Name,"")) {   /* Hit end and didn't find */
       sprintf (cr_ErrMes,"SE_Init() - Invalid Soil Type: %s ", a_SI->cr_SoilType);
       return 0; }
     if ( !strcmp (sr_SE[i].cr_Name,a_SI->cr_SoilType)){ /* Copy out                */
       memcpy (a_SE,&sr_SE[i],sizeof(d_SE));
       break; } }

   if ( a_SI->f_SoilMoist > e_SMV_Max || a_SI->f_SoilMoist < e_SMV_Min ){
      sprintf (cr_ErrMes,"Soil Moisture %6.2f is out of range (%3.0f to %3.0f)", a_SI->f_SoilMoist, e_SMV_Min, e_SMV_Max);
      return 0; }

   a_SE->r_startwc = a_SI->f_SoilMoist / 100;         /* get it in decimal           */

   if ( a_SI->i_Cnt == 0 ) {
     i_time = 100;                           /* Note-2 above                 */
     f_Heat = 50; }
   else {
     i_time = a_SI->i_Time;
     f_Heat = a_SI->f_fi; }

   if ( a_SI->i_Time <= 0 ) {                /* Note-3 above                 */
     sprintf (cr_ErrMes, "SE_Init() - Invalid Time Duration %d, must be greater than 0\n",a_SI->i_Time);
     return 0; }

   a_SE->i_burntime = ( i_time / 60 ) + 5;  /* secs to mins plus 5          */
   a_SE->r_maxRabs = f_Heat  * 1000;        /*  Note-1 above        */
   a_SE->i_heatconst = ( i_time / 60 );
   a_SE->i_coolconst = ( i_time / 60 );

/* Init the Layer and Display arrays....                                     */
   SH_Init_LayDis (a_SE->rr_z, a_SE->rr_node);

   return 1;
}

#ifdef wow
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SE_Disp
* Desc:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void SE_Disp (d_SE *a_SE)
{
int i;

  printf ("starttime; %d \n",a_SE->i_starttime);   /* starttime-time from simulation start to fire on - min */
  printf ("burntime;  %d \n",a_SE->i_burntime);    /* burntime-time from sim start to fire off - min  */
  printf ("cooltime;  %d \n",a_SE->i_cooltime);    /* cooltime-time from start to end of simulation   */
  printf ("maxRabs;   %f \n",a_SE->r_maxRabs);     /* maxrabs-maximum radiant heat input to surface   */
  printf ("heatconst; %d \n",a_SE->i_heatconst);   /* heatconst-time constant for heating, minutes    */
  printf ("coolconst; %d \n",a_SE->i_coolconst);   /* coolconst-time constant for cooling, minutes    */
  printf ("bd;        %f \n",a_SE->r_bd);          /* bd-soil bulk density - g/m3                     */
  printf ("pd;        %f \n",a_SE->r_pd);          /* pd-soil particle density - g/m3                 */
  printf ("xo;        %f \n",a_SE->r_xo);          /* xo-extrapolated water cont. at -1 J/kg          */
  printf ("ls;        %f \n",a_SE->r_ls);          /* ls-thermal conductivity of mineral fraction     */
  printf ("ga;        %f \n",a_SE->r_ga);          /* ga-de Vries shape factor                        */
  printf ("xwo;       %f \n",a_SE->r_xwo);         /* xwo-water content for liquid recirculation      */
  printf ("cop;       %f \n",a_SE->r_cop);         /* cop-power for recirculation function            */
  printf ("dt;        %d \n",a_SE->i_dt);          /* dt-time step - s                                */
  printf ("startwc;   %f \n",a_SE->r_startwc);     /* startwc-starting soil water content - m3/m3     */
  printf ("starttemp; %f \n",a_SE->r_starttemp);   /* starttemp-starting soil temperatue - C          */

  for ( i = 0; i < e_mplus1+1; i++ )
    printf ("%d:  %f - %f \n", i, a_SE->rr_z[i], a_SE->rr_node[i]);

}

#endif
