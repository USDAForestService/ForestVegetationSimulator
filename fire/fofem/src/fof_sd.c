//
// $Id
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fofs_sd.c
* Desc: Soil Duff Simulation
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


#include "fof_sh.h"
#include "fof_sh2.h"
#include "fof_sd.h"
#include "fof_sd2.h"
#include "fof_ansi.h"

#include "fof_sha.h"
#include "fof_soi.h"


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* This is percent of heat that makes it thru the duff during fire           */
/* see code where this get set, this is global so we can save it and make    */
/* it available to other parts of program via function defined herein        */
float  gf_Heatpc = 0;

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SD_Mngr
* Desc: Soil Duff Simulation. This simulation is done when there is Duff
*        prefire Duff present
* NOTE: output from the soil simulation gets returned indirectly thru
*        the SHA_* table and functions
* Note-1: The orginal soil code read this inputs from an input file, so
*          I left alot of the variables in place and just made assignments
*          to them from the inputs I send in now.
*   In: a_SD.....see the SD_Init() function,
*  Ret: 1 OK
*       0 Error - Couldn't open file, OR
*                Soil Simulation was unable to be done with the set
*                condtions, see notes in fof_soi.c soiltemp_step ()
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
int  SD_Mngr (d_SD *a_SD, char cr_TmpFN[], char cr_ErrMes[])
{
int   i;
int   i_midburn, i_burntime, i_stoptime, i_duffdensity, i_dt;
REAL  r_duffdepth, r_bd, r_pd, r_xo, r_ls, r_ga;
REAL  r_xwo, r_cop, r_starttemp, r_startwc, r_duffheat;

REAL  r_Rabs, r_Qo, r_qm, r_time, r_Rabsub, r_simdt;
REAL  rr_z[e_mplus1+1],  rr_w[e_mplus1+1], rr_t[e_mplus1+1];
int   i_success, i_Tim;
FILE  *fh_Out;

  strcpy (cr_ErrMes,"");

  if ( !strcmp (cr_TmpFN,"") ) {             /* Tmp output File Name         */
     fh_Out = NULL;                          /*  Not doing                   */
     goto A; }

  fh_Out = fopen (cr_TmpFN,"w");            /* Open the Soil Points file   */
  if ( fh_Out == NULL ) {
    sprintf (cr_ErrMes, "Can't open output file:  %s\n", cr_TmpFN);
    return 0; }

A:
  i_midburn     =  a_SD->i_midburn;          /* See Note-1 above             */
  i_burntime    =  a_SD->i_burntime;
  i_stoptime    =  a_SD->i_stoptime;
  i_duffdensity =  a_SD->i_duffdensity;
  i_dt          =  a_SD->i_dt;
  r_duffdepth   =  a_SD->r_ConDufDep;
  r_bd          =  a_SD->r_bd;
  r_pd          =  a_SD->r_pd;
  r_xo          =  a_SD->r_xo;
  r_ls          =  a_SD->r_ls;
  r_ga          =  a_SD->r_ga;
  r_xwo         =  a_SD->r_xwo;
  r_cop         =  a_SD->r_cop;
  r_starttemp   =  a_SD->r_starttemp;
  r_startwc     =  a_SD->r_startwc;
  r_duffheat    =  a_SD->r_duffheat;

  r_Rabs = 5.67e-8 * sqr ( sqr (r_starttemp + 273.0) );
  soiltemp_initconsts (r_bd, r_pd, r_ls, r_ga, r_xwo, r_cop, r_xo,
                       a_SD->rr_z);
  soiltemp_getdepths (rr_z);

  for ( i = 0; i <= e_mplus1; i++ ) {
    rr_w[i] = r_startwc;
    rr_t[i] = r_starttemp;  }

  soiltemp_initprofile (rr_w,rr_t);

  r_Qo = r_duffheat * r_duffdepth * i_duffdensity;
  r_qm = 0.57 * r_Qo / ( i_burntime * 60 );  /* max. rate of heat production; W/m2} */
  r_time = 0;

  while ( r_time < i_stoptime ) {

    r_Rabsub = r_Rabs + r_qm * exp ( -sqr ( (i_midburn - r_time ) / i_burntime));
    r_simdt = i_dt;
    i_success = 0;

    while (1) {
      if ( !soiltemp_step(r_Rabsub,i_dt,&i_success,r_time)){ /* simulate a time step  } */
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

    if ( fh_Out != NULL )                    /* if Soil Pts file open         */
      fprintf (fh_Out,"%6.1f",r_time);         /* Put out Soil Pts file        */

    for ( i = 1; i <= e_mplus1; i++ ) {
      i_Tim = r_time;
      if ( a_SD->rr_node[i] == 1  ) {
        SHA_Put (i,i_Tim, rr_t[i]);
        if ( fh_Out != NULL )                    /* if Soil Pts file open         */
          fprintf (fh_Out,"%6.1f", rr_t[i]);
       }
    }

    if ( fh_Out != NULL )                    /* if Soil Pts file open         */
      fprintf (fh_Out,"\n");

 }  /* while end */

Close:
  if ( fh_Out != NULL ) {                  /* if Soil Pts file open         */
    fclose (fh_Out);
    fh_Out = NULL; }

  if ( strcmp (cr_ErrMes,"") )          /* Error Occured                     */
    return 0;

  return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SD_Init
* Desc: Most of the init values come from the sr_SE table, selected
*        based on the Soil Type name.
*       Other inputs come from user in the s_SI struct
* Note-1: We'll be running Duff Sim because we had some duff but the fire
*         didn't consume any of it so we fake a small amount to create a
*         little bit of heat
* Note-2: It was decided that any remaining/post duff depth that we get from
*         our fuel calculations would act as an insulator allowing only
*         some much heat to pass thru, so these lines of code deal with that.
*   In: a_SI......soil input structure
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
int  SD_Init (d_SD *a_SD,  d_SI *a_SI,  char cr_ErrMes[])
{
int i;
REAL  r, r_Post, r_Pre, r_Con;

   gf_Heatpc = 0;                                /* Heat percent            */

/* Use Soil Type, get Duff Sim param record from table                       */
   for ( i = 0;  i < 100; i++ ) {                /* find match in table     */
     if ( !strcmp (sr_SD[i].cr_Name,"")) {       /* Hit end and didn't find */
       sprintf (cr_ErrMes,"SD_Init() - Invalid Soil Type: %s ", a_SI->cr_SoilType);
       return 0; }
     if ( !strcmp (sr_SD[i].cr_Name,a_SI->cr_SoilType)){ /* Copy out                */
       memcpy (a_SD,&sr_SD[i],sizeof(d_SD));
       break; } }

/* Set burntimes based on Moisture Condtion                                  */
   if (!xstrcmpi (a_SI->cr_MoistCond,e_Wet)){
     a_SD->i_burntime = 240;
     a_SD->i_midburn  = 300;}

   else if (!xstrcmpi (a_SI->cr_MoistCond,e_Moderate)) {
     a_SD->i_burntime = 120;
     a_SD->i_midburn  = 200;}

   else if ( !xstrcmpi (a_SI->cr_MoistCond,e_VeryDry) ||
             !xstrcmpi (a_SI->cr_MoistCond,e_Dry)) {
     a_SD->i_burntime = 80;
     a_SD->i_midburn  = 150;}
   else {
      sprintf (cr_ErrMes,"Invalid Moisture Condition: %s", a_SI->cr_MoistCond);
      return 0; }

/* Check Soil Moisture and set starting water content                        */
   if ( a_SI->f_SoilMoist > e_SMV_Max || a_SI->f_SoilMoist < e_SMV_Min ){
      sprintf (cr_ErrMes,"Soil Moisture %6.2f is out of range (%3.0f to %3.0f)", a_SI->f_SoilMoist, e_SMV_Min, e_SMV_Max);
      return 0; }
   a_SD->r_startwc = a_SI->f_SoilMoist / 100;    /* get it in decimal       */

   r_Post = a_SI->f_DufDepPos;               /* Post Duf Dep from Fuel Calc  */
   r_Pre = a_SI->f_DufDepPre;                /* Pre-Fire Duff Depth          */
   r_Con = r_Pre - r_Post;                   /* Consumed Duff Depth          */

/* Set the Duff Consumed amount                                              */
   if ( r_Con == 0 )                         /* see Note-1 above             */
     r_Con = e_MinDufCon;
   a_SD->r_ConDufDep = InchtoMeter (r_Con);  /* Need it in Meter fraction    */

/* Set Duff Heat Content                                                     */
   r = SD_HeatAdj (r_Post);                  /* based on remaining duf dep   */
   gf_Heatpc = r;                            /* save this                    */
   a_SD->r_duffheat = a_SD->r_duffheat * r;  /* use % of heat to use, Note-2 */

/* Init the Layer and Display arrays....                                     */
   SH_Init_LayDis (a_SD->rr_z, a_SD->rr_node);

   return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SD_Heatpercent
* Desc: Return the percent of heat that makes it thru post duff depth
*  Ret: percent
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
float  SD_Heatpercent()
{
   return gf_Heatpc;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SD_HeatAdj
* Desc: Calculate the percent of heat that passes thru a duff layer.
*       DJ ran a regression to get this formula,
*       We use this to determine how much heat passes thru the remaining
*        duff which is acting as an insulator during the fire.
*       when post duff depth is zero than all the heat (100 percent)
*       goes into the soil. Once the post duff depth layer gets to
*       around 6 the percent is 0 meaning no heat is getting thru.
* NOTE: you send in the post duff depth in INCHES
*   In: r_Post.....post duff depth in INCHES
*  Ret: percent of heat
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
REAL SD_HeatAdj (REAL r_Post)
{
REAL r, x, y0,a,b,c,d;

   x = InchtoMeter (r_Post);                 /* Inches to Meter              */
   x = x * 100;

   y0 = -1.6996;
    a = 32.7652;
    b = 7.4601;
    c = 68.9349;
    d = 0.6077;
   r = y0 + (a * exp(-b*x)) + (c * exp(-d*x));
   if ( r < 0 )                              /* this will go negative        */
     r = 0;                                  /* when x is around 6 >         */
   if ( r > 100 )                            /* this shouldn't happen        */
     r = 100;                                /* but better check             */
   r = r * 0.01;                             /* get to decimal percent       */
   return r;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: _ChkSoiDuf
* Desc: Check the Pre and Post fire depth.
*   In: see below
*  Ret: 1 ok, 0 error
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int SD_ChkSoiDuf (float f_DufDepPre, float f_DufDepPos, char cr_ErrMes[])
{
float f_Con;
char cr[300];

  if ( f_DufDepPre < 0 ) {
    sprintf (cr_ErrMes, "Prefire Duff Depth is Negative: %f\n", f_DufDepPre);
    return 0; }

  if ( f_DufDepPos < 0 ) {
    sprintf (cr_ErrMes, "Postfire Duff Depth is Negative: %f\n", f_DufDepPos);
    return 0; }

  if ( f_DufDepPos > f_DufDepPre ) {
    sprintf (cr_ErrMes, "Postfire duff depth (%f) is greater than prefire depth (%f).\n", f_DufDepPos,f_DufDepPre);
    return 0; }

  f_Con = f_DufDepPre - f_DufDepPos;         /* Consumed Duff Depth          */

  if ( f_Con > e_DufDepLim ) {
    sprintf (cr_ErrMes, "Consumed duff depth (%f) exceeds limit (%f). \n", f_Con,e_DufDepLim);
    sprintf (cr, "Consumed duff depth is the difference of prefire and postfire, %f - %f", f_DufDepPre,f_DufDepPos);         /* Consumed Duff Depth          */
    strcat  (cr_ErrMes,cr);
    return 0; }

  if ( f_Con == 0 ) {
    strcpy (cr_ErrMes,"There has been NO duff reduction under these conditions.\n");
    strcat (cr_ErrMes,"Therefore, the effect on soil heating will be minimal.");
    return 1;}

  return 1;
}




#ifdef wow
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SD_Disp
* Desc:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void SD_Disp (d_SD *a_SD)
{
int i;
  printf ("midburn     %d\n", a_SD->i_midburn);        /* midburn 720  1680 900 1100        1000  900  */
  printf ("burntime    %d\n", a_SD->i_burntime);       /* burntime 240  180  300  300        400  400  */
  printf ("stoptime    %d\n", a_SD->i_stoptime);       /* stoptime 1200 2400 1800 1800      1500  1500 */
  printf ("ConDufDep   %f\n", a_SD->r_ConDufDep);      /* consumed duffdepth 0.08 0.06 0.05 0.04 0.07  */
  printf ("duffheat    %f\n", a_SD->r_duffheat);       /* duff heat content - J/m3                     */
  printf ("duffdensity %d\n", a_SD->i_duffdensity);    /* duff density - kg/m3                         */
  printf ("bd          %f\n", a_SD->r_bd);             /* soil bulk density - g/m3                     */
  printf ("pd          %f\n", a_SD->r_pd);             /* soil particle density - g/m3                 */
  printf ("xo          %f\n", a_SD->r_xo);             /* extrapolated water cont. at -1 J/kg          */
  printf ("ls          %f\n", a_SD->r_ls);             /* thermal conductivity of mineral fraction     */
  printf ("ga          %f\n", a_SD->r_ga);             /* de Vries shape factor                        */
  printf ("xwo         %f\n", a_SD->r_xwo);            /* water content for liquid recirculation       */
  printf ("cop         %f\n", a_SD->r_cop);            /* power for recirculation function             */
  printf ("dt          %d\n", a_SD->i_dt);             /* time step - s                                */
  printf ("startwc     %f\n", a_SD->r_startwc);        /* starting soil water content - m3/m3          */
  printf ("starttemp   %f\n", a_SD->r_starttemp);      /* starting soil temperatue - C                 */

  for ( i = 0; i < e_mplus1+1; i++ )
    printf ("%d:  %f - %f \n", i, a_SD->rr_z[i], a_SD->rr_node[i]);

}
#endif
