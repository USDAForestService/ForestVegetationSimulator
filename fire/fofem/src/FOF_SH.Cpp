/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_sh.c
* Desc: Soil Heating
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/

#include <stdlib.h>
#include <string.h>
#include <math.h>
#include <stdio.h>

#include "fof_ansi.h"
#include  "fof_sgv.h"

#include  "fof_sh.h"
#include  "fof_sh2.h"
#include  "fof_sd.h"
#include  "fof_se.h"
#include  "fof_soi.h"
#include  "fof_sha.h"

void  SO_Load (d_SI *a_SI, d_SO *a_SO);

char  gcr_SoiErr[1000];                      /* See Note-1 below             */

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SH_Mngr
* Desc: Run Soil simulation. Will run the Duff or the Exp simulation
*        code, based on presence of Duff.
* Note Duff Sim:
*        The Fuel has to be run before coming here, because the Fuel
*        calculates the Post Duff Depth which DUff Sim needs to run.
* Note Exp Heat:
*        The Fuel has to be run before coming here, which should
*        have detected the no Duff Depth/Load and run burnup which calculates
*        the heat and time need by Exp Heat.
* Note-1: There use to be some Error_Window() type logic errors in the
*          soil code, it would have been hard and not worth it to do them
*          thru the functions so I did a global string.
*   In: a_SI......
*
*  Ret: 1 Ok,   0 Error
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
int  SH_Mngr (d_SI *a_SI, d_SO *a_SO, float fr_FI[], char cr_TmpFN[], char cr_ErrMes[])
{
d_SD s_SD;
d_SE s_SE;
char cr[40];

  strcpy (gcr_SoiErr,"");
  SO_Init (a_SO);                            /* Init the output struct       */

  if ( !xstrcmpi(a_SI->cr_BrnIg,"NO")) {      /* Burnup ran & didn't ignite   */
    SHA_Init_0 ();                           /* so 0 out this arrary so that */
    return 1; }                              /* 0s come out in the report    */

  SHA_Init ();                               /* Init the Soil Heat Temp Array*/

  if ( a_SI->f_DufDepPre > 0 )               /* Prefire Duff depth determines*/
     goto DuffSim;                           /* if we run Duff or Exp simulat*/

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Do the Exp Heat, because there is no duff depth                           */
  strcpy (a_SO->cr_Model,e_SM_ZDuff);           /* tells what soil model     */

  if ( !SE_Init (a_SI, &s_SE, cr_ErrMes))       /* Ready the SE input struct */
     return 0;

  if ( !SE_Mngr_Array (&s_SE,fr_FI, cr_TmpFN,cr_ErrMes))     /* Run it, makes Pt arrar& File */
     return 0;

  goto Load;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Do the Duff Simulation Modes                                              */
DuffSim:
  strcpy (a_SO->cr_Model,e_SM_Duff);

  if ( !SD_ChkSoiDuf(a_SI->f_DufDepPre, a_SI->f_DufDepPos, cr_ErrMes) )
    return 0;

  if ( !SD_Init (&s_SD, a_SI, cr_ErrMes))
     return 0;

/* Duff Sim is done when there is a Duff Depth to use....................... */
  if ( !SD_Mngr(&s_SD,cr_TmpFN,cr_ErrMes))   /* Run Soil Duff Simulation     */
    return 0;

Load:
  SO_Load (a_SI, a_SO);                      /* Get Soil Outputs             */
  if ( strcmp (gcr_SoiErr,"") ) {            /* See Note-1 above             */
     strcpy (cr_ErrMes,gcr_SoiErr);
     return 0; }
  return 1;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SO_Load
* Desc: Load up the Soil Output structure.
*       Gets the Maximum Temperature found in each layer and the time they
*        occured at.
*       Get the deepest depth that a certain temperature reached, there
*        are 2 degrees we do this for, see below, and the defines
*       Also set the Pre & Post fire Duff Depths in centimeters, just in
*        case user wants them.
* NOTE: Soil stuff is done in Celcius, Centimeters and Minutes and that is
*       how FOFEM5 reports out, in those units
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void SO_Load (d_SI *a_SI, d_SO *a_SO)
{
int i,j, i_Time;
REAL  r,r_Max;
float f;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Percent of heat that passes thru Duff during fire                         */
   if ( !strcmp (a_SO->cr_Model,e_SM_Duff)) /* Only relevant on Duff Model   */
      a_SO->f_Heatpc = SD_Heatpercent();

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Soil Depths, get them to Centimeters                                      */
   f = a_SI->f_DufDepPre;                   /* Duff Depth Before Fire       */
   a_SO->f_cDufPre = InchtoMeter (f) * 100; /* Inch to Centimeters          */

   f = a_SI->f_DufDepPos;                   /* Duff Depth Afte Fire         */
   a_SO->f_cDufPost= InchtoMeter (f) * 100; /* Inch to Centimeters          */

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Find Maximum Temp for each layer                                          */
   for ( i = 1; i <= eC_Lay; i++ ) {         /* Check each Layer             */
     r_Max = 0;
     for ( j = 1; j < eC_Tim; j++ ) {        /* For each Temp in Layer       */
        r = SHA_Get(i,j);                    /* Get Temp                     */
        if ( r == e_SHA_Init )               /* Skip init values             */
          continue;
        if ( r > r_Max ){                    /* Look for highest             */
           r_Max = r;
           i_Time = j; } }
      a_SO->ir_Temp[i-1] = r_Max;
      a_SO->ir_Time[i-1] = i_Time; }

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Do Max Temp at Layer, put to report and save for needed totals later      */
   a_SO->i_LayMaxDeg1 = e_LayNone;
   a_SO->i_LayMaxDeg2 = e_LayNone;
   for ( i = 1; i <= eC_Lay; i++ ) {             /* For each Layer          */
     for ( j = 1; j < eC_Tim; j++ ) {            /* For each Temp in Layer   */
        r = SHA_Get(i,j);                        /* Get Temp                 */
        if ( r == e_SHA_Init )                   /* Skip init values         */
          continue;
        if ( r > e_Max1 )
          a_SO->i_LayMaxDeg1 = i-1;
        if ( r > e_Max2 )
          a_SO->i_LayMaxDeg2 = i-1; } }
}


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Inializing values for the Layer and Display arrays                        */
/* These get copied into the d_SI.s_SD input struct                            */
/* These define the soil layers for simulation                               */
REAL rr_Lay[e_mplus1+1] = { 0,  0,   10, 20,   30, 40,  50, 60,
                           70, 80,  90, 100,  110,120, 130 };
/* These tell which layers should be outputed                                */
REAL rr_Dis[e_mplus1+1] = { 0, 1, 1, 1, 1, 1, 1, 1,
                            1, 1, 1, 1, 1, 1, 1 };

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SH_Init_LayDis
* Desc: Initialize the Soil Layer and Display arrays that need to be inputed
*        into the Soil simulations
*  Ret:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void SH_Init_LayDis (REAL rrL[], REAL rrD[])
{
int i;
   for ( i = 0; i <= e_mplus1; i++ ) {
     rrL[i] = rr_Lay[i];
     rrD[i] = rr_Dis[i]; }
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SO_Init
* Desc: Init the Soil Output Struct.
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void  SO_Init (d_SO *a_SO)
{
int i;
   for ( i = 0; i < e_Layers; i++ ) {
      a_SO->ir_Temp[i] = 0;
      a_SO->ir_Time[i] = 0; }

   a_SO->i_LayMaxDeg1 = e_LayNone;
   a_SO->i_LayMaxDeg2 = e_LayNone;

   a_SO->f_cDufPre = 0;
   a_SO->f_cDufPost = 0;
   a_SO->f_Heatpc   = 0;

}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: InchtoMeter
* Desc: Conver Inches to Meter, Soil Code uses duff depth as a fractional
*        portion of a meter, example .08 of a meter
*   In: inches
*  Ret: Meters
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
REAL  InchtoMeter (REAL r_Inch)
{
REAL r, r_InchinMeter;
  r_InchinMeter = 39.37;
  r = r_Inch / r_InchinMeter;
  return r;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SI_Init
* Desc: Init a Soil Input Struct.
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void  SI_Init (d_SI *a_SI)
{
   strcpy (a_SI->cr_BrnIg,"YES");
   strcpy (a_SI->cr_SoilType,"");
   strcpy (a_SI->cr_MoistCond,"");
   a_SI->f_DufDepPre = 0;
   a_SI->f_DufDepPos = 0;
   a_SI->f_SoilMoist = 0;
   a_SI->i_Time = 0;
   a_SI->i_Cnt = 0;
   a_SI->f_fi = 0;
}



/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SI_FilOpen
* Desc: This function is to allow for the
*         FRAMES Web Fofem application which is built
*         to run with no interface and runs using stdin and stdout
*         Sending in the e_FrameSFN file name will cause the soil values
*         to go out the stdout using printf's rather than to a file.
*         This was an easy way to get the soils out with out trying
*         to save them, REMEMBER theses soil values are generated
*         as soil simulation is doing its time steps.
*
*         See SE_Mngr()  SD_Mngr()
*          I set these functions up so that I can simply replace the
*           fopen,fprintf where they get used, without changing any
*           'if' or logic or even the number or parameters pass to the
*           fopen,fprintf functions
*
*   In: cr_FN......File name or the Frame Soil File Name signal
*  Ret: file handle or Frame signal
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
FILE *SI_fopen (char cr_FN[], char cr[])
{
  if ( !strcmp (cr_FN,e_FrameSFN)){
    printf ("\n%s\n",e_FrameSstart);
    return e_FrameSfh;}

  return fopen (cr_FN,cr);
}

void SI_fprintfVal (FILE *fh, char cr_Fmt[], float r_time)
{
  if ( fh == e_FrameSfh )
     printf (cr_Fmt,r_time);
  else
     fprintf (fh, cr_Fmt, r_time);
}

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
void SI_fprintfRet (FILE *fh, char cr_Ret[])
{
  if ( fh == e_FrameSfh )
    printf (cr_Ret);
  else
    fprintf (fh, cr_Ret);
}

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
void SI_fclose  (FILE *fh)
{
  if ( fh == e_FrameSfh ) {
    printf ("%s\n",e_FrameSend);
    return; }

  fclose (fh);
}
