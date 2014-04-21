
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_bcm.c    Burnup Consumed Manager
* Desc: Functions to do run Burnup
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "fof_ansi.h"
#include  "bur_brn.h"
#include  "bur_bov.h"
#include "fof_sgv.h"
#include  "fof_co.h"
#include  "fof_ci.h"
#include  "fof_bcm.h"
#include  "fof_lem.h"
#include  "fof_util.h"
#include  "fof_duf.h"

void Fue_CalcPP (float f_Pre, float f_Con, float *af_Pos, float *af_Per, char cr_ErrMes[]);
float _PerCon (float f_Pre, float f_Con);
void  _SetNoIgn (d_CO *a_CO);

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: BCM_Mngr
* Desc: Run Burnup, set & check inputs, run burnup and get the outputs.
* Note-1: This checks inputs before settting them into Burnup, Burnup
*          also has a function for checking inputs, some of the inputs
*          are checked in each case.
*   In: a_CI....Fuel Loading Structure has loads etc.
*  Out: a_CO....Output from Burnup
*  Ret: 1 OK,  2 Burnup didn't ignite,     0 Error
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int  BCM_Mngr (d_CI *a_CI,  d_CO *a_CO, char cr_ErrMes[])
{
int i;
float f_fi;

d_SGV s_SGV; 

  strcpy (cr_ErrMes,"");

  if (!BCM_Check (a_CI,cr_ErrMes))          /* See Note-1 above              */
     return 0;

  BOV_Init ();                              /* Init Remaining Value Struct  */
  BRN_Init ();                              /* Init Burnup variables        */
  BCM_SetInputs (a_CI,a_CO);                /* Give burnup load inputs       */
  if ( !BRN_CheckData(cr_ErrMes) ) {        /* Now chc the burnup input data */
    return 0; }                             /* Error in error message        */

  strcpy (a_CO->cr_BrnIg,"YES");           /* assume burnup will ignite     */
  i = BRN_Run (a_CI->cr_LoadFN,
               a_CI->cr_EmiFN,
               a_CI->f_PerDufCon,           /* percent duff consumed */
               a_CI->f_HSFCon,              /* herb/shrb/fol consumed tpa */
               cr_ErrMes);

  if ( i == 0 )                              /* Error, text in ErrMes        */
    return 0;

/* If Burnup don't ignite, see code and comments below where we deal with this */
  if ( i == 2 )                             /* Burnup didn't ignite         */
    strcpy (a_CO->cr_BrnIg,"NO");

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Litter ..........................                                         */
/* Check, was consumd lit done with Duf Eq for Coast Plain Cov typ           */
/* See Notes in BCM_SetInputs()
/* test-ld larry   */
   if ( a_CO->i_LitEqu == e_CP_PerEq)       /* was done in Duf Eq            */
     goto OneHr;

   a_CO->f_LitPre = a_CI->f_Lit;
   a_CO->i_LitEqu = BCM_Get (e_BOV_Litter, a_CI->f_Lit, &a_CO->f_LitCon);
   Fue_CalcPP (a_CO->f_LitPre, a_CO->f_LitCon, &a_CO->f_LitPos, &a_CO->f_LitPer,cr_ErrMes);

/*  1/4 inch 1 Hr Wood.....................                                  */
OneHr:
   a_CO->f_DW1Pre = a_CI->f_DW1;
   a_CO->i_DW1Equ = BCM_Get (e_BOV_DW1, a_CI->f_DW1, &a_CO->f_DW1Con);
   Fue_CalcPP (a_CO->f_DW1Pre, a_CO->f_DW1Con, &a_CO->f_DW1Pos, &a_CO->f_DW1Per,cr_ErrMes);

/*  1 inch 10 Hr Wood.....................                                  */
   a_CO->f_DW10Pre = a_CI->f_DW10;
   a_CO->i_DW10Equ = BCM_Get (e_BOV_DW10, a_CI->f_DW10, &a_CO->f_DW10Con);
   Fue_CalcPP (a_CO->f_DW10Pre, a_CO->f_DW10Con, &a_CO->f_DW10Pos, &a_CO->f_DW10Per,cr_ErrMes);

/* 1->3 inch 100 Hr Wood.....................                                  */
   a_CO->f_DW100Pre = a_CI->f_DW100;
   a_CO->i_DW100Equ = BCM_Get (e_BOV_DW100, a_CI->f_DW100, &a_CO->f_DW100Con);
   Fue_CalcPP (a_CO->f_DW100Pre, a_CO->f_DW100Con, &a_CO->f_DW100Pos, &a_CO->f_DW100Per,cr_ErrMes);

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                              3 + 1000 Hr Wood                             */
/* Consumed Amounts..................                                        */
  a_CO->f_S3Con =  BCM_Get3 (e_BOV_DW1kSnd,  3, a_CI->f_Snd_DW3);
  a_CO->f_S6Con =  BCM_Get3 (e_BOV_DW1kSnd,  6, a_CI->f_Snd_DW6);
  a_CO->f_S9Con =  BCM_Get3 (e_BOV_DW1kSnd,  9, a_CI->f_Snd_DW9);
  a_CO->f_S20Con = BCM_Get3 (e_BOV_DW1kSnd, 20, a_CI->f_Snd_DW20);

  a_CO->f_R3Con  = BCM_Get3 (e_BOV_DW1kRot,  3, a_CI->f_Rot_DW3);
  a_CO->f_R6Con  = BCM_Get3 (e_BOV_DW1kRot,  6, a_CI->f_Rot_DW6);
  a_CO->f_R9Con  = BCM_Get3 (e_BOV_DW1kRot,  9, a_CI->f_Rot_DW9);
  a_CO->f_R20Con = BCM_Get3 (e_BOV_DW1kRot, 20, a_CI->f_Rot_DW20);

/* Post Loads.........................                                       */
  a_CO->f_S3Pos  = a_CI->f_Snd_DW3  - a_CO->f_S3Con ;
  a_CO->f_S6Pos  = a_CI->f_Snd_DW6  - a_CO->f_S6Con ;
  a_CO->f_S9Pos  = a_CI->f_Snd_DW9  - a_CO->f_S9Con ;
  a_CO->f_S20Pos = a_CI->f_Snd_DW20 - a_CO->f_S20Con;

  a_CO->f_R3Pos  = a_CI->f_Rot_DW3  - a_CO->f_R3Con ;
  a_CO->f_R6Pos  = a_CI->f_Rot_DW6  - a_CO->f_R6Con ;
  a_CO->f_R9Pos  = a_CI->f_Rot_DW9  - a_CO->f_R9Con ;
  a_CO->f_R20Pos = a_CI->f_Rot_DW20 - a_CO->f_R20Con;

/* Pre-burn Loads, we just use the input loads                               */
  a_CO->f_S3Pre  = a_CI->f_Snd_DW3  ;
  a_CO->f_S6Pre  = a_CI->f_Snd_DW6  ;
  a_CO->f_S9Pre  = a_CI->f_Snd_DW9  ;
  a_CO->f_S20Pre = a_CI->f_Snd_DW20 ;

  a_CO->f_R3Pre  = a_CI->f_Rot_DW3  ;
  a_CO->f_R6Pre  = a_CI->f_Rot_DW6  ;
  a_CO->f_R9Pre  = a_CI->f_Rot_DW9  ;
  a_CO->f_R20Pre = a_CI->f_Rot_DW20 ;

/* Percent Consumer.....................                                     */
  a_CO->f_S3Per  = _PerCon (a_CO->f_S3Pre , a_CO->f_S3Con );
  a_CO->f_S6Per  = _PerCon (a_CO->f_S6Pre , a_CO->f_S6Con );
  a_CO->f_S9Per  = _PerCon (a_CO->f_S9Pre , a_CO->f_S9Con );
  a_CO->f_S20Per = _PerCon (a_CO->f_S20Pre, a_CO->f_S20Con);

  a_CO->f_R3Per  = _PerCon (a_CO->f_R3Pre , a_CO->f_R3Con );
  a_CO->f_R6Per  = _PerCon (a_CO->f_R6Pre , a_CO->f_R6Con );
  a_CO->f_R9Per  = _PerCon (a_CO->f_R9Pre , a_CO->f_R9Con );
  a_CO->f_R20Per = _PerCon (a_CO->f_R20Pre, a_CO->f_R20Con);


/* Total 3+ 1000, Equation #, Consumed, Pre-Load, Post-Load, Percent consmed */
  a_CO->i_Snd_DW1kEqu = e_BurEqu ;
  a_CO->f_Snd_DW1kCon = a_CO->f_S3Con + a_CO->f_S6Con + a_CO->f_S9Con + a_CO->f_S20Con;
  a_CO->f_Snd_DW1kPre = a_CI->f_Snd_DW3 + a_CI->f_Snd_DW6 + a_CI->f_Snd_DW9 + a_CI->f_Snd_DW20;
  a_CO->f_Snd_DW1kPos = a_CO->f_S3Pos + a_CO->f_S6Pos + a_CO->f_S9Pos + a_CO->f_S20Pos;
  if ( a_CO->f_Snd_DW1kPre != 0 )
     a_CO->f_Snd_DW1kPer = (a_CO->f_Snd_DW1kCon / a_CO->f_Snd_DW1kPre) * 100;
  else
     a_CO->f_Snd_DW1kPer = 0;

  a_CO->i_Rot_DW1kEqu = e_BurEqu ;
  a_CO->f_Rot_DW1kCon = a_CO->f_R3Con + a_CO->f_R6Con + a_CO->f_R9Con + a_CO->f_R20Con;
  a_CO->f_Rot_DW1kPre = a_CI->f_Rot_DW3 + a_CI->f_Rot_DW6 + a_CI->f_Rot_DW9 + a_CI->f_Rot_DW20;
  a_CO->f_Rot_DW1kPos = a_CO->f_R3Pos + a_CO->f_R6Pos + a_CO->f_R9Pos + a_CO->f_R20Pos;
  if ( a_CO->f_Rot_DW1kPre != 0 )
     a_CO->f_Rot_DW1kPer = (a_CO->f_Rot_DW1kCon / a_CO->f_Rot_DW1kPre) * 100;
  else
     a_CO->f_Rot_DW1kPer = 0;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Get Emissions                                                             */
   a_CO->f_PM10F = GramSqMt_To_Pounds ( ES_PM10F() );
   a_CO->f_PM25F = GramSqMt_To_Pounds ( ES_PM25F() );
   a_CO->f_CH4F  = GramSqMt_To_Pounds ( ES_CH4F () );
   a_CO->f_COF   = GramSqMt_To_Pounds ( ES_COF  () );
   a_CO->f_CO2F  = GramSqMt_To_Pounds ( ES_CO2F () );
   a_CO->f_NOXF  = GramSqMt_To_Pounds ( ES_NOXF () );
   a_CO->f_SO2F  = GramSqMt_To_Pounds ( ES_SO2F () );

   a_CO->f_PM10S = GramSqMt_To_Pounds ( ES_PM10S() );
   a_CO->f_PM25S = GramSqMt_To_Pounds ( ES_PM25S() );
   a_CO->f_CH4S  = GramSqMt_To_Pounds ( ES_CH4S () );
   a_CO->f_COS   = GramSqMt_To_Pounds ( ES_COS  () );
   a_CO->f_CO2S  = GramSqMt_To_Pounds ( ES_CO2S () );
   a_CO->f_NOXS  = GramSqMt_To_Pounds ( ES_NOXS () );
   a_CO->f_SO2S  = GramSqMt_To_Pounds ( ES_SO2S () );

   a_CO->f_FlaCon = KgSq_To_TPA (ES_FlaCon ());  /* Total Consumed amount    */
   a_CO->f_SmoCon = KgSq_To_TPA (ES_SmoCon ());

   a_CO->f_FlaDur = ES_FlaDur ();     /* duration, see explaination in       */
   a_CO->f_SmoDur = ES_SmoDur ();     /* d_CO definition                     */

/* Copy the emission time table to the CO output struct */
   for ( i = 0; i < eC_SGV; i++ ) {
      SGV_Get(i,&s_SGV);
      memcpy (&a_CO->sr_SGV[i], &s_SGV, sizeof(d_SGV)); }
   a_CO->iC_SGV = eC_SGV;    /* siz of table */

/* Get the fire intensity and duration of burnup simulation, see define      */
/* 3-14-05 Change, put in check for 0 division                               */
   a_CO->i_Cnt = BOV_Get_TimHea (&a_CO->i_Time,&f_fi,a_CO->cr_SHC);
   if ( a_CO->i_Cnt != 0 )
     a_CO->f_fi = f_fi / (float) a_CO->i_Cnt;  /* avg fire intensity           */
   else
     a_CO->f_fi = 0;

/* If Burnup didn't ignite, we'll need to 0 the consumed totals and percents */
/* We wait to do it here, so that the Pre-loads and equation, etc get set    */
/*  above, we still want to know what they were                              */
   if ( !xstrcmpi(a_CO->cr_BrnIg,"NO") ) {
      _SetNoIgn (a_CO);
      return 2; }

  return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: BCM_SetInputs
* Desc: Load up the Burnup variables directly into burnup's variable/arrays.
* Note-1: Normally Litter consumption is done in burnup, however under
*          the southeast coastal plain longleaf pine cover types, litter
*          consumption is done in the Duff consumption equation before
*          coming here; in which case we send in that amount of consumed
*          litter and make the assumption that burnup will consume the
*          entire amount.
*         After burnup is done we skip over getting the litter consumd
*           amounts burnup has produced
*    ---> The reason we still need to send in the litter is so that burnup
*          can do emissons with it.
*
* Note-2: This will make sure that there is always at something
*           in one of the Down Wood loads. So we put a small amount in 1 hr.
*           Burnup flips out if for example you send in only litter,
*           Also, it will let burnup run with no other down wood loads
*           so it can deal with Duff and other Loads that
*           are needed for emissions stuff.
*           The small amount will not show up any where in consumed loads
*            and basically come back look like 0.
*   In: a_CI.....Fuel Load Struct, see the callers function heading notes
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  BCM_SetInputs (d_CI *a_CI, d_CO *a_CO)
{
int    iX;
float  f_MoistDW10, f_Load, f_Moi;
float  f_MoiSnd, f_MoiRot;
double  d_Duff, d_MoistDuff;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Set burnups Duff and other params.........................................*/
   d_Duff = TPA_To_KiSq (a_CI->f_Duff);            /* Duff Load, to Kilogrms Meter */
   d_MoistDuff = a_CI->f_MoistDuff / (float) 100;  /* Duff Moisture                */
   BRN_SetFireDat((long)   a_CI->l_MAX_TIMES,
                  (double) a_CI->f_INTENSITY,
                  (double) a_CI->f_IG_TIME,
                  (double) a_CI->f_WINDSPEED,
                  (double) a_CI->f_DEPTH,
                  (double) a_CI->f_AMBIENT_TEMP,
                  (double) a_CI->f_R0,
                  (double) a_CI->f_DR,
                  (double) a_CI->f_TIMESTEP,
                   d_Duff,
                   d_MoistDuff);

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Set Fuel Loads, and related params.                                       */

   f_MoistDW10 = ( a_CI->f_MoistDW10 /  100.0 ) ;
   iX = 0;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Litter - See Note-1 above                                                 */
/* Check if litter was already done in the Coastal Plain Duff Eq             */
/* test-ld larry */
    if ( a_CO->i_LitEqu == e_CP_PerEq) {     /* Coast Plain - use prevusly   */
      f_Load = TPA_To_KiSq (a_CO->f_LitCon); /* calc lit amount              */
      f_Moi = a_CI->f_LitMoi / 100.0; }      /* and have lit moist so use it */
    else {
      f_Load = TPA_To_KiSq (a_CI->f_Lit);    /* Use input lit load as usual  */
      f_Moi = BCM_DW10M_Adj ("DW1", f_MoistDW10);}

    if ( f_Load <= 0 )
      goto OneHr;

    if ( BRN_SetFuel (&iX, "SND", f_Load, f_Moi, a_CI->f_SURat_Lit) )
      BOV_Entry (e_BOV_Litter,0);


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* DW 0->1/4 - 1 Hr                                                          */
OneHr:
    f_Load   = TPA_To_KiSq (a_CI->f_DW1);
    if ( f_Load == 0 )                       /* See Note-2 above             */
      f_Load = 0.0000001;
    f_Moi = BCM_DW10M_Adj ("DW1", f_MoistDW10);
    if ( BRN_SetFuel (&iX, "SND", f_Load, f_Moi, a_CI->f_SURat_DW1) )
      BOV_Entry (e_BOV_DW1,0);

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* DW 1/4->1 - 10 Hr                                                         */
    f_Load = TPA_To_KiSq (a_CI->f_DW10);
    f_Moi = BCM_DW10M_Adj ("DW10", f_MoistDW10);
    if ( BRN_SetFuel (&iX,"SND",f_Load,f_Moi,a_CI->f_SURat_DW10) )
      BOV_Entry (e_BOV_DW10,0);

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* DW 1->3 100 Hr                                                            */
    f_Load   = TPA_To_KiSq (a_CI->f_DW100);
    f_Moi = BCM_DW10M_Adj ("DW100", f_MoistDW10);
    if ( BRN_SetFuel (&iX,"SND",f_Load,f_Moi,a_CI->f_SURat_DW100) )
      BOV_Entry (e_BOV_DW100,0);

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*  1000 Hr.  3+ Wood                                                        */

    f_MoiSnd = a_CI->f_MoistDW1000 / (float) 100;
    f_MoiRot = BCM_DW1k_MoiRot(f_MoiSnd);

/* DW 3 -> 6 ................................................................*/
    f_Load   = TPA_To_KiSq (a_CI->f_Snd_DW3);      /* Sound portion           */
    if ( BRN_SetFuel (&iX,"SND",f_Load, f_MoiSnd, a_CI->f_SURat_DWk_3_6) )
       BOV_Entry (e_BOV_DW1kSnd,3);

    f_Load = TPA_To_KiSq (a_CI->f_Rot_DW3);
    if ( BRN_SetFuel (&iX,"ROT",f_Load, f_MoiRot, a_CI->f_SURat_DWk_3_6) )
      BOV_Entry (e_BOV_DW1kRot,3);

/* DW 6 -> 9 ................................................................*/
    f_Load = TPA_To_KiSq (a_CI->f_Snd_DW6);
    if ( BRN_SetFuel (&iX,"SND",f_Load,f_MoiSnd,a_CI->f_SURat_DWk_6_9) )
      BOV_Entry (e_BOV_DW1kSnd,6);

    f_Load = TPA_To_KiSq (a_CI->f_Rot_DW6);
    if ( BRN_SetFuel (&iX,"ROT",f_Load,f_MoiRot,a_CI->f_SURat_DWk_6_9) )
      BOV_Entry (e_BOV_DW1kRot,6);

/* DW 9 -> 20 ..............................................................*/
    f_Load = TPA_To_KiSq (a_CI->f_Snd_DW9);
    if ( BRN_SetFuel (&iX,"SND",f_Load,f_MoiSnd,a_CI->f_SURat_DWk_9_20) )
       BOV_Entry (e_BOV_DW1kSnd,9);

    f_Load = TPA_To_KiSq (a_CI->f_Rot_DW9);
    if ( BRN_SetFuel (&iX,"ROT",f_Load,f_MoiRot,a_CI->f_SURat_DWk_9_20) )
      BOV_Entry (e_BOV_DW1kRot,9);

/* DW 20 -> ................................................................*/
    f_Load = TPA_To_KiSq (a_CI->f_Snd_DW20);
    if ( BRN_SetFuel (&iX,"SND",f_Load,f_MoiSnd,a_CI->f_SURat_DWk_20) )
      BOV_Entry (e_BOV_DW1kSnd,20);

    f_Load = TPA_To_KiSq (a_CI->f_Rot_DW20);
    if ( BRN_SetFuel (&iX,"ROT",f_Load,f_MoiRot,a_CI->f_SURat_DWk_20) )
      BOV_Entry (e_BOV_DW1kRot,20);

}



/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: BCM_DW10M_Adj
* Desc: Get the Down Woody 1, 10 & 100 Hr Moisture, adjusted from the
*        10 Hr.
* NOTE: please see sibling function that checks limits
*   In: cr_DWHr....see the code below
*        f_Moist...Down Woody 10 Hr, NOTE this is in decimal fraction
*  Ret: adjust moisture
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float  BCM_DW10M_Adj (char cr_DWHr[], float f_Moist)
{
   if ( !strcmp (cr_DWHr,"DW1") )
      return  (f_Moist - e_DW1hr_Adj);

   else if ( !strcmp (cr_DWHr,"DW10") )
     return  f_Moist;

   else if ( !strcmp (cr_DWHr,"DW100") )
     return f_Moist + e_DW100hr_Adj;

   LEM_Put ("Bur_Adj_DW_Moi", "Shouldn't be getting here");
   return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: BCM_DW1k_MoiRot
* Desc: Get the Rotten 1000hr Down Wood Moisture needed for Burnup input
* NOTE: the adjusted amount is not allowed to exceed the burnup limit
*   In: f_Moist...Moisutre, -----> as a decimal fraction
*  Ret: Rotten moisture
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float  BCM_DW1k_MoiRot (float f_Moist)
{
float f,g;
   f = f_Moist * e_DW1000hr_AdjRot;     /* adjust for rotten                 */
   g = e_fms2;                          /* get burnup uppr limit to float    */
   if ( f > g )                         /* don't let it exceed the limit     */
    f = g;                              /* set to upper limit                */
   return f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: BCM_Get3
* Desc: Get an individual 3+ inch wood Consumed load that burnup calculated
* NOTE: Burnup calculates the remaining load so we need to calc the
*        consumed amount using original load.
* Note-1: Make sure that we don't go below 0, I think burnup might send
*          back a remaining load that could be a very small and
*          insignificant fraction great than the load sent in, so this
*          will just round it to 0
*   In: cr_BOV_Cat....Category (e_BOV_DW1kSnd or e_BOV_DW1kRot)
*       i_3InId.......3+ inch Id, use: 3,6,9 or 20
*  Ret: consumed amount
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float  BCM_Get3 (char cr_BOV_Cat[], int i_3InId, float f_Load)
{
float f,g;
  f = BOV_GetRem3 (cr_BOV_Cat, i_3InId);/* Get remaining load from burnup */
  g = KgSq_To_TPA (f);                  /* back to Tons per Acre           */
  f = f_Load - g;                       /* orginal load minus remaining load  */

  if ( f < 0 )                          /* See Note-1 above                  */
    return 0;
  return f;                             /*  gets the consumed amount          */
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: BCM_Get
* Desc: Get the Consumed amount of dead loading.
*       Remember, burnup gets run and it loads the 'remaining' load amounts
*       into the BOV structure. This function gets the amount, converts it
*       back to Tons per Acre, and subtraCO it from the pre-fire load to
*       get the consumed amount.
* NOTE NOTE NOTE:
*   In: cr_BOV_Cat....Category (Litter, DW1, etc. ) see #defines in bur_BOV.h
*       f_Load........pre-fire fuel loading, this is in Tons per Acre
*  Out: af_Con
*  Ret: burnup equation number
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int  BCM_Get (char cr_BOV_Cat[], float f_Load, float *af_Con)
{
float f_RemAmt,f_RemAmtKgSq;

  *af_Con = 0;

  if ( f_Load == 0 )                         /* No pre-fire fuel loading     */
    return e_BurEqu;

  BOV_GetRemAmt (&f_RemAmtKgSq, cr_BOV_Cat); /* Get 'remaining' load amount  */
  f_RemAmt =  KgSq_To_TPA (f_RemAmtKgSq);    /* convert back to Tons Per Acre*/

  if ( f_RemAmt < 0 )                        /* See Note-1 above             */
    f_RemAmt = 0;
  if ( f_RemAmt > f_Load)
    f_RemAmt = f_Load;

  *af_Con  = f_Load - f_RemAmt;
  if ( *af_Con > f_Load ) {
     *af_Con = f_Load; }

  return e_BurEqu;
}

#ifdef WOWwow
******************************************************************************
******************************************************************************
******************************************************************************


  I left function code in because it has the Warning_Window messages in it
  I redid the function above with the messages left out because they never
  really occured much and don t think it matter because we just adjust
  the amounts.

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: CI_Get
* Desc: Get the Consumed amount of dead loading.
*       Remember, burnup gets run and it loads the 'remaining' load amounts
*       into the BOV structure. This function gets the amount, converts it
*       back to Tons per Acre, and subtrata it from the pre-fire load to
*       get the consumed amount.
* NOTE: See the Bur_Run and Bur_CreInFil functions.
* Note-1: Burnup or the Tons-Per-Acre/metric function will calculate a
*         remaining load that is larger than the pre-fire load we send in
*         so I put this check in. The difference I ran into was really small
*         .00006 (something like that), see code I let some stuff go by with
*         no message, but fix all values it catches.
* Note-2: haven't every hit this condtion but am checking just in case
*   In: cr_BOV_Cat....Category (Litter, DW1, etc. ) see #defines in bur_BOV.h
*       f_Load........pre-fire fuel loading, this is in Tons per Acre
*  Out: af_Con
*  Ret: burnup equation number
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int  CI_Get (char cr_BOV_Cat[], float f_Load, float *af_Con, char cr_ErrMes[])
{
float f_RemAmt,f_RemAmtKgSq;
char  cr1[400], cr2[300];

  *af_Con = 0;

  if ( f_Load == 0 )                         /* No pre-fire fuel loading     */
    return e_BurEqu;

  BOV_GetRemAmt (&f_RemAmtKgSq, cr_BOV_Cat); /* Get 'remaining' load amount  */

  f_RemAmt =  KgSq_To_TPA (f_RemAmtKgSq);    /* convert back to Tons Per Acre*/
  if ( f_RemAmt < 0 ) {                      /* Sometime this happens        */
    sprintf (cr1, "Equation %d, calculated a negative consumed load value, will set to 0.\n",e_BurEqu);
    sprintf (cr2, "%s - Pre: %f,   Post:%f ", cr_BOV_Cat, f_Load, f_RemAmt);
    strcat  (cr1,cr2);
    Warning_Window (cr1,"CI_Get");
    f_RemAmt = 0; }

  if ( f_RemAmt > f_Load ) {                 /* see Note-1 in func heading   */
    if( (f_RemAmt - f_Load) > 0.01 ) {
      sprintf (cr1, "Equation %d has calculted a remaing fuel load larger than pre-fire load.\n",e_BurEqu);
      sprintf (cr2, "%s - Pre: %f,   Post:%f ", cr_BOV_Cat, f_Load, f_RemAmt);
      strcat  (cr2, " Will set post load to same as pre load.\n");
      strcat (cr1,cr2);
      Warning_Window (cr1,"CI_Get"); }
    f_RemAmt = f_Load; }

  *af_Con  = f_Load - f_RemAmt;              /* gets the consumed amount     */

  if ( *af_Con > f_Load ) {                  /* see Note-2 in func heading   */
     sprintf (cr1, "Equation %d has calculated a consumed load greater than 100 percent.\n",e_BurEqu);
     sprintf (cr2, "%s - Pre-Fire Load: %f,   Consumed:%f ", cr_BOV_Cat, f_Load, *af_Con);
     strcat  (cr2, " Will set consumed load to 100 percent.\n");
     strcat (cr1,cr2);
     Warning_Window (cr1,"CI_Get");
     *af_Con = f_Load; }

  return e_BurEqu;
}
#endif




/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc:
*   In: a_CI.....Fuel Load Struct
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int BCM_Check (d_CI *a_CI, char cr_ErrMes[])
{
float Low,Hi;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* 10 Hour Moisture                                                          */
/* we have to allow for the adjustments in BCM_DW10M_Adj()                   */
  Low =  (e_fms1 * 100) + ( e_DWChk * 100) ;
  Hi  =  (e_fms2 * 100) - ( e_DWChk * 100) ;
  if ( a_CI->f_MoistDW10 < Low || a_CI->f_MoistDW10 > Hi ) {
     sprintf (cr_ErrMes, "Down Woody 10 Hour Moisture %6.2f is out of limits. ( %6.2f -> %6.2f )",a_CI->f_MoistDW10, Low, Hi);
     return 0; }

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* 1000 Hour Moisture                                                        */
  Low =  (e_fms1 * 100);
  Hi  =  (e_fms2 * 100);
  if ( a_CI->f_MoistDW1000 < Low || a_CI->f_MoistDW1000 > Hi ) {
     sprintf (cr_ErrMes, "Down Woody 1000 Hour Moisture %6.2f is out of limits. ( %6.2f -> %6.2f )", a_CI->f_MoistDW1000, Low, Hi);
     return 0; }

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
  if ( a_CI->f_HSFCon < e_CI_LoadLimLow || a_CI->f_HSFCon > e_CI_LoadLimUp ) {
     sprintf (cr_ErrMes, "Invalid Combined Herb, Shrub, Crown and Branch Foliage  %6.2f \n ( Limit: %6.2f -> %6.2f tons per acre)",
               a_CI->f_HSFCon, e_CI_LoadLimLow, e_CI_LoadLimUp);
     return 0; }

/* check the percent of Duff consumed......                                  */
  if ( !_Pc_DufCon (a_CI->f_PerDufCon, cr_ErrMes))
     return 0;

  if ( !_ChkDuff (a_CI, cr_ErrMes))
     return 0;

  if ( !_Limit(a_CI->f_Lit,  cr_ErrMes, "Litter")) return 0;
  if ( !_Limit(a_CI->f_DW1,  cr_ErrMes, "Down Woody 0 -> 1/4 inch (1 Hour)" )) return 0;
  if ( !_Limit(a_CI->f_DW10, cr_ErrMes, "Down Woody 1/4 -> 1 inch (10 Hour)" )) return 0;
  if ( !_Limit(a_CI->f_DW100,cr_ErrMes, "Down Woody 1 -> 3 inch   (100 Hour)")) return 0;
  if ( !_Limit(a_CI->f_Snd_DW3,  cr_ErrMes, "Down Woody 3->6 inch Sound")) return 0;
  if ( !_Limit(a_CI->f_Snd_DW6,  cr_ErrMes, "Down Woody 6->9 inch Sound")) return 0;
  if ( !_Limit(a_CI->f_Snd_DW9,  cr_ErrMes, "Down Woody 9->20 inch Sound")) return 0;
  if ( !_Limit(a_CI->f_Snd_DW20, cr_ErrMes, "Down Woody 20+ inch Sound")) return 0;
  if ( !_Limit(a_CI->f_Rot_DW3,  cr_ErrMes, "Down Woody 3->6 inch Rotten")) return 0;
  if ( !_Limit(a_CI->f_Rot_DW6,  cr_ErrMes, "Down Woody 6->9 inch Rotten")) return 0;
  if ( !_Limit(a_CI->f_Rot_DW9,  cr_ErrMes, "Down Woody 9->20 inch Rotten")) return 0;
  if ( !_Limit(a_CI->f_Rot_DW20, cr_ErrMes, "Down Woody 20+ inch Rotten")) return 0;
  return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: _Limit
* Desc: Check burnup load limits, used for Litter and Woods
*   In: f_Load
*  Ret: 1 OK,  0 Error
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int _Limit (float f_Load, char cr_ErrMes[], char cr[])
{
  if ( f_Load < e_CI_LoadLimLow || f_Load > e_CI_LoadLimUp ) {
    sprintf (cr_ErrMes,"The %s load (%6.2f)is out limits ( %4.0f -> %4.0f)",
              cr, f_Load, e_CI_LoadLimLow,e_CI_LoadLimUp);
    return 0; }
  return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: _ChkDuff
* Desc: Check limits for Duff loading and Duff Moisture
*   In: a_CI...
*  Ret: 1 OK,  0 Error
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int _ChkDuff (d_CI *a_CI,  char cr_ErrMes[])
{
float f,Low,Hi;

/* test larry */
   if ( a_CI->f_Duff == 0 )
     return 1;

   f = (float) e_wdf1;
   Low = KgSq_To_TPA (f);

   f = (float) e_wdf2;
   Hi  = KgSq_To_TPA (f);

   if ( a_CI->f_Duff < Low || a_CI->f_Duff > Hi ) {
     sprintf (cr_ErrMes,"_ChkDuff() - Duff Fuel Load %6.2f is out limit. (%6.2f -> %6.2f tons per acre)", a_CI->f_Duff, Low, Hi);
     return 0; }

   Low = (e_dfm1 * 100);               /* User sets them as whole numbers    */
   Hi  = (e_dfm2 * 100);               /*  so check them as such             */
   if ( a_CI->f_MoistDuff < Low || a_CI->f_MoistDuff > Hi ) {
     sprintf (cr_ErrMes,"_ChkDuff() - Duff Moisture %6.2f is out limit. (%6.2f -> %6.2f)", a_CI->f_MoistDuff, Low, Hi);
     return 0; }

   return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: _Pc_DufCon
* Desc: Check the Percent of duff consumed that burnup will use
*   In: f_Pc......percent of duff consumed
*  Ret: 1 OK,  0 Error
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int _Pc_DufCon (float f_Pc, char cr_ErrMes[])
{
  if ( f_Pc == -1 )                     /* This is ok, it tells burnup to    */
    return 1;                           /*  a default setting                */

  if ( f_Pc >= 0 && f_Pc <= 100 )       /* if we have a % then is has to be  */
    return 1;                           /*  in these limits                  */

  sprintf (cr_ErrMes,"Percent of Duff Consumed %6.2f is out of limit 0 -> 100",f_Pc);
  return 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Fue_CalcPP
* Desc: Calc the Post Load and Percent Consumed using Pre-load and Consumed
*       this is a utility used by various Fuel catagories
*   In: f_Pre, f_Con......Pre Load and Consumed amounts
*  Out  af_Pos, af_Per....Post Load and Percent Consumed
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void Fue_CalcPP (float f_Pre, float f_Con, float *af_Pos, float *af_Per, char cr_ErrMes[])
{
   if ( f_Pre != 0 ) {
     *af_Pos = f_Pre - f_Con;           /* Post Amount                  */
     *af_Per = (f_Con / f_Pre) * 100;}  /* percen                       */
   else
      *af_Pos = *af_Per = 0;

   if ( *af_Pos > f_Pre ) {
     *af_Pos = f_Pre;
     strcat (cr_ErrMes,"\n");
     strcat (cr_ErrMes,"Logic Error - Fue_CalcPP - Post Amount larger then Pre Amount"); }
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: _PerCon
* Desc: Calculate a percent of consumed from pre burn load.
*   In: preburn load and consumed amount
*  Ret: percent as a whole number
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float _PerCon (float f_Pre, float f_Con)
{
float f;
  if ( f_Pre == 0 )              /* watch out */
    return 0;

  if ( f_Con >= f_Pre )         /* better check this too */
    return 100.0;

  f = (f_Con / f_Pre) * 100.0;
  return f;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: _SetNoIgn
* Desc: If burnup doesn't ignite we have to clean out all the Consumed
*       Post and percent amounts.
* NOTE: we leave the preload and equation numbers alone
* NOTE: We don't 0 emission because caller should have skipped doing them
* In/Out: a_CO.....Consumed Total Struct
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  _SetNoIgn (d_CO *a_CO)
{
    a_CO->f_LitCon = 0;
    a_CO->f_LitPos = 0;
    a_CO->f_LitPer = 0;

    a_CO->f_DW1Con = 0;
    a_CO->f_DW1Pos = 0;
    a_CO->f_DW1Per = 0;

    a_CO->f_DW10Con = 0;
    a_CO->f_DW10Pos = 0;
    a_CO->f_DW10Per = 0;

    a_CO->f_DW100Con = 0;
    a_CO->f_DW100Pos = 0;
    a_CO->f_DW100Per = 0;

    a_CO->f_Snd_DW1kCon = 0;
    a_CO->f_Snd_DW1kPos = 0;
    a_CO->f_Snd_DW1kPer = 0;

    a_CO->f_Rot_DW1kCon = 0;
    a_CO->f_Rot_DW1kPos = 0;
    a_CO->f_Rot_DW1kPer = 0;

    a_CO->f_DufCon = 0;
    a_CO->f_DufPos = 0;
    a_CO->f_DufPer = 0;

    a_CO->f_DufDepCon = 0;
    a_CO->f_DufDepPos = 0;
    a_CO->f_DufDepPer = 0;

    a_CO->f_MSEPer = 0;           /* Mineral Soil Exposer percent */

    a_CO->f_HerCon = 0;
    a_CO->f_HerPer = 0;

    a_CO->f_ShrCon = 0;
    a_CO->f_ShrPos = 0;
    a_CO->f_ShrPer = 0;

    a_CO->f_FolCon = 0;
    a_CO->f_FolPos = 0;
    a_CO->f_FolPer = 0;

    a_CO->f_BraCon = 0;
    a_CO->f_BraPos = 0;
    a_CO->f_BraPer = 0;

/*.........................................................................*/
    a_CO->f_S3Pos  = 0;                     /* Large Woody individual       */
    a_CO->f_S6Pos  = 0;                     /*  Post and Consumed loads     */
    a_CO->f_S9Pos  = 0;
    a_CO->f_S20Pos = 0;

    a_CO->f_R3Pos  = 0;
    a_CO->f_R6Pos  = 0;
    a_CO->f_R9Pos  = 0;
    a_CO->f_R20Pos = 0;

    a_CO->f_S3Con  = 0;
    a_CO->f_S6Con  = 0;
    a_CO->f_S9Con  = 0;
    a_CO->f_S20Con = 0;

    a_CO->f_R3Con  = 0;
    a_CO->f_R6Con  = 0;
    a_CO->f_R9Con  = 0;
    a_CO->f_R20Con = 0;

}
