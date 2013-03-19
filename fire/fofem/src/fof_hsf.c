//
// $Id
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_hsf.c
* Desc: HSF Herb,Shrub,Foliage Consumerd Manager - this is everything
*        burnup doesn't do,
*        Does Foilage Crown and Branch and The Mineral Soil Exposed
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
#ifdef ANSI
#define WINAPI
#else
#include <windows.h>
#endif


#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <float.h>

#include  "fof_co.h"
#include  "fof_ci.h"
#include  "fof_hsf.h"
#include  "fof_lem.h"
#include  "fof_duf.h"
#include  "bur_brn.h"

int _ChkLim (float f_Load, char cr_Name[], char cr_ErrMes[]);
int _Pc_CroBrn (float f_Pcn, char cr_ErrMes[]);
int  _DuffChk (d_CI *a_CI, char cr_ErrMes[]);


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: HSF_Mngr
* Desc: Consumed Manager this does
*       Duff, Duff Depth, Mineral Soil Exposer, Herb, Shrub, Foliage, Branch
*   In: a_CI.....See the notes at the top of this file
*  Out: a_CO.....Output values
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int HSF_Mngr (d_CI *a_CI, d_CO *a_CO, char cr_ErrMes[])
{
d_DUF s_Duf;

  strcpy (cr_ErrMes,"");

  if ( !HSF_Check (a_CI,cr_ErrMes) )   /* check inputs for HSF              */
     return 0;

/* Duff .....................................................................*/
   a_CO->f_DufPre = a_CI->f_Duff;
   DUF_Mngr (a_CI, &s_Duf);
   a_CO->i_DufEqu = DUF_Calc (a_CI, &s_Duf, &a_CO->f_DufCon, &a_CO->f_DufPos, &a_CO->f_DufPer);

/* Duff Depth ...............................................................*/
   a_CO->f_DufDepPre = a_CI->f_DufDep;

   DUF_GetDepRed (a_CI,&s_Duf,&a_CO->f_DufDepPos,&a_CO->f_DufDepPer);
   a_CO->f_DufDepCon = s_Duf.f_Red;
   a_CO->i_DufDepEqu = s_Duf.i_RedEqu;

   a_CO->f_MSEPer = s_Duf.f_MSEPer;         /* Mineral Soil                 */
   a_CO->i_MSEEqu = s_Duf.i_MSEEqu;

/* Herb  ....................................................................*/
   a_CO->f_HerPre = a_CI->f_Herb;
   a_CO->i_HerEqu = Calc_Herb (a_CI, &a_CO->f_HerCon, &a_CO->f_HerPos, &a_CO->f_HerPer);

/* Shrub ....................................................................*/
   a_CO->f_ShrPre = a_CI->f_Shrub;
   a_CO->i_ShrEqu = Calc_Shrub (a_CI, &a_CO->f_ShrCon, &a_CO->f_ShrPos, &a_CO->f_ShrPer);

/* Foliage ..................................................................*/
   a_CO->f_FolPre = a_CI->f_CroFol;
   a_CO->i_FolEqu = Calc_CrownFoliage(a_CI, &a_CO->f_FolCon, &a_CO->f_FolPos, &a_CO->f_FolPer);

/* Branch ...................................................................*/
   a_CO->f_BraPre = a_CI->f_CroBra;
   a_CO->i_BraEqu = Calc_CrownBranch (a_CI, &a_CO->f_BraCon, &a_CO->f_BraPos, &a_CO->f_BraPer);

   return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Calc_Shrub
* Desc: Calculate Amount of Shrub Reduction in tons per acre
* Note-1: See notes in the called function
*   In: a_CI.....Consume Input struct
*  Out: af.......Amount of Shrub Reduction in tons per acre
*  Ret: shrub reduction equation used to make calculation
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int Calc_Shrub (d_CI *a_CI, float *af_Con, float *af_Post, float *af_Percent)
{
int i_Equ;
/*...........................................................................*/

   if ( CI_isSageBrush(a_CI) ) {             /* For Sagebrush - Cover Type   */
     if ( CI_isFall(a_CI))                   /* Fall - season                */
        i_Equ = 233;
     else
        i_Equ = 232; }                       /* spring, summer winter        */

   else if ( CI_isShrubGroup(a_CI))          /* For Chaparral,Deser Shrb, etc*/
      i_Equ = 231;

   else if ( CI_isSouthEast(a_CI)) {             /* Southeast Region             */
     if ( CI_isPocosin(a_CI))                    /* Pocosin - Cover Type         */
       if ( CI_isSpring(a_CI) || CI_isWinter(a_CI) )
          i_Equ = 233;                       /* Spring  Winter               */
       else
          i_Equ = 235;                       /* Summer Fall                  */
     else
       i_Equ = 234;                          /* Non - Pocosin - Southeast    */
   }
   else
     i_Equ = 23;                             /* everything else              */

   *af_Con = Shrub_Equ(a_CI, i_Equ);
   if ( *af_Con > a_CI->f_Shrub )           /* Can't consume more that we   */
      *af_Con = a_CI->f_Shrub;              /* started with                 */
   if ( *af_Con < 0 )                        /* make sure not less then 0    */
      *af_Con = 0;

   if ( a_CI->f_Shrub != 0 ) {
     *af_Post = a_CI->f_Shrub - *af_Con;              /* Post Amount         */
     *af_Percent = (*af_Con / a_CI->f_Shrub) * 100;}  /* percent             */
   else
     *af_Con = *af_Post = *af_Percent = 0;

   return i_Equ;                             /* Equation used                */
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Shrub_Equ
*   In: a_FUL.....Fuel Loading Struct, this has tons per acre, moiture etc.
*       i_Equ....equation to use
*  Ret: amount of shrub reduction
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float  Shrub_Equ (d_CI *a_CI, int i_Equ )
{
float   f_Percent;
char cr[300];
  if ( i_Equ == 23 )   return  ( a_CI->f_Shrub * .60 );
  if ( i_Equ == 231 )  return  ( a_CI->f_Shrub * .80 );
  if ( i_Equ == 232 )  return  ( a_CI->f_Shrub * .50 );
  if ( i_Equ == 233 )  return  ( a_CI->f_Shrub * .90 );
  if ( i_Equ == 235 )  return  ( a_CI->f_Shrub * .80 );
  if ( i_Equ == 234 ) {
    f_Percent = Equ_234_Per (a_CI);
    return (a_CI->f_Shrub * f_Percent); }

  sprintf (cr,"ERROR - Shrub_Equ - Shrub Equation %d Not Implemented\n",i_Equ);
  LEM_Put ("Shrub_Equ()", cr);
  return 0;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_234_Per
* Desc: Calculat percent of reduction
* Note-1: Put this check in just to make sure, not sure what that calculation
*          will do.
*   In: a_CI......Consumed Input struct
*  Ret: percent
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float Equ_234_Per (d_CI  *a_CI)
{
float f_W, f_WPRE, f_ShrReg, f;

   f_W = Equation_16 (a_CI);
   if ( f_W == 0 )
     return 0;

   f_WPRE = a_CI->f_Lit + a_CI->f_Duff + a_CI->f_DW10 + a_CI->f_DW1;
   if ( f_WPRE == 0 )
     return 0;

/* original code f_ShrReg = a_FUL->f_Shrub + a_FUL->f_Tree; */
   f_ShrReg = a_CI->f_Shrub;
   if ( f_ShrReg == 0 )
     return 0;

   f = (((3.2484 + (0.4322 * f_WPRE) +  (0.6765 * (f_ShrReg)) - (0.0276 * a_CI->f_MoistDuff) - ( 5.0796 / f_WPRE) ) - f_W) / (f_ShrReg) );

   if ( f < 0 ) {                     /* See Note-1 above */
/* This message did come out during testing for SAF 40 */
/*   sprintf (cr,"percent %f is less than 0, will set to 0",f); */
/*   Warning_Window (cr,"Equ_234_Per"); */
     f = 0; }
   if ( f > 100 ) {
/*   Warning_Window ("percent is greater than 100, will set to 100","Equ_234_Per"); */
     f = 100; }

   return  f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equation 16
* Desc: Gets used by various other Equations
*   In: a_CI.....Fuel Loading struct
*  Ret:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float Equation_16 (d_CI  *a_CI)
{
float f_WPRE, f_W;

   f_WPRE = a_CI->f_Lit + a_CI->f_Duff + a_CI->f_DW10 + a_CI->f_DW1;
   if ( f_WPRE == 0 )
     return 0;
   f_W = 3.4958 + (.3833 * f_WPRE) - (.0237 * a_CI->f_MoistDuff) - (5.6075 / f_WPRE);
   return f_W;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Calc_Herb
* Desc: Calculate Amount of Herbacious Reduction in tons per acre
*   In: a_CI.....Burn Data Input
*  Out: af.......Amount of Herbacious Reduction in tons per acre
*  Ret: Herbacious reduction equation used to make calculation
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int   Calc_Herb (d_CI *a_CI, float *af_Con, float *af_Post, float *af_Percent)
{
int i_Equ;

   if ( CI_isGrassGroup(a_CI) && CI_isSpring (a_CI)){
      *af_Con =  a_CI->f_Herb  * 0.9;
      i_Equ = 221 ; }
   else {
      *af_Con =  a_CI->f_Herb;              /* 100 Percent                  */
      i_Equ = 22 ; }

   if ( *af_Con > a_CI->f_Herb )            /* Don't make consumed more than*/
      *af_Con = a_CI->f_Herb;               /* we start with                */
   if ( *af_Con < 0 )
      *af_Con = 0;

   if ( a_CI->f_Herb != 0 ) {
     *af_Post = a_CI->f_Herb - *af_Con;              /* Post Amount         */
     *af_Percent = (*af_Con / a_CI->f_Herb) * 100;}  /* percen              */
   else
     *af_Con = *af_Post = *af_Percent = 0;

   return i_Equ;

}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Calc_CrownBranch
* Desc: Calculate Amount of Crown Branch
*       Everything gets reduced by 50 percent equation 38
* Note-1: According to the Fofem Manual Equation 38, 50 percent gets
*         burned, but as per request E.R. 1/6/00 we now use % Crown Wnd
*         and apply that to calculation.
*         For example if user enters 10 in % Crown Burn Wnd then we
*         take 10 percent of 50 percent and do reset of calculation.
* Note-2: See notes in the called function
*   In: a_CI.....Burn Data Input
*  Out: see code
*  Ret:  reduction equation used to make calculation
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int  Calc_CrownBranch (d_CI *a_CI, float *af_Con, float *af_Post, float *af_Percent)
{
float f;
  f = a_CI->f_Pc_CroBrn / 100;                    /* % Crown Burn from Wnd  */
  *af_Con = a_CI->f_CroBra * 0.50 * f;            /* Note-1                 */
  if ( a_CI->f_CroBra != 0 ) {
     *af_Post = a_CI->f_CroBra - *af_Con;             /* Post Amount        */
     *af_Percent = (*af_Con / a_CI->f_CroBra) * 100;} /* percent            */
  else
     *af_Con = *af_Post = *af_Percent = 0;

   return 38;                                      /* Equ 38 for all         */
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Calc_CrownFoliage
* Desc: Calculate Amount of Crown Foliage Consumed, Consumed amount is
*        simpley calculated from percent send in
*   In: a_CI.....Burn Data Input
*  Out: see code
*  Ret:  reduction equation used to make calculation
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int  Calc_CrownFoliage (d_CI *a_CI, float *af_Con, float *af_Post, float *af_Percent)
{
float f;
  if ( a_CI->f_CroFol == 0 ) {              /* No Crn Folag to start with   */
     *af_Con = 0;                            /* so none consumed             */
     *af_Percent = 0;
     *af_Post = 0;
     goto X ; }                              /* Equ 37 for all               */

  f = a_CI->f_Pc_CroBrn / 100;              /* % Crown Burn that will burn  */
  *af_Con = a_CI->f_CroFol * f;             /* amount consumed              */
  *af_Post = a_CI->f_CroFol - *af_Con;      /* Post Amount                  */
  *af_Percent = a_CI->f_Pc_CroBrn;          /* percent                      */
X:
  return 37;                                /* Equ 37 for all               */
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: HSF_Check
* Desc: Make sure that the Region,Fuel Category,etc fields that need
*        to be set are set and valid.
*  NOTE: See code below as to what can be missing,
*
*
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int  HSF_Check (d_CI *a_CI, char cr_ErrMes[])
{


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/* Check Loads, percents, duff....                                           */
   if ( !_ChkLim (a_CI->f_Herb,"Herb",cr_ErrMes) )
      return 0;
   if ( !_ChkLim (a_CI->f_Shrub,"Shrub",cr_ErrMes) )
      return 0;
   if ( !_ChkLim (a_CI->f_CroFol,"Crown Foliage",cr_ErrMes) )
      return 0;
   if ( !_ChkLim (a_CI->f_CroBra,"Crown Branch",cr_ErrMes) )
      return 0;

   if ( !_Pc_CroBrn (a_CI->f_Pc_CroBrn,cr_ErrMes))  /* Crown Percent Check   */
      return 0;

   if ( !_DuffChk (a_CI,cr_ErrMes))
      return 0;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/* Regions, is required                                                      */
  if (CI_isSouthEast   (a_CI)) goto A;
  if (CI_isInteriorWest(a_CI)) goto A;
  if (CI_isPacificWest (a_CI)) goto A;
  if (CI_isNorthEast   (a_CI)) goto A;
  sprintf(cr_ErrMes,"No/Invalid Region, Valid: %s, %s, %s, %s",
    e_CI_SouthEast, e_CI_InteriorWest, e_CI_PacificWest, e_CI_NorthEast);
  return 0;

A:
/* Fuel Category is required                                                 */
  if ( CI_isPiles  (a_CI)) goto C;
  if ( CI_isNatural(a_CI)) goto C;
  if ( CI_isSlash  (a_CI)) goto C;
  sprintf(cr_ErrMes,"No/Invalid Fuel Category (%s), Valid: %s, %s, %s (Default %s)",
      a_CI->cr_FuelCategory,
      e_Natural, e_Piles, e_Slash,e_FuelCategoryDefault);
  return 0;

C:
/* Duff Mositure Measured Method                                             */
  if ( CI_isDufEntire (a_CI)) goto D;
  if ( CI_isDufLower  (a_CI)) goto D;
  if ( CI_isDufNFDR   (a_CI)) goto D;
  if ( CI_isDufAdjNFDR(a_CI)) goto D;
  if ( !strcmp (a_CI->cr_DufMoiMet,"") )
     strcpy (cr_ErrMes, "Missing Duff Moisture Measure Method");
  else
     sprintf (cr_ErrMes, "Invalid Duff Moisture Measure Method '%s'\n    Valid Codes = %s, %s, %s, %s, %s",
      a_CI->cr_DufMoiMet,
      e_Entire, e_Lower, e_NFDR, e_Adj_NFDR, e_DufMoiMetDefault);
  return 0;

D:
/* Season                                                  */
  if ( CI_isSpring (a_CI)) goto E;
  if ( CI_isFall   (a_CI)) goto E;
  if ( CI_isSummer (a_CI)) goto E;
  if ( CI_isWinter (a_CI)) goto E;
  sprintf (cr_ErrMes, "Invalid/Missing Season: (%s)\n    Valid: %s, %s, %s, %s",
    a_CI->cr_Season, e_Spring, e_Summer, e_Fall, e_Winter);
  return 0;

E:
  if ( CI_isGrassGroup (a_CI)) goto F;
  if ( CI_isShrubGroup (a_CI)) goto F;
  if ( CI_isSageBrush  (a_CI)) goto F;
  if ( CI_isPocosin    (a_CI)) goto F;
  if ( CI_isPonderosa  (a_CI)) goto F;
  if ( CI_isWhiPinHem  (a_CI)) goto F;
  if ( CI_isRedJacPin  (a_CI)) goto F;
  if ( CI_isBalBRWSpr  (a_CI)) goto F;
  if (!CI_isCoverGroup  (a_CI)) goto F;         /* None set,  ok             */
  sprintf (cr_ErrMes, "Invalid Cover Type (%s), Valid...\n %s, %s, %s, %s \n %s, %s, %s, %s or leave blank (\"\")",
     a_CI->cr_CoverGroup,
     e_GrassGroup, e_ShrubGroup,e_SageBrush,  e_Pocosin,
     e_Ponderosa,  e_WhiPinHem, e_RedJacPin,  e_BalBRWSpr);
  return 0;

F:
  return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: _ChkLim
* Desc: Check Load limits for Herb,Shr,Foliage Crown & Branch
*   In: see belowwo
*  Ret: 1 OK,  0 Error
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int _ChkLim (float f_Load, char cr_Name[], char cr_ErrMes[])
{
   if ( f_Load >= e_CI_LoadLimLow &&  f_Load <= e_CI_LoadLimUp )
     return 1;

   sprintf (cr_ErrMes, "_ChkLim() %s Fuel Load %6.2f is out limit (%d -> %d )",
     cr_Name, f_Load, (int)e_CI_LoadLimLow,  (int)e_CI_LoadLimUp );
   return 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: _Pc_CroBrn
* Desc: Check the Percent Crown Burn
*   In: see below
*  Ret: 1 OK,  0 Error
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int _Pc_CroBrn (float f_Pc, char cr_ErrMes[])
{
   if ( f_Pc >= 0 && f_Pc <= 100 )
     return 1;

  sprintf (cr_ErrMes,"Percent of Crown Foliage Burn %6.2f is out of limit 0 -> 100",f_Pc);
  return 0;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: _DuffChk
* Desc: Check the Duff Load, Depth and Moisture
* Note-1: This use to not check Duff Moist just Duff and DuffDep, so
*          zero or less than min mosit would go thru and blowup a log() func
*          in a Mineral Soil Exposed, This only affected the Batch, FOFEM5
*          would catch it.
*   In: see below
*  Ret: 1 OK,  0 Error
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int  _DuffChk (d_CI *a_CI, char cr_ErrMes[])
{
float Low,Hi;


/* Change 9-27-05, See Note-1 above                                          */
   if ( a_CI->f_Duff == 0 && a_CI->f_DufDep == 0 )
     goto Moist;

   if ( a_CI->f_Duff < e_DufMin  || a_CI->f_Duff > e_DufMax ) {
     sprintf (cr_ErrMes, "Duff Fuel Load %6.2f is out limits (%6.2f -> %6.2f)",
        a_CI->f_Duff, e_DufMin, e_DufMax );
     strcat (cr_ErrMes,"\n Duff load can only be set to 0 when duff depth is also set to 0");
     return 0; }

   if ( a_CI->f_DufDep < e_DufDepMin  || a_CI->f_DufDep > e_DufDepMax ) {
     sprintf (cr_ErrMes, "Duff Depth %6.2f is out limits (%6.2f -> %6.2f)",
        a_CI->f_DufDep, e_DufDepMin, e_DufDepMax);
     return 0; }

Moist:
   Low = (e_dfm1 * 100);
   Hi  = (e_dfm2 * 100);
   if ( a_CI->f_MoistDuff < Low || a_CI->f_MoistDuff > Hi ){
     sprintf (cr_ErrMes, "Duff Moisture  %6.2f is out limits (%6.2f -> %6.2f)",
        a_CI->f_MoistDuff, Low, Hi);
     return 0; }

   if ( a_CI->f_Duff == 0 && a_CI->f_DufDep == 0 )
      goto Ret;

   if ( a_CI->f_Duff == 0 ) {
     strcpy (cr_ErrMes, "Duff Load is 0, Set Duff Load & Depth to 0 or both to non-zero values");
     return 0; }

   if ( a_CI->f_DufDep == 0 ) {
     strcpy (cr_ErrMes, "Duff Depth is 0, Set Duff Load & Depth to 0 or both to non-zero values");
     return 0; }
Ret:
  return 1;
}
