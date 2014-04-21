/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_duff
* Desc: functions for doing duff calcuations
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include   "fof_ci.h"
#include   "fof_duf.h"
#include   "fof_util.h"

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
#define   e_Adj   1.4                /* Adjust for Duff Code: Adj-NFDR       */


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: DUF_Mngr
* Desc: Do Duff Calcs
* Note-1: Doing a check to make sure the percent stays with in limits of
*          0 to 100, I think the old Fortran code did this, I guess some
*          of the Equations can go out of limits.
* Note-2: I put this check in here because, some of the calcs for duff dep
*         reduction will calc an amount even when the duff dep that comes
*         in is 0
* Note-3: This prevents the amount of duff depth reduction from exceeding
*          the orginal duff depth, some of the Equations will calc the
*          reduction to be more than what it starts with, so I check
*          it here before its time to return to caller
* Note-4: When there is 0 duff load on input then the Mineral Soil Exposed
*          (MSE) should be set to 100, when duff load is 0 so is the duff depth
*          input, the MSE equations make calcs based on Duff moist and come
*          up with varing answers, SO I ask ER about it decied to plug in
*          100 percent when duff and depth = 0
*   In: a_CI........Consume Input struct
*  Out: a_DUF
*       a_DUF->cr_ErrMes if error
*  Ret: 1 ok
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int  DUF_Mngr (d_CI *a_CI, d_DUF *a_DUF )
{

   DUF_Init (a_DUF);

   if ( CI_isPiles(a_CI) ) {              /* Fuel Category: Piles, all Regions */
     Equ_17_Per (a_DUF);
     a_DUF->f_Red = 0;                   /* There is no Duff Reduction Eq     */
     a_DUF->i_RedEqu = 0;                /*  for this                         */
     Equ_18_MSE (a_DUF);
     goto X; }

   if ( CI_isInteriorWest (a_CI) ) {
     DUF_InteriorWest (a_CI, a_DUF);
     goto X; }

   if ( CI_isPacificWest(a_CI) ) {
     DUF_PacificWest (a_CI, a_DUF);
     goto X; }

   if ( CI_isNorthEast(a_CI) ) {
     DUF_NorthEast (a_CI, a_DUF);
     goto X; }

   if ( CI_isSouthEast (a_CI) ) {
     DUF_SouthEast (a_CI, a_DUF);
     goto X; }

X:
/*-------------------------------------------------*/
/* Change 7-29-2012                                */ 
/* We want a Duff Moist of 10 to consume all duff  */
/*  10 is the lowest allowable moist amount        */
/* this change was put in place to deal with the   */
/* duff soil heating problem                       */
   if ( a_CI->f_MoistDuff <= 10.0 ) { 
     a_DUF->f_Per = 100.0;
     a_DUF->f_Red = a_CI->f_DufDep;
     goto MSE; }

 

  if ( a_DUF->f_Per < 0 )                    /* see Note-1 above             */
     a_DUF->f_Per = 0;
   if ( a_DUF->f_Per > 100 )
     a_DUF->f_Per = 100;

   if ( a_DUF->f_Red > a_CI->f_DufDep )      /* see Note-3                   */
      a_DUF->f_Red = a_CI->f_DufDep;

   if ( a_CI->f_DufDep <= 0 )                /* see Note-2 above             */
      a_DUF->f_Red = 0;

MSE: 
/* Change 9-28-05, see Note-4 above                                          */
   if ( a_CI->f_Duff <=0 )                   /* No Duff Load                 */
      a_DUF->f_MSEPer = 100.0;


  return 1;
}



/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: DUF_SouthEast
* Desc: South East
*   In: a_CI.......Fuel Loading struct
*  Out: a_DUF.......Duff Values Struct, loads values and equ #
*  Ret: 1 Calcs done,    0 Fuel Category was Piles
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int DUF_SouthEast (d_CI *a_CI, d_DUF *a_DUF)
{

  if ( CI_isPocosin(a_CI)){                  /*  Pond Pine - Pocosin         */
    Equ_20_RedPer (a_CI, a_DUF);
    Equ_202_MSE (a_DUF);                     /* Soil Exposure                */
    return 1; }

/* test larry  */
  if ( CI_isCoastPlain(a_CI)){
    Equ_CP_Per (a_CI, a_DUF);
    Equ_CP_Red (a_CI, a_DUF);
    Equ_CP_MSE  (a_DUF);
    return 1;   }


  Equ_16_Per (a_CI, a_DUF);
  a_DUF->f_Red = (a_CI->f_DufDep * a_DUF->f_Per) / 100;
  a_DUF->i_RedEqu = 16;
  Equ_14_MSE (a_DUF);                        /* Soil Exposure                */
  return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: DUF_NorthEast
* Desc:
*   In: a_CI.......Fuel Loading struct
*  Out: a_DUF.......Duff Values Struct, loads values and equ #
*  Ret: 1 Calcs done,    0 Fuel Category was Piles
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int DUF_NorthEast (d_CI *a_CI, d_DUF *a_DUF)
{

  if ( CI_isWhiPinHem(a_CI) ) {              /* White Pine - Hemlock         */
     DUF_InteriorWest (a_CI, a_DUF);
     return 1; }

  if ( CI_isRedJacPin(a_CI)){                /* Red Pine - Jack Pine         */
    goto Jack_Red_Pine; }

  if ( CI_isBalBRWSpr (a_CI) ) {                  /* Balsam, Blk Red Whit Spruce  */
    goto Balsam_Fir_Spruce; }

  Duf_Default(a_CI,a_DUF);                  /* if Nothing else to use       */
  return 1;

/*._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.*/
Jack_Red_Pine:
   if ( CI_isDufEntire(a_CI)) {                /* Duff Code - Entire           */
     Equ_15_PerRed (a_CI,a_DUF,"JACK");
     Equ_14_MSE (a_DUF); }
   else if ( CI_isDufLower(a_CI)){
      Duf_Default(a_CI,a_DUF);                  /* if Nothing else to use       */
      return 1; }
   else {
     Equ_3_PerRed (a_CI,a_DUF);
     Equ_14_MSE (a_DUF); }
   return 1;

/*._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._._.*/
Balsam_Fir_Spruce:
   if ( CI_isDufLower(a_CI)) {               /* Duff Code - Lower            */
     Equ_5_RedPer (a_CI,a_DUF);
     Equ_14_MSE (a_DUF); }
   else if ( CI_isDufEntire(a_CI)) {         /* Duff Code - Entire           */
     Equ_15_PerRed (a_CI,a_DUF,"BALSAM");
     Equ_14_MSE (a_DUF); }
   else {                                    /* Duff Code - NFDR & Adj-MFDR  */
     Equ_3_PerRed (a_CI,a_DUF);
     Equ_14_MSE (a_DUF); }

   return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: DUF_PacificWest
* Desc: Duff Calculations for the Pacific  West Region
* Note-1: This was changed when burnup code was put in program, this
*          use to call equation 8 which used woody fuel to calc duff
*          and we didn't want to do that
* NOTE: Does NOT do Fuel Category Piles, do Slash & Natural only
*       Piles should have been done before coming here.
*   In: a_CI.......Fuel Loading struct
*  Out: a_DUF.......Duff Values Struct, loads values and equ #
*  Ret: 1 Calcs done,    0 Fuel Category was Piles
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int DUF_PacificWest  (d_CI *a_CI, d_DUF *a_DUF)
{

  if ( CI_isPiles(a_CI) )                    /* Piles should have been done  */
    return 0;                                /*  by caller                   */

/* Fuel Category:  Slash ................................................... */
  if ( CI_isSlash(a_CI)) {
    DUF_InteriorWest (a_CI,a_DUF);          /* Note-1                       */
    return 1; }

/* Natural - Ponderosa.......................................................*/

  if ( CI_isPonderosa (a_CI)){               /* if Ponderosa Cover Type      */
    if ( CI_isDufLower (a_CI)){              /* Duff Code - Lower            */
      Equ_4_Per (a_CI,a_DUF);
      Equ_5_Red (a_CI,a_DUF);
      Equ_13_MSE(a_CI,a_DUF);               /* Soil Exposure                */
      return 1; }

    if ( CI_isDufEntire(a_CI)){             /* Duff Code - Entire           */
      Equ_2_Per (a_CI,a_DUF);
      Equ_6_Red (a_CI,a_DUF);
      Equ_10_MSE (a_CI,a_DUF);              /* Soil Exposure                */
      return 1;}

    Equ_3_Per (a_CI,a_DUF);                 /* Duff Code - NFDR & Adj-NFDR  */
    Equ_7_Red (a_CI,a_DUF);
    Equ_12_MSE(a_CI,a_DUF);
    return 1;}

/* Natural - Other Cover Types (non-Pondersoa)...............................*/
  if ( CI_isDufLower(a_CI)){                /* Duff Code - Lower            */
    Equ_1_Per (a_CI,a_DUF);
    Equ_5_Red (a_CI,a_DUF);
    Equ_13_MSE(a_CI,a_DUF);                 /* Soil Exposure                */
    return 1;}

  if ( CI_isDufEntire(a_CI)) {               /* Duff Code - Entire           */
    Equ_2_Per (a_CI,a_DUF);
    Equ_6_Red (a_CI,a_DUF);
    Equ_10_MSE (a_CI,a_DUF);                /* Soil Exposure                */
    return 1;}

  Equ_3_Per (a_CI,a_DUF);                 /* Duff Code - NFDR & Adj-NFDR  */
  Equ_7_Red (a_CI,a_DUF);
  Equ_12_MSE(a_CI,a_DUF);
  return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: DUF_InteriorWest
* Desc: Duff Calculations for the Interior West Region
* NOTE: Does NOT do Fuel Category Piles, do Slash & Natural only
*       Piles should have been done before coming here.
*   In: a_CI.......Fuel Loading struct
*  Out: a_DUF.......Duff Values Struct, loads values and equ #
*  Ret: 1 Calcs done,    0 Fuel Category was Piles
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int DUF_InteriorWest  (d_CI *a_CI, d_DUF *a_DUF)
{

  if ( CI_isPiles(a_CI))                    /* Piles should have been done  */
    return 0;                                /*  by caller                   */

  if ( CI_isNatural(a_CI) )                 /* go do Natural                */
    goto Natural;

/* Fuel Category:  Slash ................................................... */
  if ( CI_isSlash(a_CI) ) {
    if ( CI_isDufLower (a_CI)) {             /* Duff Code - Lower            */
      Equ_1_Per (a_CI,a_DUF);
      Equ_5_Red (a_CI,a_DUF);
      Equ_9_MSE (a_CI,a_DUF);                /* Soil Exposure                */
      return 1; }

    if ( CI_isDufEntire(a_CI)) {             /* Duff Code - Entire           */
      Equ_2_Per (a_CI,a_DUF);
      Equ_6_Red (a_CI,a_DUF);
      Equ_10_MSE (a_CI,a_DUF);               /* Soil Exposure                */
      return 1; }

    Equ_3_Per (a_CI,a_DUF);                  /* Duff Code - NFDR & Adj-NFDR  */
    Equ_7_Red (a_CI,a_DUF);
    Equ_11_MSE(a_CI,a_DUF);
    return 1; }

/* Fuel Category:  Natural.................................................. */
Natural:

/* Ponderosa Cover Type, Fuel Category Natural ..............................*/
  if ( CI_isPonderosa (a_CI)){               /* Is it a Ponderosa Cover Type */
    if ( CI_isDufLower (a_CI)) {             /* Duff Code - Lower            */
      Equ_4_Per (a_CI,a_DUF);
      Equ_5_Red (a_CI,a_DUF);
      Equ_13_MSE(a_CI,a_DUF);                /* Soil Exposure                   */
      return 1; }

    if ( CI_isDufEntire(a_CI)){              /* Duff Code - Entire              */
      Equ_2_Per (a_CI,a_DUF);
      Equ_6_Red (a_CI,a_DUF);
      Equ_10_MSE(a_CI,a_DUF);                /* Soil Exposure                   */
      return 1; }

    Equ_3_Per (a_CI,a_DUF);                  /* Duff Code - NFDR & Adj-NFDR     */
    Equ_7_Red (a_CI,a_DUF);
    Equ_12_MSE(a_CI,a_DUF);
    return 1; }

/* Non-Ponderosa Cover Type .................................................*/
  if ( CI_isDufLower (a_CI)) {               /* Duff Code - Lower            */
    Equ_1_Per (a_CI,a_DUF);
    Equ_5_Red (a_CI,a_DUF);
    Equ_13_MSE(a_CI,a_DUF);
    return 1;}

  if ( CI_isDufEntire(a_CI)) {               /* Duff Code - Entire           */
    Equ_2_Per (a_CI,a_DUF);
    Equ_6_Red (a_CI,a_DUF);
    Equ_10_MSE(a_CI,a_DUF);
    return 1;}

  Equ_3_Per (a_CI,a_DUF);
  Equ_7_Red (a_CI,a_DUF);
  Equ_12_MSE(a_CI,a_DUF);
  return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_1_Per
* Desc: Calculate Percent of Duff Reduction
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_1_Per (d_CI *a_CI, d_DUF *a_DUF)
{
   if ( a_CI->f_MoistDuff <= 160 )
     a_DUF->f_Per = 97.1 - (0.519 * a_CI->f_MoistDuff);
   else
     a_DUF->f_Per = 13.6;
   a_DUF->i_PerEqu = 1;

   if ( a_DUF->f_Per < 0 )
     a_DUF->f_Per = 0;

}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_2_Per
* Desc: Calculate Percent of Duff Reduction
* Note-1: The percent from this calculation can go below 0 when the
*          #Moisture-Conditions = wet, the DuffMoist gets set to 200
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_2_Per (d_CI *a_CI, d_DUF *a_DUF)
{
   a_DUF->f_Per = 83.7 - (0.426 * a_CI->f_MoistDuff);
   a_DUF->i_PerEqu = 2;
   if (a_DUF->f_Per < 0 )       /* see Note-1 above */
    a_DUF->f_Per = 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_3_PerRed
* Desc: Calculate Percent of Duff Reduction and Amount of Reduction Inches
*   In: a_CI....Fuel Input Value struct
*  Out: a_DUF......calulated percent & amount of reduction
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_3_PerRed (d_CI *a_CI, d_DUF *a_DUF)
{
   Equ_3_Per (a_CI, a_DUF);     /* Do the Percent                  */

/* Now use the Percent to get the Amount of Depth Reduction in Inches        */
   a_DUF->f_Red  =  ( a_CI->f_DufDep * a_DUF->f_Per ) / 100;
   a_DUF->i_RedEqu = 3;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_3_Per
* Desc: Calculate Percent of Duff Reduction
*   In: a_CI....Fuel Input Value struct
*  Out: a_DUF......calulated percent & equation # is put in
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_3_Per (d_CI *a_CI, d_DUF *a_DUF)
{
float f_Moist;
   f_Moist = a_CI->f_MoistDuff;    /* See Define definition                 */
   if ( CI_isDufAdjNFDR(a_CI) )
     f_Moist = f_Moist / e_Adj;
   a_DUF->f_Per = 114.7 - (4.2 * f_Moist);
   a_DUF->i_PerEqu = 3;
   if ( a_DUF->f_Per < 0 )
     a_DUF->f_Per = 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_4_Per
* Desc: Calculate Percent of Duff Reduction
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_4_Per (d_CI *a_CI, d_DUF *a_DUF)
{
   a_DUF->f_Per = 89.9 - (0.55 * a_CI->f_MoistDuff);
   a_DUF->i_PerEqu = 4;
   if ( a_DUF->f_Per < 0 )
     a_DUF->f_Per = 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_5_RedPer
* Desc: Calculate Amount of Duff Depth Reduction in inches & Percent
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void Equ_5_RedPer (d_CI *a_CI, d_DUF *a_DUF)
{
  Equ_5_Red (a_CI,a_DUF);          /* Calc Amount Depth Inches              */
  if ( a_CI->f_DufDep > 0 )
    a_DUF->f_Per = (a_DUF->f_Red / a_CI->f_DufDep) * 100;
  else
    a_DUF->f_Per = 0;
  a_DUF->i_PerEqu = 5;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_5_Red
* Desc: Calculate Amount of Duff Depth Reduction in inches
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_5_Red (d_CI *a_CI, d_DUF *a_DUF)
{
  a_DUF->f_Red = 1.028 - (0.0089 * a_CI->f_MoistDuff)
                       + ( 0.417 * a_CI->f_DufDep) ;
  a_DUF->i_RedEqu = 5;
  if ( a_DUF->f_Red < 0 )
    a_DUF->f_Red = 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_6_Red
* Desc: Calculate Amount of Duff Depth Reduction in inches
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_6_Red (d_CI *a_CI, d_DUF *a_DUF)
{
  a_DUF->f_Red = 0.8811 - (0.0096 * a_CI->f_MoistDuff)
                        + (0.439  * a_CI->f_DufDep) ;
  a_DUF->i_RedEqu = 6;

  if ( a_DUF->f_Red < 0 )
    a_DUF->f_Red = 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_7_Red
* Desc: Calculate Amount of Duff Reduction in Inches
*   In: a_CI....Fuel Input Value struct
*  Out: a_DUF......calulated percent & equation # is put in
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_7_Red (d_CI *a_CI, d_DUF *a_DUF)
{
float f_Moist;

  f_Moist = a_CI->f_MoistDuff;
  if ( CI_isDufAdjNFDR(a_CI) )
    f_Moist = f_Moist / e_Adj;

  a_DUF->f_Red = 1.773 - (0.1051 * f_Moist)
                        + (0.399  * a_CI->f_DufDep) ;
  a_DUF->i_RedEqu = 7;
  if ( a_DUF->f_Red < 0 )
    a_DUF->f_Red = 0;
}



/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_15_PerRed
* Desc: Calculate Percent of Duff Reduction Inches & Percent
* NOTE-1: this equations calculates the residual duff, which I think is
*          the amount of duff depth remains after fire, not the amount
*          reducded by the fire, so have to subtract that from original
*          depth to get the amount of reduction
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_15_PerRed (d_CI *a_CI, d_DUF *a_DUF, char cr_Pine[])
{
int i_Pine;
float f_RDD;

  if  (!strcmp (cr_Pine,"JACK") )      /* Jack Pine                         */
    i_Pine = 1;
  else
    i_Pine = 0;                         /* other                             */

/* see NOTE-1 above,   get residual amount  ......................          */
  f_RDD = -0.791  + (0.004 * a_CI->f_MoistDuff)
      + (0.8   * a_CI->f_DufDep) + (0.56  * i_Pine);

  a_DUF->f_Red = a_CI->f_DufDep - f_RDD;  /* now get amount of reduction    */
  if ( a_DUF->f_Red > a_CI->f_DufDep )    /* don't reduce by more than we   */
    a_DUF->f_Red = a_CI->f_DufDep;        /*  start with                    */
  if ( a_DUF->f_Red < 0 )
    a_DUF->f_Red = 0;

  if ( a_CI->f_DufDep > 0 )
    a_DUF->f_Per =  (a_DUF->f_Red / a_CI->f_DufDep) * 100;
  else
    a_DUF->f_Per = 0;

  a_DUF->i_PerEqu = 15;
  a_DUF->i_RedEqu = 15;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_16_Per
* Desc:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_16_Per (d_CI *a_CI, d_DUF *a_DUF)
{
float f_WPRE, f_W, f_L;

  f_WPRE = a_CI->f_Lit + a_CI->f_Duff + a_CI->f_DW10 + a_CI->f_DW1;

  if ( f_WPRE > 0 )
    f_W = 3.4958 + (0.3833 * f_WPRE) - (0.0237 * a_CI->f_MoistDuff)
          - (5.6075 / f_WPRE);
  else
    f_W = 0;

  f_L = a_CI->f_Lit + a_CI->f_DW10 + a_CI->f_DW1;
  if ( f_W <= f_L ) {
    a_DUF->f_Per = 0;
    goto PerEqu; }

  if ( (f_WPRE - f_L) != 0 )
     a_DUF->f_Per = 100 * ( (f_W - f_L) / (f_WPRE - f_L) );
  else
     a_DUF->f_Per = 0;

  if ( a_DUF->f_Per < 0 )
    a_DUF->f_Per = 0;

PerEqu:
  a_DUF->i_PerEqu = 16;

}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_20_RedPer
* Desc: Calculate Duff Reduction Amount and then do the Percent
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_20_RedPer (d_CI *a_CI, d_DUF *a_DUF)
{
  if ( CI_isDuffWet(a_CI) )             /*  Moisture Condtion - Wet    */
    a_DUF->f_Red = 0;
  else
    a_DUF->f_Red = a_CI->f_DufDep - 4;      /* reduction depth by 4 inches  */

  if ( a_DUF->f_Red < 0 )                    /* don't let it get negative    */
    a_DUF->f_Red = 0;

  a_DUF->i_RedEqu = 20;                      /* Equation #                   */

  Equ_201_Per (a_CI, a_DUF);                /* go do percent                */

}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_201_Per
* Desc: Calculate Percent of Duff Reduction
*       Duff Reduction Amount MUST be done first and set in the a_DUF
*
* ---->See Page 51 of FOFEM 4.0 user guide
*
* NOTE: below 11 converts root mat depth to biomass
*             22 converts muck depth to biomass
*             88 because we assume top 8 inches of root mat are consumend
*   In: a_DUF->f_Red.....reduction amount
*  Out: a_DUF->f_Per.....percent of reduction Duff
*       a_DUF->i_PerEqu..201
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_201_Per (d_CI *a_CI, d_DUF *a_DUF)
{
float f;
   if ( a_CI->f_Duff <= 0 ) {
     a_DUF->f_Per = 0;
     goto Per; }

/* See NOTE above in header */
   if ( a_DUF->f_Red <=  8 )                      /* 8 inches                */
      f = a_DUF->f_Red * 11.0;                    /* Calc actual amount of   */
   else                                           /*  Duff Reduction         */
      f = 88.0 + (22.0 * (a_DUF->f_Red - 8.0) );

   a_DUF->f_Per = 100.0 * ( f / a_CI->f_Duff );  /* Now Calc the percent    */

Per:
   a_DUF->i_PerEqu = 201;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_9_MSE
* Desc: Calculate Percent or Mineral Soil Exposure
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void Equ_9_MSE  (d_CI *a_CI, d_DUF *a_DUF)
{
  if ( a_CI->f_MoistDuff <= 135 )
    a_DUF->f_MSEPer = 80 - (.507 * a_CI->f_MoistDuff);
  else
    a_DUF->f_MSEPer = 23.5 - (.0914 * a_CI->f_MoistDuff);
  a_DUF->i_MSEEqu = 9;

  if ( a_DUF->f_MSEPer < 0 )           /* Make sure don't go below 0        */
    a_DUF->f_MSEPer = 0;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_10_MSE
* Desc: Calculate Percent or Mineral Soil Exposure
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void Equ_10_MSE  (d_CI *a_CI, d_DUF *a_DUF)
{
   a_DUF->f_MSEPer = 167.4 - (31.6 * log(a_CI->f_MoistDuff) );
   a_DUF->i_MSEEqu = 10;
   if ( a_DUF->f_MSEPer < 0 )           /* Make sure don't go below 0        */
     a_DUF->f_MSEPer = 0;
}



/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_11_MSE
* Desc: Calculate Percent or Mineral Soil Exposure
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void Equ_11_MSE  (d_CI *a_CI, d_DUF *a_DUF)
{
float f_Moist;
  f_Moist = a_CI->f_MoistDuff;
  if ( CI_isDufAdjNFDR(a_CI) )
     f_Moist = f_Moist / e_Adj;
   a_DUF->f_MSEPer =  93.3 - ( 3.55 * f_Moist) ;
   a_DUF->i_MSEEqu = 11;
   if ( a_DUF->f_MSEPer < 0 )           /* Make sure don't go below 0        */
     a_DUF->f_MSEPer = 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_12_MSE
* Desc: Calculate Percent or Mineral Soil Exposure
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void Equ_12_MSE  (d_CI *a_CI, d_DUF *a_DUF)
{
float f_Moist;
   f_Moist = a_CI->f_MoistDuff;
  if ( CI_isDufAdjNFDR(a_CI) )
     f_Moist = f_Moist / e_Adj;
   a_DUF->f_MSEPer =  94.3 - ( 4.96 * f_Moist) ;
   a_DUF->i_MSEEqu = 12;

  if ( a_DUF->f_MSEPer < 0 )           /* Make sure don't go below 0        */
    a_DUF->f_MSEPer = 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_13_MSE
* Desc: Calculate Percent or Mineral Soil Exposure
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void Equ_13_MSE  (d_CI *a_CI, d_DUF *a_DUF)
{
   a_DUF->f_MSEPer =  60.4 - ( 0.44 * a_CI->f_MoistDuff) ;
   a_DUF->i_MSEEqu = 13;

  if ( a_DUF->f_MSEPer < 0 )           /* Make sure don't go below 0        */
    a_DUF->f_MSEPer = 0;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_14_MSE
* Desc: Calculate Percent or Mineral Soil Exposure
* NOTE: Duff Percent has to be done be comming
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void Equ_14_MSE  (d_DUF *a_DUF)
{
   a_DUF->f_MSEPer =  -8.98 + ( 0.44 * a_DUF->f_Per );
   a_DUF->i_MSEEqu = 14;
   if ( a_DUF->f_MSEPer < 0 )        /* Just in case */
     a_DUF->f_MSEPer = 0;
   if ( a_DUF->f_MSEPer > 100 )
     a_DUF->f_MSEPer = 100;

  if ( a_DUF->f_MSEPer < 0 )           /* Make sure don't go below 0        */
    a_DUF->f_MSEPer = 0;

}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_202_MSE
* Desc: Calculate Percent or Mineral Soil Exposure
* NOTE: Duff Percent has to be done be comming
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void Equ_202_MSE  (d_DUF *a_DUF)
{
   a_DUF->f_MSEPer =  0;
   a_DUF->i_MSEEqu = 202;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: DUF_Calc
* Desc: Calculate Duff Amount Consumed, Post, and Percent
* NOTE: this is done using the Percent amount in the DUF struct put
*       there by the Equation function.
* Note-1: See comments in the called function.
*   In: a_CI, a_DUF
*  Out: see code
*  Ret: equation number
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int DUF_Calc (d_CI *a_CI, d_DUF *a_DUF, float *af_Con, float *af_Post,
                float *af_Percent)
{
  if ( a_CI->f_Duff != 0 ) {
    *af_Con =  (a_CI->f_Duff * a_DUF->f_Per) / 100;
    *af_Post = a_CI->f_Duff - *af_Con;
    *af_Percent = a_DUF->f_Per; }
  else
    *af_Con = *af_Post = *af_Percent = 0;

  if ( *af_Con > a_CI->f_Duff )      /* don't consumed amount to be more    */
     *af_Con = a_CI->f_Duff;         /* then we started with amount         */

  return  a_DUF->i_PerEqu;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: DUF_GetDepRed
* Desc: Get the Depth Reduction Post and Percent, uses the amount of
*       Reduction (inches) put in the DUF struct by the Reduction
*       equation. Checks for any error message in the DUF that would say
*       no equation was available for use.
* Note-1: This is asking if a reduction equation was inplemented and used
*        This should always be the case. Even when there is no Reduction
*        equation for use, the Duff Percent equation should have been used
*        to fill in the DUF Reduction Depth fields.
*    In: a_CI........Consumed Input struct
*        a_DUF.......Duff Values Struct
*   Out: af_Post.....Post Depth in inches
*   Out: af_Per......Percent of Duff Depth reduction
*   Ret: 1 OK,  0 = found error message in DUF struct, meaning there was
*                    No Reduction Equation implemented,
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int DUF_GetDepRed(d_CI *a_CI, d_DUF *a_DUF, float *af_Post, float *af_Per)
{
  if ( strcmp (a_DUF->cr_ErrMess,"") )  /* if there is an error message      */
     return 0;                          /* so don't need to do any calcs     */

  if ( a_CI->f_DufDep == 0 ) {         /* No Duff amount to work with       */
    *af_Post = 0;
    *af_Per = 0;
     return 1; }

/* changed 1-5-00, decided to let this go thru, report will show Equation 0  */
/*  if ( a_DUF->i_RedEqu == 0 ) {             Note-1 above                   */
/*     strcpy (a_DUF->cr_ErrMess,"No Duff Reduction Equation Implemented");  */
/*     return 0; }                                                           */
/* ...........................                                               */

  *af_Post = a_CI->f_DufDep - a_DUF->f_Red;
  if ( a_CI->f_DufDep != 0 )
     *af_Per = (a_DUF->f_Red / a_CI->f_DufDep) * 100;
  else
     *af_Per = 0;
  return 1;
}




/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Duf_Default
* Desc: This is the defualt Duff and Mineral Soil Reduction Equations
*       that get called when there is nothing else to use.
*       There where conditions in the code where there was no Equations
*        to use, so it was decided that the ones below should be the
*        defualt ones used.
* In/Out: a_CI, a_DUF
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Duf_Default (d_CI *a_CI, d_DUF *a_DUF)
{
   Equ_2_Per (a_CI,a_DUF);
   Equ_6_Red (a_CI,a_DUF);
   Equ_10_MSE (a_CI,a_DUF);              /* Soil Exposure                */
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc:
*  Ret:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void   DUF_Init (d_DUF *a_DUF)
{
   a_DUF->f_Per = 0;
   a_DUF->i_PerEqu = 0;
   a_DUF->f_Red = 0;
   a_DUF->i_RedEqu = 0;
   strcpy (a_DUF->cr_ErrMess,"");

   a_DUF->f_MSEPer = 0;
   a_DUF->i_MSEEqu = 0;
   strcpy (a_DUF->cr_MSEMess,"");

   a_DUF->f_PerLit = 0;
   a_DUF->i_LitEqu = 0;

}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_17_Per
* Desc: Calculate Percent of Duff Reduction
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_17_Per (d_DUF *a_DUF)
{
   a_DUF->f_Per = 10;                  /* 10 percent                         */
   a_DUF->i_PerEqu = 17;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_18_MSE
* Desc: Calculate Percent or Mineral Soil Exposure
* NOTE: Duff Percent has to be done be comming
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void Equ_18_MSE  (d_DUF *a_DUF)
{
   a_DUF->f_MSEPer =  10;
   a_DUF->i_MSEEqu = 18;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_CP_Red       Coastal Plain
* Desc: Calc amount of Duff+Lit layer reduction
*       Calc amount of Duff layer reduction
*       This equations was developed by Geoff Wang, Clemson Univ. South
*         Carolina,
*       This equation works different that all other duff reduction eqs
*       in that it works on duff + litter depth,
* Note-1: Wang said to assume that there is 2 centimeters (20 millimeters)
*        of litter in the forest floor depth, so I check, and as long as
*        there is some litter (load) present then I add the 2 cm
*    In: a_CI->f_DufDep
*   Out: a_Duf->i_RedEqu..Equation number
*        a_Duf->f_Red.....Amuont of duff reduction
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_CP_Red (d_CI *a_CI, d_DUF *a_DUF)
{
float Y, Td, Pl, Ld, Dr,pL;
#define e_LitLay  20.0           /* 20 millimeter litter layer               */

  a_DUF->i_RedEqu = e_CP_RedEq;
  a_DUF->f_Red = 0;

  if ( a_CI->f_DufDep <= 0 )
     return;

  Td = InchToMilMeter (a_CI->f_DufDep); /* Tot depth in millimeters          */

  if ( a_CI->f_Lit > 0 )                /* See Note-1 above                  */
    Td  += e_LitLay ;                   /* 2 centimetr = 20 millimetrs       */

  Y = -9.939+(0.896 * Td)-(29.582 / Td);/* Amt of tot layer reduction       */

  if ( Y > Td )                         /* Shouldn't Happen, check  Consumd  */
    Y = Td;                             /* doesn't exceed, total depth       */

  if ( Y < 0 )                          /* See Note-1 above                  */
    Y = 0;

  Dr = Y - e_LitLay;                    /* take away the lit thickness       */
  if ( Dr < 0 )                         /* would mean lit wasn't all consmed */
    Dr = 0;

  a_DUF->f_Red = MilMeterToInch (Dr);   /* Amount of reductin in inches      */

}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:  Equ_CP_Per      Coastal Plain
* Desc:  Duff AND Litter percent of reduction
*        This equations was developed by Geoff Wang, Clemson Univ. South
*         Carolina,
*        Unlike all other duff reduction equations this one also does
*         litter and uses litter moisture (not duff moisture)
* How it Works:
*        The eq gives the total amount consumed, from that we determine
*         how much litter was consumed and how much duff gets consumed.
*        We assume that any & all litter gets consumed before any duff.
*        So if:
*            prefire Lit = 5 and Duf = 5
*            Total Consumed = 2.5
*            Then, 50 percent of the litter is consumed, but no Duff
*        If:
*            Total Consumed = 7.5
*            Then, 100 percent of litter and 50 percent of duff is consumed
* Note-1: I ask and they said this was in 0 -> 100
*
*   In:  a_CI.....see below
*  Out:  a_DUF....see below this return Litter and Duff percents
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  Equ_CP_Per (d_CI *a_CI, d_DUF *a_DUF)
{
float  Ll, Dl, Tc, Dc,Lc;
float X1_Load, X2_LitMoi;

  a_DUF->f_Per = 0;                          /* percent consumed           */
  a_DUF->f_PerLit = 0;
  a_DUF->i_PerEqu = e_CP_PerEq;              /* the equations number       */
  a_DUF->i_LitEqu = e_CP_PerEq;              /* the equations number       */

  if ((a_CI->f_Lit + a_CI->f_Duff) <= 0)     /* Need these combined        */
    return;                                  /* make sure we have some     */

   Ll = TPA_To_MTPH (a_CI->f_Lit);           /* Get to Metric Tons         */
   Dl = TPA_To_MTPH (a_CI->f_Duff);          /* Lit Load and Duff Load     */

   X1_Load = Ll + Dl;
   X2_LitMoi = a_CI->f_LitMoi;               /* See Note-1 above           */

/* Total amount consumed, includes litter and duff                         */
   Tc = -3.893 + ( (0.944*X1_Load) - (0.078*X2_LitMoi));

/* f_Con is negative */
   if ( Tc > X1_Load )               /* Don't consume mor thn we strg with*/
     Tc = X1_Load;

   if ( Tc < 0 )                     /* check for negative                */
     Tc = 0;

   Dc = Tc - Ll;                     /* Tot consmd minus pre-fir Lit load    */
   if ( Dc <= 0 )                    /* get amount of duff consumed          */
     Dc = 0;

   Lc = Tc - Dc;                     /* Lit consmd is total - Duf consmd     */

/* Now determine the results as percents,  0 -> 100                          */
/* Duff and Litter percents consumed are each kept separate                  */
   if ( Dl <= 0 )
     a_DUF->f_Per = 0;
   else
     a_DUF->f_Per = 100.0 * ( Dc / Dl );

   if ( Ll <= 0 )
     a_DUF->f_PerLit = 0;
   else
     a_DUF->f_PerLit = 100.0 * ( Lc / Ll );

/* checks prevent any small over runs like - 100.00123                      */
   if ( a_DUF->f_PerLit > 100.0 )
     a_DUF->f_PerLit = 100.0;

   if ( a_DUF->f_Per > 100.0 )
     a_DUF->f_Per = 100.0;

}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Equ_CP_MSE
* Desc: Coastal Plain - Calculate Percent or Mineral Soil Exposure
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void Equ_CP_MSE  (d_DUF *a_DUF)
{
   a_DUF->f_MSEPer =  5.0;
   a_DUF->i_MSEEqu = e_CP_MSEEq;
}
