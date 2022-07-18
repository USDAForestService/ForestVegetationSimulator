//
// FIRE-FOFEM $Id$
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fir_cct
* Desc: Canopy/Crwon Cover Coefficient Table
* "Table 8. Coefficients for tree crown widths based on data from
*    R6 Permanent Plot Grid Inventory"
*  The above is the name on top of a document ER gave me.
*
*  How Can it's calculated
*   In the fof_spp.dat file a FVS species Index No. is assigned to each
*    species.
*   The Index is then used basically as an eqaution number to go find
*     the proper coefficents.
*   This file contains the coefficent table with values assigned to their
*     Index No.
*   > For a 6 char FOFEM species code we go thru the fof_spp.dat table (read
*    in a startup) get Index No.
*   > find the match in the CCT coefficent table below,
*   > use the proper coefficients based on tree height, see
*     code and paper documentation for formulas (pretty simple formulas)
*   > the formula give a diameter of the crown
*   > area (square feet) is then calculation and accumulated for the stand
*   > another formula is used (see code) to converter total square feet
*     to a percent of cover for an acre, the fomula adjusts for overlap
*     It is done per acre because that is what user input of tree density
*      are entered as.
*   look for the  "gf_CrCoTo" variable in fir_msr.c
*
* Date: 1/31/04
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/*                        Crown Coefficients                                 */
/* The Crown Code is the FVS 2 char species code, which is whatn the Table 8 */
/*  document used.                                                           */

typedef struct  {
 int   i_No;                                 /* Spe FVS Index No.            */
 char cr_CC[10];                             /* Crown Code,                  */
 float f_a;                                  /* Large tree coefficients      */
 float f_b;
 float f_r;                                  /* Small tree coefficients      */
 } d_CCT;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/*   Sp   FVS      Trees hgt > 4.5 ft.  | Hgts. <= 4.5 ft.                         */
/*        alpha                         |                                          */
/* Idx No code    "a" coeff. "b" coeff. | "ratio" coeff.                           */
d_CCT sr_CCT[] = {
   {  1,  "SF",     3.9723,    0.5177,       0.473 },
   {  2,  "WF",     3.8166,    0.5229,       0.452 },
   {  3,  "GF",     4.1870,    0.5341,       0.489 },
   {  4,  "AF",     3.2348,    0.5179,       0.385 },
   {  5,  "RF",     3.1146,    0.5780,       0.345 },
   {  7,  "NF",     3.0614,    0.6276,       0.320 },
   {  8,  "YC",     3.5341,    0.5374,       0.331 },
   {  9,   "C",     4.0920,    0.4912,       0.412 },
   { 10,   "S",     3.6802,    0.4940,       0.412 },
   { 11,  "LP",     2.4132,    0.6403,       0.298 },
   { 12,  "JP",     3.2367,    0.6247,       0.406 },
   { 13,  "SP",     3.0610,    0.6201,       0.385 },
   { 14,  "WP",     3.4447,    0.5185,       0.476 },
   { 15,  "PP",     2.8541,    0.6400,       0.407 },
   { 16,  "DF",     4.4215,    0.5329,       0.517 },
   { 17,  "RW",     4.4215,    0.5329,       0.517 },
   { 18,  "RC",     6.2318,    0.4259,       0.698 },
   { 19,  "WH",     5.4864,    0.5144,       0.533 },
   { 20,  "MH",     2.9372,    0.5878,       0.253 },
   { 21,  "BM",     7.5183,    0.4461,       0.815 },
   { 22,  "RA",     7.0806,    0.4771,       0.730 },
   { 23,  "WA",     7.0806,    0.4771,       0.730 },
   { 24,  "PB",     5.8980,    0.4841,       0.601 },
   { 25,  "GC",     2.4922,    0.8544,       0.140 },
   { 26,  "AS",     4.0910,    0.5907,       0.351 },
   { 27,  "CW",     7.5183,    0.4461,       0.815 },
   { 28,  "WO",     2.4922,    0.8544,       0.140 },
   { 29,   "J",     4.5859,    0.4841,       0.468 },
   { 30,  "LL",     2.1039,    0.6758,       0.207 },
   { 31,  "WB",     2.1606,    0.6897,       0.255 },
   { 32,  "KP",     2.1451,    0.7132,       0.248 },
   { 33,  "PY",     4.5859,    0.4841,       0.468 },
   { 34,  "DG",     2.4922,    0.8544,       0.140 },
   { 35,  "HT",     4.5859,    0.4841,       0.468 },
   { 36,  "CH",     4.5859,    0.4841,       0.468 },
   { 37,  "WI",     4.5859,    0.4841,       0.468 },
   { 39,   "",      4.4215,    0.5329,       0.517 },  /* Other */
   { -1,   "",          0,         0,            0 }};


int CCT_Get (int i_No, d_CCT *a_CCT);

void  CCT_Display (d_CCT *a_CTT);
