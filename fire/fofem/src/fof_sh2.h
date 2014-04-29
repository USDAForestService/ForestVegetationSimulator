//
// $Id$
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_sh.c
* Desc: Soil Heating
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                    Soil Moisture Values                                   */
/* Used to check for Min and Max values......................                */
#define  e_SMV_Max (float) 25.0
#define  e_SMV_Min (float)  0.0

/* Starting Soil temperature.............                                    */
/* Used as input to Duff Sim and Exp Heat, and also need as the starting     */
/*  point when graphing                                                      */
#define  e_StaSoiTem  21


REAL  InchtoMeter (REAL r_Inch);
void  SH_Init_LayDis (REAL rrL[], REAL rrD[]);
