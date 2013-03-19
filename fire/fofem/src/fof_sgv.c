//
// $Id
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_sgv.c     Smoke Graphing values
* Desc: Functions to deal with smoke graphing values
* Date:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
#include   <stdlib.h>
#include   <string.h>
#include   <stdio.h>

#include  "fof_sgv.h"
#include  "fof_util.h"
#include  "fof_lem.h"

int   iX_SGV;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/* NOTE 1500 should be more than enough, this table gets loaded from calls   */
/*  in the burnup code, entry is made for each 15 second time, so lets the   */
/*  table hold enough entries to do (1500 / 15 sec) minutes which should be  */
/*  more than enough, if not the will just tell call there are no more       */
/*  so it will end gracefully                                                */

#ifdef DOS
  #define   eC_SGV 500      /* for DOS testing */
#else
  #define   eC_SGV 1500
#endif

d_SGV  sr_SGV[eC_SGV+1];


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SGV_Prepare
* Desc: Set the value that you want to use and convert it to pounds per Acre
*       This goes thru the table and for every entry sets into the 'amount'
*       field of that same entry the values you'll want to be graphing it as
*       converts it to Pounds PER ACRE, so you as a caller can deal with it
*       like that.
* NOTE: In order to graph smoke the burnup must have been run so that it
*       can load the SGV table, which this function will need.
*   In: cr_Name......USE defines in fof_sgv.h file
*  Out: af_MaxTim....largest second(time) value found
*       af_MaxAmt....largets amount that was found, in Pounds per ACRE
*  Ret: Total of Amount
*       -1 Error
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
float SGV_Prepare (char cr_Name[], float *af_MaxTim, float *af_MaxAmt)
{
int i;
float f_Amt, f_Total;

  *af_MaxTim = 0;
  *af_MaxAmt = 0;
  f_Total = 0;

  for ( i = 0; i < eC_SGV; i++ ) {
     if ( sr_SGV[i].f_Sec == -1 )             /* Hit end of table             */
       break;
          if (!strcmp (cr_Name,e_SGV_PM2_5)) f_Amt = sr_SGV[i].f_PM2_5;
     else if (!strcmp (cr_Name,e_SGV_PM10 )) f_Amt = sr_SGV[i].f_PM10;
     else if (!strcmp (cr_Name,e_SGV_CH4  )) f_Amt = sr_SGV[i].f_CH4;
     else if (!strcmp (cr_Name,e_SGV_CO2  )) f_Amt = sr_SGV[i].f_CO2;
     else if (!strcmp (cr_Name,e_SGV_CO   )) f_Amt = sr_SGV[i].f_CO;

     else if (!strcmp (cr_Name,e_SGV_NOX  )) f_Amt = sr_SGV[i].f_NOX;
     else if (!strcmp (cr_Name,e_SGV_SOX  )) f_Amt = sr_SGV[i].f_SOX;

     else if (!strcmp (cr_Name,e_SGV_Inten)){ /* This is in kilowatts per  */
         sr_SGV[i].f_Amt = sr_SGV[i].f_Inten;  /* meter sqr, so no convert   */
         f_Total += sr_SGV[i].f_Amt;
         goto A;}
     else {
        return -1;
     /*  Took this out on for new version */
     /*   Error_Window ("Logic Error", "SGV_Prepare"); */
     /*   return 0; */
      }

     sr_SGV[i].f_Amt = GramSqMt_To_Pounds(f_Amt);

     f_Total += sr_SGV[i].f_Amt;      /* NEED TO TOTAL befor doing interval  */

     if ( i == 0 )                                      /* deal with burnup  */
       sr_SGV[i].f_Amt = sr_SGV[i].f_Amt / (float) 60;  /* time intervals    */
     else
       sr_SGV[i].f_Amt = sr_SGV[i].f_Amt / (float) 15;

A:
     if ( sr_SGV[i].f_Sec > *af_MaxTim )      /* find largest                */
       *af_MaxTim = sr_SGV[i].f_Sec;
     if ( sr_SGV[i].f_Amt > *af_MaxAmt )
       *af_MaxAmt = sr_SGV[i].f_Amt;

   }  /* for i */

  if ( i == 0 )       /* Nothing in table */
    return -1;

  return f_Total;

}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: GramSqMt_To_Pounds
* Desc: Convert Grams per Square Meter to Pounds Per Acre
*   In: f_Gram......grams per square meter
*  Ret: pounds per acre
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
float  GramSqMt_To_Pounds  (float f_Gram)
{
float f, f_Pounds ;
   f = f_Gram * (float) 4047;              /* 4047 meters in an acre         */
   f_Pounds = f / (float) 453.59;          /* grams per pound                */
   return  f_Pounds;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SGV_GetTimTP
* Desc:
*  Ret: 1 OK,  0 No more
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
int SGV_GetTimPPA (int iX, float *af_Time,  float *af_Amt)
{
  *af_Time = sr_SGV[iX].f_Sec;
  *af_Amt  = sr_SGV[iX].f_Amt;
  if ( *af_Time == -1 )
    return 0;
  return 1;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SGV_Init
* Desc: Init the table, gotta do this before you can load it with anything
*  Ret: 1 OK
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
int SGV_Init  ()
{
int i;
  for ( i = 0; i <= eC_SGV; i++ ) {
     sr_SGV[i].f_Sec = -1;
     sr_SGV[i].f_Amt = 0;
     sr_SGV[i].f_PM2_5 = -1;
     sr_SGV[i].f_PM10 = -1;
     sr_SGV[i].f_CH4 = -1;
     sr_SGV[i].f_CO2 = -1;
     sr_SGV[i].f_CO = -1;

     sr_SGV[i].f_NOX = -1;
     sr_SGV[i].f_SOX = -1;

     sr_SGV[i].f_Inten = -1;
   }
   return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SGV_Set
* Desc: Put an entry into the table.
* NOTE: If the table gets full it doesn't matter becasue this holding
*        values for smoke graph so we'll just have less numbers to graph
*   In: a_SGV......stuff you want loaded
*                  NOTE, you don't set the f_Amt field
*  Ret: 1 OK,  0 Table is full
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
int SGV_Set  (d_SGV  *a_SGV)
{
int i;
  for ( i = 0; i < eC_SGV; i++ ) {
    if ( sr_SGV[i].f_Sec == -1 ) {
      sr_SGV[i].f_Sec    = a_SGV->f_Sec ;
      sr_SGV[i].f_PM2_5  = a_SGV->f_PM2_5;
      sr_SGV[i].f_PM10   = a_SGV->f_PM10;
      sr_SGV[i].f_CH4    = a_SGV->f_CH4;
      sr_SGV[i].f_CO2    = a_SGV->f_CO2;
      sr_SGV[i].f_CO     = a_SGV->f_CO;

      sr_SGV[i].f_NOX    = a_SGV->f_NOX;
      sr_SGV[i].f_SOX    = a_SGV->f_SOX;

      sr_SGV[i].f_Inten  = a_SGV->f_Inten;
      return 1; } }

   return 0;
}
