//
// $Id$
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: bur_BOV.c     Burnups Output Values  Structure & Time Heat values
* NOTE: I added to this module functions to save the time and heat values
*        that burnup also calculates
* Desc: When burnup runs, it generates a 'Remaining Load Amount' (RLA)
*       for each load entry we give it in the input file.
*       The order in which burnup sends out the RLA is in a sorted order,
*        but in the code where I place the loads into the input file
*        I put them in the proper order so that they come out the same
*        order they go in.
*       This sturcture gives burnup a place to but the RLA values as it runs.
*
*       When the input file is being created it makes an entry in the BOV
*        with a signiture for the load type. Burnup then runs and puts the
*        RLA into the BOV, after which they can be retrieved.
*        For some loads like Wood 3+ there is more than 1 entry in the BOV
*        So we can just go thru BOV and ad them up.
*
* Date: 4/30/00
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "fof_util.h"
#include "bur_bov.h"
#include "bur_brn.h"


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* For Soil Exp Heat.......                                                  */
int   i_Cnt;                            /* count for taking average          */
int   i_Time;                           /* time in seconds                   */
float f_fi;                             /* heat intensity                    */

/*...........................................................................*/
/* Change 1-30-07, add for Pam FOFEM,                                        */
/* the Pam-fofem will use extern gfr_fi[] for median, averaging etc          */
float f_fiSHFB;                    /* fir int Shr,Her,Fol,Bra, 1st tim step  */

float gfr_fi[eC_fi];                    /* hold fi for taking median         */
int   giX_fi;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Hold the Remaining Load amounts coming out of burnup                      */
typedef struct {
   char cr_BOV_Cat[20];               /* See #defines in bur_BOV.h file      */
   int   i_3InId;                     /* 3+ inch wood, tells if 3,6,9, 20+   */
   float f_RemAmt;                    /* Remaing Amount Value burnup calcs   */
   float f_TimBurOut;                 /* Time until burnout                  */
  } d_BOV;


/* Create a table of these, don't realy need 20                              */
#define eC_BOV 20
d_BOV  sr_BOV[eC_BOV];


void  BOV_Set_fiSHFB (float f_fi) { f_fiSHFB = f_fi; }
float BOV_Get_fiSHFB ()
{
  return f_fiSHFB;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: BOV_Set_TimHea
* Desc: When burnup runs it uses this function to:
*       - total up all heat values sent in that are great than 10
*       - save the last 'time' that comes in, the only time we'll be
*          interested in is that last one that comes in
*       - maintain a count for taking average of heat total
*   In: time......time (seconds)
*       fi........heat (kw/msq)
*  Ret: 1 OK,  0 heat was less that 10 so not totaled
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int BOV_Set_TimHea (double time, double fi)
{

/* Store all fire intensities burnup sends into an array                     */
/* Put this in for Pam soil modeling work                                    */
/* The Pam fofem will use the array                                          */
  if ( giX_fi >= (eC_fi - 2) )          /* Should fill up, just in case      */
    giX_fi = (eC_fi - 2);               /*  don't go past                    */
  gfr_fi[giX_fi] = fi;
  giX_fi++;


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
  if ( fi < 10.0 )                      /* for FOFME5 don't want vals below  */
    return 0;

  i_Cnt++;                              /* count for taking average later    */
  f_fi += fi;                           /* total up the heat for average     */
  i_Time = time;                        /* save the time                     */

  return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: BOV_Get_TimHea
* Desc: Get the time, heat, and average count
*       See function 'BOV_Set_TimHea' comments
*  Out: ai_time ...time in seconds,
*       af_fi......heat
*       cr_Mess....if user needs this, see code below
*  Ret: count for taking average of heat
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int BOV_Get_TimHea (int *ai_time, float *af_fi, char cr_Mess[])
{
  strcpy (cr_Mess,"");

  *ai_time = i_Time;                   /* save the time                     */
  *af_fi = f_fi;                       /* total up the heat for average     */

  if ( i_Time < 180 )
    strcpy (cr_Mess, "Modeled fire duration is so short minimal soil heating is expected.");
  else
    strcpy (cr_Mess, "");

/* changed 12/28/01  */
/* strcpy (cr_Mess, "Fire burned time is minimal"); */

  return i_Cnt;                        /* count for taking average later    */
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: BOV_GetRemAmt
* Desc: Get the Remaining amount of load that burnup has calculated
*       This function will go thru the BOV array and total up all the
*       the remaining amount values for the requested category
*       There can be more than one entry in the table for the category
*        because like the DW1000 gets sent into burnup broken down into
*        sound/rotten/size,
*   In: cr_BOV_Cat.....Load Code, see #defines in bur_BOV.h
*  Out: *af_RemAmt......NOTE see return values notes below
*  Ret: 1 remaining amount is coming back
*       0 Burnup had a 'didn't ignite' condition, that means that the
*         original load amount is what still remains
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int  BOV_GetRemAmt (float *af_RemAmt, char cr_BOV_Cat[])
{
int i;
float f;
   *af_RemAmt = 0;

   f = 0;
   for ( i = 1; i < eC_BOV; i++ ) {
     if ( !strcmp (sr_BOV[i].cr_BOV_Cat,"") ){
        *af_RemAmt = f;
        return 1; }

     if ( !strcmp (sr_BOV[i].cr_BOV_Cat,cr_BOV_Cat) ) {
       f += sr_BOV[i].f_RemAmt;
     }
   }
  return 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: BOV_GetRem3
* Desc: Get and individual 3+ inch wood Post-Burn (Remaining) Load
* NOTE-1: Remember an entry is made in the BOV for every category that
*         has a load going into burnup (via the tmp input file)
*         So if we don't find it in the table we assume there was no
*         pre-burn load sent in and so no entry in BOV.
*   In: cr_BOV_Cat.....e_BOV_DW1kSnd or e_BOV_DW1kRot
*       i_3InId........3,6,9,20
*  Ret: Post-Burn load, (burnup refers to it as the remaining load)
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float  BOV_GetRem3 (char cr_BOV_Cat[], int i_3InId)
{
int i;
   for ( i = 1; i < eC_BOV; i++ ) {                 /* look thru table       */
     if ( strcmp (sr_BOV[i].cr_BOV_Cat,cr_BOV_Cat)) /* not this one          */
        continue;
     if ( sr_BOV[i].i_3InId != i_3InId )            /* not this one          */
        continue;
     return  sr_BOV[i].f_RemAmt; }                  /* found......           */
  return 0;                                         /* NOTE-1                */
}



/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: BOV_PutRemAmt
* Desc: Put a 'Remaining Load Amount' value into the table
*       The Value gets put in at the 'iX' index, which is sent in.
*       Also, save the 'time until burnout'
*   In: f_RemAmt.....Remainder Amount (calculated from Burnup)
*       f_TimBurOut..T
*       iX...........index the burnup uses to output values.
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int BOV_PutRemAmt (float f_RemAmt, int iX, float f_TimBurOut )
{
   if ( iX < 1 )                        /* this would be a serious error     */
     return 0;                          /* see notes in thismodule' heading  */
   if ( iX > BOV_Last() )
     return 0;
   sr_BOV[iX].f_RemAmt = f_RemAmt;      /* Remaining (postburn) load         */
   sr_BOV[iX].f_TimBurOut = f_TimBurOut;/* time until burnout                */
   return 1;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: BOV_Entry
* Desc: Put and entry into the table
* NOTE: No Entries will ever be but in the first position of table
*       notice how the for loop starts at 1
*   In: cr_BOV_Cat...see defines in bur_BOV.h
*       i_3InId.......3+ inch wood Id, send in  3(3-6), 6(6-9),
*                      9(9-20), 20 over 20
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int   BOV_Entry (char cr_BOV_Cat[], int i_3InId)
{
int i;
   for ( i = 1; i < eC_BOV; i++ ) {
     if ( !strcmp (sr_BOV[i].cr_BOV_Cat,"") ) {
       strcpy ( sr_BOV[i].cr_BOV_Cat, cr_BOV_Cat);
       sr_BOV[i].i_3InId = i_3InId;
       return 1; } }
   return 0;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: BOV_Last
* Desc: Return index to last entry in table
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int   BOV_Last ()
{
int i;
   for ( i = 1; i < eC_BOV; i++ )
     if ( !strcmp (sr_BOV[i].cr_BOV_Cat,"") )
      return i - 1;
   return 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: BOV_Init
* Desc: Init the Table, and Ignite switch
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void BOV_Init ()
{
int i;
   i_Cnt = 0;
   i_Time = 0;
   f_fi = 0;
   f_fiSHFB = 0;

   for ( i = 0; i < eC_fi; i++ )
    gfr_fi[i] = e_FiInit;
   giX_fi = 0;

   for ( i = 0; i < eC_BOV; i++ ) {
     strcpy (sr_BOV[i].cr_BOV_Cat,"");
     sr_BOV[i].i_3InId = 0;
     sr_BOV[i].f_RemAmt = 0;
     sr_BOV[i].f_TimBurOut = 0;
   }
}
