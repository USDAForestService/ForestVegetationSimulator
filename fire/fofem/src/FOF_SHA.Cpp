//
// $Id$
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: soi_sha.c
* Desc: Soil Heat Array
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include  "fof_sgv.h"
#include  "fof_sh.h"
#include  "fof_sha.h"

extern char gcr_SoiErr[];

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/* NOTICE that we declare 'eC_Lay + 1' the 0 Layer is not used to store      */
/*  any heat values, see notes in the soi_sha.h file                         */
REAL     rr_SHA [eC_Lay+1] [eC_Tim+1];


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SHA_Init
* Desc: Init the entire Soil Heating Array (2 dimension)
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void  SHA_Init ()
{
int i,j;
   for ( i = 0; i <= eC_Lay; i++ )
     for ( j = 0; j < eC_Tim; j++ )
       rr_SHA[i][j] = e_SHA_Init;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SHA_Init_0
* Desc: Init the entire Soil Heating Array with 0s
*       I'm doing this for a Burnup NO Ignite condition, not sure if
*       I'll really use this or not.
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void  SHA_Init_0 ()
{
int i,j;
   for ( i = 0; i <= eC_Lay; i++ )
     for ( j = 0; j < eC_Tim; j++ )
       rr_SHA[i][j] = 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SHA_Largest
* Desc: Get Largest Heat Value stored in the table
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
REAL  SHA_Largest ()
{
int i,j;
REAL  r;
   r = 0;
   for ( i = 0; i <= eC_Lay; i++ ) {
     for ( j = 0; j < eC_Tim; j++ ) {
       if ( rr_SHA[i][j] > r )
         r = rr_SHA[i][j]; }}
   return r;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SHA_isLay
* Desc: Go thru a layer and see if any of the values are set to a non-init
*        value which means there has been a heat value set into layer
*        and the layer is not empty.
*        A row across the SHA table represents a layer.
*   In: i_Lay......layer
*  Ret: 0 layer is empty,   1 layer not empty
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
int SHA_isLay (int i_Lay)
{
int i;
  for ( i = 0; i < eC_Tim; i++ ) {
    if ( rr_SHA [i_Lay] [i] != e_SHA_Init )
    return 1;
  }
 return 0;
}



/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SHA_Put
* Desc: Put a value into the array
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
int   SHA_Put (int i_Lay, int i_Tim, REAL r_Val)
{
char cr1[300], cr2[300];
   if ( i_Lay > eC_Lay || i_Lay < 1 ) {
     strcpy (gcr_SoiErr,"SHA_Put() - Logic Error - Layer array bound exceeded");
     return 0; }
   if ( i_Tim > eC_Tim || i_Tim < 0 ) {
     strcpy (cr1,"SHA_Put() -Logic Error - Time array bound exceeded\n");
     sprintf (cr2,"Limit: %d,    Exceed Value: %d", eC_Tim, i_Tim);
     strcat (cr1,cr2);
     strcpy (gcr_SoiErr,cr1);
     return 0; }
   rr_SHA[i_Lay][i_Tim] = r_Val;
   return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SHA_Get
* Desc: Get the Heat Temp values from the specified location in the
*        array,
*   In: i_Lay.....Layer index (row), note--> NO 0 Layer
*       i_Tim.....Time index (col)
*  Ret: Heat Temp,
*       e_SHA_Init...is returned if there is no value, this happens depending
*                    on time increments, and also at end of each row
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
REAL  SHA_Get (int i_Lay, int i_Tim)
{
   if ( i_Lay > eC_Lay || i_Lay < 1 ) {
     strcpy (gcr_SoiErr,"SHA_Get() - Logic Error - Layer array bound exceeded");
     return -1; }
   if ( i_Tim >= eC_Tim || i_Tim < 0 ) {
     strcpy (gcr_SoiErr,"SHA_Get() - Logic Error - Layer array bound exceeded");
     return -1; }
   return rr_SHA[i_Lay][i_Tim];
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SHA_LayerDeg
* Desc: Find deepest layer that has a specified degree
*  Ret: the layer.....Note 1 is the first layer,
*                     Layer 0 in the rr_SHA[][] is not used
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
int  SHA_LayerDeg  (float f_Deg)
{
int i,j,i_Lay;
REAL  r;
   i_Lay = -1;
   r = (REAL) f_Deg;
   for ( i = 0; i <= eC_Lay; i++ ) {         /* look at each layer           */
     for ( j = 0; j < eC_Tim; j++ ) {        /*  each time increment         */
       if ( rr_SHA[i][j] >= r )              /* check heat                   */
          i_Lay = i; }}                      /* save the layer               */

   return i_Lay;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SHA_MaxTmpLay
* Desc: Find the maximum temperature in a specified layer
* NOTE: Remember the 0 row of the 2 dimensional array is not used
*       so don't send in 0,
*   In: i_Lay......see note above, 0 is NOT used
*  Ret: temperature, -1 if no
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
float  SHA_MaxTmpLay (int i_Lay)
{
int   j;
REAL  r;
float f;
   r = -1;

   for ( j = 0; j < eC_Tim; j++ ) {
     if ( rr_SHA[i_Lay] [j] >= r )
       r = rr_SHA [i_Lay] [j];
   }

   f = (float) r;
   return f;
}
