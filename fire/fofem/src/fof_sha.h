//
// $Id$
//

#define  e_SHA_Init  -999               /* Init Value                        */

/* This is the Number of Soil Layers the Soil Duff Sim and Exp Heat use      */
/* They are number 1 thur 14, look at the the SHA soil heat array is declared*/
/* there is a 0 entry put it is not used to store any heat values for it     */
/* becuase that is realy layer 0 and as is such not used                     */
#define  eC_Lay  e_mplus1                     /* number of soil layers             */
/* #define  eC_Lay  14 */                     /* number of soil layers             */

#define  eC_Tim  1000                    /* number of time increments         */



void  SHA_Init (void);
void  SHA_Init_0 (void);
int   SHA_Put (int i_Lay, int i_Tim, REAL r_Val);
REAL  SHA_Get (int i_Lay, int i_Tim);
int   SHA_isLay (int i_Lay);
REAL  SHA_Largest (void);
int   SHA_LayerDeg  (float f_Deg);
float SHA_MaxTmpLay (int i_Lay);
