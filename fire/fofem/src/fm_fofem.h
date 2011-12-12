/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fm_fofem.h
* Desc: Interface between the FFE-FVS and fofem for predicting Soil Heating.
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                   Output Array Indexes                                    */
/* The soil heat temperature for each layer will start going into the array  */
/*  at eX_Deg, of which there are 'e_Layers' layers. Following that will     */
/*  go the deepest layer number that reach 60 and 275 degrees                */
/*  these are the same things we show on the FOFEM Soil report               */
#define eX_Deg     0               /* start indx of layr degees              */
#define eX_SL60    eX_Deg+e_Layers /* Seen note above                        */
#define eX_SL275   eX_SL60 +1

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                   Input Array indexes                                     */
/* each index demotes a position in the input array where the caller is      */
/*  to put a particular input value or code                                  */
/* Also shown below are the numeric codes that are to be used for setting    */
/* season, fuel category, etc.                                               */
/* Note-1: the caller needs to send into the function the lenght of there    */
/*  error message string, so that I can check and not over run it.           */
/*                                                                           */

#define eX_EM    0                 /* len callrs err mess str, see Note-1 Above  */

#define eX_1Hr   1                 /* 1   Hr, 0->1/4 woody load              */
#define eX_10Hr  2                 /* 10  Hr, 1/4->1 woody load              */
#define eX_100Hr 3                 /* 100 Hr, 1->3   woody load              */

#define eX_Snd_DW3   4             /* 1000 hour                              */
#define eX_Snd_DW6   5
#define eX_Snd_DW9   6
#define eX_Snd_DW20  7

#define eX_Rot_DW3   8
#define eX_Rot_DW6   9
#define eX_Rot_DW9   10
#define eX_Rot_DW20  11

#define eX_Duff    12              /* Duff load                              */
#define eX_DufDep  13              /* Duff depth, inches                     */

#define eX_Litter  14              /* Litter Load                            */

#define eX_Herb      15            /* Herb load                              */
#define eX_Shrub     16            /* Shrub load                             */

#define eX_CroFol    17            /* Crown Foliage load                     */
#define eX_CroBra    18            /* Crown Branch load                      */
#define eX_Pc_CroBrn 19            /* Percent of crown that will burn        */

#define eX_MoistDW10 20            /* 10 Hr Moisture                         */
#define eX_MoistDW1k 21            /* 1000 Hr Moisture                       */
#define eX_MoistDuff 22            /* Duff Moisture                          */

/*...........................................................................*/
/* Inputs for Soil Heating                                                   */
#define eX_MoistSoil 23            /* Soil Moisture                          */

#define eX_MoistCond 24
#define e_xVeryDry  1
#define e_xDry      2
#define e_xModerate 3
#define e_xWet      4


#define eX_SoilType  25
#define e_xLoaSke 1
#define e_xFinSil 2
#define e_xFin    3
#define e_xCoaSil 4
#define e_xCoaLoa 5

/*...........................................................................*/

#define eX_MM  26                  /* Duff Moisture Method                   */
#define e_xEntire   1
#define e_xLower    2
#define e_xNFDR     3
#define e_xAdj_NFDR 4

#define eX_Season 27               /* Season                                 */
#define e_xSpring   1
#define e_xSummer   2
#define e_xFall     3
#define e_xWinter   4

#define eX_Region 28               /* Region                                 */
#define e_xIntWest  1
#define e_xPacific  2
#define e_xSE       3
#define e_xNE       4

#define eX_FuelCat 29              /* Fuel Category                          */
#define e_xNatural  1
#define e_xPiles    2
#define e_xSlash    3

#define eX_MM_CovGrp 30            /* Cover Group */
#define e_xGrassGroup 1
#define e_xSageBrush  2
#define e_xShrubGroup 3
#define e_xPocosin    4
#define e_xPonderosa  5
#define e_xWhiPinHem  6          /* White Pine Hemlock            */
#define e_xRedJacPin  7          /* Red Jack Pine                 */
#define e_xBalBRWSpr  8          /* Balsam, Blk Red Whit Spruce  */

/* 1 will Cause an output file to be created, showing inputs and outputs     */
/* 0 is used for no output file                                              */
#define eX_OutFile   31          /* will great output file                  */


#ifdef CMPgcc
int fm_fofem_ (float fr_In[], float fr_Out[], char cr_ErrMes[]);
#else
int fm_fofem  (float fr_In[], float fr_Out[], char cr_ErrMes[]);
#endif

