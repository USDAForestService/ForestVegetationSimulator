/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_ci.h       Consume Data Inputs
* Date: 11/05/03
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/


/* These are the Fuel load limits that CI checks in the CI struct before   */
/* sending into Burnup, Burnup has its' own limits it checks but they are    */
/* much larger                                                               */
/* NOTE: these are NOT user for Duff load                                    */
#define  e_CI_LoadLimLow (float) 0.0
#define  e_CI_LoadLimUp  (float) 999.0


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*  Note-1                                                                   */
/* f_PerDufCon = this is an input for Burnup,                                */
/*               percent of duff that is consumed, whole number 0 -> 100     */
/*               send in a -1 will cause burnup to use it's orginal default  */
/*               settings                                                    */
/* f_HSFCon    = this is an input for Burnup, in Tons Per Are                */
/*               This is the amount of Shrub, Herb, Foliage and Branch that  */
/*                the caller of Burnup says gets consumed in the fire        */
/*                Burnup doesn't model these inputs so they need to be sent  */
/*                in.                                                        */
/*               This value gets used by Burnup to calculate intial fire     */
/*                intensity and for emissions calculations.                  */
/*                                                                           */
/* Moistures   = send in as whole numbers                                    */
/* Loads       = tons per acre                                               */
/* file names  = enter a file name or for none use NULL ("")                 */
/*                                                                           */

typedef struct {

#define  e_DufMin  0.446      /* limits tons per acre, NOTE: these must jive */
#define  e_DufMax  356.79     /*  with the kg/2 e_wdf1,e_wdf1 in bur_brn.h   */
float    f_Duff;              /* Duff Load - Limits in tpa .446 ->356.79     */

#define e_DufDepMin  0.0      /* limits inches                               */
#define e_DufDepMax  999.0
float   f_DufDep;             /* Duff Depth - inches limit 0 --> 999         */

float   f_MoistDuff;          /* Duff moisture, Limits 10 -> 197.2           */
                              /*  See defines in bur_brn.h                   */

#define e_MoiDW10Min  3.0
#define e_MoiDW10Max  298.0
float   f_MoistDW10;          /* Down Wood 10 hr Moisture, limit  3 -> 298   */


float   f_MoistDW1000;        /* Down Wood 1k hr Moiture, limits 1 -> 300    */
                              /* See defines in bur_brn.h                    */

float   f_Lit;                /* Litter                                      */
float   f_DW1;                /* 1 Hr, 0->1/4 inch                           */
float   f_DW10;               /* 10 Hr, 1/4 -> 1 inch                        */
float   f_DW100;              /* 100 Hr, 1 -> 3 inch                         */

/*...........................................................................*/
/* NOTE:                                                                     */
/*  FOFEM5 uses f_DW1000 and f_pcRot, gets from user interface               */
/*   splits into 3,6,9,20, and used in graph, etc                            */
/*   They aren't need for the actual calcs.                                  */
/*  For other users, DLL, Batch, etc just need to fill in the 3,6,9,20       */
/*   sound and rotten                                                        */
/*  cr_WD..gets used when reading in a batch file to hold the weigh dist     */
/*          code, it will get used to distrubute the loads to size clases    */
float   f_DW1000;                       /* Total 1000 hr wood, 3+ inch       */
float   f_pcRot;                        /* percent rotten                    */
#define  eC_WD 30                       /* Weight Dist id code, Even,Left,etc*/
char     cr_WD[eC_WD];


float   f_Snd_DW3;                      /* Large Wood Sound 1000 Hr          */
float   f_Snd_DW6;                      /* 3,6,9 & 20+ size classes          */
float   f_Snd_DW9;
float   f_Snd_DW20;

float   f_Rot_DW3;                      /*  Large Wood Rotten 1000 Hr         */
float   f_Rot_DW6;
float   f_Rot_DW9;
float   f_Rot_DW20;


/*-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                    CI fields need for non-burnup calcs                   */
float  f_CroFol;         /* Crown Foliage Load                               */
float  f_CroBra;         /* Crown Branch Load                                */
float  f_Pc_CroBrn;      /* Percent of Crown that burns 0 -> 100 whole num   */
float  f_Herb;           /* Herb Load                                        */
float  f_Shrub;          /* Shrub Load                                       */

/*...........................................................................*/
/* NOTE: these fields get filled in the CM_Mngr() function after it calcs   */
/*  the consumed load for herb+Shr+Fol+Bra, duff                             */
/* So you don't need to fill these in unless you call BCM_Mngr() direct      */
/* See Note-2 above                                                          */
float   f_HSFCon;                       /* Herb,Shrub,Foliage,Branch consumed */
float   f_PerDufCon;                    /* % of Duff consumed, See Notes above*/

/*...........................................................................*/
/* If a path/filename is put in here then burnup will create the files       */
/*   and leave them there                                                    */
char    cr_LoadFN[400];                  /* Burnup output Amount File Name     */
char    cr_EmiFN[400];                  /* Burnup output Emission File Name   */

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*  These variables get initialized in the CI_Init() function with          */
/*   default values.                                                         */
/*  These values are then sent into burnup with this structure whereas they  */
/*   use to go directly to burnup, they are in here now so that they can be  */
/*   set by a user if they want, if not they will get set with defaults in   */
/*   the CI_Init() function                                                 */
/*  I put these paramters here in case a user would want to set them.        */
/*  In fofem5 they could only set them if they used the burnup input file    */
/*   option                                                                  */

/* Sigma values sent into burnup .........defaults                           */
float    f_SURat_Lit;                   /*  8200.0                           */
float    f_SURat_DW1;                   /*  1480.0                           */
float    f_SURat_DW10;                  /*   394.0                           */
float    f_SURat_DW100;                 /*   105.0                           */
float    f_SURat_DWk_3_6;               /*    39.4                           */
float    f_SURat_DWk_6_9;               /*    21.9                           */
float    f_SURat_DWk_9_20;              /*    12.7                           */
float    f_SURat_DWk_20;                /*    5.91                           */

long int l_MAX_TIMES;   /* maximum number iterations burnup does, default 3000          */

float    f_INTENSITY;   /* intensity of the igniting surface fire, kW/m2 sq m, 40.0 -> 1.0e5, burnup var - fi */

float    f_IG_TIME;     /* residence time of the ignition surface fire, seconds */
                        /* default = 60.0, fofem's burnup input file uses 30.0     */
                        /* burnup var = ti,   limits 10.0 -> 200.0                  */

float    f_WINDSPEED;   /* windspeed at top of fuelbed meters/second        */
                        /* burnup var = u, default 0, limits 0.0 -> 5.0,     */

float    f_DEPTH;       /* fuel depth, meters,                               */
                        /* burnup var = d, defualt 0.3, limits 0.1 -> 5.0    */

float    f_AMBIENT_TEMP; /* ambient air temperature, degrees Celcus          */
                         /* burnup var = tamb, default 27,                   */
                         /*  if ( tamb-273 < tam1 || tamb-273 > tam2)        */
                         /*   const double tam1 = -40.0,  tam2 = 40.0;       */

float    f_R0;           /* fire environment minumum dimension paramter      */
                         /*  default 1.83                                    */

float    f_DR;           /* fire environment increment temp parater          */
                         /*  default 0.40                                    */

float    f_TIMESTEP;     /* time step for integration of burning rates.      */
                         /* TIMESTEP * MAX_TIMES gives max simulation period */



/*-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                           Cover Group                                    */
#define e_GrassGroup    "GrassGroup"
#define e_SageBrush     "Sagebrush"
#define e_ShrubGroup    "ShrubGroup"
#define e_Pocosin       "Pocosin"
#define e_Ponderosa     "Ponderosa"
#define e_WhiPinHem     "WhiPinHem"      /* White Pine Hemlock            */
#define e_RedJacPin     "RedJacPin"      /* Red Jack Pine                 */
#define e_BalBRWSpr     "BalBRWSpr"      /* Balsam, Blk Red Whit Spruce  */


#define GRASSGROUP  e_GrassGroup
#define SAGEBRUSH   e_SageBrush
#define SHRUBGROUP  e_ShrubGroup
#define POCOSIN     e_Pocosin
#define PONDEROSA   e_Ponderosa
#define WHIPINHEM   e_WhiPinHem
#define REDJACPIN   e_RedJacPin
#define BALBRWSPR   e_BalBRWSpr
#define  eC_CoverGroup  50
char   cr_CoverGroup [eC_CoverGroup];


/* Cover Type Classification                                                 */
#define  e_CI_SAF  "SAF"
#define  e_CI_NVCS "NVCS"
#define  e_CI_FCC  "FCC"
#define  e_CI_DefaultCoverClass e_CI_SAF

char cr_CoverClass[20];


/*-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                              Season                                      */
#define e_Winter  "Winter"
#define e_Spring  "Spring"
#define e_Summer  "Summer"
#define e_Fall    "Fall"

#define WINTER     e_Winter
#define SPRING     e_Spring
#define SUMMER     e_Summer
#define FALL       e_Fall

#define e_SeasonDefault e_Summer    /* Season is a required field though     */


#define eC_Season 20
char   cr_Season[eC_Season];


/*-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                              Region                                      */
#define e_CI_SouthEast    "SouthEast"
#define e_CI_InteriorWest "InteriorWest"
#define e_CI_PacificWest  "PacificWest"
#define e_CI_NorthEast    "NorthEast"

#define SOUTHEAST    e_CI_SouthEast
#define INTERIORWEST e_CI_InteriorWest
#define PACIFICWEST  e_CI_PacificWest
#define NORTHEAST    e_CI_NorthEast
#define eC_Region 20
char   cr_Region[eC_Region];


/*-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                             Fuel Category                                */
#define e_Natural  "Natural"
#define e_Piles    "Piles"
#define e_Slash    "Slash"

#define NATURAL  e_Natural
#define PILES    e_Piles
#define SLASH    e_Slash

#define e_FuelCategoryDefault e_Natural    /* Default */

#define eC_FuelCategory 20

char   cr_FuelCategory[eC_FuelCategory];

/*-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                            Moisture Method                               */
#define e_Entire   "Entire"             /* Duff Moisture Methods            */
#define e_Lower    "Lower"              /*  tells what method was used       */
#define e_NFDR     "NFDR"               /*  to measure the duff moist.       */
#define e_Adj_NFDR "AdjNFDR"

#define ENTIRE      e_Entire
#define LOWER       e_Lower
#define NFDR        e_NFDR
#define ADJ_NFDR    e_Adj_NFDR

#define e_DufMoiMetDefault ""           /* Default */

#define eC_DufMoiMet 20
char  cr_DufMoiMet [eC_DufMoiMet];

} d_CI;


/*-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
void  WINAPI CI_Init (d_CI *a_CI);

int CI_isSpring (d_CI *a_CI);
int CI_isFall   (d_CI *a_CI);
int CI_isWinter (d_CI *a_CI);
int CI_isSummer (d_CI *a_CI);
int CI_isSeason (d_CI *a_CI);

int CI_isShrubGroup (d_CI *a_CI);
int CI_isGrassGroup (d_CI *a_CI);
int CI_isSageBrush (d_CI *a_CI);
int CI_isPocosin (d_CI *a_CI);
int CI_isPonderosa (d_CI *a_CI);
int CI_isWhiPinHem (d_CI *a_CI);
int CI_isRedJacPin (d_CI *a_CI);
int CI_isBalBRWSpr (d_CI *a_CI);
int CI_isCoverGroup (d_CI *a_CI);

int CI_isSouthEast   (d_CI *a_CI);
int CI_isInteriorWest(d_CI *a_CI);
int CI_isPacificWest (d_CI *a_CI);
int CI_isNorthEast   (d_CI *a_CI);


int CI_isNatural(d_CI *a_CI);
int CI_isPiles  (d_CI *a_CI);
int CI_isSlash  (d_CI *a_CI);

int CI_isDufEntire  (d_CI *a_CI);
int CI_isDufLower   (d_CI *a_CI);
int CI_isDufNFDR    (d_CI *a_CI);
int CI_isDufAdjNFDR (d_CI *a_CI);
int CI_isDufMethod  (d_CI *a_CI);


int CI_isDuffWet (d_CI *a_CI);

void CI_Display (d_CI *a_CI);
float CI_MaxLoad (d_CI *a_CI);
int  CI_OnlyDuff (d_CI *a_CI);
