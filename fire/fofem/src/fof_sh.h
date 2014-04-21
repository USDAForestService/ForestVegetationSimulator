/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_sh.h
* Desc: Soil Heating defines for input and output structures
*
* NOTE: To DLL (Dynamic Link Library) uses. Do NOT change any of the
*      #defines in this file.
*      The were used to compile the DLL, if you change them they will
*       NOT effect the functionality of the DLL.
*       They are in here for your use and reference. For example the
*       e_Layers #define is the number soil layers and you may want to
*       use it in a loop to process the output for each layer.
*
*
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
#pragma once 

#define REAL float

#define e_SoiSimFail "Soil Simulation Failed"

/* This gets put at the beginning of all soil simulation error message */
/* the hypen at the at makes it unique signiture that we can use to */
/* identify soil error message from other error messages */
#define e_SoilErr    "Soil-"

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-   */
/* Used by Frames Web application version                                    */
/* See comments in  SI_fOpen () fof_sh.c                                     */
#define e_FrameSFN  "#FrameSoil#"
#define e_FrameSfh  (FILE *) -1

#define e_FrameSstart "Soil-Start"
#define e_FrameSend   "Soil-End"


FILE *SI_fopen (char cr_FN[], char cr[]);
void SI_fprintfVal (FILE *fh, char cr_Fmt[], float r_time);
void SI_fprintfRet (FILE *fh, char cr_Ret[]);
void SI_fclose  (FILE *fh);


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-   */
/* Degrees (celsius), used to look for deepest soil layer reaching the       */
/*  specified temperature                                                    */
#define e_Max1 60                       /* degrees in celsius                */
#define e_Max2 275

/*...........................................................................*/
#define e_mplus1 14                     /* Soil Simulation uses this define  */
#define e_Layers e_mplus1               /* user define for # of Soil Layers  */

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                          Soil Input Parameters Struct                     */
typedef struct {
   float f_DufDepPre;                   /* Pre and Post fire Duff Depth      */
   float f_DufDepPos;                   /*  inches                           */
   float f_SoilMoist;

/* If Soil Heating is being calculated using the Consumed Managers outputs   */
/* you can set this from d_CO.cr_BrnIg, which tells if burnup ignited or not */
/* If you are running the Soil Heating as a stand alone calculation this     */
/*  field will get set to YES when you SI_Init(SI) the sturct                */
   char cr_BrnIg[10];       /* "YES" or "NO", tells Burnup didn't ignite */

/* Soil Type  Arguments...........                        */
#define  e_LoaSke     "Loamy-Skeletal"
#define  e_FinSil     "Fine-Silt"
#define  e_Fin        "Fine"
#define  e_CoaSil     "Coarse-Silt"
#define  e_CoaLoa     "Coarse-Loamy"

#define  LOAMY_SKELETAL   e_LoaSke
#define  FINE_SILT        e_FinSil
#define  FINE             e_Fin
#define  COARSE_SILT      e_CoaSil
#define  COARSE_LOAMY     e_CoaLoa


#define  eC_SoilType  30
   char  cr_SoilType[eC_SoilType];


/* Moisture Condition Arguments ...............................                        */
#define  e_VeryDry "VeryDry"
#define  e_Dry      "Dry"
#define  e_Moderate "Moderate"
#define  e_Wet      "Wet"

#define  VERY_DRY    e_VeryDry
#define  DRY         e_Dry
#define  MODERATE    e_Moderate
#define  WET         e_Wet


#define eC_MoistCond  30
   char  cr_MoistCond [eC_MoistCond];

/*...........................................................................*/
/* These values come directly from the Burnup Model                          */
/* See notes in the fof_co.h file, in the d_CO struct definition             */
   int   i_Time;                           /* time duration in seconds       */
   int   i_Cnt;                            /* count for takin averag hea intn*/
   float f_fi;                             /* heat intensity                 */


/* This holds the fire intensity array that comes out of burnup, coming thru */
/*  the d_CO struct,                                                         */
/* eC_SGV is the fire intensity arraysize in d_CO   */
   float *ar_FI; 
//   float fr_Fi[eC_SGV];
 } d_SI;


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                          Soil Output strcuture                            */
/* Soil Layer Maximum Temperatures arrays                                    */
/* Temperature is in celsius                                                 */
/* Time is in minutes from start of fire                                     */
/* [0] index represents surface, followed by next layer down [1]             */

typedef struct {
   int   ir_Temp[e_Layers];             /* Maximum Temp                      */
   int   ir_Time[e_Layers];             /* reached at this time              */

   float f_cDufPre;                     /* Prefire Duff Dep - centimeters    */
   float f_cDufPost;                    /* Postfire Duff Dep - centimeters   */

   float f_Heatpc;                      /* Percent of heat that makes it thru*/
                                        /*  the duff during fire             */
                                        /* This is only relevant when the    */
                                        /* Duff Model is run,  0.0 -> 1.00   */

#define  e_LayNone -1                   /* tells when no layer reached degree*/
   int   i_LayMaxDeg1;                  /* deepest layers reaching specified */
   int   i_LayMaxDeg2;                  /*  degrees, see defines e_Max1, e_Max2 */

#define  e_SM_Duff "Duff"               /* Tells which soil model/simulation */
#define  e_SM_ZDuff "Zero-Duff"         /* was used to generate output       */
   char  cr_Model[30];

 } d_SO;


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
void  SI_Init (d_SI *a_SI);
void  SO_Init (d_SO *a_SO);
int   SH_Mngr (d_SI  *a_SI, d_SO  *a_SO,float fr_FI[], char cr_TmpPth[], char cr_ErrMes[]);
