/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: bur_brn.h
* Desc: Defines for Burnup code.
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* This is for use with the FRAMES Web fofem program                         */
/* See bur_brn.c  EFM_Open()                                                 */
#define e_FrameFN       "#FRAMES#"
#define e_Framefh       (FILE *) -1
#define e_Frame_Start   "Emis-Start"
#define e_Frame_End     "Emis-End"



/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
#define e_BurEqu 999                    /* Burnpup Equation number           */

/* Combustion Efficiencies, Flaming and Smoldering                           */
#define e_ComEffFla 0.97
#define e_ComEffSmo 0.67

/* These get used to adjust values going into the tpig & tchar arrays        */
/*  while they get loaded before burnup runs                                 */
#define e_tpig_adj    273.0
#define e_tchar_adj   273.0

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Defines Burnup uses in 'BRN_CheckData' function to check limits           */
#define e_cht1  1000.0                       /* burnup's limit checks for    */

/*........................................................                   */
/* original  #define e_cht2  2000.0                                          */
/* ER said ok to change to 3000                                              */
#define e_cht2  3000.0                       /* 'cheat' arrary               */


#define e_tig1  200.0
#define e_tig2  400.0

#define e_tch1  250.0
#define e_tch2  500.0

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
// Change 7-18-12 - change lower limit, a user was using lower limits - 
//                  I tried some test with lowering the limit and it seemed 
//                  to be OK, not sure how/who came up with the original
//                  lower limit
// NOTE see fof_ci.h  e_DufMin - both need to match 
 #define e_wdf1 0.022   // this is kg/m2 - and equal to 0.1 tons per acre 
//  #define  e_wdf1 (double) 0.1                  /* Orig - duff loading limits, kg/m2 */


/* #define  wdf2 (double) 30.0    Original Amount */
/* changed 12/28/01, they wanted higher, some cover types have lots of duf   */
/*  80 kg/m2 is about 357 tons per acre */
#define  e_wdf2 (double) 80.0                  /* kg/m2                       */


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
#define  e_dfm1  (double) 0.1                  /* Duff Moisture limits        */
#define  e_dfm2  (double) 1.972

#define  e_fms1  (double) 0.01                 /* Moisture Limits             */
#define  e_fms2  (double) 3.0

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* These are the actual Fuel Load Limits the Burnup calc uses                */
/* #define  small (double) 1.e-06                                           */
#define  e_small (double) 1.e-08               /* load limits                 */
#define  e_big   (double) 1.e+06

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Defualt settings for Burnup input parameters, like the ones used when     */
/* Building an input file for burnup to use                                  */
#define  e_MAX_TIMES       3000
#define  e_INTENSITY       50.00
#define  e_IG_TIME         60.00
#define  e_WINDSPEED        0.00
#define  e_DEPTH            0.3
#define  e_AMBIENT_TEMP    27.00
#define  e_R0               1.83
#define  e_DR               0.40
#define  e_TIMESTEP        15.00

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                   Sigmas input parameters used by burnup                  */
#define  e_SURat_Lit       8200.0
#define  e_SURat_DW1       1480.0
#define  e_SURat_DW10       394.0
#define  e_SURat_DW100      105.0
#define  e_SURat_DWk_3_6     39.4
#define  e_SURat_DWk_6_9     21.9
#define  e_SURat_DWk_9_20    12.7
#define  e_SURat_DWk_20      5.91





/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* These get loaded into burnup arrays along with the individual fuel loads  */
/*  before runing Burnup                                                     */
/* below we have #defines for running Burnup with and without input file     */
/* ***>>>   NOTE READ this                                                   */
/* NOTE READ                                                                 */
/* If you change any of these you got change them in both places, in the     */
/*  printf format statement and the individual #defines below                */

/* printf format statements for creating burnup input file                   */
#define  e_SoundFmt  "1 %2d %13.8f  18600.0  %5.3f  513.0  %7.2f 2750.0  0.133 327.0 377.0 0.05\n"
#define  e_RottenFmt "1 %2d %13.8f  18600.0  %5.3f  224.0  %7.2f 2750.0  0.133 302.0 377.0 0.05\n"


/* Defines sent into Burnup directly, when NOT using an input file           */
/* **>> NOTE READ explaination above, before CHANGING any of these           */
#define e_htval 18600.0          /* low heat of combustion , J / kg          */
#define e_Snd_dendry  513.0      /* ovendry mass density, kg/cum Sound       */
#define e_Rot_dendry  224.0      /* ovendry mass density, kg/cum Rotten      */
#define e_cheat       2750.0     /* specific heat capacity (J / K) / kg dry mass                   */
#define e_condry      0.133      /* thermal conductivity W / m  K , ovendry  */
#define e_Snd_tpig    327.0      /* ignition temperature , K   Sound         */
#define e_Rot_tpig    302.0      /* ignition temperature , K   Rotten        */
#define e_tchar       377.0      /* char temperature , K                     */
#define e_ash         0.05       /* mineral content , fraction dry mass      */


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
#define false 0
#define true  1
#define bool  int

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/

/* This was orginally 150, this is used to set array sizies                  */
/* #define MAXNO  150  */              /* Max # of Fuel Loadings allowed    */
#ifdef DOS
  #define MAXNO   12                   /* Max # of Fuel Loadings allowed    */
#else
  #define MAXNO   20                     /* Max # of Fuel Loadings allowed    */
#endif

#define MAXTYPES 3
#define MAXKL  (MAXNO * ( MAXNO + 1 ) / 2 + MAXNO )
#define MXSTEP 20

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* This struct is used to check data in the input Burnup loading file        */
typedef struct {
     int i_MAX_TIMES;
     int i_INTENSITY;
     int i_IG_TIME;
     int i_WINDSPEED;
     int i_DEPTH;
     int i_AMBIENT_TEMP;
     int i_r0;
     int i_dr;
     int i_TIMESTEP;
     int i_DUFF_LOAD;
     int i_DUFF_MOIST;
  } d_CFF;

void   CFF_Init    (d_CFF *a_CFF);
int    CFF_ChkAll  (d_CFF  *a_CFF);


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/



int   BRN_Run_UIF (char cr_InFN[], char cr_Err[], float f_DufConPerCent,
                    char cr_LoadFN[], char cr_HeatFN[], float f_BrnCon);
int   BRN_Run (char cr_LoadFN[], char cr_HeatFN[], float f_DufConPerCent, float f_BrnCon, char cr_ErrMes[]);

void  BRN_Init(void);

int   BRN_ReadInFil (char cr_FN[], char cr_Err[]);
int   BRN_CheckData (char cr_ErrMes[]);
void  BRN_SetFireDat (long NumIter, double Fi, double Ti, double U, double D, double Tamb,
                  double R0, double Dr, double Dt, double Wdf, double Dfm);
int   BRN_ChkSwi  (char cr[]);
float BRN_Intensity (float f_Con);
int BRN_SetFuel (int *aiX, char cr_SR[], float f_Load, float f_Moist, float f_Sigma);




int     BurnupNone (char cr_HeatFN[], float f_Con );
void    Heat_Heading (FILE  *fh);
void    Heat_HeadingFS (FILE  *fh);
void    Arrays (void);
long    loc (long k, long l);
double  func (double h, double theta);
double  ff (double x, double tpfi, double tpig);
int     Start(double tis, long now, long *ncalls, double *ad_Con);
void    OverLaps(void);
double  FireIntensity(double *ad_pcSmoCon);
double  DryTime(double enu, double theta);
int     Stash(char *HistFile,  double tis, long now);
void    Sorter(void);
double  TIgnite(double tpdr, double tpig, double tpfi, double cond, double chtd, double fmof, double dend, double hbar);
double  TempF(double q, double r);
void    HeatExchange (double dia, double tf, double ts, double *hfm, double *hbar, double cond, double *en);
void    DuffBurn (double wdf, double dfm, double *dfi, double *tdf,
                  float f_DufConPerCent, double *ad_Duf_CPTS);
void    Step (double dt, double tin, double fid, long *ncalls, double *ad_Con);
int     Summary (char *OutFile,char cr_ErrMes[]);
long    Nint(double input);
void    CalculateEmissions(int i_Fst);


double  pow2(double input);

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*                           Emission Sturcture                              */
typedef  struct {
   double d_FlaCon;                    /* Store consumed for time step      */
   double d_SmoCon;

   double dN_FlaCon;                   /* Store consumed totals             */
   double dN_SmoCon;

   double d_PM25;                      /* single time step amounts           */
   double d_PM10;
   double d_CH4;
   double d_CO2;
   double d_CO;
   double d_NOX;
   double d_SO2;


/*..........................................................................*/
/* Running Totals */
   double dN_PM25F;                    /* Flame Totals                       */
   double dN_CH4F ;
   double dN_COF  ;
   double dN_CO2F ;
   double dN_PM10F;
   double dN_NOXF;
   double dN_SO2F;

   double dN_PM25S;                    /* Smoldering Totals                  */
   double dN_CH4S ;
   double dN_COS  ;
   double dN_CO2S ;
   double dN_PM10S;
   double dN_NOXS;
   double dN_SO2S;


/*..........................................................................*/
/* amounts at each time step */
   double d_PM25F;                    /* Flame Totals                       */
   double d_CH4F ;
   double d_COF  ;
   double d_CO2F ;
   double d_PM10F;
   double d_NOXF;
   double d_SO2F;

   double d_PM25S;                    /* Smoldering Totals                  */
   double d_CH4S ;
   double d_COS  ;
   double d_CO2S ;
   double d_PM10S;
   double d_NOXS;
   double d_SO2S;


   double d_FlaDur;                     /* Duration, last time step that     */
   double d_SmoDur;                     /* consumed something                */

 } d_ES;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
int  ES_Calc (d_ES *a_ES, double d_WooLit,  double d_Duff, double d_HSFB,
               double d_pcSmo, double d_time);

void  ES_Init (d_ES *a_ES);
float ES_FlaDur (void);
float ES_SmoDur (void);
float ES_PM25F(void);
float ES_PM10F(void);
float ES_CH4F (void);
float ES_COF  (void);
float ES_CO2F (void);
float ES_NOXF (void);
float ES_SO2F (void);


float ES_PM25S(void);
float ES_PM10S(void);
float ES_CH4S (void);
float ES_COS  (void);
float ES_CO2S (void);
float ES_NOXS (void);
float ES_SO2S (void);

float ES_FlaCon (void);
float ES_SmoCon (void);

void   Save_SGV (d_ES *a_ES, double d_time, double d_FirInt);
double Get_Cons (double d_old, double d_new);
double Duff_CPTS (double *ad_Duf_Tot, double d_Duf_Sec, double d_NumSec);
int    Bur_ChkArgs (char cr_Line[]);
void   Bur_Error   (char cr_Err[], char cr1[], char cr_Line[]);
int    EFM_Write  (d_ES  *a_ES, double d_Time, double d_FirInt);
int    EFM_Open   (char cr_FN[]);
void   EFM_Close  (void);
void   Bur_RemoveTmp (void);
void   Bur_SumDivErr (void);
void  bstrupr (char cr[]);
