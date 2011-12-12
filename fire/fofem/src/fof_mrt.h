/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_mrt.h Species Master Table
* Desc:
* Date: 2/4/04
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/*                        Input Mortality Struct                             */
typedef struct {
     char  cr_Spe[20];                  /* Species                           */
     float f_DBH;                       /* Diameter, inches                  */
     float f_FS;                        /* Flame Lenght or Scorch Height feet*/
     char  cr_FS[10];                   /* "F" or "S" identifies f_FS field  */
     float f_Hgt;                       /* Species Height feet               */
     float f_CroRat;                    /* Crown Ration 1 -> 10              */
     char  cr_FirSev[10];               /* Fire Severity Code "Low" or ""  */
     float f_Den;                       /* Trees per acre                    */
   } d_MI;

void MI_ISS (d_MI *a_MI, d_ISS *a_ISS);
void MI_SetSF  (float f_Sch, d_MI *a_MI);

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/*                        Output Mortality Struct                            */
typedef struct {

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* These values are calculated for a single Species                          */
   float f_Mort;                        /* Individ Spe Probility of Mortality*/
   float f_TotPreTree;                  /* Total Prefire Trees               */
   float f_Killed;

   float f_BasPre;                      /* Prefire Basal Area                */
   float f_BasKil;                      /* Basal Area of Killed trees        */
   float f_BasPos;                      /* Post fire Basal Area              */

   float f_CovPreLiv;                   /* Prefire Canopy Cover              */
   float f_CovPosLiv;                   /* Postfire Canopy Cover             */


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* These values are accumulated for the Stand                                */
   float f_AvgMort;                     /* Averged all Mortality             */
   float f_4AvgMort;                    /* Averged all Mortality > 4 DBH    */
   float f_TotKilled;                   /* Total Killed Trees                */
   float f_AvgDBHKilled;                /* Avg DBH of Killed Trees           */

   float f_BasTotPre;                   /* Basal Area, Prefire               */
   float f_BasTotKil;                   /* Basal Area of Killed trees        */
   float f_BasTotPos;                   /* Basal Area Post fire,(Pre - Killd)*/

   float f_CovTotPreLiv;                /* Prefire Canopy Cover              */
   float f_CovTotPosLiv;                /* Postfire Canopy Cover             */

   int i_MortEqu;                       /* Mortality Equation used in calc  */

   } d_MO;


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
#define e_Flame  "F"
#define e_Scorch "S"

/* Low and High Limits on Flame Length and Scorch Height                     */
#define e_FlaLow   1.0
#define e_FlaHig   20.0

#define e_ScoLow   1.0
#define e_ScoHig   245.0




/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
float WINAPI MRT_Calc  (d_MI *a_MI, d_MO *a_MO, char cr_ErrMes[]);
int   WINAPI MRT_LoadSpe (char cr_Pth[], char cr_ErrMes[]);

void  MRT_InitST (void);

void  MRT_Total (d_MI *a_MI, d_MO *a_MO, float f_Prob);

void  WINAPI MO_Init (d_MO *a_MO);
float MRT_Overlap( float f_SqFtCov);
