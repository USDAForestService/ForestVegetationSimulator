/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_lcc.h      FOFEM Library Container Class
* Desc:
*
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/

#ifdef FOF_DLL_EXPORT
#define FOF_EXPORT __declspec(dllexport)
#else
#define FOF_EXPORT __declspec(dllimport)
#endif

/*-------------------------------*/
// typedef class FOF_EXPORT d_LCC {

 class  FOF_EXPORT d_LCC {
 public:
 d_CO s_CO;      /* Consumed Output */
 d_CI s_CI;      /* Consumed Inputs */

 d_MI s_MI; 
 d_MO s_MO; 


 void CI_init (); 
 void CO_init (); 

 void CI_f_Duff     ( float f);
 void CI_f_DufDep   ( float f);
 void CI_f_MoistDuf ( float f);

 void   CI_f_Herb          ( float f);
 void   CI_f_Shrub         ( float f);
 void   CI_f_CroFol        ( float f);
 void   CI_f_CroBra        ( float f);
 void   CI_f_Pc_CroBrn     ( float f);
 void   CI_f_MoistDW10     ( float f);
 void   CI_f_MoistDW1000   ( float f);
 void   CI_f_Lit           ( float f);
 void   CI_f_DW1           ( float f);
 void   CI_f_DW10          ( float f);
 void   CI_f_DW100         ( float f);
 void   CI_f_Snd_DW3       ( float f);
 void   CI_f_Snd_DW6       ( float f);
 void   CI_f_Snd_DW9       ( float f);
 void   CI_f_Snd_DW20      ( float f);
 void   CI_f_Rot_DW3       ( float f);
 void   CI_f_Rot_DW6       ( float f);
 void   CI_f_Rot_DW9       ( float f);
 void   CI_f_Rot_DW20      ( float f);

 void   CI_cr_Region       (char cr[]);
 void   CI_cr_Season       (char cr[]);
 void   CI_cr_FuelCategory (char cr[]);
 void   CI_cr_DufMoiMet    (char cr[]);
 void   CI_cr_CoverGroup   (char cr[]);
 void   CI_cr_LoadFN       (char cr[]);
 void   CI_cr_EmiFN        (char cr[]);

 int   CM_mngr (char cr_ErrMes[]);

 void  CO_cr_BrnIg (char cr[]);

int   CO_GetEm(int i, float *PM25, float *PM10, float *CH4, float *CO2, float *CO, 
            float *NOX, float *SOD, float *Intensity);

void  CO_GetEmTot (char cr[], float *PM25, float *PM10, float *CH4, float *CO2, 
                                   float *CO,  float *NOX, float *SO2);


 float CO_f_DufPos      ();
 float CO_f_LitPos      ();
 float CO_f_HerPos      ();
 float CO_f_ShrPos      ();
 float CO_f_DW1Pos      ();
 float CO_f_DW10Pos     ();
 float CO_f_DW100Pos    ();

 float CO_f_S3Pos       ();
 float CO_f_S6Pos       ();
 float CO_f_R3Pos       ();
 float CO_f_R6Pos       ();
 float CO_f_S9Pos       ();
 float CO_f_S20Pos      ();
 float CO_f_R9Pos       ();
 float CO_f_R20Pos      ();


 float CO_f_S3Con      ();
 float CO_f_S6Con       ();
 float CO_f_R3Con       ();
 float CO_f_R6Con       ();
 float CO_f_S9Con       ();
 float CO_f_S20Con      ();
 float CO_f_R9Con      ();
 float CO_f_R20Con      ();


 float CO_f_Rot_DW1kCon ();
 float CO_f_DufCon      ();

float CO_f_LitCon      ();
float CO_f_HerCon      ();
float CO_f_ShrCon      ();



 float CO_f_DW1Con      ();
 float CO_f_DW10Con     ();
 float CO_f_DW100Con    ();
 float CO_f_Snd_DW1kCon ();
 float CO_f_TotPer      ();

/*----------------------------------------------------------*/

 void MRT_LoadSpec (char cr_ErrMes[]);
 float  MRT_Calculate (char cr_ErrMes[]);
 void MO_Initial (); 
 void MI_cr_Spe (char cr[]); 
 void MI_f_CroRat (float f);
 void MI_f_Den (float f);
 void MI_f_DBH (float f);
 void MI_f_Hgt (float f);
 void MI_f_FS  (float f); 
 void MI_cr_FS (char cr[]);
 void MI_cr_FivSev (char cr[]); 

 float MO_f_Mort (); 
 float MO_f_TotKilled (); 
 float MO_f_AvgMort (); 
}  ; 
