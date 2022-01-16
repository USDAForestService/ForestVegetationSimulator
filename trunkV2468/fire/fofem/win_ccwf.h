//
// $Id$
//

/* Soil Type  Arguments...........                        */
#define  e_Soi_LoaSke     "Loamy-Skeletal"  /* use to be Boulder Creek */
#define  e_Soi_FinSil     "Fine-Silt"       /* use to be Palouse       */
#define  e_Soi_Fin        "Fine"            /* use to be Salkum        */
#define  e_Soi_CoaSil     "Coarse-Silt"     /* use to be Walla_Walla   */
#define  e_Soi_CoaLoa     "Coarse-Loam"     /* use to be Royal */



/* Weight Distribution for 3+ Woody Fuel .................                   */
#define  e_WD_Right        "Right"   /* weight to larger, 20+                */
#define  e_WD_Left         "Left"    /* weight to smaller, 3-6               */
#define  e_WD_Even         "Even"
#define  e_WD_End          "End"
#define  e_WD_Center       "Center"

#define eC_WndTxt 150         /* Siz of strings getting text from a window   */


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
void  CCW_Init (void);
int   CCW_isBatch (void);
int   CCW_Spring     (void);
int   CCW_Summer     (void);
int   CCW_Fall       (void);
int   CCW_Winter     (void);

int   CCW_SouthEast    (void);
int   CCW_PacificWest  (void);
int   CCW_InteriorWest (void);
int   CCW_NorthEast    (void);
int   CCW_isDufAdjNFDR (void);
int   CCW_isDufNFDR    (void);
int   CCW_isDufEntire  (void);
int   CCW_isDufLower   (void);
char  CCW_MoistCond    (void);
int   CCW_isFCC        (void);
void  CCW_GetWeiDis    (char cr_WndTxt[]);

int   CCW_Set_Season (int i_Season);

int   CCW_Set_MoistCond (int i_MoistCond);

int CCW_Set_Region (int i_Region);
int  CCW_SetRegion (char cr_Region[]);

int  CCW_Set_DufMoiMet (int i_DufMoiMet);
int  CCW_Set_FirSev    (int i_FirSev);

int  CCW_Set_Soil (int i_Soil);
int   CCW_SoilMoist (float *af_Moist);
void  CCW_GetSoilType (char cr_SoiTyp[]);
void  CCW_SetSoilMoist (float f_Moist);
int  CCW_isLow        (void);
