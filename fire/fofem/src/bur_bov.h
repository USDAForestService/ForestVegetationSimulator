/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: bur_BOV.h     Burnups Output Values  Structure & Time Heat values
* Desc:
*
* Date: 4/30/00
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/

#define     e_BOV_Litter     "Litter"
#define     e_BOV_DW1        "DW1"
#define     e_BOV_DW10       "DW10"
#define     e_BOV_DW100      "DW100"
#define     e_BOV_DW1kSnd    "DW1kSnd"
#define     e_BOV_DW1kRot    "DW1kRot"


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Fire Intensity Array, used for Pam Soil Model                             */
#define   eC_fi 1000          /* Size of the Array                           */
#define   e_FiInit   -1       /* Init value used                             */


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
int   BOV_GetRemAmt (float *af_RemAmt, char cr_Cat[]);
int   BOV_PutRemAmt (float f_RemAmt, int iX, float f_TimBurOut );
int   BOV_Entry (char cr_BOV_Cat[], int i_3InId);
void  BOV_Init  (void);
int   BOV_Last  (void);
void  BOV_SetIgnite (char cr_YN[]);
int   BOV_GetIgnite (void);
int   BOV_Get_TimHea (int *ai_time, float *af_fi, char cr_Mess[]);
float BOV_GetRem3 (char cr_BOV_Cat[], int i_3InId);
int   BOV_Set_TimHea (double time, double fi);
void  BOV_Set_fiSHFB (float f_fi);
float BOV_Get_fiSHFB (void);
