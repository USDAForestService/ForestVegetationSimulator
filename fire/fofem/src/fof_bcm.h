/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_bcm.h    Burnup Consumed Manager
* Desc: Defines
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/* Down Wood 1 Hr Moisture adjustment                                        */
#define e_DW1hr_Adj       0.02   /* NOTE - before changing check all usage   */
#define e_DW100hr_Adj     0.02   /* READ the NOTE below                      */

/* NOTE --> this define has to be the larger of the two above defines        */
/*           (they happen to be the same now) this gets used to check        */
/*           moistures before running burnup                                 */
#define e_DWChk  0.02

#define e_DW1000hr_AdjRot 2.5


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
int   WINAPI BCM_Mngr (d_CI *a_CI,  d_CO *a_CO, char cr_ErrMes[]);
void  BCM_SetInputs (d_CI *a_CI);
int   BCM_Get (char cr_BOV_Cat[], float f_Load, float *af_Con);
float BCM_Get3 (char cr_BOV_Cat[], int i_3InId, float f_Load);
float BCM_DW10M_Adj (char cr_DWHr[], float f_Moist);
float  BCM_DW1k_MoiRot (float f_Moist);

int  BCM_Check (d_CI *a_CI, char cr_ErrMes[]);
int _Limit (float f_Load, char cr_ErrMes[], char cr[]);
int _Pc_DufCon (float f_Pc, char cr_ErrMes[]);
int _ChkDuff (d_CI *a_CI, char cr_ErrMes[]);
