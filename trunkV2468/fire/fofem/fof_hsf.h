//
// $Id$
//

int HSF_Mngr  (d_CI *a_CI, d_CO *a_CO, char cr_ErrMes[]);
int HSF_Check (d_CI *a_CI, char cr_ErrMes[]);


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
int   Calc_CrownFoliage (d_CI *a_CI, float *af_Con, float *af_Post, float *af_Percent);
int   Calc_CrownBranch (d_CI *a_CI, float *af_Con, float *af_Post, float *af_Percent);
int   Calc_Herb (d_CI *a_CI, float *af_Con, float *af_Post, float *af_Percent);
int   Calc_Shrub (d_CI *a_CI, float *af_Con, float *af_Post, float *af_Percent);
float Shrub_Equ (d_CI *a_CI, int i_Equ);
float Equ_234_Per (d_CI  *a_CI);
float Equation_16 (d_CI  *a_CI);
