//
// FIRE-FOFEM $Id$
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc:
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
typedef struct {
   float f_Per;
   int   i_PerEqu;
   float f_Red;
   int   i_RedEqu;
   char  cr_ErrMess[400];

   float f_MSEPer;
   int   i_MSEEqu;
   char  cr_MSEMess[400];

}  d_DUF;

int  DUF_Mngr (d_CI *a_CI, d_DUF *a_DUF);
void   DUF_Init (d_DUF *a_DUF);


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
void  Duf_Default (d_CI *a_CI, d_DUF *a_DUF);
int  DUF_Mngr (d_CI *a_CI, d_DUF *a_DUF );
int  DUF_InteriorWest  (d_CI *a_CI, d_DUF *a_DUF);
int  DUF_PacificWest  (d_CI *a_CI, d_DUF *a_DUF);
int  DUF_NorthEast (d_CI *a_CI, d_DUF *a_DUF);
int  DUF_SouthEast (d_CI *a_CI, d_DUF *a_DUF);

void  DUF_Init (d_DUF *a_DUF);


int  isPonderosa(void);

int DUF_Calc (d_CI *a_CI, d_DUF *a_DUF, float *af_Con, float *af_Post,
                float *af_Percent);

int  Equ_8_PerRed (d_CI *a_CI, d_DUF *a_DUF);

int DUF_GetDepRed(d_CI *a_CI, d_DUF *a_DUF, float *af_Post, float *af_Percent);

void Equ_1_Per    (d_CI *a_CI, d_DUF *a_DUF);
void Equ_2_Per    (d_CI *a_CI, d_DUF *a_DUF);
void Equ_3_Per    (d_CI *a_CI, d_DUF *a_DUF);
void Equ_3_PerRed (d_CI *a_CI, d_DUF *a_DUF);
int  Equ_3_7      (d_CI  *a_CI,  d_DUF  *a_DUF) ;
void Equ_4_Per    (d_CI *a_CI, d_DUF *a_DUF);
void Equ_5_RedPer (d_CI *a_CI, d_DUF *a_DUF);
void Equ_5_Red    (d_CI *a_CI, d_DUF *a_DUF);
void Equ_6_Red    (d_CI *a_CI, d_DUF *a_DUF);
void Equ_7_Red    (d_CI *a_CI, d_DUF *a_DUF);
void Equ_9_MSE    (d_CI *a_CI, d_DUF *a_DUF);
void Equ_10_MSE   (d_CI *a_CI, d_DUF *a_DUF);
void Equ_11_MSE   (d_CI *a_CI, d_DUF *a_DUF);
void Equ_12_MSE   (d_CI *a_CI, d_DUF *a_DUF);
void Equ_13_MSE   (d_CI *a_CI, d_DUF *a_DUF);
void Equ_14_MSE   (d_DUF *a_DUF);
void Equ_15_PerRed(d_CI *a_CI, d_DUF *a_DUF, char cr_Pine[]);
void Equ_16_Per   (d_CI *a_CI, d_DUF *a_DUF);
void Equ_17_Per   (d_DUF *a_DUF);
void Equ_18_MSE   (d_DUF *a_DUF);
void Equ_20_RedPer(d_CI *a_CI, d_DUF *a_DUF);
void Equ_201_Per  (d_CI *a_CI, d_DUF *a_DUF);
void Equ_202_MSE  (d_DUF *a_DUF);
