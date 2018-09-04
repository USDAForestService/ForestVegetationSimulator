//
// FIRE-FOFEM $Id$
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fir_smt.h Species Master Table
* Desc:
* Date: 5/18/99
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/*                          Species Master Table Struct                      */
typedef struct  {
   char cr_Spe[10];
   char cr_Name[150];
   int  i_MrtEqu;
   int  i_BrkEqu;
   int  i_Reg1;
   int  i_Reg2;
   int  i_Reg3;
   int  i_Reg4;
   int  i_No;      /* canopy cover equation #, (FVS Species Index No. )      */
}  d_SMT;


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */

void  SMT_InitTable (void);
void  SMT_DispTbl (void);
int   SMT_Get (int iX,  d_SMT  *a_SMT);
int   SMT_ChkRegion     (char cr_Spe[],  int i_Reg);
float SMT_CrownRation   (char cr_Spe[]);
float SMT_CalcBarkThick (char cr_Spe[], float f_DBH, char cr_ErrMes[]);
int   SMT_MortEqu       (char cr_Spe[]);
float SMT_CalcHeight    (char cr_Spe[], float f_DBH);
int   SMT_GetIdx        (char cr_Spe[]);
int   SMT_ChkReg (d_SMT *a_SMT, int i_Reg);
int   SMT_NotImp (int iX);

float  SMT_CalcCrnCov (char cr_Spe[], float f_Dia, float f_Hgt);

int  SMT_Load (char cr_Pth[], char cr_ErrMes[]);
