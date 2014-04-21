/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_util.h
* Desc: Various utility functions
* Author: Larry Gangi
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/

void HrMinSec (int i_Seconds, char cr_Out[]);

void   Remove_FN   (char cr[]);
void   Rem_Path    (char cr_Arg[], char  cr_New[] );
float  Basal_Area  (float  f_DBH, float  f_Cnt );
float  Calc_Scorch (float f_flame );
float  Calc_Flame  (float f_Scorch);

void  Blk_End_Line ( char cr_Line[], int i_Len);
void  ToInt        ( int *ai, char *cr_RB, int iA_Col, int iB_Col );
void  ToStr        ( char *cr_Out, char *cr_In, int iA_Col, int iB_Col );
void  ToFlo        ( float *af, char *cr_RB, int iA_Col, int iB_Col );
int   isBlankCR    (char  cr[] );
int   isBlank      (char  cr[] );
int   isBlank_Tab  (char  cr[] );
void  Rem_LT_Blanks(char cr[]);
char  Get_NumTyp   (char cr_Data[]);
void  EndNull      (char cr[]);
void  Trim_LT      (char cr[]);
void  Left_Just    (char  cr[] );
void  StrRepChr    (char cr[], char c_This,  char c_That );
void  Remove_Path  (char cr_Arg[], char  cr_New[] );
void  App_Ext      (char cr_FN[], char  cr_Ext[] );
int   isFile        (char cr_PathFN[]);
float InchMeter    (float f_Inch);
float TPA_To_KiSq  (float f_TPA);
float KgSq_To_TPA  (float f_KgSq);

int   isBlankCR (char  cr[]);
void  SetLen7 (char cr[]);

int   _GetLine (char cr_Bur[], char cr_Line[], int i_Row);
float  Sqr (float f);
float InchToCent (float f_Inch);
float CentToInch (float f_Cent); 

float TPA_To_MTPH (float TPA);
float MTPH_To_TPA (float Mg);
float InchToMilMeter  (float f_Inch);
float MilMeterToInch  (float f);
