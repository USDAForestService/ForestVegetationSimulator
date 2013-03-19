//
// $Id
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_co.c
* Desc: Consumerd Output
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
#ifdef ANSI
#define WINAPI
#else
#include <windows.h>
#endif


#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include  "fof_co.h"

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: CO_Init
* Desc: Consumerd Output init structure
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void WINAPI CO_Init (d_CO *a_CO)
{
   strcpy (a_CO->cr_BrnIg,"");

   a_CO->i_LitEqu = 0;
   a_CO->f_LitPre = 0;
   a_CO->f_LitCon = 0;
   a_CO->f_LitPos = 0;
   a_CO->f_LitPer = 0;

   a_CO->i_DW1Equ = 0;
   a_CO->f_DW1Pre = 0;
   a_CO->f_DW1Con = 0;
   a_CO->f_DW1Pos = 0;
   a_CO->f_DW1Per = 0;

   a_CO->i_DW10Equ = 0;
   a_CO->f_DW10Pre = 0;
   a_CO->f_DW10Con = 0;
   a_CO->f_DW10Pos = 0;
   a_CO->f_DW10Per = 0;

   a_CO->i_DW100Equ = 0;
   a_CO->f_DW100Pre = 0;
   a_CO->f_DW100Con = 0;
   a_CO->f_DW100Pos = 0;
   a_CO->f_DW100Per = 0;

   a_CO->i_Snd_DW1kEqu = 0;
   a_CO->f_Snd_DW1kPre = 0;
   a_CO->f_Snd_DW1kCon = 0;
   a_CO->f_Snd_DW1kPos = 0;
   a_CO->f_Snd_DW1kPer = 0;

   a_CO->i_Rot_DW1kEqu = 0;
   a_CO->f_Rot_DW1kPre = 0;
   a_CO->f_Rot_DW1kCon = 0;
   a_CO->f_Rot_DW1kPos = 0;
   a_CO->f_Rot_DW1kPer = 0;

   a_CO->i_DufEqu = 0;
   a_CO->f_DufPre = 0;
   a_CO->f_DufCon = 0;
   a_CO->f_DufPos = 0;
   a_CO->f_DufPer = 0;

   a_CO->i_DufDepEqu = 0;
   a_CO->f_DufDepPre = 0;
   a_CO->f_DufDepCon = 0;
   a_CO->f_DufDepPos = 0;
   a_CO->f_DufDepPer = 0;

   a_CO->f_MSEPer = 0;
   a_CO->i_MSEEqu = 0;

   a_CO->i_HerEqu = 0;
   a_CO->f_HerPre = 0;
   a_CO->f_HerCon = 0;
   a_CO->f_HerPos = 0;
   a_CO->f_HerPer = 0;

   a_CO->i_ShrEqu = 0;
   a_CO->f_ShrPre = 0;
   a_CO->f_ShrCon = 0;
   a_CO->f_ShrPos = 0;
   a_CO->f_ShrPer = 0;

   a_CO->i_FolEqu = 0;
   a_CO->f_FolPre = 0;
   a_CO->f_FolCon = 0;
   a_CO->f_FolPos = 0;
   a_CO->f_FolPer = 0;

   a_CO->i_BraEqu = 0;
   a_CO->f_BraPre = 0;
   a_CO->f_BraCon = 0;
   a_CO->f_BraPos = 0;
   a_CO->f_BraPer = 0;

   a_CO->f_TotPre = 0;
   a_CO->f_TotCon = 0;
   a_CO->f_TotPos = 0;
   a_CO->f_TotPer = 0;

   a_CO->f_S3Pos  = 0;
   a_CO->f_S3Con  = 0;
   a_CO->f_S3Pre  = 0;
   a_CO->f_S3Per  = 0;

   a_CO->f_S6Pos  = 0;
   a_CO->f_S6Con  = 0;
   a_CO->f_S6Pre  = 0;
   a_CO->f_S6Per  = 0;

   a_CO->f_S9Pos  = 0;
   a_CO->f_S9Con  = 0;
   a_CO->f_S9Pre  = 0;
   a_CO->f_S9Per  = 0;

   a_CO->f_S20Pos = 0;
   a_CO->f_S20Con = 0;
   a_CO->f_S20Pre = 0;
   a_CO->f_S20Per = 0;

   a_CO->f_R3Pos  = 0;
   a_CO->f_R3Con  = 0;
   a_CO->f_R3Pre  = 0;
   a_CO->f_R3Per  = 0;

   a_CO->f_R6Pos  = 0;
   a_CO->f_R6Con  = 0;
   a_CO->f_R6Pre  = 0;
   a_CO->f_R6Per  = 0;

   a_CO->f_R9Pos  = 0;
   a_CO->f_R9Con  = 0;
   a_CO->f_R9Pre  = 0;
   a_CO->f_R9Per  = 0;

   a_CO->f_R20Pos = 0;
   a_CO->f_R20Con = 0;
   a_CO->f_R20Pre = 0;
   a_CO->f_R20Per = 0;

   a_CO->i_Time = 0;
   a_CO->i_Cnt  = 0;
   a_CO->f_fi   = 0;
   strcpy (a_CO->cr_SHC,"");
}
