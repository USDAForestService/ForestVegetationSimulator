/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: FOF_DLL.cpp
* Desc: FOFEM DLL class functions
*
*
* Date: 10-20-10
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
#include <windows.h>
#include <stdio.h>
#include <string.h>
#include <direct.h>
#include <conio.h> 
#include <process.h>


#include "fof_dll.h"
#include "fof_ci.h"

#include "fof_sgv.h"

#include "fof_co.h"
#include "fof_co2.h"
#include "fof_cm.h"

#include "fof_iss.h" 
#include "fof_mrt.h"

#include "fof_lcc.h"

/*******************************************************************************/
FOFEM::FOFEM() 
{  
  lcc = new d_LCC();
}

FOFEM::~FOFEM() 
{  
  delete lcc;
}


void FOFEM::Init ()
{
  lcc->CI_init(); 
  lcc->CO_init(); 
}




void  FOFEM::CI_f_Duff    (float f)   { lcc->CI_f_Duff(f); }
void  FOFEM::CI_f_DufDep  ( float f)  { lcc->CI_f_DufDep(f);}
void  FOFEM::CI_f_MoistDuf ( float f) { lcc->CI_f_MoistDuf(f);}

void   FOFEM::CI_f_Herb          ( float f)  { lcc->CI_f_Herb        (f);}
void   FOFEM::CI_f_Shrub         ( float f)  { lcc->CI_f_Shrub       (f);}
void   FOFEM::CI_f_CroFol        ( float f)  { lcc->CI_f_CroFol      (f);}
void   FOFEM::CI_f_CroBra        ( float f)  { lcc->CI_f_CroBra      (f);}
void   FOFEM::CI_f_Pc_CroBrn     ( float f)  { lcc->CI_f_Pc_CroBrn   (f);}
void   FOFEM::CI_f_MoistDW10     ( float f)  { lcc->CI_f_MoistDW10   (f);}
void   FOFEM::CI_f_MoistDW1000   ( float f)  { lcc->CI_f_MoistDW1000 (f);}
void   FOFEM::CI_f_Lit           ( float f)  { lcc->CI_f_Lit         (f);}
void   FOFEM::CI_f_DW1           ( float f)  { lcc->CI_f_DW1         (f);}
void   FOFEM::CI_f_DW10          ( float f)  { lcc->CI_f_DW10        (f);}
void   FOFEM::CI_f_DW100         ( float f)  { lcc->CI_f_DW100       (f);}
void   FOFEM::CI_f_Snd_DW3       ( float f)  { lcc->CI_f_Snd_DW3     (f);}
void   FOFEM::CI_f_Snd_DW6       ( float f)  { lcc->CI_f_Snd_DW6     (f);}
void   FOFEM::CI_f_Snd_DW9       ( float f)  { lcc->CI_f_Snd_DW9     (f);}
void   FOFEM::CI_f_Snd_DW20      ( float f)  { lcc->CI_f_Snd_DW20    (f);}
void   FOFEM::CI_f_Rot_DW3       ( float f)  { lcc->CI_f_Rot_DW3     (f);}
void   FOFEM::CI_f_Rot_DW6       ( float f)  { lcc->CI_f_Rot_DW6     (f);}
void   FOFEM::CI_f_Rot_DW9       ( float f)  { lcc->CI_f_Rot_DW9     (f);}
void   FOFEM::CI_f_Rot_DW20      ( float f)  { lcc->CI_f_Rot_DW20    (f);}

void   FOFEM::CI_cr_Region       (char cr[]) { lcc->CI_cr_Region(cr); }


void   FOFEM::CI_cr_Season       (char cr[]) { lcc->CI_cr_Season       (cr); }
void   FOFEM::CI_cr_FuelCategory (char cr[]) { lcc->CI_cr_FuelCategory (cr); }

void   FOFEM::CI_cr_DufMoiMet    (char cr[]) { lcc->CI_cr_DufMoiMet    (cr); }
void   FOFEM::CI_cr_CoverGroup   (char cr[]) { lcc->CI_cr_CoverGroup   (cr); }
void   FOFEM::CI_cr_LoadFN       (char cr[]) { lcc->CI_cr_LoadFN       (cr); }
void   FOFEM::CI_cr_EmiFN        (char cr[]) { lcc->CI_cr_EmiFN        (cr); }


int  FOFEM::CM_mngr (char cr_ErrMes[])
{
int i;
   i = lcc->CM_mngr(cr_ErrMes);
   return i;
}


 void  FOFEM::CO_cr_BrnIg (char cr[]) { lcc->CO_cr_BrnIg(cr); }

/***********************************************************************************/
int  FOFEM::CO_GetEm_cFOFEM (int iX, float *PM25, float *PM10, float *CH4, float *CO2,
                                     float *CO,   float *NOX,  float *SO2, float *Intensity)
{
int i;
   i = lcc->CO_GetEm(iX, PM25, PM10, CH4, CO2, CO, NOX, SO2, Intensity);
   return i; 
} 

/***********************************************************************************/
void  FOFEM::CO_GetEmTotal (char cr[],  float *PM25, float *PM10, float *CH4, float *CO2,
                                        float *CO,   float *NOX,  float *SO2)
{
  lcc->CO_GetEmTot( cr, PM25, PM10, CH4, CO2, CO, NOX, SO2); 
} 







float FOFEM::CO_BullShit()
{
  return 1; 
}

/***************************************************************************/
 float FOFEM::CO_f_DufPos      () { return lcc->CO_f_DufPos      ();}
 float FOFEM::CO_f_LitPos      () { return lcc->CO_f_LitPos      ();}
 float FOFEM::CO_f_HerPos      () { return lcc->CO_f_HerPos      ();}
 float FOFEM::CO_f_ShrPos      () { return lcc->CO_f_ShrPos      ();}
 float FOFEM::CO_f_DW1Pos      () { return lcc->CO_f_DW1Pos      ();}
 float FOFEM::CO_f_DW10Pos     () { return lcc->CO_f_DW10Pos     ();}
 float FOFEM::CO_f_DW100Pos    () { return lcc->CO_f_DW100Pos    ();}



 float FOFEM::CO_f_S3Pos       () { return lcc->CO_f_S3Pos       ();}
 float FOFEM::CO_f_S6Pos       () { return lcc->CO_f_S6Pos       ();}
 float FOFEM::CO_f_R3Pos       () { return lcc->CO_f_R3Pos       ();}
 float FOFEM::CO_f_R6Pos       () { return lcc->CO_f_R6Pos       ();}
 float FOFEM::CO_f_S9Pos       () { return lcc->CO_f_S9Pos       ();}
 float FOFEM::CO_f_S20Pos      () { return lcc->CO_f_S20Pos      ();}
 float FOFEM::CO_f_R9Pos       () { return lcc->CO_f_R9Pos       ();}
 float FOFEM::CO_f_R20Pos      () { return lcc->CO_f_R20Pos      ();}

 float FOFEM::CO_f_S3Con       () { return lcc->CO_f_S3Con       ();}
 float FOFEM::CO_f_S6Con       () { return lcc->CO_f_S6Con       ();}
 float FOFEM::CO_f_R3Con       () { return lcc->CO_f_R3Con       ();}
 float FOFEM::CO_f_R6Con       () { return lcc->CO_f_R6Con       ();}
 float FOFEM::CO_f_S9Con       () { return lcc->CO_f_S9Con       ();}
 float FOFEM::CO_f_S20Con      () { return lcc->CO_f_S20Con      ();}
 float FOFEM::CO_f_R9Con       () { return lcc->CO_f_R9Con       ();}
 float FOFEM::CO_f_R20Con      () { return lcc->CO_f_R20Con      ();}






 float FOFEM::CO_f_Rot_DW1kCon () { return lcc->CO_f_Rot_DW1kCon ();}

 float FOFEM::CO_f_HerCon      () { return lcc->CO_f_HerCon (); } 
 float FOFEM::CO_f_LitCon      () { return lcc->CO_f_LitCon (); } 
 float FOFEM::CO_f_ShrCon      () { return lcc->CO_f_ShrCon (); } 


 float FOFEM::CO_f_DufCon      () { return lcc->CO_f_DufCon      ();}
 float FOFEM::CO_f_DW1Con      () { return lcc->CO_f_DW1Con      ();}
 float FOFEM::CO_f_DW10Con     () { return lcc->CO_f_DW10Con     ();}
 float FOFEM::CO_f_DW100Con    () { return lcc->CO_f_DW100Con    ();}
 float FOFEM::CO_f_Snd_DW1kCon () { return lcc->CO_f_Snd_DW1kCon ();}
 float FOFEM::CO_f_TotPer      () { return lcc->CO_f_TotPer      ();}

void FOFEM::MRT_LoadSpec (char cr_ErrMes[])
{
  lcc->MRT_LoadSpec (cr_ErrMes);
}

float FOFEM::MRT_Calculate (char cr_ErrMes[])
{
float f;
  f = lcc->MRT_Calculate (cr_ErrMes); 
  return f; 
}


void FOFEM::MO_Initial ()
{
  lcc->MO_Initial();
}

void FOFEM::MI_cr_Spec (char cr[]) { lcc->MI_cr_Spe (cr); } 
void FOFEM::MI_f_CroRat (float f) { lcc->MI_f_CroRat (f); } 
void FOFEM::MI_f_Den (float f)  { lcc->MI_f_Den (f); }
void FOFEM::MI_f_DBH (float f)  { lcc->MI_f_DBH (f); }
void FOFEM::MI_f_Hgt (float f)  { lcc->MI_f_Hgt (f); }
void FOFEM::MI_f_FS  (float f)  { lcc->MI_f_FS  (f); }

void FOFEM::MI_cr_FS     (char cr[]) { lcc->MI_cr_FS(cr) ; }
void FOFEM::MI_cr_FivSev (char cr[]) { lcc->MI_cr_FivSev(cr) ; }

float FOFEM::MO_f_Mort ()      { return lcc->MO_f_Mort(); }
float FOFEM::MO_f_TotKilled () { return lcc->MO_f_TotKilled(); }
float FOFEM::MO_f_AvgMort ()   { return lcc->MO_f_AvgMort(); }

