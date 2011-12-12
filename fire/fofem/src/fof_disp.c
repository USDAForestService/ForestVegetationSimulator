/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_disp.c
* Desc: Functions to display various input/output structures
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
#ifndef ANSI
#include <windows.h>
#else
#define WINAPI
#endif


#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>
#include <float.h>

#include  "fof_ci.h"
#include  "fof_co.h"
#include  "fof_sh.h"
#include  "fof_disp.h"

#ifndef ANSI
#define printf xprintf
void xprintf (char *Format, ...);
#endif

void _Disp (char cr[], int i_Equ, float f_Pre, float f_Con, float f_Pos, float f_Per, int i_Head);


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Disp_ConTot
* Desc: Display the Consumed Totals
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void Disp_ConTot (d_CO  *a_CO)
{
 printf ("--------------------------------------------\n");
 printf ("   Consumed Totals                          \n");
 printf (" Pre       %7.3f \n",  a_CO->f_TotPre);
 printf (" Consumed  %7.3f \n",  a_CO->f_TotCon);
 printf (" Post      %7.3f \n",  a_CO->f_TotPos);
 printf (" Percent   %7.3f \n",  a_CO->f_TotPer);

}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Disp_DHSF
* Desc: Display Duff,Herb,Shrub,Foliage  calculated values.
*       Equation, Pre-Load, Consumed Load, Post Load, and Percent Consumed
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void Disp_DHSF (d_CO  *a_CO)
{

  _Disp ("Duff", a_CO->i_DufEqu, a_CO->f_DufPre, a_CO->f_DufCon, a_CO->f_DufPos, a_CO->f_DufPer,1);

  _Disp ("Duff Depth\n", a_CO->i_DufDepEqu, a_CO->f_DufDepPre, a_CO->f_DufDepCon, a_CO->f_DufDepPos,a_CO->f_DufDepPer,0);

  _Disp ("Herbaceous",a_CO->i_HerEqu,a_CO->f_HerPre,a_CO->f_HerCon,a_CO->f_HerPos,a_CO->f_HerPer,0);

  _Disp ("Shrub",a_CO->i_ShrEqu,a_CO->f_ShrPre,a_CO->f_ShrCon,a_CO->f_ShrPos,a_CO->f_ShrPer,0);

  _Disp ("Foliage",a_CO->i_FolEqu,a_CO->f_FolPre,a_CO->f_FolCon,a_CO->f_FolPos,a_CO->f_FolPer,0);

  _Disp ("Branch", a_CO->i_BraEqu,a_CO->f_BraPre,a_CO->f_BraCon,a_CO->f_BraPos,a_CO->f_BraPer,0);

   printf ("\nMineral Soil Exposed: \n");
   printf ("Percent: %6.2f\n",  a_CO->f_MSEPer );               /* Mineral Soil Exposer percent       */
   printf ("Equation: %d\n",  a_CO->i_MSEEqu );               /* Equation                           */

}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Disp_BrnUp
* Desc: Display Burnup's calculated values.
*       Equation, Pre-Load, Consumed Load, Post Load, and Percent Consumed
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void Disp_BrnUp (d_CO  *a_CO)
{
  printf ("\n--------------------------------------------------------\n");
  printf (" \n");

 _Disp ("Litter",a_CO->i_LitEqu,a_CO->f_LitPre,a_CO->f_LitCon,a_CO->f_LitPos,a_CO->f_LitPer,0);

 _Disp ("Down Wood 1 Hour (less than 1/4 inch)",a_CO->i_DW1Equ,a_CO->f_DW1Pre,a_CO->f_DW1Con,a_CO->f_DW1Pos,a_CO->f_DW1Per,0);

 _Disp ("Down Wood 10 Hour ( 1/4 to 1 inch)",a_CO->i_DW10Equ,a_CO->f_DW10Pre,a_CO->f_DW10Con,a_CO->f_DW10Pos,a_CO->f_DW10Per,0);

 _Disp ("Down Wood 100 Hour ( 1 to 3 inch)",a_CO->i_DW100Equ,a_CO->f_DW100Pre,a_CO->f_DW100Con,a_CO->f_DW100Pos,a_CO->f_DW100Per,0);

  printf ("\n..............................................\n");
  printf ("Sound - Down Wood 1000 Hour \n");
 _Disp ("  3 to 6 inch", a_CO->i_Snd_DW1kEqu,a_CO->f_S3Pre, a_CO->f_S3Con,  a_CO->f_S3Pos,  a_CO->f_S3Per, 0);
 _Disp ("  6 to 9 inch)", a_CO->i_Snd_DW1kEqu,a_CO->f_S6Pre, a_CO->f_S6Con,  a_CO->f_S6Pos,  a_CO->f_S6Per, 0);
 _Disp ("  9 to 20 inch)",a_CO->i_Snd_DW1kEqu,a_CO->f_S9Pre, a_CO->f_S9Con,  a_CO->f_S9Pos,  a_CO->f_S9Per, 0);
 _Disp ("  over 20 inch)",a_CO->i_Snd_DW1kEqu,a_CO->f_S20Pre,a_CO->f_S20Con, a_CO->f_S20Pos, a_CO->f_S20Per, 0);
 _Disp (" Sound Total", a_CO->i_Snd_DW1kEqu,a_CO->f_Snd_DW1kPre,a_CO->f_Snd_DW1kCon,a_CO->f_Snd_DW1kPos,a_CO->f_Snd_DW1kPer,0);

  printf ("\n..............................................\n");
  printf ("Rotten - Down Wood 1000 Hour  \n");
 _Disp ("  3 to 6 inch",  a_CO->i_Rot_DW1kEqu,a_CO->f_R3Pre, a_CO->f_R3Con,  a_CO->f_R3Pos,  a_CO->f_R3Per, 0);
 _Disp ("  6 to 9 inch)", a_CO->i_Rot_DW1kEqu,a_CO->f_R6Pre, a_CO->f_R6Con,  a_CO->f_R6Pos,  a_CO->f_R6Per, 0);
 _Disp ("  9 to 20 inch)",a_CO->i_Rot_DW1kEqu,a_CO->f_R9Pre, a_CO->f_R9Con,  a_CO->f_R9Pos,  a_CO->f_R9Per, 0);
 _Disp ("  over 20 inch)",a_CO->i_Rot_DW1kEqu,a_CO->f_R20Pre,a_CO->f_R20Con, a_CO->f_R20Pos, a_CO->f_R20Per, 0);
 _Disp (" Rotten Total", a_CO->i_Rot_DW1kEqu,a_CO->f_Rot_DW1kPre,a_CO->f_Rot_DW1kCon,a_CO->f_Rot_DW1kPos,a_CO->f_Rot_DW1kPer,0);


   printf ("-------------------------------------------------------\n");
   printf ("  Flaming Emissions \n");
   printf ("PM10  %f\n",  a_CO->f_PM10F);
   printf ("PM25  %f\n",  a_CO->f_PM25F);
   printf ("CH4   %f\n",  a_CO->f_CH4F);
   printf ("CO    %f\n",  a_CO->f_COF);
   printf ("CO2   %f\n",  a_CO->f_CO2F);
   printf ("NOX   %f\n",  a_CO->f_NOXF);
   printf ("SOX   %f\n",  a_CO->f_SOXF);

   printf ("  Smoldering Emissions \n");
   printf ("PM10  %f\n",  a_CO->f_PM10S);
   printf ("PM25  %f\n",  a_CO->f_PM25S);
   printf ("CH4   %f\n",  a_CO->f_CH4S);
   printf ("CO    %f\n",  a_CO->f_COS);
   printf ("CO2   %f\n",  a_CO->f_CO2S);
   printf ("NOX   %f\n",  a_CO->f_NOXS);
   printf ("SOX   %f\n",  a_CO->f_SOXS);
   printf ("\n");
   printf ("Flame Duration: %f\n",  a_CO->f_FlaDur);
   printf ("Smold Duration: %f\n",  a_CO->f_SmoDur);
   printf ("\n");
   printf ("Flame Consumed: %f\n", a_CO->f_FlaCon);
   printf ("Smold Consumed: %f\n", a_CO->f_SmoCon);
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: _Disp
* Desc: Common Function to display load info, It's all done here so
*        that they all get formated the same
*   In: see below
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void _Disp (char cr[], int i_Equ, float f_Pre, float f_Con, float f_Pos, float f_Per, int i_Head)
{

 if ( i_Head == 1 ) {
    printf ("--------------------------------------------\n");
    printf ("Equ      = FOFEM Equation used  \n");
    printf ("Pre      = Pre Fire Load \n");
    printf ("Consumed = Consumed Load\n");
    printf ("Post     = Post Fire Load \n");
    printf ("Percent  = Percent Consumed - ( 0 -> 100 ) \n");
    printf ("Units = Tons per acre \n");
 }

   printf ("\n%s\n",cr);
   printf (" Equ         %d \n",  i_Equ );                      /* Herbaceous                        */
   printf (" Pre       %7.3f\n",  f_Pre );
   printf (" Consumed  %7.3f\n",  f_Con );
   printf (" Post      %7.3f\n",  f_Pos );
   printf (" Percent   %7.3f\n",  f_Per );
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Disp_Soil
* Desc: Display the Soil Simulation outputs
*   In: a_SO...Soil Output structure
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int Disp_Soil (d_SO *a_SO)
{
int i;

   printf ("\n----------------------------------------------------------------\n");
   printf (" Duff Depth.....: Pre-Fire: %6.2f cm.,  Post-Fire: %6.2f cm.\n\n",
            a_SO->f_cDufPre,a_SO->f_cDufPost);

   printf ("                               Soil Layer Maximum Temperature\n");
   printf ("                         ( measurements are in centimeters and Celsius )\n\n");
   printf ("Depth     0    1    2    3    4    5    6    7    8    9   10   11   12   13\n");


   printf ("Temp.  ");
   for (i = 0; i < e_Layers; i++ )
     printf (" %3d ", a_SO->ir_Temp[i]);
   printf ("\n");
   printf ("Time   ");
   for (i = 0; i < e_Layers; i++ )
     printf (" %3d ", a_SO->ir_Time[i]);
   printf ("\n");

   if ( a_SO->i_LayMaxDeg1 == e_LayNone )
     printf ("Max Depth Having %d degrees: - None - \n", e_Max1);
   else
     printf ("Max Depth Having %d degrees: %d\n", e_Max1, a_SO->i_LayMaxDeg1);

   if ( a_SO->i_LayMaxDeg2 == e_LayNone )
     printf ("Max Depth Having %d degrees: - None - \n", e_Max2);
   else
     printf ("Max Depth Having %d degrees: %d\n", e_Max2, a_SO->i_LayMaxDeg2);

   printf ("Model: %s \n", a_SO->cr_Model);

   return 1;
}




/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Disp_ConIn
* Desc: Display the Consumed Input Structure
*   In: a_CI....consumed input structure
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void Disp_ConIn(d_CI *a_CI)
{
   printf (" Duff;        %f \n",  a_CI->f_Duff);              /* Duff Load - Limits in tpa .446 ->356.79     */
   printf (" DufDep;      %f \n",  a_CI->f_DufDep);             /* Duff Depth - inches limit 0 --> 999         */
   printf (" MoistDuff;   %f \n",  a_CI->f_MoistDuff);          /* Duff moisture, Limits 10 -> 197.2           */
   printf (" MoistDW10;   %f \n",  a_CI->f_MoistDW10);          /* Down Wood 10 hr Moisture, limit  3 -> 298   */
   printf (" MoistDW1000; %f \n",  a_CI->f_MoistDW1000);        /* Down Wood 1k hr Moiture, limits 1 -> 300    */

   printf (" Lit;         %f \n",  a_CI->f_Lit);                /* Litter                                      */
   printf (" DW1;         %f \n",  a_CI->f_DW1);                /* 1 Hr, 0->1/4 inch                           */
   printf (" DW10;        %f \n",  a_CI->f_DW10);               /* 10 Hr, 1/4 -> 1 inch                        */
   printf (" DW100;       %f \n",  a_CI->f_DW100);              /* 100 Hr, 1 -> 3 inch                         */

   printf (" DW1000;      %f \n",  a_CI->f_DW1000);                       /* Total 1000 hr wood, 3+ inch       */
   printf (" pcRot;       %f \n",  a_CI->f_pcRot);                        /* percent rotten                    */
   printf (" Snd_DW3;     %f \n",  a_CI->f_Snd_DW3);                      /* Large Wood Sound 1000 Hr          */
   printf (" Snd_DW6;     %f \n",  a_CI->f_Snd_DW6);                      /* 3,6,9 & 20+ size classes          */
   printf (" Snd_DW9;     %f \n",  a_CI->f_Snd_DW9);
   printf (" Snd_DW20;    %f \n",  a_CI->f_Snd_DW20);

   printf (" Rot_DW3;     %f \n",  a_CI->f_Rot_DW3);                      /*  Large Wood Rotten 1000 Hr         */
   printf (" Rot_DW6;     %f \n",  a_CI->f_Rot_DW6);
   printf (" Rot_DW9;     %f \n",  a_CI->f_Rot_DW9);
   printf (" Rot_DW20;    %f \n",  a_CI->f_Rot_DW20);

   printf (" HSFCon;      %f \n",  a_CI->f_HSFCon);                       /* Herb,Shrub,Foliage,Branch consumed */
   printf (" PerDufCon;   %f \n",  a_CI->f_PerDufCon);                    /* % of Duff consumed, See Notes above*/

   printf (" CroFol;      %f \n",  a_CI->f_CroFol);
   printf (" CroBra;      %f \n",  a_CI->f_CroBra);
   printf (" Pc_CroBrn;   %f \n",  a_CI->f_Pc_CroBrn);
   printf (" Herb;        %f \n",  a_CI->f_Herb);
   printf (" Shrub;       %f \n",  a_CI->f_Shrub);

   printf (" CoverGroup   %s \n",  a_CI->cr_CoverGroup);
   printf (" Season       %s \n",  a_CI->cr_Season   );
   printf (" Region       %s \n",  a_CI->cr_Region   );
   printf (" FuelCategory %s \n",  a_CI->cr_FuelCategory);
   printf (" DufMoiMet    %s \n",  a_CI->cr_DufMoiMet );



}
