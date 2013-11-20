//
// $Id$
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fm_fofem.c
* Desc: Interface between the FFE-FVS and fofem for predicting Soil Heating.
*
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
#ifdef ANSI
#define WINAPI
#else
#include <windows.h>
#endif

#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "fm_fofem.h"
#include "fof_co.h"
#include "fof_co2.h"
#include "fof_ci.h"
#include "fof_cm.h"
#include "fof_sh.h"

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
#define eC_fhT "soiltest.txt"           /* Test output file                  */
FILE *fhT;

char gcr_EM[3000];

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
void TestParm (float fr_In[], float fr_Out[], char cr[]);
void DumpFiles (d_CI *a_CI, d_SI *a_SI, d_SO *a_SO, char cr[]);
int  Put_Soil (d_SO *a_SO);
int  Put_CISI (d_CI *a_CI, d_SI *a_SI);
void Test_In (float fr_In[]);

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
*   In: fr_In: input array, each position contains a specified input.
*              see the fm_fofem.h for #defines on index positions.
*  Out: fr_Out: output array, temperatures for each layer and layer numbers
*               that reach 60 and 275 degree.
*               NOTE: this is the same soil info that is shown on the
*               FOFEM5 report.
* NOTE: there are 2 test modes that can be executed by this function.
*        1.  fr_In[0] = -1   See code below for explanations.
*        2.  fr_In[0] = -2
*       I wrote these so that if there is any trouble when Moscow trys
*       to link their FORTRAN code,  we'll have a way to do some checking
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/


#ifdef CMPgcc
  #ifdef unix
    extern int fm_fofem_ (                       // GCC compiler, Unix OS
      float *fr_In,
      float *fr_Out,
      char  *cr_ErrMes);
  #else
    extern __declspec(dllexport) int fm_fofem_ ( // GCC compiler, Windows OS
      float *fr_In,
      float *fr_Out,
      char  *cr_ErrMes);
  #endif
#else
  extern __declspec(dllexport) int FM_FOFEM (    // VS2010 compiler, Windows OS
    float *fr_In,
    float *fr_Out,
    char  *cr_ErrMes);
#endif


#ifdef CMPgcc
int fm_fofem_ (
#else
int FM_FOFEM (
#endif
  float *fr_In,
  float *fr_Out,
  char *cr_ErrMes)

{
int i;
unsigned int iN_ErrMes;
char cr[100];
d_CI s_CI;
d_CO s_CO;
d_SI s_SI;
d_SO s_SO;

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Check if caller wants to do a test run                                    */
   if ( fr_In[0] == -1 ) {              /* Simple add numbers test           */
     TestParm (fr_In, fr_Out, cr);
     strcpy (cr_ErrMes,cr);
     return strlen(cr); }

   if ( fr_In[0] == -2 )                /* load test data and run            */
     Test_In (fr_In);

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
   strcpy (cr_ErrMes,"");
   iN_ErrMes = fr_In[eX_EM];

   CI_Init (&s_CI);
   CO_Init (&s_CO);

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Get input numbers from In array                                           */
   s_CI.f_DW1   = fr_In[ eX_1Hr  ];
   s_CI.f_DW10  = fr_In[ eX_10Hr ];
   s_CI.f_DW100 = fr_In[ eX_100Hr];

   s_CI.f_Snd_DW3  = fr_In[eX_Snd_DW3 ];     /* Sound Large Woods            */
   s_CI.f_Snd_DW6  = fr_In[eX_Snd_DW6 ];
   s_CI.f_Snd_DW9  = fr_In[eX_Snd_DW9 ];
   s_CI.f_Snd_DW20 = fr_In[eX_Snd_DW20];

   s_CI.f_Rot_DW3  = fr_In[eX_Rot_DW3 ];     /* Rotten Large Wood            */
   s_CI.f_Rot_DW6  = fr_In[eX_Rot_DW6 ];
   s_CI.f_Rot_DW9  = fr_In[eX_Rot_DW9 ];
   s_CI.f_Rot_DW20 = fr_In[eX_Rot_DW20];

   s_CI.f_Duff   = fr_In[eX_Duff  ];         /* Duff load                    */
   s_CI.f_DufDep = fr_In[eX_DufDep];         /* Duff depth, inches           */

   s_CI.f_Lit = fr_In[eX_Litter];            /* Litter Load                  */
   s_CI.f_Herb  = fr_In[eX_Herb ];           /* Herb load                    */
   s_CI.f_Shrub = fr_In[eX_Shrub];

   s_CI.f_CroFol    = fr_In[eX_CroFol];      /* Crown Foliage-Branch percent */
   s_CI.f_CroBra    = fr_In[eX_CroBra];
   s_CI.f_Pc_CroBrn = fr_In[eX_Pc_CroBrn];

   s_CI.f_MoistDW10   = fr_In[eX_MoistDW10];
   s_CI.f_MoistDW1000 = fr_In[eX_MoistDW1k];
   s_CI.f_MoistDuff   = fr_In[eX_MoistDuff];

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Set defaults then check and set each                                      */
   strcpy (s_CI.cr_DufMoiMet,e_Entire);
   if ( fr_In[eX_MM] == e_xEntire  ) strcpy (s_CI.cr_DufMoiMet,e_Entire);
   if ( fr_In[eX_MM] == e_xLower   ) strcpy (s_CI.cr_DufMoiMet,e_Lower );
   if ( fr_In[eX_MM] == e_xNFDR    ) strcpy (s_CI.cr_DufMoiMet,e_NFDR  );
   if ( fr_In[eX_MM] == e_xAdj_NFDR) strcpy (s_CI.cr_DufMoiMet,e_Adj_NFDR);

   strcpy (s_CI.cr_Season,e_Spring);
   if ( fr_In[eX_Season] == e_xSpring ) strcpy (s_CI.cr_Season,e_Spring);
   if ( fr_In[eX_Season] == e_xSummer ) strcpy (s_CI.cr_Season,e_Summer);
   if ( fr_In[eX_Season] == e_xFall   ) strcpy (s_CI.cr_Season,e_Fall  );
   if ( fr_In[eX_Season] == e_xWinter ) strcpy (s_CI.cr_Season,e_Winter);

   strcpy (s_CI.cr_Region, e_CI_InteriorWest);
   if ( fr_In[eX_Region] == e_xIntWest ) strcpy (s_CI.cr_Region, e_CI_InteriorWest);
   if ( fr_In[eX_Region] == e_xPacific ) strcpy (s_CI.cr_Region, e_CI_PacificWest);
   if ( fr_In[eX_Region] == e_xSE      ) strcpy (s_CI.cr_Region, e_CI_SouthEast);
   if ( fr_In[eX_Region] == e_xNE      ) strcpy (s_CI.cr_Region, e_CI_NorthEast);

   strcpy (s_CI.cr_FuelCategory, e_Natural);
   if (  fr_In[eX_FuelCat] == e_xNatural) strcpy (s_CI.cr_FuelCategory, e_Natural);
   if (  fr_In[eX_FuelCat] == e_xPiles  ) strcpy (s_CI.cr_FuelCategory, e_Piles);
   if (  fr_In[eX_FuelCat] == e_xSlash  ) strcpy (s_CI.cr_FuelCategory, e_Slash);

   strcpy (s_CI.cr_CoverGroup,"");  /* Default is - None                      */
   if ( fr_In[eX_MM_CovGrp] == e_xGrassGroup)  strcpy (s_CI.cr_CoverGroup, e_GrassGroup);
   if ( fr_In[eX_MM_CovGrp] == e_xSageBrush )  strcpy (s_CI.cr_CoverGroup, e_SageBrush);
   if ( fr_In[eX_MM_CovGrp] == e_xShrubGroup)  strcpy (s_CI.cr_CoverGroup, e_ShrubGroup);
   if ( fr_In[eX_MM_CovGrp] == e_xPocosin   )  strcpy (s_CI.cr_CoverGroup, e_Pocosin  );
   if ( fr_In[eX_MM_CovGrp] == e_xPonderosa )  strcpy (s_CI.cr_CoverGroup, e_Ponderosa);
   if ( fr_In[eX_MM_CovGrp] == e_xWhiPinHem )  strcpy (s_CI.cr_CoverGroup, e_WhiPinHem);
   if ( fr_In[eX_MM_CovGrp] == e_xRedJacPin )  strcpy (s_CI.cr_CoverGroup, e_RedJacPin);
   if ( fr_In[eX_MM_CovGrp] == e_xBalBRWSpr )  strcpy (s_CI.cr_CoverGroup, e_BalBRWSpr);


   strcpy (s_CI.cr_LoadFN,"");          /* Don't create a  Fuel Load file    */
   strcpy (s_CI.cr_EmiFN,"");           /* Don't Create emission time file   */
   strcpy (gcr_EM,"");                  /* blank error string                */

   i = CM_Mngr (&s_CI, &s_CO, gcr_EM);  /* Calc Consumed - does Burnup       */
   if ( i == 0 ) {                      /* Error                             */
     if ( iN_ErrMes < strlen(gcr_EM) )  /* Chk len of callrs err mess string */
       gcr_EM[iN_ErrMes] = 0;           /* need to shorten error mess        */
     strcpy (cr_ErrMes,gcr_EM);         /* copy to callrs string             */
     return strlen(cr_ErrMes); }


/* Soil Heating..............................................................*/
   s_SI.f_DufDepPre = s_CO.f_DufDepPre;
   s_SI.f_DufDepPos = s_CO.f_DufDepPos;

   s_SI.f_SoilMoist =  fr_In[eX_MoistSoil];

   strcpy (s_SI.cr_BrnIg,s_CO.cr_BrnIg);    /* Tells if Burnup Ignited       */

   strcpy (s_SI.cr_MoistCond,e_Moderate);
   if ( fr_In[eX_MoistCond] == e_xVeryDry ) strcpy (s_SI.cr_MoistCond,e_VeryDry);
   if ( fr_In[eX_MoistCond] == e_xDry     ) strcpy (s_SI.cr_MoistCond,e_Dry    );
   if ( fr_In[eX_MoistCond] == e_xModerate) strcpy (s_SI.cr_MoistCond,e_Moderate);
   if ( fr_In[eX_MoistCond] == e_xWet     ) strcpy (s_SI.cr_MoistCond,e_Wet);


   strcpy (s_SI.cr_SoilType,e_FinSil);
   if ( fr_In[eX_SoilType] == e_xLoaSke ) strcpy (s_SI.cr_SoilType,e_LoaSke);
   if ( fr_In[eX_SoilType] == e_xFinSil ) strcpy (s_SI.cr_SoilType,e_FinSil);
   if ( fr_In[eX_SoilType] == e_xFin    ) strcpy (s_SI.cr_SoilType,e_Fin   );
   if ( fr_In[eX_SoilType] == e_xCoaSil ) strcpy (s_SI.cr_SoilType,e_CoaSil);
   if ( fr_In[eX_SoilType] == e_xCoaLoa ) strcpy (s_SI.cr_SoilType,e_CoaLoa);


   s_SI.i_Cnt  =  s_CO.i_Cnt;               /* This is need if Soil Heating  */
   s_SI.i_Time =  s_CO.i_Time;              /* Does Exp simulation - no Duff */
   s_SI.f_fi   =  s_CO.f_fi;                /*  Model, otherwise not used    */

   i = SH_Mngr (&s_SI, &s_SO, "", gcr_EM);
   if ( i == 0 ) {                      /* Error                             */
     if ( iN_ErrMes < strlen(gcr_EM) )  /* Chk len of callrs err mess string */
       gcr_EM[iN_ErrMes] = 0;           /* need to shorten error mess        */
     strcpy (cr_ErrMes,gcr_EM);         /* copy to callrs string             */
     return strlen(cr_ErrMes); }

   for (i = 0; i < e_Layers; i++ )
     fr_Out[i] = s_SO.ir_Temp[i];

   if ( s_SO.i_LayMaxDeg1 == e_LayNone ) /* Max Dep reach e_Max1, 60 degees */
     fr_Out[eX_SL60] = -1;                /* None                            */
   else
     fr_Out[i] = s_SO.i_LayMaxDeg1;

   if ( s_SO.i_LayMaxDeg2 == e_LayNone ) /* Max Dep reach e_Max2 275 degees */
     fr_Out[eX_SL275] = -1;              /* None                            */
   else
     fr_Out[eX_SL275] = s_SO.i_LayMaxDeg2;

   if ( fr_In[eX_OutFile] == 1 ) {       /* if need to create output files   */
     DumpFiles (&s_CI, &s_SI,&s_SO,cr);
     strcpy (cr_ErrMes,cr);
     return strlen (cr_ErrMes);  }

  return 0;

}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: DumpFiles
* Desc: Put input and output to text files.
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void DumpFiles (d_CI *a_CI, d_SI *a_SI, d_SO *a_SO, char cr[])
{
   fhT = fopen (eC_fhT, "w");
   if ( fhT == NULL ) {
     strcpy (cr,"Can't open test output file");
     return; }
  Put_CISI (a_CI, a_SI);
  Put_Soil (a_SO);
  sprintf (cr, "Output File: %s", eC_fhT);
  fclose (fhT);
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int  Put_CISI (d_CI *a_CI, d_SI *a_SI)
{

   fprintf (fhT," Duff;        %5.2f \n",  a_CI->f_Duff);              /* Duff Load - Limits in tpa .446 ->356.79     */
   fprintf (fhT," DufDep;      %5.2f \n",  a_CI->f_DufDep);             /* Duff Depth - inches limit 0 --> 999         */
   fprintf (fhT," MoistDuff;   %5.2f \n",  a_CI->f_MoistDuff);          /* Duff moisture, Limits 10 -> 197.2           */
   fprintf (fhT," MoistDW10;   %5.2f \n",  a_CI->f_MoistDW10);          /* Down Wood 10 hr Moisture, limit  3 -> 298   */
   fprintf (fhT," MoistDW1000; %5.2f \n",  a_CI->f_MoistDW1000);        /* Down Wood 1k hr Moiture, limits 1 -> 300    */

   fprintf (fhT," Lit;         %5.2f \n",  a_CI->f_Lit);                /* Litter                                      */
   fprintf (fhT," DW1;         %5.2f \n",  a_CI->f_DW1);                /* 1 Hr, 0->1/4 inch                           */
   fprintf (fhT," DW10;        %5.2f \n",  a_CI->f_DW10);               /* 10 Hr, 1/4 -> 1 inch                        */
   fprintf (fhT," DW100;       %5.2f \n",  a_CI->f_DW100);              /* 100 Hr, 1 -> 3 inch                         */

   fprintf (fhT," DW1000;      %5.2f \n",  a_CI->f_DW1000);                       /* Total 1000 hr wood, 3+ inch       */
   fprintf (fhT," pcRot;       %5.2f \n",  a_CI->f_pcRot);                        /* percent rotten                    */
   fprintf (fhT," Snd_DW3;     %5.2f \n",  a_CI->f_Snd_DW3);                      /* Large Wood Sound 1000 Hr          */
   fprintf (fhT," Snd_DW6;     %5.2f \n",  a_CI->f_Snd_DW6);                      /* 3,6,9 & 20+ size classes          */
   fprintf (fhT," Snd_DW9;     %5.2f \n",  a_CI->f_Snd_DW9);
   fprintf (fhT," Snd_DW20;    %5.2f \n",  a_CI->f_Snd_DW20);

   fprintf (fhT," Rot_DW3;     %5.2f \n",  a_CI->f_Rot_DW3);                      /*  Large Wood Rotten 1000 Hr         */
   fprintf (fhT," Rot_DW6;     %5.2f \n",  a_CI->f_Rot_DW6);
   fprintf (fhT," Rot_DW9;     %5.2f \n",  a_CI->f_Rot_DW9);
   fprintf (fhT," Rot_DW20;    %5.2f \n",  a_CI->f_Rot_DW20);

   fprintf (fhT," HSFCon;      %5.2f \n",  a_CI->f_HSFCon);                       /* Herb,Shrub,Foliage,Branch consumed */
   fprintf (fhT," PerDufCon;   %5.2f \n",  a_CI->f_PerDufCon);                    /* % of Duff consumed, See Notes above*/

   fprintf (fhT," CroFol;      %5.2f \n",  a_CI->f_CroFol);
   fprintf (fhT," CroBra;      %5.2f \n",  a_CI->f_CroBra);
   fprintf (fhT," Pc_CroBrn;   %5.2f \n",  a_CI->f_Pc_CroBrn);
   fprintf (fhT," Herb;        %5.2f \n",  a_CI->f_Herb);
   fprintf (fhT," Shrub;       %5.2f \n",  a_CI->f_Shrub);

   fprintf (fhT," CoverGroup   %s \n",  a_CI->cr_CoverGroup);
   fprintf (fhT," Season       %s \n",  a_CI->cr_Season   );
   fprintf (fhT," Region       %s \n",  a_CI->cr_Region   );
   fprintf (fhT," FuelCategory %s \n",  a_CI->cr_FuelCategory);
   fprintf (fhT," DufMoiMet    %s \n",  a_CI->cr_DufMoiMet );

   fprintf (fhT," Soil Mois  %5.2f \n", a_SI->f_SoilMoist);
   fprintf (fhT," Soil Type  %s \n", a_SI->cr_SoilType );
   fprintf (fhT," Soil Cond  %s \n", a_SI->cr_MoistCond);

   return 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int Put_Soil (d_SO *a_SO)
{
int i;

   fprintf (fhT,"\n\n----------------------------------------------------------------\n");
   fprintf (fhT," Duff Depth.....: Pre-Fire: %6.2f cm.,  Post-Fire: %6.2f cm.\n\n",
            a_SO->f_cDufPre,a_SO->f_cDufPost);

   fprintf (fhT,"                               Soil Layer Maximum Temperature\n");

   fprintf (fhT,"                         ( measurements are in centimeters and Celsius )\n\n");
   fprintf (fhT,"Depth     0    1    2    3    4    5    6    7    8    9   10   11   12   13\n");

   fprintf (fhT,"Temp.  ");
   for (i = 0; i < e_Layers; i++ )
     fprintf (fhT," %3d ", a_SO->ir_Temp[i]);
   fprintf (fhT,"\n");

   fprintf (fhT,"Time   ");
   for (i = 0; i < e_Layers; i++ )
     fprintf (fhT," %3d ", a_SO->ir_Time[i]);
   fprintf (fhT,"\n");

   if ( a_SO->i_LayMaxDeg1 == e_LayNone )
     fprintf (fhT,"Max Depth Having %d degrees: - None - \n", e_Max1);
   else
     fprintf (fhT,"Max Depth Having %d degrees: %d\n", e_Max1, a_SO->i_LayMaxDeg1);

   if ( a_SO->i_LayMaxDeg2 == e_LayNone )
     fprintf (fhT,"Max Depth Having %d degrees: - None - \n", e_Max2);
   else
     fprintf (fhT,"Max Depth Having %d degrees: %d\n", e_Max2, a_SO->i_LayMaxDeg2);

  return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: TestParm
* Desc: This is a simple test to test the parameters.
*       Adds 3 numbers, etc see code
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void TestParm (float fr_In[], float fr_Out[], char cr[])
{
float f;
   f = fr_In[1] + fr_In[2] + fr_In[3];
   fr_Out[0] = f;
   fr_Out[1] = f * 2.0;
   strcpy (cr,"Testing");
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Test_MFSL - Moscow Fire Science Lab - Soil Heating
* Desc: Test the MFSL_SoilHeat() function in the FOFEM DLL,
*   In:
*  Out:
*  Ret:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void Test_In (float fr_In[])
{
int i;
#define eeC_EML 300
char cr_ErrMes[eeC_EML];
char cr[100];

    fr_In[eX_OutFile] = 1;           /* want to dump to output file          */

    fr_In [eX_EM] = eeC_EML;          /* max len err mess string           */

    fr_In [eX_1Hr  ] = 0.07;
    fr_In [eX_10Hr ] = 0.63;
    fr_In [eX_100Hr] = 0.80;

    fr_In[eX_Snd_DW3 ] = 1.0;           /* Sound Large Woods                 */
    fr_In[eX_Snd_DW6 ] = 1.0;
    fr_In[eX_Snd_DW9 ] = 1.0;
    fr_In[eX_Snd_DW20] = 1.0;

    fr_In[eX_Rot_DW3 ] = 1.0;           /* Rotten Large Wood                 */
    fr_In[eX_Rot_DW6 ] = 1.0;
    fr_In[eX_Rot_DW9 ] = 1.0;
    fr_In[eX_Rot_DW20] = 1.0;

    fr_In[eX_Duff  ] = 8.3;               /* Duff load                        */
    fr_In[eX_DufDep] = 1.0;               /* Duff depth, inches                */

    fr_In[eX_Litter] = 1.4;             /* Litter Load                       */

    fr_In[eX_Herb  ] = 0.5;               /* Herb load                         */
    fr_In[eX_Shrub ] = 0.1;

    fr_In[eX_CroFol] = 6.0;
    fr_In[eX_CroBra] = 0.7;
    fr_In[eX_Pc_CroBrn] = 0.0;

    fr_In[eX_MoistDW10] =  6;
    fr_In[eX_MoistDW1k] = 10;
    fr_In[eX_MoistDuff] = 20;

    fr_In[eX_MM] = 1;      /* 1-Ent, 2-Low, 3-NFDR, 4-AdjNFDR */

    fr_In[eX_Season] = 1;  /* 1-Spr, 2-Sum, 3-Fall, 4-Win */

    fr_In[eX_Region] = 1;  /* 1-IntWest, 2-Pac, 3-SE, 4-NE */

    fr_In[eX_FuelCat] = 1; /* 1-Nat, 2-Pile, 3-Slash */

    fr_In[eX_MM_CovGrp] = 5; /* 0-None, 1-Gras, 2-Sag, 3-Shr,  4-Poc, 5-Pon,  6-Whi, 7-Red,  8-Bal */

    fr_In[eX_MoistSoil] =  1; /* Soil Moisture */
    fr_In[eX_SoilType]  = 1;  /* 1-LoaSke,2-FinSil,3-Fin,4-CoaSil,5-CoaLoa */
    fr_In[eX_MoistCond] = 1;  /*  1-VeryDry,2-Dry,3-Moderate,4-Wet */

}

