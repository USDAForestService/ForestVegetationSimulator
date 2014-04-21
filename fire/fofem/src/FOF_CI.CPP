
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_CI.c -  Consumed Data Inputs
* Desc:
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include  "bur_brn.h"
#include  "fof_ci.h"
#include  "fof_util.h"
#include  "fof_lem.h"
#include  "fof_ansi.h"


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: CI_Init
* Desc: Initialize the structure.
* NOTE: CI struct MUST BE initialized before use. There are default
*        values that burnup uses that need to be init, like Sigma, etc.
*        see below.
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
// void  WINAPI CI_Init (d_CI *a_CI)
// TEST Larry
void  CI_Init (d_CI *a_CI)
{
    a_CI->f_Duff = 0;
    a_CI->f_DufDep = 0;
    a_CI->f_MoistDuff = 0;
    a_CI->f_MoistDW10 = 0;
    a_CI->f_MoistDW1000 = 0;
    a_CI->f_LitMoi = 0;
    a_CI->f_Lit = 0;
    a_CI->f_DW1 = 0;
    a_CI->f_DW10 = 0;
    a_CI->f_DW100 = 0;
    a_CI->f_DW1000 = 0;
    a_CI->f_pcRot = 0;

    a_CI->f_Snd_DW3 = 0;
    a_CI->f_Snd_DW6 = 0;
    a_CI->f_Snd_DW9 = 0;
    a_CI->f_Snd_DW20 = 0;

    a_CI->f_Rot_DW3 = 0;
    a_CI->f_Rot_DW6 = 0;
    a_CI->f_Rot_DW9 = 0;
    a_CI->f_Rot_DW20 = 0;

    a_CI->f_HSFCon = 0;
    a_CI->f_PerDufCon = 0;

    strcpy (a_CI->cr_LoadFN,"");
    strcpy (a_CI->cr_EmiFN,"");

    a_CI->f_SURat_Lit      = e_SURat_Lit;        /* Sigma Values            */
    a_CI->f_SURat_DW1      = e_SURat_DW1;
    a_CI->f_SURat_DW10     = e_SURat_DW10;
    a_CI->f_SURat_DW100    = e_SURat_DW100;
    a_CI->f_SURat_DWk_3_6  = e_SURat_DWk_3_6;
    a_CI->f_SURat_DWk_6_9  = e_SURat_DWk_6_9;
    a_CI->f_SURat_DWk_9_20 = e_SURat_DWk_9_20;
    a_CI->f_SURat_DWk_20   = e_SURat_DWk_20;

   a_CI->l_MAX_TIMES    = e_MAX_TIMES;
   a_CI->f_INTENSITY    = e_INTENSITY;
   a_CI->f_IG_TIME      = e_IG_TIME;
   a_CI->f_WINDSPEED    = e_WINDSPEED;
   a_CI->f_DEPTH        = e_DEPTH;
   a_CI->f_AMBIENT_TEMP = e_AMBIENT_TEMP;
   a_CI->f_R0           = e_R0;
   a_CI->f_DR           = e_DR;
   a_CI->f_TIMESTEP     = e_TIMESTEP;

   a_CI->f_CroFol = 0;
   a_CI->f_CroBra = 0;
   a_CI->f_Pc_CroBrn = 0;
   a_CI->f_Herb = 0;
   a_CI->f_Shrub = 0;

   strcpy (a_CI->cr_WD,"");
   strcpy (a_CI->cr_Region,"");
   strcpy (a_CI->cr_FuelCategory,e_FuelCategoryDefault);
   strcpy (a_CI->cr_CoverGroup,"");
   strcpy (a_CI->cr_CoverClass,e_CI_DefaultCoverClass);
   strcpy (a_CI->cr_Season,e_SeasonDefault);
   strcpy (a_CI->cr_DufMoiMet,e_DufMoiMetDefault); /* How Duff Moisture Was measured */

}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: CI_ Season
* Desc: Check the season that is set in the CI structure
*  Ret: 1 Yes, 0 NO
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int CI_isSpring (d_CI *a_CI) { if ( !xstrcmpi(a_CI->cr_Season,e_Spring)) return 1; return 0;}
int CI_isFall   (d_CI *a_CI) { if ( !xstrcmpi(a_CI->cr_Season,e_Fall))   return 1; return 0;}
int CI_isSummer (d_CI *a_CI) { if ( !xstrcmpi(a_CI->cr_Season,e_Summer)) return 1; return 0;}
int CI_isWinter (d_CI *a_CI) { if ( !xstrcmpi(a_CI->cr_Season,e_Winter)) return 1; return 0;}
int CI_isSeason (d_CI *a_CI) { if ( !xstrcmpi(a_CI->cr_Season,"")) return 0; return 1;}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: CI_ Cover Type
* Desc: Check the Cover Type that is set in the CI structure
*  Ret: 1 Yes, 0 NO
*
*  NOTE: 4-2-2012 I I modified the functions below to also check for the 
*        2 letter codes, so that I could use those same 2 letter codes
*        as they come in from species cover .dat file 
* 
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int CI_isGrassGroup (d_CI *a_CI){ if ( !xstrcmpi(a_CI->cr_CoverGroup,e_GrassGroup))return 1;  
                                  if ( !xstrcmpi(a_CI->cr_CoverGroup,e_CVT_GrassGroup)) return 1; return 0;}

int CI_isShrubGroup (d_CI *a_CI){ if ( !xstrcmpi(a_CI->cr_CoverGroup,e_ShrubGroup)) return 1; if ( !xstrcmpi(a_CI->cr_CoverGroup,e_CVT_ShrubGroup)) return 1;  return 0;}

int CI_isSageBrush  (d_CI *a_CI){ if ( !xstrcmpi(a_CI->cr_CoverGroup,e_SageBrush))  return 1; if ( !xstrcmpi(a_CI->cr_CoverGroup,e_CVT_SageBrush))  return 1;  return 0;}

int CI_isPocosin    (d_CI *a_CI){ if ( !xstrcmpi(a_CI->cr_CoverGroup,e_Pocosin  ))  return 1; if ( !xstrcmpi(a_CI->cr_CoverGroup,e_CVT_Pocosin  ))  return 1; return 0; }

int CI_isPonderosa  (d_CI *a_CI){ if ( !xstrcmpi(a_CI->cr_CoverGroup,e_Ponderosa))  return 1; if ( !xstrcmpi(a_CI->cr_CoverGroup,e_CVT_Ponderosa))  return 1; return 0;}

int CI_isWhiPinHem  (d_CI *a_CI){ if ( !xstrcmpi(a_CI->cr_CoverGroup,e_WhiPinHem))  return 1; if ( !xstrcmpi(a_CI->cr_CoverGroup,e_CVT_WhiPinHem))  return 1; return 0;}

int CI_isRedJacPin  (d_CI *a_CI){ if ( !xstrcmpi(a_CI->cr_CoverGroup,e_RedJacPin))  return 1; if ( !xstrcmpi(a_CI->cr_CoverGroup,e_CVT_RedJacPin))  return 1; return 0;}

int CI_isBalBRWSpr  (d_CI *a_CI){ if ( !xstrcmpi(a_CI->cr_CoverGroup,e_BalBRWSpr))  return 1; if ( !xstrcmpi(a_CI->cr_CoverGroup,e_CVT_BalBRWSpr))  return 1; return 0;}

/* test-ld larry */
int CI_isCoastPlain (d_CI *a_CI){ if ( !xstrcmpi(a_CI->cr_CoverGroup,e_CoastPlain)) return 1; if ( !xstrcmpi(a_CI->cr_CoverGroup,e_CVT_CoastPlain)) return 1; return 0;}


int CI_isCoverGroup  (d_CI *a_CI){ if ( !xstrcmpi(a_CI->cr_CoverGroup,"")) return 0; return 1;}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: CI_ Region
* Desc: Check the Region that is set in the CI structure
*  Ret: 1 Yes, 0 NO
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int CI_isSouthEast    (d_CI *a_CI){ if ( !xstrcmpi(a_CI->cr_Region,e_CI_SouthEast   )) return 1; return 0; }
int CI_isInteriorWest (d_CI *a_CI){ if ( !xstrcmpi(a_CI->cr_Region,e_CI_InteriorWest)) return 1; return 0; }
int CI_isPacificWest  (d_CI *a_CI){ if ( !xstrcmpi(a_CI->cr_Region,e_CI_PacificWest )) return 1; return 0; }
int CI_isNorthEast    (d_CI *a_CI){ if ( !xstrcmpi(a_CI->cr_Region,e_CI_NorthEast   )) return 1; return 0; }

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: CI_ Fuel Category
* Desc: Check the Fuel Category that is set in the CI structure
*  Ret: 1 Yes, 0 NO
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int CI_isNatural (d_CI *a_CI) { if ( !xstrcmpi(a_CI->cr_FuelCategory,e_Natural)) return 1; return 0;}
int CI_isPiles (d_CI *a_CI)   { if ( !xstrcmpi(a_CI->cr_FuelCategory,e_Piles))   return 1; return 0; }
int CI_isSlash (d_CI *a_CI)   { if ( !xstrcmpi(a_CI->cr_FuelCategory,e_Slash))   return 1; return 0; }

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: CI_ Duff Moisture Method functions
* Desc: How was Duff Moisture Measured
*  Ret: 1 Yes, 0 NO
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int CI_isDufEntire  (d_CI *a_CI)
{ if ( !xstrcmpi(a_CI->cr_DufMoiMet,e_Entire  ))
    return 1;
   return 0;
}
int CI_isDufLower   (d_CI *a_CI)
{
   if ( !xstrcmpi(a_CI->cr_DufMoiMet,e_Lower ))
      return 1;
   return 0;
}

int CI_isDufNFDR    (d_CI *a_CI)
{
  if ( !xstrcmpi(a_CI->cr_DufMoiMet,e_NFDR ))
    return 1;
   return 0;
}

int CI_isDufAdjNFDR (d_CI *a_CI)
{
if ( !xstrcmpi(a_CI->cr_DufMoiMet,e_Adj_NFDR))
     return 1;
  return 0;
}

int CI_isDufMethod  (d_CI *a_CI)
{
  if ( !xstrcmpi(a_CI->cr_DufMoiMet,""))
    return 0;
   return 1;
 }


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: CI_isWet
* Desc: See if the Duff Moisture is a 'Wet' condition
*       The FOFEM interface sets the duff moisture box to the following
*        based on the Moisture Method and Moisture Condition List Boxes
*                   Moderate    Wet
*      Entire          75      130
*      Lower           75      130
*      NFDR            20       30
*      AdjNFDR         28       42
*    SO, I picked a midpoint for each Moisture Method and decide if
*     it is 'wet' of not
*  Originally I was going to have Moisture Condition set by the user into
*   a_CI, but 'wet' is only looked at in the Duff Pocosin, we LR said
*   Just check for wet like this..
*  Ret: 1 Yes, 0 NO
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int CI_isDuffWet (d_CI *a_CI)
{
  if ( CI_isDufEntire (a_CI) && a_CI->f_MoistDuff >= 103.0 ) return 1;
  if ( CI_isDufLower  (a_CI) && a_CI->f_MoistDuff >= 103.0 ) return 1;
  if ( CI_isDufNFDR   (a_CI) && a_CI->f_MoistDuff >=  25.0 ) return 1;
  if ( CI_isDufAdjNFDR(a_CI) && a_CI->f_MoistDuff >=  35.0 ) return 1;
  return 0;
}



/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: CI_MaxLoad
* Desc: Find the greatest TPA pre loading amount in the CI struct;
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
float CI_MaxLoad (d_CI *a_CI)
{
float f;
   f = 0;
   if ( a_CI->f_Lit    > f )  f = a_CI->f_Lit;
   if ( a_CI->f_DW1    > f )  f = a_CI->f_DW1;
   if ( a_CI->f_DW10   > f )  f = a_CI->f_DW10;
   if ( a_CI->f_DW100  > f )  f = a_CI->f_DW100;
   if ( a_CI->f_DW1000 > f )  f = a_CI->f_DW1000;
   if ( a_CI->f_Duff   > f )  f = a_CI->f_Duff;
   if ( a_CI->f_Herb   > f )  f = a_CI->f_Herb;
   if ( a_CI->f_Shrub  > f )  f = a_CI->f_Shrub;
   if ( a_CI->f_CroBra > f )  f = a_CI->f_CroBra;
   if ( a_CI->f_CroFol > f )  f = a_CI->f_CroFol;
    return f;
}
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: CI_OnlyDuff
* Desc: See if the only loading we have in the CI struct is a Duff
*       loading.
*   In: a_CI.....structure
*  Ret: 1 Yes only have duff, else 0
*        Note 0 could mean that all loads are 0
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int  CI_OnlyDuff (d_CI *a_CI)
{
   if ( a_CI->f_Lit    > 0 ) return 0;      /* If have any of theses        */
   if ( a_CI->f_DW1    > 0 ) return 0;
   if ( a_CI->f_DW10   > 0 ) return 0;
   if ( a_CI->f_DW100  > 0 ) return 0;
   if ( a_CI->f_DW1000 > 0 ) return 0;
   if ( a_CI->f_Herb   > 0 ) return 0;
   if ( a_CI->f_Shrub  > 0 ) return 0;
   if ( a_CI->f_CroBra > 0 ) return 0;
   if ( a_CI->f_CroFol > 0 ) return 0;

   if ( a_CI->f_Duff > 0 )                  /* everything else is 0 and     */
     return 1;                               /* have some duff               */

   return 0;
}


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/*  used for testing                                                         */

// #define printf xprintf
// void far xprintf (char *Format, ...);

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: CI_Display
* Desc:
*  Ret:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void CI_Display (d_CI *a_CI)
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
