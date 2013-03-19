//
// $Id
//
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: ansi_mai.c
* Desc: Example front end code to be compiled and linked with the FOFEM ANSI
*        Code.
*       Program demonstrates how to use the Consumed, Emissions, Soil and
*        Tree Mortality ANSI FOFEM code/functions.
*
* Author: Larry Gangi
* Date:  July  2005
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
#include <stdio.h>
#include <string.h>
#include <math.h>

#define WINAPI                /* A dummy define, needed */

/* Include header files for, Consumed, Emission, Soil Heating                */
#include  "fof_ci.h"
#include  "fof_co.h"
#include  "fof_co2.h"
#include  "fof_cm.h"
#include  "fof_ansi.h"
#include  "fof_sh.h"
#include  "fof_disp.h"

/* Include header files for Tree Mortaity                                    */
#include "fof_iss.h"
#include "fof_mrt.h"

/* Declare a large character string for any error message that may get       */
/*  get return from the FOFEM functions                                      */
/* This is declare as global, so large size won't blowup function local stack*/
char cr_ErrMes[3000];

/* Define test functions                                                     */
int  Mortality (void);
int  ConEmiSoi (void);
void Show_Mort(d_MO *a_MO );


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: main
* Desc: Sample code main function
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int   main ()
{

   ConEmiSoi ();              /* Consumed,Emission,Soil sample function  */

   Mortality ();              /* Tree Mortality sample function          */

   return 0;

}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: ConEmiSoi
* Desc: Sample function shows how to run the Consumed, Emissions and
*        Soil Heating calulations.
*  Ret: 0 OK, 1 Error
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int  ConEmiSoi ()
{
/* Declare input and output structures                                       */
d_CI s_CI;                             /* Consumed inputs                    */
d_CO s_CO;                             /* Consumed outputs                   */
d_SI s_SI;                             /* Soil inputs                        */
d_SO s_SO;                             /* Soil outputs                       */

   CI_Init (&s_CI);                     /* Init Consumed Input Struct        */
   CO_Init (&s_CO);                     /* Init Consumed Output Struct       */

   s_CI.f_Duff = 1;                     /* Duff load                         */
   s_CI.f_DufDep = 1;                   /* Duff Depth                        */
   s_CI.f_MoistDuff = 10;               /* Duff Moisture                     */

   s_CI.f_Herb = 1;                     /* Herbaceous load                   */
   s_CI.f_Shrub = 1;                    /* Shrub load                        */

   s_CI.f_CroFol = 1;                   /* Crown Foliage load                */
   s_CI.f_CroBra = 1;                   /* Crown Branch load                 */
   s_CI.f_Pc_CroBrn = 50;               /* Percent of Crown that will burn   */

   s_CI.f_MoistDW10 =  20;              /* Moisture Down Wood 10 hr wood     */
   s_CI.f_MoistDW1000 = 20;             /* Moisture Down Wood 1000 hr wood   */

   s_CI.f_Lit = 1;                      /* Litter load                       */

   s_CI.f_DW1 = 1;                      /* Down Wood 1 hour fuel load        */
   s_CI.f_DW10 = 1;                     /* Down Wood 10 hour fuel load       */
   s_CI.f_DW100 = 1;                    /* Down Wood 100 hour fuel load      */

/* Sound & Rotten Down Wood 1000 hour loads                                  */
/* for this example we use a 1 ton load and distrubute it evenly into        */
/*  each size class, sound and rotten                                        */
   s_CI.f_Snd_DW3 = 1.0 / 8.0;
   s_CI.f_Snd_DW6 = 1.0 / 8.0; ;
   s_CI.f_Snd_DW9 = 1.0 / 8.0; ;
   s_CI.f_Snd_DW20 = 1.0 / 8.0; ;

   s_CI.f_Rot_DW3 = 1.0 / 8.0; ;
   s_CI.f_Rot_DW6 = 1.0 / 8.0; ;
   s_CI.f_Rot_DW9 = 1.0 / 8.0; ;
   s_CI.f_Rot_DW20 = 1.0 / 8.0; ;


   strcpy (s_CI.cr_Region, INTERIORWEST);         /* Set Region              */
   strcpy (s_CI.cr_FuelCategory, NATURAL);        /* Fuel Category           */
   strcpy (s_CI.cr_Season, SUMMER);               /* Season                  */
   strcpy (s_CI.cr_DufMoiMet,ENTIRE);             /* Moisture Measured Method*/

/* If the plot's cover type is in a particular Cover Group                   */
/* strcpy (s_CI.cr_CoverGroup,PONDEROSA);  */


/* To create a fuel load output or emissions file enter a file name          */
/*   strcpy (s_CI.cr_LoadFN, "load.txt"); */
/*   strcpy (s_CI.cr_EmiFN,  "emis.txt"); */


/* Display the Consumed Inputs */
/*   Disp_ConIn (&s_CI); */

/* Call the Consume Manager, and get back answers                            */
   if ( !CM_Mngr (&s_CI, &s_CO,cr_ErrMes)){
     printf ("ERROR - \n");
     printf ("%s\n",cr_ErrMes);
     return 1; }

/* Display outputs from consumed simulation                                  */
   Disp_DHSF (&s_CO);                  /* Duff Herb Shrub Branch             */
   Disp_BrnUp (&s_CO);                 /* DownWood, Litter, Emissions        */
   Disp_ConTot (&s_CO);                /* Consumed totals                    */


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Do Soil Heating                                                           */
/* Some Soil Inputs are taken from the Consumed outputs, see s_CO below      */
/*                                                                           */
   SI_Init (&s_SI);                     /* Initialize these first            */
   SO_Init (&s_SO);

   s_SI.f_DufDepPre = s_CO.f_DufDepPre; /* Set PreFire & PostFire Duff Depth */
   s_SI.f_DufDepPos = s_CO.f_DufDepPos; /*  use values from Consumd Simulaton*/

   s_SI.f_SoilMoist = 15;               /* Duff Moisutre                     */

   strcpy (s_SI.cr_BrnIg,s_CO.cr_BrnIg);/* Tells if Consumd Simlation ignitd*/
   strcpy (s_SI.cr_SoilType,FINE_SILT);
   strcpy (s_SI.cr_MoistCond,MODERATE);

/* This is needed if Soil Heating Simualtion does Exp Model (No Duff Model)  */
/*  otherwise not used                                                       */
   s_SI.i_Cnt  =  s_CO.i_Cnt;           /* Consumed Simulation steps done    */
   s_SI.i_Time =  s_CO.i_Time;          /* consumed time in seconds          */
   s_SI.f_fi   =  s_CO.f_fi;            /* fire intensity                    */


/* Do Soil Heationg simulation,                                              */
/* soil.tmp = save soil heating layer file, else use "" for none             */
   if (!SH_Mngr (&s_SI, &s_SO, "soil.tmp", cr_ErrMes)) {
     printf ("ERROR - %s\n", cr_ErrMes);
     return 1; }

   Disp_Soil (&s_SO);                   /* Display results                   */

   return 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Mortality
* Desc:
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int Mortality ()
{

int  Ret;
int i;
d_MI s_MI;                                     /* Input Mortality Struct       */
d_MO s_MO;                                     /* Output Mortality Struct      */


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Load the Species Equation Table                                           */
/* This can be done using the standard FOFEM species input file or from the  */
/*  internal table                                                           */

/* Load from internal table                                                  */
   MRT_LoadSpe ("", cr_ErrMes); /* load from internal table */

#ifdef commentout
   i = MRT_LoadSpe ("fof_spp.dat", cr_ErrMes);
   if ( i == 0 ) {
     printf (cr_ErrMes,"ERROR - can't find/read file: %s \n", "fof_spp.dat");
     return 0; }
#endif


   MO_Init(&s_MO);                           /* init input struct            */

/* Copy fields the Mortality Input Struct..................                  */
  strcpy (s_MI.cr_Spe, "ABICON");
  s_MI.f_Den = 50;                       /* Density                           */
  s_MI.f_DBH =  8.0;                    /* Diameter Breast Height            */
  s_MI.f_Hgt =  35;                     /* Height                            */
  s_MI.f_FS  =  4.0;                    /* Flame Length                      */
  strcpy (s_MI.cr_FS, "F" );            /* Using Flame Length                */
  strcpy (s_MI.cr_FirSev, "");          /* Use "Low" or  "" = all other      */
  s_MI.f_CroRat = 8;                        /* Crown Raton 1->10              */
  if ( !MRT_Calc (&s_MI, &s_MO,cr_ErrMes)){ /* Calc & accum, Mort,Basal,etc */
    printf ("ERROR - %s \n", cr_ErrMes);
    return 0; }

/* Copy fields the Mortality Input Struct..................                  */
  strcpy (s_MI.cr_Spe, "ABIBAL");
  s_MI.f_Den = 60;                       /* Density                           */
  s_MI.f_DBH =  9.0;                    /* Diameter Breast Height            */
  s_MI.f_Hgt =  40;                     /* Height                            */
  s_MI.f_FS  =  4.0;                    /* Flame Length                      */
  strcpy (s_MI.cr_FS, "F" );            /* Using Flame Length                */
  strcpy (s_MI.cr_FirSev, "");          /* Use "Low" or  "" = all other      */
  s_MI.f_CroRat = 8;                        /* Crown Raton 1->10              */
  if ( !MRT_Calc (&s_MI, &s_MO,cr_ErrMes)){ /* Calc & accum, Mort,Basal,etc */
    printf ("ERROR - %s \n", cr_ErrMes);
    return 0; }

  Show_Mort(&s_MO);

  return 1;


}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Mortality
* Desc:
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/

void Show_Mort(d_MO *a_MO )
{
  printf ("\n\n-----------------------------------------------------------\n");
  printf ("                      Tree Mortality\n");
  printf ("Average Mortality:   %5.0f \n", a_MO->f_AvgMort);
  printf ("Total Trees Killed:  %2.0f \n", a_MO->f_TotKilled);
  printf ("Average DBH Killed:  %5.2f \n", a_MO->f_AvgDBHKilled);
  printf ("Average Mortality > 4 DBH: %5.0f \n", a_MO->f_4AvgMort);
  printf ("\n");

  printf ("Total Pre-Fire Density: %3.0f \n", a_MO->f_TotPreTree);


  printf ("Basal Area Pre-Fire:  %5.2f \n",  a_MO->f_BasTotPre);
  printf ("Basal Area Post-Fire: %5.2f \n",  a_MO->f_BasTotPos);
  printf ("Basal Area Killed:    %5.2f \n",  a_MO->f_BasTotKil);


  printf ("Canopy Cover Pre-Fire:  %3.2f \n",  a_MO->f_CovTotPreLiv);
  printf ("Canopy Cover Post-Fire: %3.2f \n",  a_MO->f_CovTotPosLiv);


  printf ("\n\n-----------------------------------------------------------\n");

}
