/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc: Functions to calculate Mortality
* Date: 12-03
* Author: Larry Gangi
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
#include <stdlib.h>
#include <stdio.h>
#include <math.h>
#include <string.h>

#include "fof_ansi.h"
#include  "fof_iss.h"
#include "fof_mrt.h"
#include "fof_smt.h"
#include "fof_spp.h"
#include "fof_cct.h"
#include "fof_util.h"


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
#define eC_smt 1000
d_SMT sr_SMT[eC_smt];

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
/* These are used in Mortality to keep running totals and averages           */
float  gf_TotMort = 0;
float  gfN_TotMort = 0;
float  gf_4TotMort = 0;
float  gfN_4TotMort = 0;
float  gf_TotKilDBH = 0;
float  gf_CovTotPreLiv = 0;
float  gf_CovTotPosLiv = 0;

int PINPAL_Eq5 (d_MI *a_MI, float f_CSL, float f_BT, double *af_P);

float Whitefir (float cs);
float RedFir  (float cs);
float SubalpineFir (float cs);
float IncenseCedar (float cs);
float WesternLarch (float cs,float f_dbh);
float EngelmannSpruce  (float cs);
float WhitebarkPine (float cs, float dbh);
float SugarPine (float cs);
float PonderosaJeffreyPine (float cs);
float DouglasFir (float cs);

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: MRT_Calc
* Desc: Calculate the Probility of Mortality, Basal Area, Trees Killed
*        Canopy Cover. See the d_MO structure defines in fof_mt.h
*        This function is called with a Species, Density etc, see below.
*        Individual calcs are made for the species, the values are also
*         accumulated so that the caller can have accumlated totals and
*         averages, for Prob Mort, Tree Totals, Basal Area, Canopy Cover.
*
*        For example the FOFEM Bach programs uses this function. The Batch
*         program processes multiple Stands, when beginning a Stand it
*         initializes the MO struct to zero it out, then it begins calling
*         this function for every species in a particular Stand, values
*         continue to get accumulated and get averaged. When the Batch
*         program has a Stand change the Stand values are ready in the
*         MO struct. The MO can be initialize and the next Stand can be
*         started.
*        Running totals and averages are kept in the MO, thru the use of
*         values in MO and some global variables the values are kept
*         current so the caller doesn't have to do anything, just use
*         the totals and averages when they are ready.
*
* Note-1: We use to calculate these if they were missing but now we make
*         the user enter them, so when I clean up the code I just left
*         these old lines here, so I don't have to change the rest of code.
* Note-2: Equation 5 can get done in a varity of ways but that are all
*          considerated to be Eq 5 and report back to caller in d_MO as a 5
*          1. coastal plain with relative humidity
*          2. coastal plain no relative humidity
*          3. non-coastal plain uses previous versions equation 1, even
*              though we report back in d_MO we used eq 5
*
* Note-3: change 7-19-12 I add the compare for "L" because the batch file uses
*         "L" or "" - I could catch it when I read in the file and covert to
"         "Low" but I decided to just check for "L" here 
*   In: cr_Spe......Species                 - reguired
*       f_DBH.......Diameter Breast Height  - reguired
*       f_FS........Flame Length or Scorch Height  - reguired
*       cr_FS.......e_Flame, e_Scorch see defines in fof_mt.h - reguired
*       f_Hgt.......Height                  - reguired
*       f_CrnRat....Crown Ratio             - reguired
*       cr_FirSev..."Low" or "" Fire Severity, - reguired, you must set
*                     if to "Low" or ""
*  Out: cr_ErrMes...Error Message
* NOTE: Check for an Error by checking the cr_ErrMes, it will be "" if no
*        errors occured
*  Ret: percent of mortality
*       -1.0 if Error - see cr_ErrMes
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float  MRT_Calc  (d_MI *a_MI, d_MO *a_MO, char cr_ErrMes[])
{
double f,B, f_BT, f_DBHcm, f_Hgt, f_LCR, f_HCR, f_B,  f_CK, f_CSL, f_P, f_Fl, f_CH;
float  f_Scorch,f_DBH;
int  i_MortEqu;
char cr_Spe[25], cr_FS[10], cr_FirSev[10], cr[100];
   
   strcpy (cr_ErrMes,"");

   strcpy (cr_Spe, a_MI->cr_Spe);           /* put here so easier to use    */
   f_DBH = a_MI->f_DBH;
   strcpy (cr_FS, a_MI->cr_FS);
   strcpy (cr_FirSev,a_MI->cr_FirSev);
   strcpy (cr_ErrMes,"");

   f_Scorch = a_MI->f_FS;
   if ( !xstrcmpi (cr_FS,e_Flame) )           /* if flame length              */
     f_Scorch = Calc_Scorch (a_MI->f_FS);    /*  convert to scorch height    */

   else if (xstrcmpi (cr_FS,e_Scorch)){       /* check code                   */
     sprintf (cr_ErrMes,"Invalid Scorch/Flame Parameter (%s), use %s or %s", cr_FS, e_Flame, e_Scorch);
     return -1.0;}

/* NOTE NOTE - FuelCalc relies on the exact text "Species" being in this error message */
   if (SMT_GetIdx (cr_Spe) == -1) {          /* Chk for Valid Spe            */
     sprintf (cr_ErrMes,"Invalid Species: %s", cr_Spe);
     return -1.0;}

   f_BT = SMT_CalcBarkThick (cr_Spe,f_DBH,cr_ErrMes); /* Calc a Bark THickness       */

   f_Hgt = a_MI->f_Hgt;                     /* Note-1                        */
   f_LCR = a_MI->f_CroRat;

   f_HCR = f_Hgt * ( f_LCR / 10.0 );
   f_B = f_Scorch - (f_Hgt - f_HCR);
   if ( f_B <= 0 )
     f_B = 0;
   if ( f_B > f_HCR )
     f_B = f_HCR;

/* Calc crown killed                                                         */
/*  f_CK = % Crown Volume Scorched, f_CSL = % Crown Height/Length Scorched   */
/*   % = 0 -> 100, scorched basically means killed                            */
   if ( f_HCR != 0 ) {
     f_CK = 100.0 * (f_B * (2.0 * f_HCR - f_B) / ( f_HCR * f_HCR ) );
     f_CSL = 100.0 * (f_B / f_HCR); }
   else {
     f_CK = 0;
     f_CSL = 0;
     strcpy (cr_ErrMes, "Mortality Calculaton is attempting to Divide by 0"); 
     return -1.0;}

/*...........................................................................*/
   i_MortEqu = SMT_MortEqu (cr_Spe);    /* Get equation number               */

   a_MO->i_MortEqu = i_MortEqu;

   switch ( i_MortEqu ) {
                    
/*...................................................*/
/* Change - 8-20-2012 */
/* New formula from DL  */
/* we were having trouble with this but is seem like we need to */
/*  do have this value at 1->10   */
     case 5:  
       if ( f_CSL <= 0 ) { 
          f_P = 0;
          break; }
        f_DBHcm = InchToCent(f_DBH);
        f_BT = 0.435 + ( 0.031 * f_DBHcm ) ;
        f = f_CK / 10.0;                       /* see comments just above */
        B = 0.169 + ( 5.136 * f_BT) + (14.492 * Sqr(f_BT)) - (0.348 * Sqr(f));
        f_P = 1.0 / (1.0 + exp (B)); 
        sprintf (cr, "%20.18f ", f_P); 
        if ( f_P > 1.0 )               /* I don't know if these conditions could */ 
          f_P = 1.0;                   /* happen but I'm checking anyway  */ 
        if ( f_P < 0 ) 
          f_P = 0; 
        break;
/*...................................................*/ 

 //    case 5:  
     case 1:

 //      if ( i_MortEqu == 5 ) {          /* See Notes in PINPAL_Eq()   */
 //        if (PINPAL_Eq5(a_MI,f_CSL,f_BT,&f_P))/* If it was in Coast Plan and done  */
 //          break; }                     /* else fall thru use follwng eq     */

       if ( f_DBH >= 1 )
          f_P = 1.0 / (1.0 + exp(-1.941 + ( 6.316 * (1.0 - exp(-f_BT) ) ) - 0.000535 * (f_CK * f_CK) ) );
       else if ( f_CSL > 50 )
          f_P = 1;
       else if ( f_Hgt < 3 )
          f_P = 1;
       else {
          f_BT = SMT_CalcBarkThick (cr_Spe,1,cr_ErrMes);
          f_P = 1.0 / (1.0 + exp (-1.941 + (6.316 * (1.0 -  exp(-f_BT))) - 0.000535 * (f_CK * f_CK) ));
          f_P = f_P + (1.0-f_P) * (1.0 - ((f_Hgt - 3.0) / (((1.0/f_DBH) * f_Hgt) - 3.0))); }
       break;
     case 3:
          if ( f_DBH > 1 )
             f_P = 1.0 / (1.0 + exp (-1.941 + (6.316 * (1.0 - exp(-f_BT))) - 0.000535 * (f_CK * f_CK) ));
          else if ( f_CSL > 50 )
            f_P = 1.0;
          else if ( f_Hgt < 3 )
            f_P = 1.0;
          else {
            f_BT = SMT_CalcBarkThick (cr_Spe,1,cr_ErrMes);
            f_P = 1.0 / (1.0 + exp(-1.941 + (6.316 * (1.0 - exp(-f_BT))) - 0.000535 * (f_CK * f_CK)));
            f_P = f_P + (1.0 - f_P) * (1.0 - (( f_Hgt - 3.0) / (((1.0 / f_DBH) * f_Hgt) - 3.0))); }
          if ( f_P < 0.8)
             f_P = 0.8;
          break;

     case 4:
           f_Fl = Calc_Flame(f_Scorch);
           f_CH = f_Fl / 1.8 ;
           if ( !xstrcmpi (cr_FirSev,"Low") || !xstrcmpi (cr_FirSev,"L" )) {        /* Fire Severity - See Note-3 Above     */
             f_P = 1.0 / (1.0 + exp( (0.251 * f_DBH * 2.54) - (0.07 * f_CH * 2.54 * 12.0) - 4.407) ); }
           else
             f_P = 1.0 / (1.0 + exp( (0.0858 * f_DBH * 2.54) - (0.118 * f_CH * 2.54 * 12.0) - 2.157) );
          break;

/* test-pfi */
/* New equations from Sharon - april/may 2008                                */
     case 10:
          f_P = Whitefir (f_CSL); /* ABICON                            */
          break;

     case 11:
          f_P = SubalpineFir (f_CK);    /* ABILAS                            */
          break;

     case 12:
          f_P = IncenseCedar (f_CSL);   /* LIBDEC                            */
          break;

     case 14:
          f_P = WesternLarch (f_CK,f_DBH);    /* LAROCC                            */
          break;

     case 15:
          f_P = EngelmannSpruce (f_CK); /* PICENG                            */
          break;

     case 16:
          f_P = RedFir (f_CSL);         /*  ABIMAG                          */
          break;

     case 17:
          f_P = WhitebarkPine (f_CK,f_DBH); /*  PINALB                      */
          break;

     case 18:
          f_P = SugarPine (f_CSL);
          break;

     case 19:
          f_P = PonderosaJeffreyPine(f_CK);
          break;

     case 20:
          f_P = DouglasFir(f_CK);
          break;

/* test-pfi */

     default:
        sprintf (cr_ErrMes, "Equation Not implemented,  Equ Num: %d\n", i_MortEqu);
        f_P = 0;
        return -1.0; 
        break;
   }  /* switch end */

  MRT_Total (a_MI, a_MO, f_P);           /* Accum and Calc, avergest etc.     */
  return f_P;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: EngelmannSpruce       PICENG
* Desc: Pre-fire regular fofem mortality
*       New Eq from Sharon Hood, April 2008
* NOTE: See where this function is called from to get its Eq number
*   In: cs....crown VOLUME scorched percent
*  Ret: mortality probability 0 -> 1.0
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float EngelmannSpruce  (float cs)
{
float f;
     f = 1.0 / ( 1.0 + exp( - (0.0845 + ( cs * 0.0445 ) )));
  return f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: WesternLarch    LAROCC
* Desc: Pre-fire regular fofem mortality
*       New Eq from Sharon Hood, April 2008
* NOTE: See where this function is called from to get its Eq number
*   In: cs....crown VOLUME scorched percent
*  Ret: mortality probability 0 -> 1.0
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float WesternLarch (float cs, float dbh)
{
float f;
     f = 1.0 / ( 1.0 + exp( - ( -1.6594 +
        ( cs * 0.0327 ) -
        ( dbh * 0.1241) )) );

  return f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: IncenseCedar     LIBDEC
* Desc: Pre-fire regular fofem mortality
*       New Eq from Sharon Hood, April 2008
* NOTE: See where this function is called from to get its Eq number
*   In: cs....crown LENGTH scorched percent
*  Ret: mortality probability 0 -> 1.0
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float IncenseCedar (float cs)
{
float f;
     f = 1.0 / ( 1.0 + exp( - ( -4.2466 +
     ( pow((float)cs,(float)3.0) * 0.000007172 ) )));
  return f;
}




/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SugarPine
* Desc: Pre-fire regular fofem mortality
*       New Eq from Sharon Hood, April 2008
* NOTE: See where this function is called from to get its Eq number
*   In: cs....crown LENGTH scorched percent
*  Ret: mortality probability 0 -> 1.0
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float SugarPine (float cs)
{
float f;
     f = 1.0 / ( 1.0 + exp( - ( -2.0588 +
        ( pow((float)cs,(float)2.0) * 0.000814 ) )));

  return f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: PonderosaJeffreyPine
* Desc: Pre-fire regular fofem mortality
*       New Eq from Sharon Hood, April 2008
* NOTE: See where this function is called from to get its Eq number
*   In: cs....crown VOLUME scorched percent
*  Ret: mortality probability 0 -> 1.0
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float PonderosaJeffreyPine (float cs)
{
float f;
     f = 1.0 / ( 1.0 + exp ( - ( -2.7103 +
        ( pow((float)cs,(float)3.0) * 0.000004093) )));

  return f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: DouglasFir
* Desc: Pre-fire regular fofem mortality
*       New Eq from Sharon Hood, April 2008
* NOTE: See where this function is called from to get its Eq number
*   In: cs....crown VOLUME scorched percent
*  Ret: mortality probability 0 -> 1.0
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float  DouglasFir (float cs)
{
float f;
     f = 1.0 / ( 1.0 + exp ( - ( -2.0346 +
        ( cs * 0.0906) -
        ( pow((float)cs,(float)2.0) * 0.0022) +
        ( pow((float)cs,(float)3.0) * 0.000019) )));

  return f;
}



/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: WhitebarkPine  PINALB
* Desc: Pre-fire regular fofem mortality
*       New Eq from Sharon Hood, April 2008
* NOTE: See where this function is called from to get its Eq number
*   In: cs....crown VOLUME scorched percent
*       f_dbh....diam breast height
*  Ret: mortality probability 0 -> 1.0
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float WhitebarkPine (float cs, float dbh)
{
float f;
     f = 1.0 / ( 1.0 + exp( - ( -0.3268 +
        ( cs * 0.1387 ) -
        ( pow((float)cs,(float)2.0) * 0.0033 ) +
        ( pow((float)cs,(float)3.0) * 0.000025 ) -
        ( dbh * 0.0676) )) );
  return f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: RedFir    ABIMAG
* Desc: Pre-fire regular fofem mortality
*       New Eq from Sharon Hood, May 2008
* NOTE: See where this function is called from to get its Eq number
*   In: cs....crown LENGHT scorched percent
*  Ret: mortality probability 0 -> 1.0
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float RedFir  (float cs)
{
float f;
   f = 1.0 / (1.0 + exp( - ( -2.3085 +
       ( pow ((float)cs,(float)3.0) * 0.000004059) )) );
  return f;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SubalpineFir     ABILAS
* Desc: Pre-fire regular fofem mortality
*       New Eq from Sharon Hood, April 2008
* NOTE: See where this function is called from to get its Eq number
*   In: cs....crown VOLUME scorched percent
*       DBH...dbh inch
*  Ret: mortality probability 0 -> 1.0
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float SubalpineFir (float cs)
{
float f;
// printf ("Vol Kill: %6.2f\n",cs);
     f = 1.0 / ( 1.0 + exp( - ( -1.6950 +
     ( cs * 0.2071 ) -
     ( pow((float)cs,(float)2.0) * 0.0047 ) +
     ( pow((float)cs,(float)3.0) * 0.000035 ) )));
  return f;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Whitefir   ABICON
* Desc: Pre-fire regular fofem mortality
*       New Eq from Sharon Hood, April 2008
* NOTE: See where this function is called from to get its Eq number
*   In: cs....crown LENGHT scorched percent
*  Ret: mortality probability 0 -> 1.0
*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float Whitefir (float cs)
{
float f;
   f = 1.0 / (1.0 + exp( - ( -3.5083 +
       ( cs * 0.0956) -
       ( pow ((float)cs,(float)2.0) * 0.00184 ) +
       ( pow ((float)cs,(float)3.0) * 0.000017 ) ) ) );
  return f;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: _PINPAL_Eq5
* Desc: PINPAL longleaf pine uses a special mortality eq developed by
*        Geoff Wang in Coastal Plain, Equation number 5
*       The equation only applys to PINPALs in Coastal Plains of the
*        Southeast.
* NOTE: See Documentation in CoastPln sub-directory, Had a bunch of trouble
*        with Geoff & Steve sending me stuff that didn't work and changing
*        things - So I don't have a lot of confidence in this stuff but
*        it's what they gave me.
*
*   In: a_MI->cr_CP..."Y" means we are in a coastal plain of the Southeast
*       a_MI->f_RelHum...0 means the rel hum isn't known
*                        1->100 rel hum
*       f_CSL.....percent of crown that gets scorched 0 -> 100
*       f_BT......Bark thickness in inches, this function will convert it
*
* NOTE: NOTE: Inputs (RelHum, CSL, BT) get units changed as needed
*
*  Out: *af_P.....probability of mortality NOTE this number will still need
*                  to be checked to see if tree actually gets killed
*  Ret: 1 calc was do,
*       0 calc not done because not in Coastal Plain
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int PINPAL_Eq5 (d_MI *a_MI, float f_CSL, float f_BT, double *af_P)
{
float f,SCH, f_P, f_BTc, f_RelHum;

  if ( strcmp (a_MI->cr_CP,"Y"))        /* If Not Coastal Plain            */
    return 0;                           /* return, call will deal with it    */

  SCH = f_CSL / 100.0;                  /* Need as a decimal percent         */
  f_RelHum = a_MI->f_RelHum / 100.0;
  f_BTc = InchToCent (f_BT);            /* to centimeters                    */

/* Now for PINPAL in the southeast coastal plain without Rel Humidity input  */
  if ( a_MI->f_RelHum == e_RHNonen){   /* No Rel Hum entered by user         */
    f =  -1.507 + (4.45 * SCH) - (4.309 * Sqr(SCH)) + (12.7888 * f_BTc) ;
    f_P = 1.0 / (1.0 + exp (f)) ;
    *af_P = f_P;
/*    printf ("P: %f, SCH: %f, BTc: %f  \n", f_P,SCH, f_BTc ); */
    return 1;  }


/* With Relative Humidity input.............                                 */
   f = -2.191 + (6.482*Sqr(f_RelHum) + (30.076*Sqr(f_BTc)));
   f_P = 1.0 / (1.0 + exp (f)) ;
  *af_P = f_P;
/*  printf ("P = %f,  BTc = %3.2f,   RH = %3.2f \n", f_P,  f_BTc, f_RelHum); */
   return 1;


}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: MRT_Total
* Desc: Do individual species and stand level calcs and totals.
*       This function calculations the stand level values everytime it
*        is called and plugs them into the MO structure so that they
*        are always available to the caller.
*       PLEASE see notes in calling function.
* Note-1: change 11-05--5
*        I caught this div by 0 bug when I was test batch input files,
*        the only time accumulate total killed 'a_MO->f_TotKilled' will
*        be 0 is when nothing has been killed on the stand yet in the
*        course of processing each tree record, this div by 0 inparticular
*        will happen when the first tree record for the stand has nothing
*        killed (no mortality)
*   In: a_MI....Mortality Inputs
*       f_Prob..Probability of Mortality for the species
*  Out: a_MO....Mortalitity Output struct
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  MRT_Total (d_MI *a_MI, d_MO *a_MO, float f_Prob)
{
int i_DBH, i_Den;
float f, f_Killed;

/* Individual Calcs..........................................................*/
   a_MO->f_Mort = (int)((f_Prob * 100.0) + 0.5); /* Individual Species Mort */
   f_Killed = ( f_Prob * a_MI->f_Den ) + 0.5;    /* get # trees killed      */
   a_MO->f_Killed = (int) f_Killed;

/* Individual Basal Area................                                     */
   i_DBH = a_MI->f_DBH;
   i_Den = a_MI->f_Den;
   a_MO->f_BasPre = Basal_Area(i_DBH,i_Den); /* Tot Basal Area              */
   if ( f_Prob > .99 )
      f = a_MI->f_Den;
   else
      f = f_Prob * a_MI->f_Den ;             /* # killed                    */
   a_MO->f_BasKil = Basal_Area(i_DBH,f);     /* Killed basal area           */

   a_MO->f_BasPos = a_MO->f_BasPre - a_MO->f_BasKil;

   a_MO->f_BasPre = a_MO->f_BasPre / 144.0;  /* Get Sqr inches to Sqr Feet   */
   a_MO->f_BasKil = a_MO->f_BasKil / 144.0;
   a_MO->f_BasPos = a_MO->f_BasPos / 144.0;

/* Canopy Cover .............................................................*/
   f = SMT_CalcCrnCov (a_MI->cr_Spe,          /* Crwn Cover Area for 1 tree   */
       a_MI->f_DBH, a_MI->f_Hgt);

   a_MO->f_CovPreLiv = f * a_MI->f_Den;      /* Crwn Cov area * number trees */
   a_MO->f_CovPosLiv = a_MO->f_CovPreLiv - (f * a_MO->f_Killed);

   gf_CovTotPreLiv += a_MO->f_CovPreLiv;   /* Need to accum these before doing*/
   gf_CovTotPosLiv += a_MO->f_CovPosLiv;   /*  overlap                        */

   a_MO->f_CovPreLiv = MRT_Overlap (a_MO->f_CovPreLiv);
   a_MO->f_CovPosLiv = MRT_Overlap (a_MO->f_CovPosLiv);

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.*/
/* Accumulate for Stand                                                      */

   a_MO->f_TotPreTree += a_MI->f_Den;         /* Total Prefire trees         */

   gf_TotMort += a_MO->f_Mort;               /* Tot Prob of Mort            */
   gfN_TotMort++;                            /* count for averaging         */
   a_MO->f_AvgMort = (int)((gf_TotMort / gfN_TotMort) + 0.5);

   if ( a_MI->f_DBH >= 4.0 ) {               /* Average Prob of Mort on      */
     gf_4TotMort += (f_Prob) * 100.0;        /*  trees over 4 inches         */
     gfN_4TotMort++;
     a_MO->f_4AvgMort = (int) ((gf_4TotMort / gfN_4TotMort) + 0.5); }


   a_MO->f_TotKilled += a_MO->f_Killed;       /* total up                    */
   gf_TotKilDBH += a_MI->f_DBH * a_MO->f_Killed;
/* Change 11-5-05 - Fix to catch divide by 0 - See Note-1 above              */
   if ( a_MO->f_TotKilled != 0 )
      a_MO->f_AvgDBHKilled = gf_TotKilDBH / a_MO->f_TotKilled;

/* Basal Area.................................                               */
   a_MO->f_BasTotPre += a_MO->f_BasPre;      /* Total Basal Areas            */
   a_MO->f_BasTotKil += a_MO->f_BasKil;
   a_MO->f_BasTotPos += a_MO->f_BasPos;

/* Canopy Cover..........................                                    */
/* See above where we've been accumulating these covers                      */
   a_MO->f_CovTotPreLiv = MRT_Overlap (gf_CovTotPreLiv);
   a_MO->f_CovTotPosLiv = MRT_Overlap (gf_CovTotPosLiv);
   a_MO->f_CovTotPreLiv = (int) (a_MO->f_CovTotPreLiv + 0.5);
   a_MO->f_CovTotPosLiv = (int) (a_MO->f_CovTotPosLiv + 0.5);

/* Note: I do this here but not on the actual FOFEM5 Mortality Report, so    */
/*  there can be a slight difference between them                            */
/* The FOFEM5 report does some of its own calcs, totals, averages and        */
/*  use the ones in the MO struct                                            */
   if ( a_MO->f_CovTotPreLiv < 1.0 )         /* Show at least 1 percent      */
      a_MO->f_CovTotPreLiv = 1;
   if ( a_MO->f_CovTotPosLiv < 1.0 )
      a_MO->f_CovTotPosLiv = 1;


}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: MO_Init
* Desc: Zeros out all variables need to do Mortality running totals
*        and averages.
*       See Notes in heading of MT_Calc()
* NOTE: the global variable that get init too.
* In/Out: a_MO....Mortality Output struct
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  MO_Init (d_MO *a_MO)
{
   a_MO->f_TotPreTree = 0;
   a_MO->f_Mort   = 0;
   a_MO->f_Killed = 0;
   a_MO->f_BasPre = 0;
   a_MO->f_BasKil = 0;
   a_MO->f_BasPos = 0;
   a_MO->f_CovPreLiv = 0;
   a_MO->f_CovPosLiv = 0;

/* Stand Level Totals,Averages                                                */
   a_MO->f_AvgMort = 0;
   a_MO->f_4AvgMort = 0;
   a_MO->f_TotKilled = 0;
   a_MO->f_AvgDBHKilled = 0;
   a_MO->f_BasTotPre = 0;
   a_MO->f_BasTotKil = 0;
   a_MO->f_BasTotPos = 0;
   a_MO->f_CovTotPreLiv = 0;
   a_MO->f_CovTotPosLiv = 0;

   a_MO->i_MortEqu = 0;


/* Global Static Totals                                                      */
   gf_TotMort = 0;
   gfN_TotMort = 0;
   gf_4TotMort = 0;
   gfN_4TotMort = 0;
   gf_TotKilDBH = 0;
   gf_CovTotPreLiv = 0;
   gf_CovTotPosLiv = 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: MRT_LoadSpe
* Desc: Open Species data file and read it into the SMT (Species Master Table)
* Note-1: This is a check to make sure that each bark equation number that
*         is used in the Species Input file is valid. Which should always be
*         the case unless someone messes with it.
*   In: cr_Pth...Path & File Name of Tree Species input file
*                "" = If no file name is supplied this will load the
*                      hard coded default table see fof_spp.h
*       cr_Ver....Version number to check for in species file
*                 NULL "" means don't check, the batch does this
*
*  Out: cr_ErrMes...Error message
*  Ret: 1 OK,    0 Error
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int   MRT_LoadSpe (char cr_Pth[],char cr_Ver[],  char cr_ErrMes[])
{
int i, iX;
float f;
FILE *fh;
#define ec_line 300
char cr_Line [ec_line], *a;
d_CCT s_CCT;

    strcpy (cr_ErrMes,"");

   iX = 0;
    SMT_InitTable();

    if ( !strcmp (cr_Pth,"")) {         /* Load table using hard coded       */
       MRT_InitST();                    /*  default table                    */
       return 1; }

    fh = fopen (cr_Pth, "r" );
    if ( fh == NULL ) {
      sprintf (cr_ErrMes, "Can't Open Input Species File: %s\n", cr_Pth);
      strcat  (cr_ErrMes, "Check path and file name.");
      return 0;}

    for ( i = 0; i < eC_smt; i++ ) {
      a = fgets (cr_Line, ec_line, fh );     /* Read a line from file        */
        if ( a == NULL ) {                   /* End of File                 */
           fclose (fh);
           return iX; }                        /* Hum didn't find rec we want */
      Blk_End_Line (cr_Line,ec_line);              /* Blank to end of line   */
      if ( isBlank (cr_Line) )                     /* Watch for empty lines  */
        continue;                                  /*  skip them             */


/* Make sure Version number in file match that of FOFEM */
      if ( i == 0 && strcmp (cr_Ver,"")  ) {  /* First line of file     */
        if ( !strstr (cr_Line,cr_Ver) ) {
          sprintf (cr_ErrMes,"Incorrect version number in %s file", cr_Pth);
          fclose (fh);
          return 0; }  }

      if ( cr_Line[0] == '#' )                     /* lines is a comment so  */
        continue;                                  /*  skip it               */
      ToStr (sr_SMT[iX].cr_Spe, cr_Line, 1, 7);
      Trim_LT(sr_SMT[iX].cr_Spe );
      xstrupr(sr_SMT[iX].cr_Spe );

      ToStr (sr_SMT[iX].cr_Name, cr_Line, 9, 54);
      Trim_LT(sr_SMT[iX].cr_Name );
      ToInt (&sr_SMT[iX].i_MrtEqu, cr_Line, 55, 56);
      ToInt (&sr_SMT[iX].i_BrkEqu, cr_Line, 58, 60 );
      ToInt (&sr_SMT[iX].i_Reg1, cr_Line, 62,62);
      ToInt (&sr_SMT[iX].i_Reg2, cr_Line, 63,63);
      ToInt (&sr_SMT[iX].i_Reg3, cr_Line, 64,64);
      ToInt (&sr_SMT[iX].i_Reg4, cr_Line, 65,65);

      ToInt (&sr_SMT[iX].i_No,cr_Line,67,68);/* Crown Equ. Code,             */
      if ( !CCT_Get (sr_SMT[iX].i_No, &s_CCT)) {
         sprintf (cr_ErrMes,"Species: %s, in File:%s, has invalid Crown Equation Code (%d)", sr_SMT[iX].cr_Spe, cr_Pth,sr_SMT[iX].i_No);
         return 0; }

      ToStr (sr_SMT[iX].cr_PFIEqu, cr_Line, 70, 71);
      Trim_LT(sr_SMT[iX].cr_PFIEqu);

      ToStr (sr_SMT[iX].cr_PFIO, cr_Line, 73, 73);
      Trim_LT(sr_SMT[iX].cr_PFIO);

      f = SMT_CalcBarkThick (sr_SMT[iX].cr_Spe, (float)1,cr_ErrMes);/* Note-1      */
      if ( f == -1 ) {
        sprintf (cr_ErrMes,"Species: %s, in File:%s, has invalid Bark Eq. number",
              sr_SMT[iX].cr_Spe, cr_Pth);
        return 0; }

      if ( sr_SMT[iX].i_MrtEqu != 1  &&
           sr_SMT[iX].i_MrtEqu != 3  &&
           sr_SMT[iX].i_MrtEqu != 5  &&
           sr_SMT[iX].i_MrtEqu != 4  &&
/* test-pfi */
           sr_SMT[iX].i_MrtEqu != 10 &&
           sr_SMT[iX].i_MrtEqu != 11 &&
           sr_SMT[iX].i_MrtEqu != 12 &&
           sr_SMT[iX].i_MrtEqu != 14 &&
           sr_SMT[iX].i_MrtEqu != 15 &&
           sr_SMT[iX].i_MrtEqu != 16 &&
           sr_SMT[iX].i_MrtEqu != 17 &&
           sr_SMT[iX].i_MrtEqu != 18 &&
           sr_SMT[iX].i_MrtEqu != 19 &&
           sr_SMT[iX].i_MrtEqu != 20 ){
/* test-pfi */

        sprintf (cr_ErrMes,"Species: %s, in File:%s, has invalid Mortality Eq. number",
              sr_SMT[iX].cr_Spe, cr_Pth);
        return 0; }

      if (sr_SMT[iX].i_Reg1 == -1 &&
          sr_SMT[iX].i_Reg2 == -1 &&
          sr_SMT[iX].i_Reg3 == -1 &&
          sr_SMT[iX].i_Reg4 == -1 ) {
        sprintf (cr_ErrMes,"Species: %s, in File:%s, has no Region",
              sr_SMT[iX].cr_Spe, cr_Pth);
        return 0; }

      iX++; }

 strcpy (cr_ErrMes,"Too Many Species in Species File");
 return 0;
}
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SMT_InitTable
* Desc: Initialize the empty table
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  SMT_InitTable ()
{
int i;
  for ( i = 0; i < eC_smt -1; i++ ) {
    strcpy (sr_SMT[i].cr_Spe,"");
    strcpy (sr_SMT[i].cr_Name,"");
    sr_SMT[i].i_MrtEqu = -1;
    sr_SMT[i].i_BrkEqu = -1;
    sr_SMT[i].i_Reg1 = -1;
    sr_SMT[i].i_Reg2 = -1;
    sr_SMT[i].i_Reg3 = -1;
    sr_SMT[i].i_Reg4 = -1;
    sr_SMT[i].i_No = -1;
/* test-pfi */
    strcpy (sr_SMT[i].cr_PFIEqu,"");
    strcpy (sr_SMT[i].cr_PFIO,"");

  }
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SMT_NotImp
* Desc: Checks to see if any of the Equation numbers or the Crown
*        Ratio  is set to -1 meaning missing/not implented yet
*        I check for 0 or less just in case a 0 comes in, but the
*        fields get set to -1 for missing when the table is read in
*        from the species file.
*   In: iX.....index into table
*  Ret: 1 OK,    0 not implented
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int SMT_NotImp (int iX)
{
   if ( sr_SMT[iX].i_MrtEqu <= 0 ) return 0;
   if ( sr_SMT[iX].i_BrkEqu <= 0 ) return 0;
   return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SMT_Get
* Desc: Get a copy of a record in the table
*       Send in a iterative index to got all recs from table.
*   In: iX....index into table
*  Ret: 1 OK,    0 hit end record of table
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int SMT_Get (int iX,  d_SMT  *a_SMT)
{
  if ( !strcmp (sr_SMT[iX].cr_Spe,"") )
    return 0;
  memcpy (a_SMT,&sr_SMT[iX],sizeof(d_SMT));
  return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SMT_ChkReg
* Desc: See species is in a region,
*   In: a_SMT........Specie Master Record
*       i_Reg........Region
*  Ret: 1 Yes, Species is in region,   0 NO its not
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int  SMT_ChkReg (d_SMT *a_SMT, int i_Reg)
{
   if      ( i_Reg == 1 && a_SMT->i_Reg1 == 1 )
     return 1;
   else if ( i_Reg == 2 && a_SMT->i_Reg2 == 2 )
     return 1;
   else if ( i_Reg == 3 && a_SMT->i_Reg3 == 3 )
     return 1;
   else if ( i_Reg == 4 && a_SMT->i_Reg4 == 4 )
     return 1;
   return 0;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SMT_ChkRegion
* Desc: See if the Species is in the regions
* Note-1: regions are marked by putting the region number in the region
*         variable, ahh not real elegant, but what's the diff
*   In: cr_Spe.......Species to locate
*       i_Reg........Region
*  Ret: 1 Yes, Species is in region,   0 NO its not
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int     SMT_ChkRegion ( char cr_Spe[],  int i_Reg)
{
int iX;
   iX = SMT_GetIdx (cr_Spe);                 /* get idx into tbl of spe      */
   if ( iX == -1 )                           /* Hum, didn't even find it     */
     return 0;
   if      ( i_Reg == 1 && sr_SMT[iX].i_Reg1 == 1 )    /* See Note-1         */
     return 1;
   else if ( i_Reg == 2 && sr_SMT[iX].i_Reg2 == 2 )
     return 1;
   else if ( i_Reg == 3 && sr_SMT[iX].i_Reg3 == 3 )
     return 1;
   else if ( i_Reg == 4 && sr_SMT[iX].i_Reg4 == 4 )
     return 1;
   return 0;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SMT_CalcBarkThick
* Desc: Calculate the Bark Thickness
*   In: cr_Spe....Species
*       f_DBH.....DBH
*  Out: cr_ErrMes....Error Message
*  Ret: bark thinkness,
*      -1 if no equation
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float SMT_CalcBarkThick (char cr_Spe[], float f_DBH, char cr_ErrMes[])
{
int iX, i_BrkEqu;
float f,g;
   iX = SMT_GetIdx (cr_Spe);
   if ( iX < 0 ) {
     strcpy (cr_ErrMes,"Logic Error - Can't Find Species in function SMT_CalcBarkThick");
     return -1; }

   i_BrkEqu = sr_SMT[iX].i_BrkEqu;

   if      ( i_BrkEqu == 1  ) f = 0.019;
   else if ( i_BrkEqu == 2  ) f = 0.022;
   else if ( i_BrkEqu == 3  ) f = 0.024;
   else if ( i_BrkEqu == 4  ) f = 0.025;
   else if ( i_BrkEqu == 5  ) f = 0.026;
   else if ( i_BrkEqu == 6  ) f = 0.027;
   else if ( i_BrkEqu == 7  ) f = 0.028;
   else if ( i_BrkEqu == 8  ) f = 0.029;
   else if ( i_BrkEqu == 9  ) f = 0.03 ;
   else if ( i_BrkEqu == 10 ) f = 0.031;
   else if ( i_BrkEqu == 11 ) f = 0.032;
   else if ( i_BrkEqu == 12 ) f = 0.033;
   else if ( i_BrkEqu == 13 ) f = 0.034;
   else if ( i_BrkEqu == 14 ) f = 0.035;
   else if ( i_BrkEqu == 15 ) f = 0.036;
   else if ( i_BrkEqu == 16 ) f = 0.037;
   else if ( i_BrkEqu == 17 ) f = 0.038;
   else if ( i_BrkEqu == 18 ) f = 0.039;
   else if ( i_BrkEqu == 19 ) f = 0.04 ;
   else if ( i_BrkEqu == 20 ) f = 0.041;
   else if ( i_BrkEqu == 21 ) f = 0.042;
   else if ( i_BrkEqu == 22 ) f = 0.043;
   else if ( i_BrkEqu == 23 ) f = 0.044;
   else if ( i_BrkEqu == 24 ) f = 0.045;
   else if ( i_BrkEqu == 25 ) f = 0.046;
   else if ( i_BrkEqu == 26 ) f = 0.047;
   else if ( i_BrkEqu == 27 ) f = 0.048;
   else if ( i_BrkEqu == 28 ) f = 0.049;
   else if ( i_BrkEqu == 29 ) f = 0.05 ;
   else if ( i_BrkEqu == 30 ) f = 0.052;
   else if ( i_BrkEqu == 31 ) f = 0.055;
   else if ( i_BrkEqu == 32 ) f = 0.057;
   else if ( i_BrkEqu == 33 ) f = 0.059;
   else if ( i_BrkEqu == 34 ) f = 0.06 ;
   else if ( i_BrkEqu == 35 ) f = 0.062;
   else if ( i_BrkEqu == 36 ) f = 0.063;
   else if ( i_BrkEqu == 37 ) f = 0.068;
   else if ( i_BrkEqu == 38 ) f = 0.072;
   else if ( i_BrkEqu == 39 ) f = 0.081;
   else
     return -1;

   g = f * f_DBH;
   return g;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SMT_MortEqu
* Desc: Get Mortality Equation number
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int SMT_MortEqu (char cr_Spe[])
{
int iX;
   iX = SMT_GetIdx (cr_Spe);
   return sr_SMT[iX].i_MrtEqu;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SMT_PFIEqu
* Desc: Get Post Fire Injury Mortality Equation number for
*       Only some species have these eq codes assigned to them
*   In: cr_Spe.......species code
*  Out : cr_Eq.......Eqution code/num
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
/* test-pfi */
void  SMT_PFIEqu (char cr_Spe[], char cr_Eq[])
{
int iX;
   iX = SMT_GetIdx (cr_Spe);
   strcpy (cr_Eq,sr_SMT[iX].cr_PFIEqu);
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SMT_isPFI
* Desc: See if the record has a Post Fire Injury Eq,
*   In: a_SMT...record
*  Ret: 1 Yes, 0 No
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
/* test-pfi */
int  SMT_isPFI (d_SMT *a_SMT)
{
   if ( !stricmp (a_SMT->cr_PFIEqu,""))
     return 0;
   return 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SMT_GetIdx
* Desc: Look for Species in the SMT and return the table index
*   In: cr_Spe.......Species to locate
*  Ret: index of species in table
*       -1 if not found
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int  SMT_GetIdx (char cr_Spe[])
{
int i;
char cr_tmp[100];
   strcpy (cr_tmp,cr_Spe);
   xstrupr (cr_tmp);                        /* got to upper case */

   for ( i = 0; i < 1000; i++ ) {
     if ( !strcmp (sr_SMT[i].cr_Spe, "") )
        break;
     if ( !strcmp (sr_SMT[i].cr_Spe, cr_tmp) )
       return i;
   }
   return -1;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SMT_DispTbl
* Desc: Display all the records in the SMT
*       Needed for testing
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void SMT_DispTbl ()
{
int i;
  for ( i = 0; i < 10000; i++ ) {
    if (sr_SMT[i].cr_Spe[0] == 0 )
       break;
    printf ("%8s, %60s, %2d, %2d,    Reg: %2d, %2d, %2d, %2d, %d \n",
        sr_SMT[i].cr_Spe,
        sr_SMT[i].cr_Name,
        sr_SMT[i].i_MrtEqu,
        sr_SMT[i].i_BrkEqu,
        sr_SMT[i].i_Reg1,
        sr_SMT[i].i_Reg2,
        sr_SMT[i].i_Reg3,
        sr_SMT[i].i_Reg4,
        sr_SMT[i].i_No);  }
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: SMT_CalcCrnCov
* Desc: Calculate the Crown Cover, based on the Crown Cover using the
*        coefficients in the CCT (Crown Cover Table).
*       Species in the fof_spp.dat file are assigned 2 char Crown Cover Codes
*        which are used to find the coefficients in the CCT Table
*
*   In: cr_Spe....Species, this is the 6 character Species codes that
*                  fofem uses from the fof_spp.dat files.
*       f_Dia.....Diamenter
*       f_Hgt.....Tree Height
*  Ret: Crown Cover - in square feet
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float  SMT_CalcCrnCov (char cr_Spe[], float f_Dia, float f_Hgt)
{
int iX;
float f,r,a;
d_CCT s_CCT;

   iX = SMT_GetIdx (cr_Spe);             /* Get index into Species table     */
   CCT_Get (sr_SMT[iX].i_No, &s_CCT);

/* Get Diameter of Crown using Coefficients                                  */
   if ( f_Hgt <= 4.5 )                       /* Small trees                  */
      f = s_CCT.f_r * f_Dia;
   else {                                    /* Large Trees                  */
      f = pow((float)f_Dia,(float)s_CCT.f_b);              /* raise it to power            */
      f = f * s_CCT.f_a; }                   /*  and times                   */

/* Use Diameter of Crown to get Area..........                               */
   r = f / 2.0;                              /* Get diameter to radius       */
   r = r * r;                                /* pi r sqrd gets area of crwn  */
   a = 3.14159 * r ;
   return a;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: CCT_Get
* Desc: Get the record from the Crown Cover Coefficient Table
*   In: i_No.....Fvs Species Index No to identify the coefficient record
*                    in the table.
*  Ret: 1 Code found,
*       0 Code not found, default last record returned
* NOTE: the last record of table has default coeff.
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
int CCT_Get (int i_No, d_CCT *a_CCT)
{
int i;
   for ( i = 0; i < 500; i++ ) {
     if ( sr_CCT[i].i_No == i_No ) {
       memcpy (a_CCT, &sr_CCT[i], sizeof(d_CCT));
       return 1; }
     if ( sr_CCT[i].i_No == -1 )  {
       memcpy (a_CCT, &sr_CCT[i], sizeof(d_CCT));
       return 0; }
   }
   return 0;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: CCT_Display
* Desc:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  CCT_Display (d_CCT *a_CTT)
{
  printf ("--------------CCT--------------------\n");
  printf ("%d, %s, %f, %f, %f\n",
 a_CTT->i_No,  a_CTT->cr_CC, a_CTT->f_a, a_CTT->f_b, a_CTT->f_r);
  printf ("--------------CCT--------------------\n");

}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: MRT_InitST
* Desc: Load the Species Mortality in Memory Table.
*       The SMT table can either get load from the fof_spp.dat file
*        of from the Memory Table
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
void  MRT_InitST ()
{
int i;
  for ( i = 0; i < 10000; i++ ) {
    memcpy (&sr_SMT[i], &sr_MSMT[i], sizeof (d_SMT));
    if ( !strcmp (sr_SMT[i].cr_Spe,"") )
      break;
  }
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: MRT_Overlap
* Desc: Given a square foot amount of Canopy Cover, this will return
*        what percentage that is of and acre,
* NOTE: This will calculation the amount of overlap.
*       For example. if you send in a large amount like 43000 sq feet,
*        which is almost equal to the number squar feet in an acre,
*        you will get back a number around 67, because of the overlap.
*       You would have to send in a number up over 200,000 before you
*        would get back 99 for a percent.
*       Also for example, a number like 400 square feet sent in would get
*         you a return value of about 1 percent
* NOTE: this function will never return a value greater than 100
*   In: canopy cover in square feet.
*  Ret: percent as a whole number  0 > 100
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}*/
float MRT_Overlap( float f_SqFtCov)
{
float x,p;
#define  e_SqFtAcre 43560.0

   x = (f_SqFtCov / e_SqFtAcre);
   p = 100.0 * ( 1.0 -  exp ( -x ) ) ;
   return p;
}


