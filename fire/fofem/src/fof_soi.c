/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: fof_soi.c
* Desc: this was Soilheat.pas
*       This is used by Duff Sim & Exp Heat
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

#include  "fof_sh.h"
#include  "fof_sd.h"
#include  "fof_soi.h"

extern  char gcr_SoiErr[];

/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */
#define   e_zmax     0.3   /* {depth of lower boundary, m                 } */
#define   e_Patm   92000   /* {atmospheric pressure at simulation site, Pa} */
#define   e_Dvo  2.12e-5   /* {vapor diffusivity in air, m2/s             } */
#define   e_Tstd  273.15   /* {standard temperature, K                    } */
#define   e_Po    101300   /* {sea level or standard pressure, Pa         } */
#define   e_R     8.3143   /* {gas constant, J/mol/K                      } */
#define   e_Mw     0.018   /* {mole mass of water, kg/mol                 } */
#define   e_hc        20   /* {surface boundary layer resistance          } */
#define   e_epse     100   /* {energy balance error - W/m2                } */
#define   e_epsw    1e-5   /* {water mass balance error - kg/(m2 s)       } */
#define   e_dw      1000   /* {density of water - kg/m3                   } */
#define   e_tor     0.66   /* {soil tortuosity - dimensionless            } */
#define          e_maxits    20   /* {maximum number of iterations in solution   } */
#define   e_airvp   1000   /* {air vapor pressure - Pascals               } */
#define   e_Tair      20   /* {starting air temp - C                      } */


/*.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.-.- */

FILE *fh_In;

REAL  r_seh, r_sev, r_wav,  r_tav,  r_dJv,  r_dJvdt, r_dJvdp;
REAL  r_bd,  r_pd,  r_ls,   r_ga,   r_xwo,  r_cop,   r_xo;
REAL  r_dC,  r_dv,  r_dCdp, r_dvdp, r_dvdt, r_dCdt,  r_m;
REAL  r_tk,  r_tk3, r_dtn,  r_gvol, r_ch,   r_xs,    r_xws;

REAL  rr_wn   [e_mplus1+1],  rr_w    [e_mplus1+1],  rr_z[e_mplus1+1];
REAL  rr_p    [e_mplus1+1],  rr_dwdp [e_mplus1+1],  rr_v[e_mplus1+1];
REAL  rr_h    [e_mplus1+1],  rr_tn   [e_mplus1+1],  rr_dhdp [e_mplus1+1];
REAL  rr_psat [e_mplus1+1],  rr_kev  [e_mplus1+1],  rr_u    [e_mplus1+1];
REAL  rr_Hvap [e_mplus1+1],  rr_s    [e_mplus1+1],  rr_ke   [e_mplus1+1];
REAL  rr_kh   [e_mplus1+1],  rr_kv   [e_mplus1+1],  rr_cp   [e_mplus1+1];
REAL  rr_conv [e_mplus1+1],  rr_vcon [e_mplus1+1],  rr_enh  [e_mplus1+1];
REAL  rr_t    [e_mplus1+1];

REAL  rr_AirPor[e_mplus1+1];


REAL  sPOW (REAL x, REAL y);


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: soiltemp_step
* Desc: This code was converted from orginal Pascal code, not really sure
*        what it exactly does
* Note-1: Change 11-6-05, put work around for bug found.
*         Code can get stuck in an infinite loop. Discovered this with
*          some of DL's batch data.
*         I checked back with the orginal Pascal code and verified that
*          I converted the code correctly which it looks like I did.
*         This function the and code that call it have some serious logic
*          errors, see the i_its variable below, it never gets incremented
*          but is checked in the loop as a break control. I tried implementing
*          it but didn't completly help as the calling functions loop
*          would keep calling it again and again, seems that calling loop
*          doesn't have a way of timing out.
*         The inputs being sent into the upper lever Soil Sim via the
*           d_SI input struct that cause this problem are, roughly speaking,
*           because they seem vary with ranges are approx. fi 89, time 180
*           moisture 25 'WET'
*         ER suggest we just put a check in to time out the loop and report
*          back to user that Soil Sim doesn't handle the situation
*         NOTE: I modified this function from a void to and int return,
*          and return 0 for the bug loop time out
* Ret: 1 ok, else 0 = error, read above
*
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
int soiltemp_step (REAL r_Rabs, REAL r_dt,  int  *ai_success, REAL r_time)
{

int i,i_its;
int iN_SoilBug;

   iN_SoilBug = 0;

/* REAL fff; */
   rr_tn[0] = (REAL)e_Tair;
   i_its = 0;

/*.......................................................................    */
   while (1) {                    /* {begin heat and water solutions}       */
     r_seh = 0;
     r_sev = 0;
     rr_ke[0] = 0;                      /* {hc}; {hc taken out so that surf. temp. not needed} */
     rr_kev[0] = 6.2e-9 * (REAL)e_hc;  /* {6.2e-9*hc makes heat and water cond. equal} */
     rr_psat[0] = vaporpressure (rr_tn[0]);
     rr_h[0] = (REAL)e_airvp / rr_psat[0];
     rr_psat[1] = vaporpressure (rr_tn[1]);
     r_wav = 0.5 * (rr_wn[1] + rr_wn[2]);
     rr_s[1] = slope (rr_tn[1], rr_psat[1]);
     rr_Hvap[1] = Hv (rr_tn[1]);
     rr_kh[1] = tcond (rr_tn[1], r_wav, r_xs, r_ls, r_ga, r_xwo, r_cop,  rr_h[1] * rr_psat[1], rr_s[1], &rr_enh[1]);
     rr_AirPor[1] = (r_xws - r_wav);
     rr_kv[1] = rr_enh[1] * rr_AirPor[1] * (REAL)e_tor * Kvap(rr_t[1], rr_psat[1] * rr_h[1]);

/*..........................................................................*/
     for ( i = 1; i <= r_m; i++ ) {
       rr_cp[i] = rr_v[i] * (0.87 * r_bd + 4.18e6 * rr_wn[i]) / r_dt;
       rr_psat[i+1] = vaporpressure (rr_tn[i+1]);
       if ( i < r_m ) {
         r_wav = 0.5 * (rr_wn[i+1] + rr_wn[i+2]);
         r_tav = 0.5 * (rr_tn[i+1] + rr_tn[i+2]) + 273; }
       else {
           r_wav = rr_wn[e_mplus1];
           r_tav = rr_tn[i+1] + 273; }
       rr_conv[i] = 0.5 * (rr_u[i-1] + rr_u[i]) * 1200 * 293 / r_tav;
       rr_vcon[i] = rr_conv[i] * (REAL)e_Mw / ( (REAL)e_R * 1200 * 293);
       rr_s[i+1] = slope(rr_tn[i+1], rr_psat[i+1]);
       rr_Hvap[i+1] = Hv (rr_tn[i+1]);
       rr_kh[i+1] = tcond (rr_tn[i+1], r_wav, r_xs, r_ls, r_ga, r_xwo, r_cop, rr_h[i+1] * rr_psat[i+1], rr_s[i+1], &rr_enh[i+1]);
       rr_ke[i] = (rr_kh[i]) / ((rr_z[i+1] - rr_z[i])) + rr_conv[i];
       rr_AirPor[i+1] = (r_xws - r_wav);
       rr_kv[i+1] = rr_enh[i+1] * rr_AirPor[i+1] * (REAL)e_tor * Kvap (rr_t[i+1], rr_psat[i+1] * rr_h[i+1]);
       rr_kev[i] = (rr_kv[i] + rr_kv[i+1]) / (2 * (rr_z[i+1] - rr_z[i])) + rr_vcon[i];
       r_dJv = rr_kev[i-1] * (rr_psat[i] * rr_h[i] - rr_psat[i-1] * rr_h[i-1]) - rr_kev[i] * (rr_psat[i+1] * rr_h[i+1] - rr_psat[i] * rr_h[i]);
       r_dJvdt = rr_s[i] * rr_h[i] * (rr_kev[i-1] + rr_kev[i]);
       r_dJvdp = rr_psat[i] * (rr_kev[i-1] + rr_kev[i]) * rr_dhdp[i];
       r_dC =   rr_ke[i-1] * (rr_tn[i] - rr_tn[i-1])
              - rr_ke[i] * (rr_tn[i+1] - rr_tn[i])
              + rr_cp[i] * (rr_tn[i] - rr_t[i])
              - rr_Hvap[i] * (REAL)e_dw * rr_v[i]
              * (rr_wn[i] - rr_w[i]) / r_dt;
       r_dv = r_dJv + (REAL)e_dw * rr_v[i] * (rr_wn[i] - rr_w[i]) / r_dt;
       r_dCdp = - rr_Hvap[i] * (REAL)e_dw * rr_v[i] * rr_dwdp[i] / r_dt;
       r_dvdp = r_dJvdp + (REAL)e_dw * rr_v[i] * rr_dwdp[i] / r_dt;
       r_dvdt = r_dJvdt;
       r_dCdt = rr_ke[i] + rr_ke[i-1] + rr_cp[i];
       if ( i == 1) {
         r_tk = rr_tn[1] + 273;
         r_tk3 = r_tk * r_tk * r_tk;
         r_dC = r_dC - r_Rabs + 5.67e-8 * r_tk * r_tk3;
         r_dCdt = r_dCdt + 4 * 5.67e-8 * r_tk3; }

       r_sev = r_sev + abs_Real(r_dv);
       r_seh = r_seh + abs_Real(r_dC);

       r_dtn = (r_dv * r_dCdp - r_dC * r_dvdp) / (r_dCdp * r_dvdt - r_dCdt * r_dvdp);
       if ( r_dtn < -100 )
         r_dtn = -100;
       rr_tn[i] = rr_tn[i] - r_dtn;
       r_dtn = ( r_dv - r_dvdt * r_dtn) / r_dvdp;
       rr_p[i] = rr_p[i] - r_dtn;
       if ( rr_p[i] > 0 )
         rr_p[i] = ( rr_p[i] + r_dtn ) * 0.5;
       if ( rr_p[i] < -1e20 )
         rr_p[i] = -1e20;
       rr_wn[i] = watercontent (rr_p[i], r_xo, &rr_dwdp[i]);
       rr_h[i] = humidity (rr_p[i], rr_tn[i], &rr_dhdp[i]);
     } /* for i end */

/*...........................................................................*/

/* Change 11-6-05, Catch infinite loop bug, See Note-1 above                 */
     iN_SoilBug++;
     if ( iN_SoilBug >= 50 )
       return 0;
/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

     if ( (r_sev < (REAL)e_epsw  && r_seh < (REAL)e_epse) || i_its > e_maxits )
       break;

    } /* while (1) end */

/*.........................................................................*/
   if ( i_its < e_maxits ) {
     *ai_success = 1;

/* changed 3-27-00 */
/*     rr_u[r_m] = 0;    got error when compliling as C++ */
       rr_u[(int)r_m] = 0;


     for ( i = r_m; i >= 1; i-- ) {
       r_gvol = (REAL)e_dw * rr_v[i] * (REAL)e_R * (rr_tn[i] + 273) * (rr_w[i] - rr_wn[i]) / (r_dt * rr_AirPor[i] * (REAL)e_Mw * (REAL)e_Patm);
       if ( r_gvol < 0 )
         r_gvol = 0;
       rr_u[i-1] = rr_u[i] + r_gvol; }
     for ( i = 1; i <= e_mplus1; i++ ) {
       r_ch = rr_wn[i] - rr_w[i];
       rr_w[i] = rr_wn[i];
       rr_wn[i] = rr_w[i] + r_ch;
       r_ch = rr_tn[i] - rr_t[i];
       rr_t[i] = rr_tn[i];
       rr_tn[i] = rr_t[i] + r_ch; }  }
   else
     *ai_success = 0;

   return 1;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: tcond
* Desc:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
REAL   tcond (REAL r_t, REAL r_xw, REAL r_xs, REAL r_ls, REAL r_ga,
              REAL r_xwo, REAL r_cop, REAL r_p, REAL r_s, REAL *ar_enh)
{
/*  REAL  f; */
REAL  r_wf,r_ka,r_ks,r_kw,r_la,r_lw,r_lf,r_xa,r_xws,r_gc,r_lda,r_xv,r_tc;
/*  REAL  g,h; */
REAL  r_A, r_B, r_C;
   r_xws = 1 - r_xs;
   r_xa = r_xws - r_xw;
   if ( r_t < 100 ) {
     r_lw = 0.554 + r_t * (2.24e-3 - 9.87e-6 * r_t);
     r_tc = sqr (sqr( (r_t + 273) / 303)); }
   else {
     r_lw = 0.68;
     r_tc = 2.3; }
   r_lda = 0.024 +  r_t * (7.73e-5 - 2.6e-8 *  r_t);
   if ( r_xw < (0.01 * r_xwo) )
     r_wf = 0;
   else
     r_wf = 1 / (1 + sPOW(r_xw / r_xwo,-r_cop * r_tc));

/*...orginal line  r_la = r_lda + r_wf * Hv(r_t) * r_s * Kvap(r_t,r_p);      */
   r_A = Kvap(r_t,r_p);
   r_B = Hv(r_t);
   r_C = ( r_wf * r_B * r_s * r_A);
   r_la = r_lda + r_C ;

   r_gc = 1 - 2 * r_ga;
   r_lf = r_la + (r_lw - r_la) * r_wf;
   r_ka = (2 / (1 + (r_la / r_lf -1) * r_ga) + 1 / (1 + (r_la / r_lf - 1) * r_gc ) ) / 3;
   r_kw = (2 / (1 +(r_lw / r_lf - 1) * r_ga) + 1 / (1 + (r_lw / r_lf - 1) * r_gc )) /3;
   r_ks = (2 / (1 +(r_ls / r_lf - 1) * r_ga) + 1 / (1 + (r_ls / r_lf - 1) * r_gc )) /3;
  *ar_enh = ( 1 + 2 * r_wf) * r_ka;
   r_tc = (r_kw * r_lw * r_xw + r_ka * r_la * r_xa + r_ks * r_ls * r_xs) / (r_kw * r_xw + r_ka * r_xa + r_ks * r_xs);
   if ( r_tc > 0  &&  r_tc < 5)
     return r_tc;
   return 1;
}



/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc:
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void soiltemp_initconsts (REAL r_bdi, REAL r_pdi, REAL r_lsi, REAL r_gai,
                     REAL r_xwoi, REAL r_copi, REAL r_xoi,
                     REAL rr_zi[] )
{
int  i;

   r_bd  = r_bdi;
   r_pd  = r_pdi;
   r_ls  = r_lsi;
   r_ga  = r_gai;
   r_xwo = r_xwoi;
   r_cop = r_copi;
   r_xo  = r_xoi;
   r_m   = e_mplus1 - 1 ;
   r_xs  = r_bd / r_pd;
   r_xws = 1 - r_xs;

   for ( i = 0; i <= e_mplus1; i++ ) {
     if ( rr_zi[i] < 0 )
       break;
     rr_z[i] = rr_zi[i];
     rr_z[i] = rr_z[i] / 1000;
   }

/*........................................*/

   for ( i = 0; i <= e_mplus1; i++  )
     rr_v[i] = 0;
   for ( i = 1; i <= r_m; i++  )
     rr_v[i] = 0.5 * ( rr_z[i+1] - rr_z[i-1] );
   rr_AirPor[0] = 1;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc:
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void   soiltemp_initprofile (REAL rr_wi[], REAL rr_ti[])
{
int  i;

   Copy_Array ( rr_w,  rr_wi);
   Copy_Array ( rr_wn, rr_wi);
   Copy_Array ( rr_t,  rr_ti);
   Copy_Array ( rr_tn, rr_ti);
   for ( i = 0; i <= e_mplus1; i++ )  {
     rr_p[i] = -exp ( 13.82 * ( 1 - rr_w[i] / r_xo ) );
     rr_w[i] = watercontent ( rr_p[i], r_xo, &rr_dwdp[i]);
     rr_h[i] = humidity ( rr_p[i], rr_t[i], &rr_dhdp[i] );
     rr_kev[i] = 0;
     rr_u[i] = 0;
     rr_enh[i] = 0; }
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: watercontent
* Desc:
* Note-1: This log(0) never happened but figured I'd better put a check in
*         in case it ever does. I think trying to do a log(0) will blowup
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
REAL watercontent (REAL r_p, REAL r_xo, REAL *ar_dwdp)
{
#define e_lnpo  13.82         /* ln of oven dry water content */
REAL f;

  if ( r_p >= 0 )
     r_p = -0.001;
  *ar_dwdp = -r_xo / ( (REAL)e_lnpo * r_p );

  if ( r_p == 0 ) {                     /* See Note-1 above                  */
    strcpy (gcr_SoiErr, "watercontent() - Math Error - attempted to do a log(0)");
    r_p = 1; }

  f =  r_xo * ( 1 - log(-r_p) / (REAL)e_lnpo);
  return f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: humidity
* Desc:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
REAL  humidity (REAL r_p, REAL r_t,  REAL *ar_dhdp)

{
REAL r_h, r_Tk;
  r_Tk = r_t + (REAL)e_Tstd;
  r_h = exp( (REAL)e_Mw * r_p / ( (REAL)e_R * r_Tk));
  *ar_dhdp = (REAL)e_Mw * r_h / ((REAL)e_R * r_Tk);
  return r_h;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: vaporpressure
* Desc:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
REAL   vaporpressure (REAL r_tin)
{
REAL r0,r1,r2,r3, r_t;
  r_t = r_tin * 1000;
  r0 =  ( r_t + 273150);
  r1 = 373150 / r0;
  r_t = 1 - r1;
/*  r_t = 1 - 373.15 / ( r_t + 273.15); */

  r1 = r_t * (13.3016 + r_t * (-2.042 + r_t * (0.26 + r_t * 2.69)));
  r2 = exp (r1);
  r3 = 101325 * r2;
  return r3;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: slope
* Desc:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
REAL   slope  (REAL r_t, REAL r_p)
{
REAL r_dydt,r_Tk, f;
   r_Tk = r_t + (REAL)e_Tstd;
   r_t = 1 - 373.15 / r_Tk;
   r_dydt = 373.15 / sqr(r_Tk);
   f = r_p * r_dydt * (13.3015 + r_t * (-4.082 + r_t * (0.78 + r_t * 10.76) ));
   return f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc: returns latent heat of vaporization in J/kg
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
REAL   Hv  (REAL r_t)
{
 return ( 2.508e6 - 2670 * r_t );
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: Kvap
* Desc: returns vapor conductivity in kg/(m s Pa)
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
REAL  Kvap (REAL r_t, REAL r_p)
{
REAL r_Tk, r_Dv, r_stcor, f, g;
   r_Tk = r_t + (REAL)e_Tstd;
   f = sPOW (r_Tk / (REAL)e_Tstd, 1.75);
   g = ((REAL)e_Po / (REAL)e_Patm);
   r_Dv =  (REAL)e_Dvo * g * f ;
   r_stcor = 1 - r_p / (REAL)e_Patm;
   if ( r_stcor < 0.3 )
     r_stcor = 0.3;
   f = (REAL)e_Mw * r_Dv / ( (REAL)e_R * r_Tk * r_stcor);
   return f;
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: sPOW
* Desc: the pascal code use the lower case 'pow' which was a function
*        function right in the pascal code, not a math lib function
*        so I re-did it here and made it upper case 'POW' so it doesn't
*        confict with the C math libary one.
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
REAL  sPOW (REAL x, REAL y)
{
REAL f,g,h;
  if ( x < 0 )
    x = -x;

  if ( x == 0 )
    f = 0;
  else {
    g = log(x);
    h = y * g;
    f = exp ( h ); }

  return f;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void soiltemp_getwater ( REAL rr_wi[])
{
   Copy_Array (rr_wi,rr_wn);
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void soiltemp_gettemps ( REAL rr_ti[])
{
   Copy_Array (rr_ti,rr_tn);
}


/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc:
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void soiltemp_getdepths ( REAL rr_zi[])
{
 Copy_Array (rr_zi,rr_z);
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name: sqr
* Desc:
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
REAL  sqr  (REAL  r)
{
  return r * r;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc:
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
REAL abs_Real (REAL r)
{
  if ( r < 0 )
    return (r * -1);
  return r;
}

/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc:
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void Copy_Array ( REAL rr_to[], REAL rr_from[])
{
int i;
   for ( i = 0; i <= e_mplus1; i++  )
      rr_to[i] = rr_from[i];
}


#ifdef wow
/*{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}
* Name:
* Desc:
*
{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{*}{**/
void Display_Array ( REAL rr[])
{
int i;
  printf ("------------------------------------------\n");
   for ( i = 0; i <= e_mplus1; i++  )
    printf ("%d - %15.6f \n", i, rr[i]);
  printf ("------------------------------------------\n");
}
#endif
