//
// FIRE-FOFEM $Id$
//

/* #define e_mplus1 14     */

void soiltemp_initconsts (REAL r_bdi, REAL r_pdi, REAL r_lsi, REAL r_gai,
                     REAL r_xwoi, REAL r_copi, REAL r_xoi,
                     REAL rr_Lay[] );


void   soiltemp_initprofile (REAL rr_wi[], REAL rr_ti[]);

REAL  watercontent (REAL r_p, REAL r_xo, REAL *ar_dwdp);
REAL  humidity (REAL r_p, REAL r_t, REAL *ar_dhdp);
int   soiltemp_step (REAL r_Rabs, REAL r_dt,  int  *ai_success, REAL r_time);
REAL  vaporpressure (REAL r_tn);
REAL  tcond (REAL r_t, REAL r_xw, REAL r_xs, REAL r_ls, REAL r_ga,
              REAL r_xwo, REAL r_cop, REAL r_p, REAL r_s, REAL *ar_enh);
REAL  Kvap (REAL r_t, REAL r_p);
REAL  Hv (REAL t);
REAL  POW (REAL x, REAL y);
REAL  sqr  (REAL  r);
REAL  abs_Real (REAL r);
REAL  slope  (REAL r_tn, REAL r_psat);

void soiltemp_getwater ( REAL rr_wi[]);
void soiltemp_gettemps ( REAL rr_ti[]);
void soiltemp_getdepths ( REAL rr_zi[]);


void  Copy_Array ( REAL rr_to[], REAL rr_from[]);
void Display_Array ( REAL rr[]);

void  App_Ext ( char cr_FN[], char  cr_Ext[] );
