      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C  **FMCBA   FIRE-NC-DATE OF LAST REVISION:  03/15/11
C----------
C     SINGLE-STAND VERSION
C     CALLED FROM: FMMAIN

*  Purpose:
*     Find the dominant species (by basal area). Set the initial live
*     and dead fuel values as well. The dead fuels are only initialized
*     in the first year, but COVTYP and the live fuels must be done
*     each year.
*
*  Local variable definitions:
*
*     BAMOST:  The highest basal area in a single species
*     CAREA:   The area covered by the crown at its widest point (sqft)
*     COVINI  The seral cover type to be used for initiating fuels in
*              bare stands
*     CRL:     Crown length
*     CWIDTH:  The maximum width of the crowns (ft)
*     FUINIE:  The initial fuel loadings for established stands (from JBrown)
*     FUINII:  The initial fuel loadings for initiating stands (from JBrown)
*     FULIVE:  The herb/shrub/regen for established stands (from JBrown)
*     FULIVI:  The herb/shrub/regen for initiating stands (from JBrown)
*     ISWTCH:  =1 if called by SVSTART
*              =0 if called by any other subroutine (FMMAIN, FMPPHV)
*     TOTBA:   The total basal area in the stand (used in the fuels calcs)
*     TOTCRA:  The sum of the area of the crowns, per acre (sqft)

C  Common block variables and parameters:

C     Parameter include files.
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C     Common include files.
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'

C     Variable declarations.

C     MAXIMUM NUMBER OF VEGETATION CODES; MUST MATCH THE
C     DIMENSION OF THE VEGETATION CODE ARRAY IN WS **HABTYP**

      INTEGER    MXR5CODE,MXR6CODE
      PARAMETER (MXR5CODE=406)
      PARAMETER (MXR6CODE= 90)

      REAL      BAMOST, TOTCRA, CWIDTH, OPERCOV, X
      INTEGER*2 COVINI5(MXR5CODE),COVINI6(MXR6CODE)
      REAL FULIVE(2,MAXSP), FULIVI(2,MAXSP)
      REAL FUINIE(MXFLCL,MAXSP), FUINII(MXFLCL,MAXSP)
      REAL STFUEL(MXFLCL,2),XCOV(2),YLOAD(2), FOTOVAL(MXFLCL)
      REAL PRMS(12), FOTOVALS(9)
      LOGICAL DEBUG

      INTEGER MYACT(3)

      INTEGER IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC
      INTEGER IXS(8),COVCA(2),ICT(MAXSP)
      REAL    BIGDBH,TOTBA,XX,CAREA,ALGSLP,PRCL,ADD
      REAL    XD(8),YD(8),XS(8),YS(8),XSR(8),YSR(8),DCYMLT
      REAL    COVCAWT(2)

C     XD, YD: BREAKPOINTS FOR DUNNING VALUES (XD) AND DECAY RATE MULTIPLIER (Y)
C     YS    : BREAKPOINTS FOR SITE INDEX VALUES FOR DUNNING VALUES (YS);
C             XS IS RETURNED BY **GETDUNN** (ENTRY POINT DUNN)
C     XSR,YSR: LOWEST-FIRST SORTED XS AND YS

      DATA XD /0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0/
      DATA YD /1.5,1.5,1.0,1.0,1.0,0.5,0.5,0.5/

      DATA YS /0.0,1.0,2.0,3.0,4.0,5.0,6.0,7.0/

C     BREAKPOINTS (% CC) OF INTERPOLATION FUNCTION TO PROVIDE WEIGHTED
C     ESTIMATE OF LIVE (EVERY TIMESTEP) AND DEAD (INITIAL) FUEL

      DATA XCOV / 10.0, 60.0 /

C     INITIAL LIVE FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER

C                  herbs, shrubs
      DATA FULIVE /
     >             0.20, 0.20, !   1 = Port Orford cedar (pc) - use DF
     >             0.20, 0.20, !   2 = incense cedar (ic) - use DF
     >             0.20, 0.20, !   3 = western redcedar (rc)
     >             0.15, 0.10, !   4 = white fir (wf) - use GF
     >             0.15, 0.10, !   5 = California red fir (rf) - use GF
     >             0.15, 0.10, !   6 = Shasta red fir (sh) - use GF
     >             0.20, 0.20, !   7 = Douglas-fir (df)
     >             0.20, 0.20, !   8 = western hemlock (wh)
     >             0.15, 0.20, !   9 = mountain hemlock (mh) - use AF
     >             0.20, 0.10, !  10 = whitebark pine (wb) - use LP
     >             0.20, 0.10, !  11 = knobcone pine (kp) - use LP
     >             0.20, 0.10, !  12 = lodgepole pine (lp)
     >             0.20, 0.10, !  13 = Coulter pine (cp) - use LP
     >             0.20, 0.10, !  14 = limber pine (lm) - use LP
     >             0.20, 0.25, !  15 = Jeffrey pine (jp) - use PP
     >             0.20, 0.25, !  16 = sugar pine (sp) - use PP
     >             0.15, 0.10, !  17 = western white pine (wp)
     >             0.20, 0.25, !  18 = ponderosa pine (pp)
     >             0.20, 0.20, !  19 = Monterey pine (mp) - use DF
     >             0.23, 0.22, !  20 = gray pine (gp) - use BO
     >             0.14, 0.35, !  21 = western juniper (ju) (OTTMAR VOLUME 1)
     >             0.15, 0.20, !  22 = Brewer spruce (br) - use ES
     >             0.20, 0.20, !  23 = giant sequoia (gs) - use DF
     >             0.20, 0.20, !  24 = pacific yew (py) - use DF
     >             0.20, 0.25, !  25 = other softwoods (os) - use PP
     >             0.23, 0.22, !  26 = coast live oak (lo) - use BO
     >             0.25, 0.25, !  27 = canyon live oak (cy) - use TO/GC
     >             0.23, 0.22, !  28 = blue oak (bl) - use BO
     >             0.23, 0.22, !  29 = Engelmann oak (eo) - use BO
     >             0.23, 0.22, !  30 = Oregon white oak (wo) - use BO
     >             0.23, 0.22, !  31 = California black oak (bo)
     >             0.23, 0.22, !  32 = valley white oak (vo) - use BO
     >             0.23, 0.22, !  33 = interior live oak (io) - use BO
     >             0.20, 0.20, !  34 = bigleaf maple (bm) - use DF
     >             0.20, 0.20, !  35 = California buckeye (bu) - use DF
     >             0.20, 0.20, !  36 = red alder (ra) - use DF
     >             0.20, 0.20, !  37 = pacific madrone (ma) - use DF
     >             0.25, 0.25, !  38 = golden chinkapin (gc) - use AS
     >             0.20, 0.20, !  39 = pacific dogwood (dg) - use DF
     >             0.20, 0.25, !  40 = oregon ash (fl) - use PP
     >             0.20, 0.20, !  41 = walnut (wn) - use DF
     >             0.25, 0.25, !  42 = tanoak (to) - use AS
     >             0.25, 0.25, !  43 = California sycamore (sy) - use DF
     >             0.25, 0.25, !  44 = quaking aspen (as)
     >             0.25, 0.25, !  45 = black cottonwood (cw) - use AS
     >             0.25, 0.25, !  46 = willow (wi) ! Salix spp.- use AS
     >             0.25, 0.25, !  47 = California nutmeg (cn) - use DF
     >             0.25, 0.25, !  48 = California laurel (cl) - use DF
     >             0.23, 0.22/ !  49 = other hardwoods (oh) - use BO


C     INITIAL LIVE FUEL LOADING FOR 'INTIALIZING STANDS WITH 10% COVER

C                  herbs, shrubs
      DATA FULIVI /0.40, 2.00, !   1 = Port Orford cedar (pc) - use DF
     >             0.40, 2.00, !   2 = incense cedar (ic) - use DF
     >             0.40, 2.00, !   3 = western redcedar (rc)
     >             0.30, 2.00, !   4 = white fir (wf) - use GF
     >             0.30, 2.00, !   5 = California red fir (rf) - use GF
     >             0.30, 2.00, !   6 = Shasta red fir (sh) - use GF
     >             0.40, 2.00, !   7 = Douglas-fir (df)
     >             0.40, 2.00, !   8 = western hemlock (wh)
     >             0.30, 2.00, !   9 = mountain hemlock (mh) - use AF
     >             0.40, 1.00, !  10 = whitebark pine (wb) - use LP
     >             0.40, 1.00, !  11 = knobcone pine (kp) - use LP
     >             0.40, 1.00, !  12 = lodgepole pine (lp)
     >             0.40, 1.00, !  13 = Coulter pine (cp) - use LP
     >             0.40, 1.00, !  14 = limber pine (lm) - use LP
     >             0.25, 1.00, !  15 = Jeffrey pine (jp) - use PP
     >             0.25, 1.00, !  16 = sugar pine (sp) - use PP
     >             0.30, 2.00, !  17 = western white pine (wp)
     >             0.25, 1.00, !  18 = ponderosa pine (pp)
     >             0.40, 2.00, !  19 = Monterey pine (mp) - use DF
     >             0.55, 0.35, !  20 = gray pine (gp) - use BO
     >             0.10, 2.06, !  21 = western juniper (ju) (OTTMAR VOLUME 1)
     >             0.30, 2.00, !  22 = Brewer spruce (br) - use ES
     >             0.40, 2.00, !  23 = giant sequoia (gs) - use DF
     >             0.40, 2.00, !  24 = pacific yew (py) - use DF
     >             0.25, 1.00, !  25 = other softwoods (os) - use PP
     >             0.55, 0.35, !  26 = coast live oak (lo) - use BO
     >             0.18, 2.00, !  27 = canyon live oak (cy) - use AS
     >             0.55, 0.35, !  28 = blue oak (bl) - use BO
     >             0.55, 0.35, !  29 = Engelmann oak (eo) - BO
     >             0.55, 0.35, !  30 = Oregon white oak (wo) - use BO
     >             0.55, 0.35, !  31 = California black oak (bo)
     >             0.55, 0.35, !  32 = valley white oak (vo) - use BO
     >             0.55, 0.35, !  33 = interior live oak (io) - use BO
     >             0.40, 2.00, !  34 = bigleaf maple (bm) - use DF
     >             0.40, 2.00, !  35 = California buckeye (bu) - use DF
     >             0.40, 2.00, !  36 = red alder (ra) - use DF
     >             0.40, 2.00, !  37 = pacific madrone (ma) - use DF
     >             0.18, 2.00, !  38 = golden chinkapin (gc) - use AS
     >             0.40, 2.00, !  39 = pacific dogwood (dg) - use DF
     >             0.25, 1.00, !  40 = oregon ash (fl) - use PP
     >             0.40, 2.00, !  41 = walnut (wn) - use DF
     >             0.18, 2.00, !  42 = tanoak - use AS
     >             0.40, 2.00, !  43 = California sycamore (sy) - use DF
     >             0.18, 2.00, !  44 = quaking aspen (as)
     >             0.18, 2.00, !  45 = black cottonwood (cw) - use AS
     >             0.18, 2.00, !  46 = willow (wi) ! Salix spp.- use AS
     >             0.40, 2.00, !  47 = California nutmeg (cn) - use DF
     >             0.40, 2.00, !  48 = California laurel (cl) - use DF
     >             0.55, 0.35/ !  49 = other hardwood - use BO

C     INITIAL FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD

C     <.25 to1  1-3   3-6 6-12 12-20 20-35 35-50 >50  Lit  Duf
      DATA FUINIE /
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, !  1 = Port Orford cedar (pc) - use DF
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, !  2 = incense cedar (ic) - use DF
     &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0, 0.0, 0.0, 1.0,35.0, !  3 = western redcedar (rc)
     &0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0, 0.0, 0.0, 0.6,25.0, !  4 = white fir (wf) - use GF
     &0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0, 0.0, 0.0, 0.6,25.0, !  5 = California red fir (rf) - use GF
     &0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0, 0.0, 0.0, 0.6,25.0, !  6 = Shasta red fir (sh) - use GF
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, !  7 = Douglas-fir (df)
     &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0, 0.0, 0.0, 1.0,35.0, !  8 = western hemlock (wh)
     &1.1, 1.1, 2.2,10.0,10.0, 0.0, 0.0, 0.0, 0.0, 0.6,30.0, !  9 = mountain hemlock (mh) - use AF
     &0.7, 0.7, 1.6, 2.5, 2.5, 0.0, 0.0, 0.0, 0.0, 1.4, 5.0, ! 10 = whitebark pine (wb) - use LP
     &0.7, 0.7, 1.6, 2.5, 2.5, 0.0, 0.0, 0.0, 0.0, 1.4, 5.0, ! 11 = knobcone pine (kp) - use LP
     &0.7, 0.7, 1.6, 2.5, 2.5, 0.0, 0.0, 0.0, 0.0, 1.4, 5.0, ! 12 = lodgepole pine (lp)
     &0.7, 0.7, 1.6, 2.5, 2.5, 0.0, 0.0, 0.0, 0.0, 1.4, 5.0, ! 13 = Coulter pine (cp) - use LP
     &0.7, 0.7, 1.6, 2.5, 2.5, 0.0, 0.0, 0.0, 0.0, 1.4, 5.0, ! 14 = limber pine (lm) - use LP
     &0.7, 0.7, 1.6, 2.5, 2.5, 0.0, 0.0, 0.0, 0.0, 1.4, 5.0, ! 15 = Jeffrey pine (jp) - use LP
     &0.7, 0.7, 1.6, 2.5, 2.5, 0.0, 0.0, 0.0, 0.0, 1.4, 5.0, ! 16 = sugar pine (sp) - use LP
     &1.0, 1.0, 1.6,10.0,10.0,10.0, 0.0, 0.0, 0.0, 0.8,30.0, ! 17 = western white pine (wp)
     &0.9, 0.9, 1.2, 7.0, 8.0, 0.0, 0.0, 0.0, 0.0, 0.6,15.0, ! 18 = ponderosa pine (pp)
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, ! 19 = Monterey pine (mp) - use DF
     &0.3, 0.7, 1.4, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 3.9, 0.0, ! 20 = gray pine (gp) - use BO
     &0.1, 0.2, 0.4, 0.5, 0.8, 1.0, 0.0, 0.0, 0.0, 0.1, 0.0, ! 21 = western juniper (ju) - ottmar volume 1
     &1.1, 1.1, 2.2,10.0,10.0, 0.0, 0.0, 0.0, 0.0, 0.6,30.0, ! 22 = Brewer spruce (br) - use ES
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, ! 23 = giant sequoia (gs) - use DF
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, ! 24 = pacific yew (py) - use DF
     &0.9, 0.9, 1.2, 7.0, 8.0, 0.0, 0.0, 0.0, 0.0, 0.6,15.0, ! 25 = other softwoods (os) - use PP
     &0.3, 0.7, 1.4, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 3.9, 0.0, ! 26 = coast live oak (lo) - use BO
     &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0, 0.0, 0.0, 1.4,16.8, ! 27 = canyon live oak (cy) - use AS
     &0.3, 0.7, 1.4, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 3.9, 0.0, ! 28 = blue oak (bl) - use BO
     &0.3, 0.7, 1.4, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 3.9, 0.0, ! 29 = Engelmann oak (eo) - use BO
     &0.3, 0.7, 1.4, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 3.9, 0.0, ! 30 = Oregon white oak (wo) - use BO
     &0.3, 0.7, 1.4, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 3.9, 0.0, ! 31 = California black oak (bo)
     &0.3, 0.7, 1.4, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 3.9, 0.0, ! 32 = valley white oak (vo) - use BO
     &0.3, 0.7, 1.4, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 3.9, 0.0, ! 33 = interior live oak (io)- use BO
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, ! 34 = bigleaf maple (bm) - use DF
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, ! 35 = California buckeye (bu) - use DF
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, ! 36 = red alder (ra) - use DF
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, ! 37 = pacific madrone (ma) - use DF
     &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0, 0.0, 0.0, 1.4,16.8, ! 38 = golden chinkapin (gc) - use AS
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, ! 39 = pacific dogwood (dg) - use DF
     &0.9, 0.9, 1.2, 7.0, 8.0, 0.0, 0.0, 0.0, 0.0, 0.6,15.0, ! 40 = oregon ash (fl) - use PP
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, ! 41 = walnut (wn) - use DF
     &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0, 0.0, 0.0, 1.4,16.8, ! 42 = tanoak - use AS
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, ! 43 = California sycamore (sy) - use DF
     &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0, 0.0, 0.0, 1.4,16.8, ! 44 = quaking AS (as)
     &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0, 0.0, 0.0, 1.4,16.8, ! 45 = black cottonwood (cw) - use AS
     &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0, 0.0, 0.0, 1.4,16.8, ! 46 = willow (wi) ! Salix spp.- use AS
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, ! 47 = California nutmeg (cn) - use DF
     &0.9, 0.9, 1.6, 3.5, 3.5, 0.0, 0.0, 0.0, 0.0, 0.6,10.0, ! 48 = California laurel (cl) - use DF
     &0.3, 0.7, 1.4, 0.2, 0.1, 0.0, 0.0, 0.0, 0.0, 3.9, 0.0/ ! 49 = other hardwood - use BO

C     INITIAL FUEL LOADING FOR 'INITIALIZING' STANDS WITH 10% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD

c LP/PP reversed compared to WS
c
C     <.25 to1  1-3   3-6 6-12 12-20 20-35 35-50 >50  Lit  Duf
      DATA FUINII /
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, !  1 = Port Orford cedar (pc) - use DF
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, !  2 = incense cedar (ic) - use DF
     &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0, 0.0, 0.0, 0.5,12.0, !  3 = western redcedar (rc)
     &0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0, 0.0, 0.0, 0.3,12.0, !  4 = white fir (wf) - use GF
     &0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0, 0.0, 0.0, 0.3,12.0, !  5 = California red fir (rf) - use GF
     &0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0, 0.0, 0.0, 0.3,12.0, !  6 = Shasta red fir (sh) - use GF
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, !  7 = Douglas-fir (df)
     &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0, 0.0, 0.0, 0.5,12.0, !  8 = western hemlock (wh)
     &0.7, 0.7, 1.6, 4.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.3,12.0, !  9 = mountain hemlock (mh) - use AF
     &0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.5, 0.8, ! 10 = whitebark pine (wb) - use LP
     &0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.5, 0.8, ! 11 = knobcone pine (kp) - use LP
     &0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.5, 0.8, ! 12 = lodgepole pine (lp)
     &0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.5, 0.8, ! 13 = Coulter pine (cp) - use LP
     &0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.5, 0.8, ! 14 = limber pine (lm) - use LP
     &0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.5, 0.8, ! 15 = Jeffrey pine (jp) - use PP
     &0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0, 0.0, 0.0, 0.5, 0.8, ! 16 = sugar pine (sp) - use LP
     &0.6, 0.6, 0.8, 6.0, 6.0, 6.0, 0.0, 0.0, 0.0, 0.4,12.0, ! 17 = western white pine (wp)
     &0.6, 0.7, 0.8, 2.8, 3.2, 0.0, 0.0, 0.0, 0.0, 0.3, 7.0, ! 18 = ponderosa pine (pp)
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, ! 19 = Monterey pine (mp) - use DF
     &0.1, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.9, 0.0, ! 20 = gray pine (gp) - use BO
     &0.2, 0.4, 0.2, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.2, 0.0, ! 21 = western juniper (ju) - Ottmar Volume I
     &0.7, 0.7, 1.6, 4.0, 4.0, 0.0, 0.0, 0.0, 0.0, 0.3,12.0, ! 22 = Brewer spruce (br) - use ES
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, ! 23 = giant sequoia (gs) - use DF
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, ! 24 = pacific yew (py) - use DF
     &0.6, 0.7, 0.8, 2.8, 3.2, 0.0, 0.0, 0.0, 0.0, 0.3, 7.0, ! 25 = other softwoods (os) - use PP
     &0.1, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.9, 0.0, ! 26 = coast live oak (lo) - use BO
     &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0, 0.0, 0.0, 0.8, 5.6, ! 27 = canyon live oak (cy) - use AS
     &0.1, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.9, 0.0, ! 28 = blue oak (bl) - use BO
     &0.1, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.9, 0.0, ! 29 = Engelmann oak (eo) - use BO
     &0.1, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.9, 0.0, ! 30 = Oregon white oak (wo) - use BO
     &0.1, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.9, 0.0, ! 31 = California black oak (bo)
     &0.1, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.9, 0.0, ! 32 = valley white oak (vo) - use BO
     &0.1, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.9, 0.0, ! 33 = interior live oak (io)- use BO
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, ! 34 = bigleaf maple (bm) - use DF
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, ! 35 = California buckeye (bu) - use DF
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, ! 36 = red alder (ra) - use DF
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, ! 37 = pacific madrone (ma) - use DF
     &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0, 0.0, 0.0, 0.8, 5.6, ! 38 = golden chinkapin (gc) - use AS
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, ! 39 = pacific dogwood (dg) - use DF
     &0.6, 0.7, 0.8, 2.8, 3.2, 0.0, 0.0, 0.0, 0.0, 0.3, 7.0, ! 40 = oregon ash (fl) - use PP
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, ! 41 = walnut (wn) - use DF
     &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0, 0.0, 0.0, 0.8, 5.6, ! 42 = tanoak - use AS
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, ! 43 = California sycamore (sy) - use DF
     &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0, 0.0, 0.0, 0.8, 5.6, ! 44 = quaking aspen (as)
     &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0, 0.0, 0.0, 0.8, 5.6, ! 45 = black cottonwood (cw) - use AS
     &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0, 0.0, 0.0, 0.8, 5.6, ! 46 = willow (wi) ! Salix spp.- use AS
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, ! 47 = California nutmeg (cn) - use DF
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0, 0.0, 0.0, 0.3, 5.0, ! 48 = California laurel (cl) - use DF
     &0.1, 0.1, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 2.9, 0.0/ ! 49 = other hardwood - use BO

C     DOMINANT SPECIES FOR EACH (OPTIONAL) VEGETATION CODE.
C     DERIVED FROM R5 VEG CODES AND CA **HABTYP**

      DATA (COVINI5(I), I=   1,  50) /
     & 25, 25, 25, 25, 12, 44, 44, 44, 44, 44,
     & 44, 44, 45, 45, 45, 45, 45, 45, 36, 49,
     & 49, 49, 46, 46, 46, 46, 46, 46, 46, 46,
     & 46, 46, 46, 46, 46, 46, 46, 46, 46, 46,
     & 46, 46, 46, 46, 46, 46, 46, 46, 46, 46/
      DATA (COVINI5(I), I=  51, 100) /
     & 46, 39, 39,  0,  0,  0,  0,  0,  0,  0,
     &  0,  0,  0,  0,  0,  0,  0,  0,  0,  1,
     &  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
     &  1,  1,  1,  7,  7,  7,  7,  7,  7,  7,
     &  7,  7,  7,  7,  7,  7,  7,  7,  7,  7/
      DATA (COVINI5(I), I= 101, 150) /
     &  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
     &  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
     &  7,  7,  7,  7, 15, 15, 15, 15, 15, 15,
     & 15, 15, 15, 15, 15, 15, 12, 12, 12, 12,
     & 12, 12, 16, 16, 16, 16, 16, 16, 16, 16/
      DATA (COVINI5(I), I= 151, 200) /
     & 17, 17, 17, 17, 17, 17, 17, 17, 17, 42,
     & 42, 42, 42, 42, 42, 42, 42, 42, 42, 42,
     & 42, 42, 42, 42, 42, 42, 42, 42, 42, 42,
     & 42, 42, 42, 42, 42, 42, 42, 42, 42, 42,
     & 42, 42, 42, 42, 42, 42, 42, 42, 42, 42/
      DATA (COVINI5(I), I= 201, 250) /
     & 42, 42, 42, 42, 42, 42, 42, 42, 42, 42,
     &  2,  4,  4, 18,  4, 15, 15, 15, 15, 18,
     & 18, 18, 18, 18, 18, 18,  4, 15, 18, 18,
     &  4, 15,  4,  4,  4,  4, 18, 18,  4,  4,
     &  4,  4, 18, 18, 18, 18, 18, 18, 18, 18/
      DATA (COVINI5(I), I= 251, 300) /
     & 18, 18,  4,  4, 18,  7,  7,  7,  7,  7,
     & 18, 16,  4,  4,  4,  4,  4,  4,  4,  7,
     &  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
     & 15, 15, 15, 15, 15,  4,  4,  4, 25, 25,
     & 18,  4, 18,  4,  7,  4,  7, 39,  7,  7/
      DATA (COVINI5(I), I= 301, 350) /
     &  7, 27, 18, 18,  7, 42,  7,  7,  7,  7,
     & 25, 25,  7,  0,  4,  4, 18, 18,  7,  7,
     &  4,  4,  0, 18, 18,  4, 25,  7,  5,  4,
     & 21, 21,  9, 15, 15, 15, 44, 44,  5,  5,
     &  5, 17, 17, 17, 12, 12, 12,  5,  5,  5/
      DATA (COVINI5(I), I= 351, 400) /
     &  4,  4,  7, 23, 23, 23, 23, 23, 23, 23,
     &  4, 28, 28, 28, 28, 28, 28, 28, 28, 28,
     & 28, 28, 28, 28, 28, 28, 28,  0,  0,  0,
     &  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &  0, 27, 33, 33, 33, 33,  0,  0, 49,  0/
      DATA (COVINI5(I), I= 401, 406) /
     &  0,  0,  0,  0,  0,  0/

C     DOMINANT SPECIES FOR EACH (OPTIONAL) PLANT ASSOCIATION.
C     DERIVED FROM R6 VEG CODES AND CA **HABTYP**

      DATA (COVINI6(I), I=   1,  50) /
     &  7,  7,  7,  7,  7,  7,  7,  7,  7,  7,
     &  7,  7,  7,  7,  7,  7,  7,  7,  7,  8,
     &  8,  0,  8,  8,  8,  8,  8,  9, 18, 15,
     & 18, 15, 15, 15, 17,  6,  6,  6,  6,  1,
     &  1,  1,  1,  1,  1,  4,  4,  4,  4,  4/
      DATA (COVINI6(I), I=  51,  90) /
     &  4,  4,  4,  4,  4,  4,  4,  4,  4,  4,
     &  4,  0,  4,  4,  0,  0,  0,  0,  4, 42,
     & 42, 42, 42, 42, 42, 42, 42, 42, 42, 42,
     & 42, 42, 42, 42, 42, 42, 42, 42, 42, 42/

      DATA MYACT / 2521, 2548, 2553 /

C     CHECK FOR DEBUG.

      CALL DBCHK (DEBUG,'FMCBA',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
    7 FORMAT(' ENTERING FMCBA CYCLE = ',I2)

C     BEGIN ROUTINE. ZERO OUT THE CUMMULATIVE VARIABLES

      COVTYP = 0

C     CA VARIANTS USE TOP 2 SPP FOR LIVE FUELS AND INITIAL
C     CWD INTERPOLATION; %CC CHANGE ALSO REQUIRES SAVING
C     OLD PERCOV

      DO I = 1,2
        COVCA(I)   = 0
        COVCAWT(I) = 0.0
      ENDDO
      OPERCOV = PERCOV

      PERCOV  = 0.0
      BIGDBH  = 0.0
      TOTBA   = 0.0

C     LOOP THROUGH THE TREE LIST

      IF (ITRN .GT. 0) THEN

C       ZERO OUT THE CUMMULATIVE VARIABLES

        BAMOST = 0.0
        TOTCRA = 0.0

        DO KSP = 1,MAXSP
          FMTBA(KSP) = 0.0
        ENDDO

        DO I = 1,ITRN
          IF (FMPROB(I) .GT. 0.0) THEN

            KSP = ISP(I)

            FMTBA(KSP) = FMTBA(KSP) +
     &                   FMPROB(I) * DBH(I) * DBH(I) * 0.0054542

            IF (DBH(I) .GT. BIGDBH) BIGDBH = DBH(I)

C           CALCULATE THE CROWN WIDTH OF THE TREE AND TOTAL THE AREA
C           ENCOMPASSED BY ALL TREES

            CWIDTH=CRWDTH(I)

            CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
            TOTCRA = TOTCRA + CAREA*FMPROB(I)
          ENDIF

C         USE THIS LOOP TO ZERO THIS VARIABLE, FOR LACK OF A BETTER PLACE.

          CURKIL(I) = 0.0

        ENDDO

C       CALCULATE TOTAL COVER TYPE

        DO KSP = 1,MAXSP
          TOTBA = TOTBA + FMTBA(KSP)
        ENDDO

C       DETERMINE WHICH SPECIES HAS THE MOST BASAL AREA
C       COVTYP    - FFE-WIDE DOMINANT
C       COVCA(2)  - CA-FFE VARIANTS ONLY: PRIMARY & SECONDARY BA

        CALL RDPSRT(MAXSP,FMTBA,ICT,.TRUE.)
        IF (FMTBA(ICT(1)) .GT. 0.001) COVTYP = ICT(1)

        XX = 0.0
        DO I = 1,2
          IF (FMTBA(ICT(I)) .GT. 0.0) XX = XX + FMTBA(ICT(I))
        ENDDO
        IF (XX .GT. 0.001) THEN
          DO I = 1,2
            COVCA(I)   = ICT(I)
            COVCAWT(I) = FMTBA(ICT(I))/XX
          ENDDO
        ENDIF

C       Use the crown width information to determine the percent cover
C       of the stand. Use the equation sent by Nick which assumes that
C       crowns are randomly distrubuted in the stand:

C       PerCov = 100*(1-exp(-total crown areas per acre / sqft in an acre))

        PERCOV = 100. * (1.0 - EXP(-TOTCRA/43560.))

      ENDIF

C     COMPUTE % CHANGE (DROP) IN %CC FOR SHRUB MODEL **FMSHRUB**

      X = OPERCOV
      IF (X .GT. 1.0) THEN
        CCCHNG = -100.0 * (PERCOV - X) / X
      ELSE
        CCCHNG = 0.0
      ENDIF

C     IF THERE ARE NO TREES (COVTYP=0) IN CYCLE 1,
C     USE THE OPTIONAL ITYPE IF IT IS VALID. OTHERWISE
C     ISSUE A WARNING AND USE A DEFAULT LP COVER. AFTER THE
C     FIRST CYCLE, USE THE PREVIOUS COVER TYPE IF NO COVER IS
C     PRESENT.

      IF (COVTYP .EQ. 0) THEN
        IF (IYR .EQ. IY(1)) THEN
          IF (IFOR .GE. 6) THEN
            IF (ITYPE .GE. 1 .AND. ITYPE .LE. MXR6CODE)
     &        COVTYP = COVINI6(ITYPE)  ! R6 HABITAT TYPE
          ELSE
            IF (ITYPE .GE. 1 .AND. ITYPE .LE. MXR5CODE)
     &        COVTYP = COVINI5(ITYPE)  ! R5 PLANT ASSOCIATION
          ENDIF

          IF (COVTYP .EQ. 0) THEN
             WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO INITIAL ',
     &       'BASAL AREA AND NO VALID HABITAT INFORMATION:',
     &       /1X,'*** COVER TYPE SET TO DOUGLAS-FIR',/1X)")
            CALL RCDSET (2,.TRUE.)
            COVTYP = 7
          ENDIF
        ELSE
          COVTYP = OLDCOVTYP
        ENDIF
      COVCA(1)   = COVTYP
      COVCAWT(1) = 1.0
      ENDIF

      OLDCOVTYP = COVTYP

C     LOAD LIVE FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
C     STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.
C       IN WS,NC,CA VARIANTS, THE TOP 2 SPECIES ARE USED TO INTIALIZE THE POOLS

      DO I = 1,2  ! herbs, shrub loop
        FLIVE(I) = 0.0
        DO J = 1,2  ! primary secondary cover types loop
          IF (COVCA(J) .GT. 0) THEN
            YLOAD(1) = FULIVI(I,COVCA(J)) * COVCAWT(J)
            YLOAD(2) = FULIVE(I,COVCA(J)) * COVCAWT(J)
            FLIVE(I) = FLIVE(I) + ALGSLP(PERCOV,XCOV,YLOAD,2)
          ENDIF
        ENDDO
      ENDDO

      IF (DEBUG) WRITE(JOSTND,8) COVTYP,PERCOV,FLIVE(1),FLIVE(2)
    8 FORMAT(' IN FMCBA, COVTYP=',I3,' PERCOV=',F6.2,' FLIVE(1&2)=',
     >    2F6.3)

C     INITIALIZE THE DEAD FUELS ONLY FOR THE FIRST YEAR OF THE SIMULATION

      IF (IYR .EQ. IY(1)) THEN

C     ADJUST DECAY RATES (FIRST YEAR ONLY) USING  DUNNING CODE/SITE
C     INDEX CORRESPONDENCE. DUNNING-BASED SI VECTOR IS RE-SORTED SO
C     THAT XSR IS INCREASING (REQ'D BY ALGSLP); ASSUMES DUNNING
C     CODES 0-7 ONLY. DEPENDS ON **DUNN**

C       ONLY DO THIS IF DURING THE NORMAL CALL, NOT FROM SVSTART

        IF ( ISWTCH .NE. 1 ) THEN

          CALL GETDUNN(XS)
          CALL RDPSRT(8,XS,IXS,.TRUE.)
          J = 8
          DO I= 1,8
            XSR(I) = XS(IXS(J))
            YSR(I) = YS(IXS(J))
            J=J-1
          ENDDO
          
          DCYMLT = ALGSLP(ALGSLP(SITEAR(ISISP),XSR,YSR,8),XD,YD,8)
          
          DO I = 1,MXFLCL
            DO J = 1,4
              DKR(I,J) = DKR(I,J) * DCYMLT
            ENDDO
          ENDDO
        ENDIF

C       LOAD DEAD FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
C       STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.
C       IN WS,NC,CA VARIANTS, THE TOP 2 SPECIES ARE USED TO INTIALIZE THE POOLS

        DO ISZ = 1,MXFLCL  ! CWD size category loop
          STFUEL(ISZ,1) = 0.0
          STFUEL(ISZ,2) = 0.0
          DO J = 1,2  ! primary secondary cover types loop
            IF (COVCA(J) .GT. 0) THEN
              YLOAD(1)=FUINII(ISZ,COVCA(J)) * COVCAWT(J)
              YLOAD(2)=FUINIE(ISZ,COVCA(J)) * COVCAWT(J)
              STFUEL(ISZ,2) = STFUEL(ISZ,2)+ALGSLP(PERCOV,XCOV,YLOAD,2)
            ENDIF
          ENDDO
        ENDDO
        
C       CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT

        CALL OPFIND(1,MYACT(2),J)
        IF (J .GT. 0) THEN
          CALL OPGET(J,2,JYR,IACTK,NPRM,PRMS)
          IF ((PRMS(1) .GE. 0) .AND. (PRMS(2) .GE. 0)) THEN
            CALL FMPHOTOVAL(NINT(PRMS(1)), NINT(PRMS(2)), FOTOVAL, 
     >                      FOTOVALS)
            DO I = 1, MXFLCL
              IF (FOTOVAL(I) .GE. 0) STFUEL(I,2) = FOTOVAL(I)
              IF (I .LE. 9) STFUEL(I,1) = FOTOVALS(I)
            ENDDO                 

C           IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
C           DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C           NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

            IF (FOTOVAL(1).GE.0 .AND. ISWTCH.NE.1) CALL OPDONE(J,IYR)

          ELSE
            WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',
     &      'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ',
     &      'REQUIRED.',/1X)")
            CALL RCDSET (2,.TRUE.)
          ENDIF
        ENDIF
        
C       CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
C       FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT

        CALL OPFIND(1,MYACT(1),J)
        IF (J .GT. 0) THEN
          CALL OPGET(J,12,JYR,IACTK,NPRM,PRMS)
          IF (PRMS(2) .GE. 0) STFUEL(3,2) = PRMS(2)
          IF (PRMS(3) .GE. 0) STFUEL(4,2) = PRMS(3)
          IF (PRMS(4) .GE. 0) STFUEL(5,2) = PRMS(4)
          IF (PRMS(5) .GE. 0) STFUEL(6,2) = PRMS(5)
          IF (PRMS(6) .GE. 0) STFUEL(10,2) = PRMS(6)
          IF (PRMS(7) .GE. 0) STFUEL(11,2) = PRMS(7)
          IF (PRMS(8) .GE. 0) STFUEL(1,2) = PRMS(8)          
          IF (PRMS(9) .GE. 0) STFUEL(2,2) = PRMS(9)           
          IF (PRMS(1) .GE. 0) THEN
            IF ((PRMS(8) .LT. 0) .AND. (PRMS(9) .LT. 0)) THEN
              STFUEL(1,2) = PRMS(1) * 0.5
              STFUEL(2,2) = PRMS(1) * 0.5
            ENDIF                 
            IF ((PRMS(8) .LT. 0) .AND. (PRMS(9) .GE. 0)) THEN
              STFUEL(1,2) = MAX(PRMS(1) - PRMS(9),0.)
            ENDIF  
            IF ((PRMS(8) .GE. 0) .AND. (PRMS(9) .LT. 0)) THEN
              STFUEL(2,2) = MAX(PRMS(1) - PRMS(8),0.)
            ENDIF  
          ENDIF                
          IF (PRMS(10) .GE. 0) STFUEL(7,2) = PRMS(10) 
          IF (PRMS(11) .GE. 0) STFUEL(8,2) = PRMS(11) 
          IF (PRMS(12) .GE. 0) STFUEL(9,2) = PRMS(12)   

C         DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C         NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

          IF ( ISWTCH .NE. 1 ) CALL OPDONE(J,IYR)

        ENDIF

        CALL OPFIND(1,MYACT(3),J)
        IF (J .GT. 0) THEN
          CALL OPGET(J,9,JYR,IACTK,NPRM,PRMS)
          IF (PRMS(1) .GE. 0) STFUEL(1,1) = PRMS(1)
          IF (PRMS(2) .GE. 0) STFUEL(2,1) = PRMS(2)
          IF (PRMS(3) .GE. 0) STFUEL(3,1) = PRMS(3)
          IF (PRMS(4) .GE. 0) STFUEL(4,1) = PRMS(4)
          IF (PRMS(5) .GE. 0) STFUEL(5,1) = PRMS(5)
          IF (PRMS(6) .GE. 0) STFUEL(6,1) = PRMS(6)
          IF (PRMS(7) .GE. 0) STFUEL(7,1) = PRMS(7)          
          IF (PRMS(8) .GE. 0) STFUEL(8,1) = PRMS(8)                           
          IF (PRMS(9) .GE. 0) STFUEL(9,1) = PRMS(9)         

C         DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C         NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.

          IF ( ISWTCH .NE. 1 ) CALL OPDONE(J,IYR)

        ENDIF


C       DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
C       OF BASAL AREA IN THE STAND. ASSUME THE FUELS ARE UNPILED AND HARD.
C       THIS LAST ASSUMPTION MAY CHANGE IN THE FUTURE.

        DO ISZ = 1,MXFLCL
          IF (TOTBA .GT. 0.0) THEN
            DO KSP = 1,MAXSP
              IF (FMTBA(KSP) .GT. 0.0) THEN
                DO J = 1,2
                  PRCL = FMTBA(KSP) / TOTBA
                  IDC = DKRCLS(KSP)
                  ADD = PRCL * STFUEL(ISZ,J)
                  CWD(1,ISZ,J,IDC) = CWD(1,ISZ,J,IDC) + ADD
                ENDDO
              ENDIF
            ENDDO
          ELSE
            IDC = DKRCLS(COVTYP)
            DO J = 1,2
              CWD(1,ISZ,J,IDC) = CWD(1,ISZ,J,IDC) + STFUEL(ISZ,J)
            ENDDO
          ENDIF
        ENDDO

      ENDIF

      ENTRY SNGCOE

C     ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
C     IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.

      RETURN
      END
