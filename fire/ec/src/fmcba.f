      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C  **FMCBA   FIRE-EC-DATE OF LAST REVISION: 05/12/12
C----------
C     SINGLE-STAND VERSION
C     CALLED FROM: FMMAIN
C
*  Purpose:
*     Find the dominant species (by basal area). Set the initial live
*     and dead fuel values as well. The dead fuels are only initialized
*     in the first year, but COVTYP and the live fuels must be done
*     each year.
*----------------------------------------------------------------------
*
*  Local variable definitions:
*     BAMOST:  The highest basal area in a single species
*     CAREA:   The area covered by the crown at its widest point (sqft)
*     COVINI:  The seral cover type to be used for initiating fuels in
*              bare stands
*     MAPDRY:  The moist/dry habitat type, used for altering decay rate
*              and selecting fuel model
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
*
*  Common block variables and parameters:
*
***********************************************************************
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
Cppe      INCLUDE 'PPEPRM.F77'
C
C
      INCLUDE 'FMPARM.F77'
C
C
Cppe      INCLUDE 'PPCNTL.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
COMMONS
C----------
C.... Variable declarations.
C
C     MAXIMUM NUMBER OF VEGETATION CODES; MUST MATCH THE
C     DIMENSION OF THE VEGETATION CODE ARRAY IN EC **HABTYP**
C----------
      INTEGER MXVCODE      
      PARAMETER (MXVCODE = 155)
      LOGICAL DEBUG
      INTEGER MYACT(3)
      INTEGER IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC,IMD
      INTEGER*2 COVINI(MXVCODE), MAPDRY(MXVCODE)
      REAL BAMOST, TOTCRA, CWIDTH
      REAL FULIVE(2,MAXSP), FULIVI(2,MAXSP)
      REAL FUINIE(MXFLCL,MAXSP), FUINII(MXFLCL,MAXSP)
      REAL STFUEL(MXFLCL,2),XCOV(2),YLOAD(2), FOTOVAL(MXFLCL)
      REAL PRMS(12), FOTOVALS(9)
      REAL    BIGDBH,TOTBA,XX,CAREA,ALGSLP,PRCL,ADD
      REAL    DCYMLT
C
C----------
C     INITIAL LIVE FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C----------
C                  herbs, shrubs
      DATA FULIVE /0.15, 0.10, ! 1 white pine NI
     &             0.20, 0.20, ! 2 western larch NI
     &             0.20, 0.20, ! 3 Douglas-fir NI
     &             0.15, 0.10, ! 4 pacific silver - use grand fir
     &             0.20, 0.20, ! 5 western redcedar NI
     &             0.15, 0.10, ! 6 grand fir NI
     &             0.20, 0.10, ! 7 lodgepole pine NI
     &             0.15, 0.20, ! 8 Engelmann spruce NI
     &             0.15, 0.20, ! 9 subalpine fir NI
     &             0.20, 0.25, !10 ponderosa pine NI
     &             0.20, 0.20, !11 western hemlock from WC
     &             0.15, 0.20, !12 mountain hemlock from other softwoods NI
     &             0.20, 0.20, !13 Pacific yew from WC - df
     &             0.20, 0.10, !14 whitebark pine from WC - lp
     &             0.15, 0.10, !15 noble fir from WC - gf
     &             0.15, 0.10, !16 white fir - use gf
     &             0.20, 0.20, !17 subalpine larch from WC - wl
     &             0.20, 0.20, !18 Alaska cedar from WC - rc
     &             0.14, 0.35, !19 western juniper from WC - (Ottmar Volume I)
     &             0.20, 0.20, !20 bigleaf maple from WC - df
     &             0.20, 0.20, !21 vine maple use bm
     &             0.20, 0.20, !22 red alder from WC - df
     &             0.20, 0.20, !23 paper birch from WC - df
     &             0.25, 0.25, !24 golden chinkapin from WC - as
     &             0.20, 0.20, !25 Pacific dogwood from WC - df
     &             0.25, 0.25, !26 quaking aspen from WC - as
     &             0.25, 0.25, !27 black cottonwood from WC - as
     &             0.23, 0.22, !28 Oregon white oak from WC
     &             0.25, 0.25, !29 cherry and plum species from WC bc - as
     &             0.25, 0.25, !30 willow species from WC - as
     &             0.15, 0.20, !31 other softwoods NI
     &             0.25, 0.25/ !32 other hardwoods use as
C----------
C     INITIAL LIVE FUEL LOADING FOR 'INTIALIZING STANDS WITH 10% COVER
C----------
C                  herbs, shrubs
      DATA FULIVI /0.30, 2.00, ! 1 white pine NI
     &             0.40, 2.00, ! 2 western larch NI
     &             0.40, 2.00, ! 3 Douglas-fir NI
     &             0.30, 2.00, ! 4 pacific silver fir - use grand fir
     &             0.40, 2.00, ! 5 western redcedar NI
     &             0.30, 2.00, ! 6 grand fir NI
     &             0.40, 1.00, ! 7 lodgepole pine NI
     &             0.30, 2.00, ! 8 Engelmann spruce NI
     &             0.30, 2.00, ! 9 subalpine fir NI
     &             0.25, 0.10, !10 ponderosa pine NI
     &             0.40, 2.00, !11 western hemlock from WC
     &             0.30, 2.00, !12 mountain hemlock from other softwoods NI
     &             0.40, 2.00, !13 Pacific yew from WC - df
     &             0.40, 1.00, !14 whitebark pine from WC - lp
     &             0.30, 2.00, !15 noble fir from WC - gf
     &             0.30, 2.00, !16 white fir - use gf
     &             0.40, 2.00, !17 subalpine larch from WC - wl
     &             0.40, 2.00, !18 Alaska cedar from WC - rc
     &             0.10, 2.06, !19 western juniper from WC - (Ottmar Volume I)
     &             0.40, 2.00, !20 bigleaf maple from WC - df
     &             0.40, 2.00, !21 vine maple use bm
     &             0.40, 2.00, !22 red alder from WC - df
     &             0.40, 2.00, !23 paper birch from WC - df
     &             0.18, 2.00, !24 golden chinkapin from WC - as
     &             0.40, 2.00, !25 Pacific dogwood from WC - df
     &             0.18, 2.00, !26 quaking aspen from WC - as
     &             0.18, 2.00, !27 black cottonwood from WC - as
     &             0.55, 0.35, !28 Oregon white oak from WC
     &             0.18, 2.00, !29 cherry and plum species from WC bc - as
     &             0.18, 2.00, !30 willow species from WC - as
     &             0.30, 2.00, !31 other softwoods NI
     &             0.18, 2.00/ !32 other hardwoods use as
C----------
C     INITIAL FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD
C
C      <.25   to1   1-3   3-6  6-12 12-20 20-35 35-50   >50   Lit  Duf
C----------
      DATA FUINIE /
     &  1.0,  1.0,  1.6, 10.0, 10.0, 10.0,  0.0,  0.0,  0.0,  0.8, 30.0, ! 1 wp NI wp
     &  0.9,  0.9,  1.6,  3.5,  3.5,  0.0,  0.0,  0.0,  0.0,  0.6, 10.0, ! 2 wl NI wl
     &  0.9,  0.9,  1.6,  3.5,  3.5,  0.0,  0.0,  0.0,  0.0,  0.6, 10.0, ! 3 df NI df
     &  0.7,  0.7,  3.0,  7.0,  7.0,  0.0,  0.0,  0.0,  0.0,  0.6, 25.0, ! 4 sf NI gf
     &  2.2,  2.2,  5.2, 15.0, 20.0, 15.0,  0.0,  0.0,  0.0,  1.0, 35.0, ! 5 rc NI rc
     &  0.7,  0.7,  3.0,  7.0,  7.0,  0.0,  0.0,  0.0,  0.0,  0.6, 25.0, ! 6 gf NI gf
     &  0.9,  0.9,  1.2,  7.0,  8.0,  0.0,  0.0,  0.0,  0.0,  0.6, 15.0, ! 7 lp NI lp
     &  1.1,  1.1,  2.2, 10.0, 10.0,  0.0,  0.0,  0.0,  0.0,  0.6, 30.0, ! 8 es NI es
     &  1.1,  1.1,  2.2, 10.0, 10.0,  0.0,  0.0,  0.0,  0.0,  0.6, 30.0, ! 9 af NI af
     &  0.7,  0.7,  1.6,  2.5,  2.5,  0.0,  0.0,  0.0,  0.0,  1.4,  5.0, !10 pp NI pp
     &  0.7,  0.7,  3.0,  7.0,  7.0, 10.0,  0.0,  0.0,  0.0,  1.0, 35.0, !11 wh WC wh
     &  1.1,  1.1,  2.2, 10.0, 10.0,  0.0,  0.0,  0.0,  0.0,  0.6, 30.0, !12 mh WC mh = NI es/af
     &  2.2,  2.2,  5.2, 15.0, 20.0, 15.0,  0.0,  0.0,  0.0,  1.0, 35.0, !13 py WC py = NI wh/rc
     &  1.1,  1.1,  2.2, 10.0, 10.0,  0.0,  0.0,  0.0,  0.0,  0.6, 30.0, !14 wb WC wb = NI es/af
     &  0.7,  0.7,  3.0,  7.0,  7.0,  0.0,  0.0,  0.0,  0.0,  0.6, 25.0, !15 nf WC nf = NI gf
     &  0.7,  0.7,  3.0,  7.0,  7.0,  0.0,  0.0,  0.0,  0.0,  0.6, 25.0, !16 wf NI gf
     &  0.2,  0.6,  2.4,  3.6,  5.6,  0.0,  0.0,  0.0,  0.0,  1.4, 16.8, !17 ll WC bc = WC as
     &  2.2,  2.2,  5.2, 15.0, 20.0, 15.0,  0.0,  0.0,  0.0,  1.0, 35.0, !18 yc WC yc = NI wh/rc
     &  0.1,  0.2,  0.4,  0.5,  0.8,  1.0,  0.0,  0.0,  0.0,  0.1,  0.0, !19 wj WC ju - Ottmar Vol I
     &  2.2,  2.2,  5.2, 15.0, 20.0, 15.0,  0.0,  0.0,  0.0,  1.0, 35.0, !20 bm WC bm = NI wh/rc
     &  2.2,  2.2,  5.2, 15.0, 20.0, 15.0,  0.0,  0.0,  0.0,  1.0, 35.0, !21 vn WC bm = NI wh/rc
     &  0.7,  0.7,  1.6,  2.5,  2.5,  5.0,  0.0,  0.0,  0.0,  0.8, 30.0, !22 ra WC ra
     &  2.2,  2.2,  5.2, 15.0, 20.0, 15.0,  0.0,  0.0,  0.0,  1.0, 35.0, !23 pb WC pb = NI wh/rc
     &  0.7,  0.7,  0.8,  1.2,  1.2,  0.5,  0.0,  0.0,  0.0,  1.4,  0.0, !24 gc WC gc
     &  2.2,  2.2,  5.2, 15.0, 20.0, 15.0,  0.0,  0.0,  0.0,  1.0, 35.0, !25 dg WC dg = NI wh/rc
     &  0.2,  0.6,  2.4,  3.6,  5.6,  0.0,  0.0,  0.0,  0.0,  1.4, 16.8, !26 as WC as
     &  0.2,  0.6,  2.4,  3.6,  5.6,  0.0,  0.0,  0.0,  0.0,  1.4, 16.8, !27 cw WC cw = as
     &  0.7,  0.7,  0.8,  1.2,  1.2,  0.5,  0.0,  0.0,  0.0,  1.4,  0.0, !28 wo WC wo
     &  0.2,  0.6,  2.4,  3.6,  5.6,  0.0,  0.0,  0.0,  0.0,  1.4, 16.8, !29 pl WC bc = as
     &  0.2,  0.6,  2.4,  3.6,  5.6,  0.0,  0.0,  0.0,  0.0,  1.4, 16.8, !30 wi WC wi = as
     &  1.1,  1.1,  2.2, 10.0, 10.0,  0.0,  0.0,  0.0,  0.0,  0.6, 30.0, !31 os NI ot
     &  0.2,  0.6,  2.4,  3.6,  5.6,  0.0,  0.0,  0.0,  0.0,  1.4, 16.8/ !32 oh use as
C----------
C     INITIAL FUEL LOADING FOR 'INITIALIZING' STANDS WITH 10% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD
C
C      <.25   to1   1-3   3-6  6-12  12-20 20-35 35-50  >50   Lit  Duf
C----------
      DATA FUINII /
     &  0.6,  0.6,  0.8,  6.0,  6.0,  6.0,  0.0,  0.0,  0.0,  0.4, 12.0, ! 1 wp NI wp
     &  0.5,  0.5,  1.0,  1.4,  1.4,  0.0,  0.0,  0.0,  0.0,  0.3,  5.0, ! 2 wl NI wl
     &  0.5,  0.5,  1.0,  1.4,  1.4,  0.0,  0.0,  0.0,  0.0,  0.3,  5.0, ! 3 df NI df
     &  0.5,  0.5,  2.0,  2.8,  2.8,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0, ! 4 sf NI gf
     &  1.6,  1.6,  3.6,  6.0,  8.0,  6.0,  0.0,  0.0,  0.0,  0.5, 12.0, ! 5 rc NI rc
     &  0.5,  0.5,  2.0,  2.8,  2.8,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0, ! 6 gf NI gf
     &  0.6,  0.7,  0.8,  2.8,  3.2,  0.0,  0.0,  0.0,  0.0,  0.3,  7.0, ! 7 lp NI lp
     &  0.7,  0.7,  1.6,  4.0,  4.0,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0, ! 8 es NI es
     &  0.7,  0.7,  1.6,  4.0,  4.0,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0, ! 9 af NI af
     &  0.1,  0.1,  0.2,  0.5,  0.5,  0.0,  0.0,  0.0,  0.0,  0.5,  0.8, !10 pp NI pp
     &  0.5,  0.5,  2.0,  2.8,  2.8,  6.0,  0.0,  0.0,  0.0,  0.5, 12.0, !11 wh WC wh
     &  0.7,  0.7,  1.6,  4.0,  4.0,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0, !12 mh WC mh = NI es/af
     &  1.1,  1.1,  3.6,  6.0,  8.0,  6.0,  0.0,  0.0,  0.0,  0.5, 12.0, !13 py WC py = NI wh/rc
     &  0.7,  0.7,  1.6,  4.0,  4.0,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0, !14 wb WC wb = NI es/af
     &  0.5,  0.5,  2.0,  2.8,  2.8,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0, !15 nf WC nf = NI gf
     &  0.5,  0.5,  2.0,  2.8,  2.8,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0, !16 wf NI gf
     &  0.5,  0.5,  1.0,  1.4,  1.4,  0.0,  0.0,  0.0,  0.0,  0.3,  5.0, !17 ll WC bc = WC as
     &  1.1,  1.1,  3.6,  6.0,  8.0,  6.0,  0.0,  0.0,  0.0,  0.5, 12.0, !18 yc WC yc = NI wh/rc
     &  0.2,  0.4,  0.2,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.2,  0.0, !19 wj WC ju - Ottmar Vol I
     &  1.1,  1.1,  3.6,  6.0,  8.0,  6.0,  0.0,  0.0,  0.0,  0.5, 12.0, !20 bm WC bm = NI wh/rc
     &  1.1,  1.1,  3.6,  6.0,  8.0,  6.0,  0.0,  0.0,  0.0,  0.5, 12.0, !21 vn WC bm = NI wh/rc
     &  0.1,  0.1,  0.2,  0.5,  0.5,  3.0,  0.0,  0.0,  0.0,  0.4, 12.0, !22 ra WC ra
     &  1.1,  1.1,  3.6,  6.0,  8.0,  6.0,  0.0,  0.0,  0.0,  0.5, 12.0, !23 pb WC pb = NI wh/rc
     &  0.1,  0.1,  0.1,  0.2,  0.2,  0.0,  0.0,  0.0,  0.0,  0.5,  0.0, !24 gc WC gc
     &  1.1,  1.1,  3.6,  6.0,  8.0,  6.0,  0.0,  0.0,  0.0,  0.5, 12.0, !25 dg WC dg = NI wh/rc
     &  0.1,  0.4,  5.0,  2.2,  2.3,  0.0,  0.0,  0.0,  0.0,  0.8,  5.6, !26 as WC as
     &  0.1,  0.4,  5.0,  2.2,  2.3,  0.0,  0.0,  0.0,  0.0,  0.8,  5.6, !27 cw WC cw = as
     &  0.1,  0.1,  0.1,  0.2,  0.2,  0.0,  0.0,  0.0,  0.0,  0.5,  0.0, !28 wo WC wo
     &  0.1,  0.4,  5.0,  2.2,  2.3,  0.0,  0.0,  0.0,  0.0,  0.8,  5.6, !29 pl WC bc = as
     &  0.1,  0.4,  5.0,  2.2,  2.3,  0.0,  0.0,  0.0,  0.0,  0.8,  5.6, !30 wi WC wi = as
     &  0.7,  0.7,  1.6,  4.0,  4.0,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0, !31 os NI ot
     &  0.1,  0.4,  5.0,  2.2,  2.3,  0.0,  0.0,  0.0,  0.0,  0.8,  5.6/ !32 oh use as
C----------
C     DOMINANT SPECIES FOR EACH (OPTIONAL) VEGETATION CODE.
C     DERIVED FROM R6 VEG CODES AND EC **HABTYP**
C----------
      DATA (COVINI(I), I=   1,  50) /
     & 14, 14,  5,  5,  5,  5,  5,  5,  3,  3,
     &  3,  3,  3,  3, 10,  3,  3,  3,  3,  3,
     &  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
     &  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
     &  3,  3,  3,  3,  3,  3,  9,  9,  9,  9/
      DATA (COVINI(I), I=  51, 100) /
     &  9,  9,  9,  9,  9,  9,  8,  9,  9,  9,
     &  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
     &  9,  9,  4,  4,  4,  4,  4,  4,  4,  4,
     &  4,  4, 11, 11, 11, 11, 11, 11, 11, 11,
     & 11, 11, 11, 11, 11, 11, 11,  7, 12, 12/
      DATA (COVINI(I), I= 101, 150) /
     & 12, 12, 12, 12, 12, 12, 12, 12, 12, 10,
     & 10, 10, 10, 10,  6,  6,  6,  6,  6,  6,
     &  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,
     &  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,
     &  6,  6,  6,  6,  6,  6,  6,  6,  6,  6/
      DATA (COVINI(I), I= 151, 155) /
     &  6,  6,  6, 26, 26/
C----------
C     MAPDRY -
C     INDEXING BASED ON ITYPE; ELEMENTS MUST MATCH ONE OF
C     TYPES FOUND IN **HABTYP**.
C     0 -> DRY HABITAT
C     1 -> MESIC HABITAT
C     2 -> MOIST HABITAT
C     MAPPING PROVIDED BY TOM DEMEO (ECOLOGIST, USDA FOR SERV,
C     PNW, PORTLAND) BASED ON WILLIAMS ET AL. 1995 (PNW-GTR-360)
C     WILLIAMS ET AL. 1983 (R6-Ecol-132b-1983), WILLIAMS ET AL. 1990
C     (COLVILLE ASSOCIATIONS) & JOHNSON 1988 (INDICATOR SPECIES/NE
C     OREGON/SE WASHINGTON); SOME UPDATES BY TERRY LILLYBRIDGE
C----------
      DATA (MAPDRY(I), I=   1,  50) /
     & 0, 1, 2, 2, 1, 2, 2, 1, 0, 0,
     & 0, 0, 0, 0, 0, 1, 0, 0, 0, 1,
     & 0, 0, 0, 0, 1, 1, 0, 0, 0, 0,
     & 0, 0, 0, 1, 0, 0, 0, 0, 0, 1,
     & 1, 1, 1, 2, 1, 1, 1, 2, 2, 2/
      DATA (MAPDRY(I), I=  51, 100) /
     & 2, 2, 2, 2, 1, 1, 2, 2, 2, 1,
     & 1, 1, 1, 1, 1, 1, 1, 2, 2, 1,
     & 1, 1, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 1, 1,
     & 2, 2, 2, 2, 2, 2, 2, 1, 2, 2/
      DATA (MAPDRY(I), I= 101, 150) /
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 0,
     & 0, 0, 0, 0, 2, 2, 1, 2, 2, 2,
     & 2, 0, 0, 0, 0, 1, 2, 2, 2, 2,
     & 1, 1, 1, 0, 2, 0, 0, 0, 0, 0,
     & 2, 0, 2, 0, 0, 1, 2, 2, 2, 1/
      DATA (MAPDRY(I), I= 151, 155) /
     & 2, 0, 2, 0, 0/
C
      DATA MYACT / 2521, 2548, 2553 /
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMCBA',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
    7 FORMAT(' ENTERING FMCBA CYCLE = ',I2)
C----------
C.... Begin routine.
C
C     Zero out the cummulative variables
C----------
      COVTYP = 0
      PERCOV = 0.0
      BIGDBH = 0.0
      TOTBA  = 0.0
C----------
C     Loop through the tree list
C----------
      IF (ITRN.GT.0) THEN
C----------
C       Zero out the cummulative variables
C----------
        BAMOST = 0.0
        TOTCRA = 0.0
C
        DO KSP=1,MAXSP
          FMTBA(KSP) = 0.0
        ENDDO
C
        DO I=1,ITRN
          IF (FMPROB(I) .GT. 0.0) THEN
C
            KSP = ISP(I)
C
            FMTBA(KSP) = FMTBA(KSP) +
     &                   FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
C
            IF (DBH(I) .GT. BIGDBH) BIGDBH = DBH(I)
C----------
C           Calculate the crown width of the tree and total the area
C           encompassed by all trees
C----------
            CWIDTH=CRWDTH(I)
C
            CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
            TOTCRA = TOTCRA + CAREA*FMPROB(I)
          ENDIF
C----------
C         Use this loop to zero this variable, for lack of a better place.
C----------
          CURKIL(I) = 0.0

        ENDDO
C----------
C        Determine which species has the most basal area
C        -> that will be the cover type
C----------
        DO KSP=1,MAXSP
          IF (FMTBA(KSP) .GT. BAMOST) THEN
            BAMOST = FMTBA(KSP)
            COVTYP = KSP
          ENDIF
          TOTBA = TOTBA + FMTBA(KSP)
        ENDDO
C----------
C       Use the crown width information to determine the percent cover
C       of the stand. Use the equation sent by Nick which assumes that
C       crowns are randomly distrubuted in the stand:
C
C       PerCov = 100*(1-exp(-total crown areas per acre / sqft in an acre))
C----------
        PERCOV = 100. * (1.0 - EXP(-TOTCRA/43560.))
C
      ENDIF
C----------
C     If there are no trees (COVTYP=0) in cycle 1,
C     use the optional ITYPE if it is valid. Otherwise
C     issue a warning and use a default LP cover. After the
C     first cycle, use the previous cover type if no cover is
C     present.
C----------
      IF (COVTYP .EQ. 0) THEN
        IF (IYR .EQ. IY(1)) THEN
          IF (ITYPE .GE. 1 .AND. ITYPE .LE. MXVCODE)
     &      COVTYP = COVINI(ITYPE)
          IF (COVTYP .EQ. 0) THEN
             WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO INITIAL ',
     &       'BASAL AREA AND NO VALID HABITAT INFORMATION:',
     &       /1X,'*** COVER TYPE SET TO DOUGLAS-FIR',/1X)")
            CALL RCDSET (2,.TRUE.)
            COVTYP = 3
          ENDIF
        ELSE
          COVTYP = OLDCOVTYP
        ENDIF
      ENDIF
C
      OLDCOVTYP = COVTYP
C----------
C     Load live fuels as a function of PERCOV...assume that the initiating
C     stands correspond to about 10% cover and established are 60% or more.
C----------
      XCOV(1)=10.
      XCOV(2)=60.
      DO I=1,2
        YLOAD(1)=FULIVI(I,COVTYP)
        YLOAD(2)=FULIVE(I,COVTYP)
        FLIVE(I)=ALGSLP(PERCOV,XCOV,YLOAD,2)
      ENDDO
C
      IF (DEBUG) WRITE(JOSTND,8) COVTYP,PERCOV,FLIVE(1),FLIVE(2)
    8 FORMAT(' IN FMCBA, COVTYP=',I3,' PERCOV=',F6.2,' FLIVE(1&2)=',
     >    2F6.3)
C----------
C     Initialize the dead fuels only for the first year of the simulation
C----------
      IF (IYR .EQ. IY(1)) THEN
C----------
C       MODIFY CWD DECAY RATE BASED ON HABITAT MOISTURE GROUP
C       0 = DRIER - LOWER RATE
C       1 = MESIC - UNCHANGED
C       2 = WETTER - HIGHER RATE
C
C       ONLY DO THIS IF DURING THE NORMAL CALL, NOT FROM SVSTART
C----------
        IF ( ISWTCH .NE. 1 ) THEN
          DCYMLT = 1.0
          SELECT CASE (MAPDRY(ITYPE))
            CASE (0)
              DCYMLT = 0.66
            CASE (1)
              DCYMLT = 1.0
            CASE (2)
              DCYMLT = 1.33
          END SELECT
C          
          DO I = 1,MXFLCL
            DO J = 1,4
              DKR(I,J) = DKR(I,J) * DCYMLT
            ENDDO
          ENDDO
        ENDIF
C----------
C       Load dead fuels as a function of PERCOV...assume that the initiating
C       stands correspond to about 10% cover and established are 60% or more.
C----------
        XCOV(1)=10.
        XCOV(2)=60.
        DO ISZ = 1, MXFLCL
          YLOAD(1)=FUINII(ISZ,COVTYP)
          YLOAD(2)=FUINIE(ISZ,COVTYP)
          STFUEL(ISZ,2) = ALGSLP(PERCOV,XCOV,YLOAD,2)
          STFUEL(ISZ,1) = 0
        ENDDO
C----------        
C       CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT
C---------
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
C----------
C           IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
C           DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C           NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.
C----------
            IF (FOTOVAL(1).GE.0 .AND. ISWTCH.NE.1) CALL OPDONE(J,IYR)
C
          ELSE
            WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: INCORRECT ',
     &      'PHOTO REFERENCE OR PHOTO CODE ENTERED.  BOTH FIELDS ARE ',
     &      'REQUIRED.',/1X)")
            CALL RCDSET (2,.TRUE.)
          ENDIF
        ENDIF
C----------        
C       CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
C       FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT
C----------
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
C----------
C         DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C         NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.
C----------
          IF ( ISWTCH .NE. 1 ) CALL OPDONE(J,IYR)
C
        ENDIF
C
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
C----------
C         DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C         NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.
C----------
          IF ( ISWTCH .NE. 1 ) CALL OPDONE(J,IYR)
C
        ENDIF
C----------
C       DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
C       OF BASAL AREA IN THE STAND. ASSUME THE FUELS ARE UNPILED AND HARD.
C       THIS LAST ASSUMPTION MAY CHANGE IN THE FUTURE.
C----------
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
C
      ENDIF
      RETURN
C----------
C     HOOK TO ALLOW THE MAPDRY() ARRAY TO BE READ BY **FMCFMD**
C----------
      ENTRY ECMOIST(IMD)
      IMD = MAPDRY(ITYPE)
      RETURN
C
      ENTRY SNGCOE
C----------
C     ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
C     IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.
C----------
      RETURN
      END
