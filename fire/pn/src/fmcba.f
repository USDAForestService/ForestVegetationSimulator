      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C  **FMCBA   FIRE-PN-DATE OF LAST REVISION:  04/25/13
C----------
C     SINGLE-STAND VERSION
C     CALLED FROM: FMMAIN

*  Purpose:
*     Find the dominant species (by basal area). Set the initial live
*     and dead fuel values as well. The dead fuels are only initialized
*     in the first year, but COVTYP and the live fuels must be done
*     each year.
*
*
*  Local variable definitions:
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
*
*  Common block variables and parameters:
*
***********************************************************************

C     Parameter statements.

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

      INTEGER    MXR6CODE
      PARAMETER (MXR6CODE=75)

      REAL      BAMOST, TOTCRA, CWIDTH
      INTEGER*2 COVINI6(MXR6CODE),MAPFGS(MXR6CODE),MAPDRY(MXR6CODE)
      REAL FULIVE(2,MAXSP), FULIVI(2,MAXSP)
      REAL FUINIE(MXFLCL,MAXSP), FUINII(MXFLCL,MAXSP)
      REAL STFUEL(MXFLCL,2),XCOV(2),YLOAD(2),DCYMLT, FOTOVAL(MXFLCL)
      REAL PRMS(12), FOTOVALS(9)
      LOGICAL DEBUG

      INTEGER MYACT(3)

      INTEGER IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC
      INTEGER ICT(MAXSP),IFGS,IWET
      REAL    BIGDBH,TOTBA,XX,CAREA,ALGSLP,PRCL,ADD
      INTEGER PNWMC(75),PNWMD(75), TEMP, MOIST, K
      REAL DKRADJ(3,3,3)

C     EACH PN HABITAT CODE MAPS TO EITHER WARM (1), MODERATE (2)
C     OR COLD (3).  (FROM FMR6SDCY)

      DATA (PNWMC(I), I=   1,  50) /
     & 2, 2, 2, 3, 3, 3, 3, 3, 2, 3,
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 3, 3, 3, 3, 3, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2/
      DATA (PNWMC(I), I=  51,  75) /
     & 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2, 2, 3, 2, 2, 2,
     & 2, 2, 2, 2, 2/

C     EACH PN HABITAT CODE MAPS TO EITHER WET (1), MESIC (2) OR DRY (3).  (FROM FMR6SDCY)

      DATA (PNWMD(I), I=   1,  50) /
     & 3, 3, 3, 3, 3, 3, 3, 1, 3, 2,
     & 1, 1, 2, 2, 2, 2, 2, 2, 2, 2,
     & 2, 2, 1, 2, 2, 1, 1, 2, 1, 1,
     & 2, 3, 2, 1, 2, 2, 3, 2, 3, 2,
     & 3, 2, 2, 2, 2, 2, 1, 3, 3, 1/
      DATA (PNWMD(I), I=  51,  75) /
     & 3, 2, 2, 2, 2, 2, 2, 1, 2, 1,
     & 1, 2, 2, 2, 2, 2, 2, 1, 1, 1,
     & 1, 1, 1, 1, 1/

      DATA (((DKRADJ(I,J,K), K=1,3), J=1,3), I=1,3) /
     &  1.21,   2,  1.21, 1.35, 2, 1.35,   1.7,   2,  1.7, 
     & 0.825, 1.3, 0.825,    1, 2,    1,  1.35,   2, 1.35, 
     &  0.75,   1,  0.75, 0.75, 1, 0.75, 0.925, 1.7, 0.925/	
     
C     BREAKPOINTS (% CC) OF INTERPOLATION FUNCTION TO PROVIDE WEIGHTED
C     ESTIMATE OF LIVE (EVERY TIMESTEP) AND DEAD (INITIAL) FUEL

      DATA XCOV / 10.0, 60.0 /

C     INITIAL LIVE FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER

C                  herbs, shrubs
      DATA FULIVE /
     >             0.15, 0.10, ! 1 = Pacific silver fir - use GF
     >             0.15, 0.10, ! 2 = white fir (wf) - use GF
     >             0.15, 0.10, ! 3 = grand fir
     >             0.15, 0.10, ! 4 = subalpine fir - use GF
     >             0.15, 0.10, ! 5 = Cal red fir/Shasta red fir - use GF
     >             0.30, 0.20, ! 6 = Sitka spruce - use ES
     >             0.15, 0.10, ! 7 = noble fir - use GF
     >             0.20, 0.20, ! 8 = Alaska cedar/western larch - use RC
     >             0.20, 0.20, ! 9 = incense-cedar - use DF
     >             0.30, 0.20, !10 = Engelmann spruce
     >             0.20, 0.10, !11 = lodgepole pine
     >             0.20, 0.25, !12 = Jeffrey pine - use PP
     >             0.20, 0.25, !13 = sugar pine - use PP
     >             0.15, 0.10, !14 = western white pine
     >             0.20, 0.25, !15 = ponderosa pine
     >             0.20, 0.20, !16 = Douglas-fir
     >             0.20, 0.20, !17 = coast redwood - use DF
     >             0.20, 0.20, !18 = western redcedar
     >             0.20, 0.20, !19 = western hemlock
     >             0.15, 0.10, !20 = mountain hemlock - use GF
     >             0.20, 0.20, !21 = bigleaf  - use DF
     >             0.20, 0.20, !22 = red alder - use DF
     >             0.20, 0.20, !23 = white alder/pacific madrone - use DF
     >             0.20, 0.20, !24 = paper birch - use DF
     >             0.25, 0.25, !25 = giant chinkapin/tanoak - use AS
     >             0.25, 0.25, !26 = quaking aspen
     >             0.25, 0.25, !27 = black cottonwood - use AS
     >             0.23, 0.22, !28 = Or. white oak/Cal. black oak
     >             0.14, 0.35, !29 = juniper (Ottmar Volume I)
     >             0.20, 0.20, !30 = subalpine larch - use WL
     >             0.20, 0.10, !31 = whitebark pine - use LP
     >             0.20, 0.10, !32 = knobcone pine - use LP
     >             0.20, 0.20, !33 = Pacific yew - use DF
     >             0.20, 0.20, !34 = Pacific dogwood - use DF
     >             0.25, 0.25, !35 = hawthorn (r4 definition) - use AS
     >             0.25, 0.25, !36 = bitter cherry - use AS
     >             0.25, 0.25, !37 = willow - use AS
     >             0.00, 0.00, !38 = ---
     >             0.25, 0.25/ !39 = other - use AS

C     INITIAL LIVE FUEL LOADING FOR 'INTIALIZING STANDS WITH 10% COVER

C                  herbs, shrubs
      DATA FULIVI /
     >             0.30, 2.00, ! 1 = Pacific silver fir - use GF
     >             0.30, 2.00, ! 2 = white fir (wf) - use GF
     >             0.30, 2.00, ! 3 = grand fir
     >             0.30, 2.00, ! 4 = subalpine fir - use GF
     >             0.30, 2.00, ! 5 = Cal red fir/Shasta red fir - use GF
     >             0.30, 2.00, ! 6 = Sitka spruce - use ES
     >             0.30, 2.00, ! 7 = noble fir - use GF
     >             0.40, 2.00, ! 8 = Alaska cedar/western larch - use RC
     >             0.40, 2.00, ! 9 = incense-cedar - use DF
     >             0.30, 2.00, !10 = Engelmann spruce
     >             0.40, 1.00, !11 = lodgepole pine
     >             0.25, 0.10, !12 = Jeffrey pine - use PP
     >             0.25, 0.10, !13 = sugar pine - use PP
     >             0.30, 2.00, !14 = western white pine
     >             0.25, 0.10, !15 = ponderosa pine
     >             0.40, 2.00, !16 = Douglas-fir
     >             0.40, 2.00, !17 = coast redwood - use DF
     >             0.40, 2.00, !18 = western redcedar
     >             0.40, 2.00, !19 = western hemlock
     >             0.30, 2.00, !20 = mountain hemlock - use GF
     >             0.40, 2.00, !21 = bigleaf  - use DF
     >             0.40, 2.00, !22 = red alder - use DF
     >             0.40, 2.00, !23 = white alder/pacific madrone - use DF
     >             0.40, 2.00, !24 = paper birch - use DF
     >             0.18, 2.00, !25 = giant chinkapin/tanoak - use AS
     >             0.18, 2.00, !26 = quaking aspen
     >             0.18, 2.00, !27 = black cottonwood - use AS
     >             0.55, 0.35, !28 = Or. white oak/Cal. black oak
     >             0.10, 2.06, !29 = juniper (Ottmar Volume I)
     >             0.40, 2.00, !30 = subalpine larch - use WL
     >             0.40, 1.00, !31 = whitebark pine - use LP
     >             0.40, 1.00, !32 = knobcone pine - use LP
     >             0.40, 2.00, !33 = Pacific yew - use DF
     >             0.40, 2.00, !34 = Pacific dogwood - use DF
     >             0.18, 2.00, !35 = hawthorn (r4 definition) - use AS
     >             0.18, 2.00, !36 = bitter cherry - use AS
     >             0.18, 2.00, !37 = willow - use AS
     >             0.00, 0.00, !38 = ---
     >             0.18, 2.00/ !39 = other - use AS

C     INITIAL FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD

C     <.25 to1  1-3   3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA FUINIE /
     &1.1, 1.1, 2.2,10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, ! 1 = Pacific silver fir
     &0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0,0.0,0.0,0.6,25.0, ! 2 = white fir (wf) - use GF
     &0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0,0.0,0.0,0.6,25.0, ! 3 = grand fir
     &1.1, 1.1, 2.2,10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, ! 4 = subalpine fir
     &0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0,0.0,0.0,0.6,25.0, ! 5 = Cal red fir/Shasta red fir - use GF
     &0.7, 0.7, 3.0, 7.0, 7.0,10.0, 0.0,0.0,0.0,1.0,35.0, ! 6 = Sitka spruce
     &0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0,0.0,0.0,0.6,25.0, ! 7 = noble fir - use GF
     &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0, ! 8 = Alaska cedar/western larch - use RC
     &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0, ! 9 = incense-cedar - use DF
     &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0, !10 = Engelmann spruce - use DF
     &0.9, 0.9, 1.2, 7.0, 8.0, 0.2, 0.0,0.0,0.0,0.6,30.0, !11 = lodgepole pine
     &0.7 ,0.7, 1.6, 2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0, !12 = Jeffrey pine - use PP
     &0.7 ,0.7, 1.6, 2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0, !13 = sugar pine - use PP
     &0.7, 0.7, 3.0, 7.0, 7.0, 0.0, 0.0,0.0,0.0,0.6,25.0, !14 = western white pine
     &0.7 ,0.7, 1.6, 2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0, !15 = ponderosa pine
     &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0, !16 = Douglas-fir
     &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0, !17 = coast redwood - use DF
     &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0, !18 = western redcedar
     &0.7, 0.7, 3.0, 7.0, 7.0,10.0, 0.0,0.0,0.0,1.0,35.0, !19 = western hemlock
     &1.1, 1.1, 2.2,10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, !20 = mountain hemlock
     &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0, !21 = bigleaf  - use DF
     &0.7, 0.7, 1.6, 2.5, 2.5, 5.0, 0.0,0.0,0.0,0.8,30.0, !22 = red alder
     &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0, !23 = white alder/pacific madrone - use DF
     &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0, !24 = paper birch - use DF
     &0.7, 0.7, 0.8, 1.2, 1.2, 0.5, 0.0,0.0,0.0,1.4, 0.0, !25 = giant chinkapin/tanoak
     &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, !26 = quaking aspen
     &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, !27 = black cottonwood - use AS
     &0.7, 0.7, 0.8, 1.2, 1.2, 0.5, 0.0,0.0,0.0,1.4, 0.0, !28 = Or. white oak/Cal. black oak
     &0.1, 0.2, 0.4, 0.5, 0.8, 1.0, 0.0,0.0,0.0,0.1, 0.0, !29 = juniper - Ottmar Volume I
     &0.9 ,0.9, 1.6, 3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !30 = subalpine larch - use WL
     &1.1, 1.1, 2.2,10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, !31 = whitebark pine
     &0.9, 0.9, 1.2, 7.0, 8.0, 0.2, 0.0,0.0,0.0,0.6,30.0, !32 = knobcone pine - use LP
     &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0, !33 = Pacific yew - use DF
     &2.2, 2.2, 5.2,15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0, !34 = Pacific dogwood - use DF
     &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, !35 = hawthorn (r4 definition) - use AS
     &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, !36 = bitter cherry - use AS
     &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, !37 = willow - use AS
     &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,0.0,0.0,0.0, 0.0, !38 = ---
     &0.2, 0.6, 2.4, 3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8/ !39 = other - use AS

C     INITIAL FUEL LOADING FOR 'INITIALIZING' STANDS WITH 10% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD

C     <.25 to1  1-3  3-6 6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA FUINII /
     &0.7, 0.7, 1.6, 4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, ! 1 = Pacific silver fir
     &0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0,0.0,0.0,0.3,12.0, ! 2 = white fir (wf) - use GF
     &0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0,0.0,0.0,0.3,12.0, ! 3 = grand fir
     &0.7, 0.7, 1.6, 4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, ! 4 = subalpine fir
     &0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0,0.0,0.0,0.3,12.0, ! 5 = Cal red fir/Shasta red fir - use GF
     &0.5, 0.5, 2.0, 2.8, 2.8, 6.0, 0.0,0.0,0.0,0.5,12.0, ! 6 = Sitka spruce - use ES
     &0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0,0.0,0.0,0.3,12.0, ! 7 = noble fir - use GF
     &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0, ! 8 = Alaska cedar/western larch - use RC
     &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0, ! 9 = incense-cedar - use DF
     &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0, !10 = Engelmann spruce
     &0.6, 0.7, 0.8, 2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3,12.0, !11 = lodgepole pine
     &0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8, !12 = Jeffrey pine - use PP
     &0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8, !13 = sugar pine - use PP
     &0.5, 0.5, 2.0, 2.8, 2.8, 0.0, 0.0,0.0,0.0,0.3,12.0, !14 = western white pine
     &0.1, 0.1, 0.2, 0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8, !15 = ponderosa pine
     &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0, !16 = Douglas-fir
     &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0, !17 = coast redwood - use DF
     &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0, !18 = western redcedar
     &0.5, 0.5, 2.0, 2.8, 2.8, 6.0, 0.0,0.0,0.0,0.5,12.0, !19 = western hemlock
     &0.7, 0.7, 1.6, 4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, !20 = mountain hemlock
     &1.6, 1.6, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0, !21 = bigleaf  - use DF
     &0.1, 0.1, 0.2, 0.5, 0.5, 3.0, 0.0,0.0,0.0,0.4,12.0, !22 = red alder
     &1.1, 1.1, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0, !23 = white alder/pacific madrone - use DF
     &1.1, 1.1, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0, !24 = paper birch - use DF
     &0.1, 0.1, 0.1, 0.2, 0.2, 0.0, 0.0,0.0,0.0,0.5, 0.0, !25 = giant chinkapin/tanoak
     &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, !26 = quaking aspen
     &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, !27 = black cottonwood - use AS
     &0.1, 0.1, 0.1, 0.2, 0.2, 0.0, 0.0,0.0,0.0,0.5, 0.0, !28 = Or. white oak/Cal. black oak
     &0.2, 0.4, 0.2, 0.0, 0.0, 0.0, 0.0,0.0,0.0,0.2, 0.0, !29 = juniper - Ottmar Volume I
     &0.5, 0.5, 1.0, 1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !30 = subalpine larch - use WL
     &0.7, 0.7, 1.6, 4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, !31 = whitebark pine
     &0.6, 0.7, 0.8, 2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3,12.0, !32 = knobcone pine - use LP
     &1.1, 1.1, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0, !33 = Pacific yew - use DF
     &1.1, 1.1, 3.6, 6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0, !34 = Pacific dogwood - use DF
     &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, !35 = hawthorn (r4 definition) - use AS
     &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, !36 = bitter cherry - use AS
     &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, !37 = willow - use AS
     &0.0, 0.0, 0.0, 0.0, 0.0, 0.0, 0.0,0.0,0.0,0.0, 0.0, !38 = ---
     &0.1, 0.4, 5.0, 2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6/ !39 = other - use AS

C     DOMINANT SPECIES FOR EACH (OPTIONAL) VEGETATION CODE.
C     DERIVED FROM R6 VEG CODES AND PN **HABTYP**

      DATA (COVINI6(I), I=   1,  50) /
     & 16, 16, 16,  4,  4,  4,  4,  1,  1,  1,
     &  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
     &  1,  1,  1,  1,  1, 19, 19, 19, 19, 19,
     & 19, 19, 19, 19, 19, 19, 19, 19, 19, 19,
     & 19, 19, 19, 19, 19, 19, 19, 19, 19, 19/
      DATA (COVINI6(I), I=  51,  75) /
     & 19, 19, 19, 19, 19, 19, 19, 19, 19, 19,
     & 19, 19, 19, 19, 19, 19, 20,  6,  6,  6,
     &  6,  6,  6,  6,  6/

C     EACH PN HABITAT CODE MAPS TO EITHER FORB (1), GRASS (2)
C     OR SHRUB (3), FOR USE BY **FMCFMD**. THOSE WITHOUT ANY
C     VALID MAPPING ARE SET TO 0

      DATA (MAPFGS(I), I=   1,  50) /
     & 3, 3, 3, 3, 3, 3, 3, 1, 1, 1,
     & 1, 1, 0, 3, 3, 3, 3, 3, 3, 3,
     & 3, 3, 3, 3, 3, 1, 1, 1, 1, 1,
     & 3, 1, 0, 1, 3, 3, 3, 3, 3, 3,
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 1/
      DATA (MAPFGS(I), I=  51,  75) /
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 3, 3, 3, 3, 3, 3, 3, 1, 1, 1,
     & 3, 3, 3, 3, 3/

C     INDEXING BASED ON ITYPE; ELEMENTS MUST MATCH ONE OF
C     TYPES FOUND IN **HABTYP**.
C     0 -> DRY HABITAT
C     1 -> MESIC HABITAT
C     2 -> MOIST HABITAT
C     MAPPING PROVIDED BY TOM DEMEO (ECOLOGIST, USDA FOR SERV,
C     PNW, PORTLAND) AND KIM MELLEN (DITTO)

      DATA (MAPDRY(I), I=   1,  50) /
     & 0, 0, 0, 0, 0, 0, 0, 2, 0, 0,
     & 1, 2, 1, 1, 0, 1, 1, 0, 1, 2,
     & 1, 1, 2, 0, 1, 2, 2, 1, 2, 2,
     & 2, 0, 1, 2, 1, 1, 0, 1, 0, 0,
     & 0, 2, 1, 1, 1, 1, 2, 0, 0, 2/
      DATA (MAPDRY(I), I=  51,  75) /
     & 0, 1, 0, 0, 0, 2, 1, 2, 1, 2,
     & 2, 1, 1, 0, 2, 2, 2, 2, 2, 2,
     & 2, 2, 2, 2, 2/

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


C       Use the crown width information to determine the percent cover
C       of the stand. Use the equation sent by Nick which assumes that
C       crowns are randomly distrubuted in the stand:

C       PerCov = 100*(1-exp(-total crown areas per acre / sqft in an acre))

        PERCOV = 100. * (1.0 - EXP(-TOTCRA/43560.))

      ENDIF


C     IF THERE ARE NO TREES (COVTYP=0) IN CYCLE 1,
C     USE THE OPTIONAL ITYPE IF IT IS VALID. OTHERWISE
C     ISSUE A WARNING AND USE A DEFAULT LP COVER. AFTER THE
C     FIRST CYCLE, USE THE PREVIOUS COVER TYPE IF NO COVER IS
C     PRESENT.

      IF (COVTYP .EQ. 0) THEN
        IF (IYR .EQ. IY(1)) THEN

          IF (ITYPE .GE. 1 .AND. ITYPE .LE. MXR6CODE)
     &      COVTYP = COVINI6(ITYPE)  ! R6 HABITAT TYPE

          IF (COVTYP .EQ. 0) THEN
             WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO INITIAL ',
     &       'BASAL AREA AND NO VALID HABITAT INFORMATION:',
     &       /1X,'*** COVER TYPE SET TO DOUGLAS-FIR',/1X)")
            CALL RCDSET (2,.TRUE.)
            COVTYP = 16
          ENDIF
        ELSE
          COVTYP = OLDCOVTYP
        ENDIF

      ENDIF

      OLDCOVTYP = COVTYP

C     LOAD LIVE FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
C     STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.
C       IN WS,NC,CA VARIANTS, THE TOP 2 SPECIES ARE USED TO INTIALIZE THE POOLS

      DO I = 1,2  ! herbs, shrub loop
        YLOAD(1) = FULIVI(I,COVTYP)
        YLOAD(2) = FULIVE(I,COVTYP)
        FLIVE(I) = ALGSLP(PERCOV,XCOV,YLOAD,2)
      ENDDO

      IF (DEBUG) WRITE(JOSTND,8) COVTYP,PERCOV,FLIVE(1),FLIVE(2)
    8 FORMAT(' IN FMCBA, COVTYP=',I3,' PERCOV=',F6.2,' FLIVE(1&2)=',
     >    2F6.3)

C     INITIALIZE THE DEAD FUELS ONLY FOR THE FIRST YEAR OF THE SIMULATION

      IF (IYR .EQ. IY(1)) THEN

        TEMP = PNWMC(ITYPE)
        MOIST = PNWMD(ITYPE)

        DO I = 1,9
          DO J = 1,4
            IF (I .LE. 3) THEN
              K = 1
            ELSEIF (I .LE. 5) THEN 
              K = 2
            ELSE 
              K = 3
            ENDIF
C       adjust the decay rates only if the user hasn't reset them with FuelDcay
C       also, adjust the decay rates if smaller wood is decaying more slowly than larger wood.
C       in this case, bump up the decay rate of the smaller wood to that of the larger wood.
            IF ((SETDECAY(I,J) .LT. 0) .AND. (ISWTCH .NE. 1)) THEN
              DKR(I,J) = DKR(I,J)*DKRADJ(TEMP,MOIST,K)
              IF (DKR(I,J) .GT. 1.0) DKR(I,J) = 1.0
              TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J)
            ENDIF
          ENDDO
        ENDDO

        DO I = 9,2,-1
          DO J = 1,4
            IF (((DKR(I,J)-DKR(I-1,J)) .GT. 0).AND.(ISWTCH.NE.1)) THEN 
              IF (SETDECAY(I-1,J) .LT. 0) THEN
                DKR(I-1,J) = DKR(I,J)
                TODUFF(I-1,J) = DKR(I-1,J) * PRDUFF(I-1,J)                
              ENDIF             
            ENDIF
          ENDDO
        ENDDO

C       LOAD DEAD FUELS AS A FUNCTION OF PERCOV...ASSUME THAT THE INITIATING
C       STANDS CORRESPOND TO ABOUT 10% COVER AND ESTABLISHED ARE 60% OR MORE.
C       IN WS,NC,CA VARIANTS, THE TOP 2 SPECIES ARE USED TO INTIALIZE THE POOLS

        DO ISZ = 1,MXFLCL  ! CWD size category loop
          YLOAD(1) = FUINII(ISZ,COVTYP)
          YLOAD(2) = FUINIE(ISZ,COVTYP)
          STFUEL(ISZ,2) = ALGSLP(PERCOV,XCOV,YLOAD,2)
          STFUEL(ISZ,1) = 0
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
      RETURN

C     HOOK TO ALLOW THE MAPFGS() ARRAY TO BE READ BY **FMCFMD*

      ENTRY PNFGS(IFGS)
      IFGS = MAPFGS(ITYPE)
      RETURN

C     HOOK TO ALLOW THE MAPWET() ARRAY TO BE READ BY **FMCFMD*

      ENTRY PNWET(IWET)
      IWET = MAPDRY(ITYPE)
      RETURN

      ENTRY SNGCOE

C     ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
C     IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.

      RETURN
      END
