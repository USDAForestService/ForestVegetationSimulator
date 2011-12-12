      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C  **FMCBA   FIRE-CR-DATE OF LAST REVISION:  01/03/11
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

C.... Parameter statements.

C.... Parameter include files.
      INCLUDE 'PRGPRM.F77'
Cppe      INCLUDE 'PPEPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... Common include files.
Cppe      INCLUDE 'PPCNTL.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'FMCOM.F77'

C.... Variable declarations.
C
C     MAXIMUM NUMBER OF VEGETATION CODES FOR REGIONS 2 & 3
C     THESE MUST MATCH THE DIMENSIONS OF THE VEGETATION CODE
C     ARRAYS IN **HABTYP**
C
      INTEGER   NR2, NR3
      PARAMETER (NR2=376)
      PARAMETER (NR3=242)

      INTEGER*2 COVINI2(NR2), COVINI3(NR3)
	    INTEGER   IREGN

      REAL BAMOST, TOTCRA, CWIDTH
      REAL FULIVE(2,MAXSP), FULIVI(2,MAXSP)
      REAL FUINIE(MXFLCL,MAXSP), FUINII(MXFLCL,MAXSP)
      REAL STFUEL(MXFLCL,2),XCOV(2),YLOAD(2), FOTOVAL(MXFLCL)
      REAL PRMS(12), FOTOVALS(9)
      LOGICAL DEBUG

      INTEGER MYACT(3)

      INTEGER IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC
      REAL    BIGDBH,TOTBA,XX,CAREA,ALGSLP,PRCL,ADD
C
C     INITIAL LIVE FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C
C                  herbs, shrubs
      DATA FULIVE /
     &            0.15,  0.20, !  1 subalpine fir
     &            0.15,  0.20, !  2 corkbark fir - use subalpine fir
     &            0.20,  0.20, !  3 Douglas-fir
     &            0.15,  0.10, !  4 grand fir
     &            0.15,  0.10, !  5 white fir
     &            0.15,  0.20, !  6 mountain hemlock (NI)
     &            0.20,  0.20, !  7 western redcedar (NI)
     &            0.20,  0.20, !  8 western larch (NI)
     &            0.04,  0.05, !  9 bristlecone pine - use pinyon pine / juniper
     &            0.20,  0.10, ! 10 limber pine - use lodgepole pine
     &            0.20,  0.10, ! 11 lodgepole pine
     &            0.04,  0.05, ! 12 pinyon pine - Ottmar, 2000
     &            0.20,  0.25, ! 13 ponderosa pine
     &            0.20,  0.10, ! 14 whitebark pine - use LP
     &            0.15,  0.10, ! 15 southwestern white pine (NI WP)
     &            0.04,  0.05, ! 16 Utah juniper - Ottmar, 2000
     &            0.15,  0.20, ! 17 blue spruce - use Engelmann spruce
     &            0.15,  0.20, ! 18 Engelmann spruce
     &            0.15,  0.20, ! 19 white spruce - use Engelmann spruce
     &            0.25,  0.25, ! 20 quaking aspen - Ottmar, 2000
     &            0.25,  0.25, ! 21 narrowleaf cottonwood - use aspen
     &            0.25,  0.25, ! 22 plains cottonwood - use aspen
     &            0.23,  0.22, ! 23 Gambel oak - Ottmar, 2000
     &            0.23,  0.22, ! 24 Arizona white oak - use gambel oak
     &            0.23,  0.22, ! 25 emory oak - use gambel oak
     &            0.23,  0.22, ! 26 bur oak - use gambel oak
     &            0.23,  0.22, ! 27 silverleaf oak - use gambel oak
     &            0.25,  0.25, ! 28 paper birch - use quaking aspen
     &            0.04,  0.05, ! 29 alligator juniper - use Utah juniper
     &            0.04,  0.05, ! 30 Rocky Mountain juniper - use Utah juniper
     &            0.04,  0.05, ! 31 oneseed juniper - use Utah juniper
     &            0.04,  0.05, ! 32 Eastern redcedar - use Utah juniper
     &            0.04,  0.05, ! 33 singleleaf pinyon - use pinyon pine
     &            0.04,  0.05, ! 34 border pinyon - use pinyon pine
     &            0.04,  0.05, ! 35 Arizona pinyon - use pinyon pine
     &            0.20,  0.25, ! 36 Chihuahua pine - use ponderosa pine
     &            0.20,  0.10, ! 37 other softwoods - use lodgepole pine
     &            0.25,  0.25  ! 38 other hardwoods - use aspen
     &            /
C
C     INITIAL LIVE FUEL LOADING FOR 'INTIALIZING STANDS WITH 10% COVER
C
C                  herbs, shrubs
      DATA FULIVI /
     &            0.30,  2.00, !  1 subalpine fir
     &            0.30,  2.00, !  2 corkbark fir - use subalpine fir
     &            0.40,  2.00, !  3 Douglas-fir
     &            0.30,  2.00, !  4 grand fir
     &            0.30,  2.00, !  5 white fire
     &            0.30,  2.00, !  6 mountain hemlock (NI)
     &            0.40,  2.00, !  7 western redcedar (NI)
     &            0.40,  2.00, !  8 western larch (NI)
     &            0.13,  1.63, !  9 bristlecone pine - use pinyon pine / juniper
     &            0.40,  1.00, ! 10 limber pine - use LP
     &            0.40,  1.00, ! 11 lodgepole pine
     &            0.13,  1.63, ! 12 pinyon pine - Ottmar, 2000
     &            0.25,  0.10, ! 13 ponderosa pine
     &            0.40,  1.00, ! 14 whitebark pine - use LP
     &            0.30,  2.00, ! 15 southwestern white pine (NI WP)
     &            0.13,  1.63, ! 16 Utah juniper - Ottmar, 2000
     &            0.30,  2.00, ! 17 blue spruce - use ES
     &            0.30,  2.00, ! 18 Engelmann spruce
     &            0.30,  2.00, ! 19 white spruce - use Engelmann spruce
     &            0.18,  1.32, ! 20 quaking aspen - Ottmar, 2000
     &            0.18,  1.32, ! 21 narrowleaf cottonwood - use quaking aspen
     &            0.18,  1.32, ! 22 plains cottonwood - use quaking aspen
     &            0.55,  0.35, ! 23 Gambel oak - Ottmar, 2000
     &            0.55,  0.35, ! 24 Arizona white oak - use gambel oak
     &            0.55,  0.35, ! 25 emory oak - use gambel oak
     &            0.55,  0.35, ! 26 bur oak - use gambel oak
     &            0.55,  0.35, ! 27 silverleaf oak - use gambel oak
     &            0.18,  1.32, ! 28 paper birch - use quaking aspen
     &            0.13,  1.63, ! 29 alligator juniper - use Utah juniper
     &            0.13,  1.63, ! 30 Rocky Mountain juniper - use Utah juniper
     &            0.13,  1.63, ! 31 oneseed juniper - use Utah juniper
     &            0.13,  1.63, ! 32 Eastern redcedar - use Utah juniper
     &            0.13,  1.63, ! 33 singleleaf pinyon - use pinyon pine
     &            0.13,  1.63, ! 34 border pinyon - use pinyon pine
     &            0.13,  1.63, ! 35 Arizona pinyon - use pinyon pine
     &            0.25,  0.10, ! 36 Chihuahua pine - use ponderosa pine
     &            0.40,  1.00, ! 37 other softwoods - use lodgepole pine
     &            0.18,  1.32  ! 38 other hardwoods - use quaking aspen
     &            /
C
C     INITIAL FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD
C
C                  <.25 to1  1-3  3-6  6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA FUINIE /
     &            1.1, 1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, !  1 subalpine fir
     &            1.1, 1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, !  2 corkbark fir - use subalpine fir
     &            0.9, 0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !  3 Douglas-fir
     &            0.7, 0.7, 3.0,  7.0, 7.0, 0.0, 0.0,0.0,0.0,0.6,25.0, !  4 grand fir
     &            0.7, 0.7, 3.0,  7.0, 7.0, 0.0, 0.0,0.0,0.0,0.6,25.0, !  5 white fir
     &            1.1 ,1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, !  6 mountain hemlock (NI)
     &            1.6, 1.6, 5.2, 15.0,20.0,15.0, 0.0,0.0,0.0,1.0,35.0, !  7 western redcedar (NI)
     &            0.9 ,0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !  8 western larch (NI)
     &            0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, !  9 bristlecone pine - use pinyon pine / juniper
     &            0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, ! 10 limber pine - use lodgepole pine
     &            0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, ! 11 lodgepole pine
     &            0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, ! 12 pinyon pine - Ottmar, 2000
     &            0.7, 0.7, 1.6,  2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0, ! 13 ponderosa pine
     &            0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, ! 14 whitebark pine - use lodgepole pine
     &            1.0, 1.0, 1.6, 10.0,10.0,10.0, 0.0,0.0,0.0,0.8,30.0, ! 15 southwestern white pine - use NI WP
     &            0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, ! 16 Utah juniper - Ottmar, 2000
     &            1.1, 1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, ! 17 blue spruce - use Engelmann spruce
     &            1.1, 1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, ! 18 Engelmann spruce
     &            1.1, 1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, ! 19 white spruce - use Engelmann spruce
     &            0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, ! 20 quaking aspen - Ottmar, 2000
     &            0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, ! 21 narrowleaf cottonwood - use aspen
     &            0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, ! 22 plains cottonwood - use aspen
     &            0.3, 0.7, 1.4,  0.2, 0.1, 0.0, 0.0,0.0,0.0,3.9, 0.0, ! 23 Gambel oak - Ottmar, 2000
     &            0.3, 0.7, 1.4,  0.2, 0.1, 0.0, 0.0,0.0,0.0,3.9, 0.0, ! 24 Arizona white oak - use gambel oak
     &            0.3, 0.7, 1.4,  0.2, 0.1, 0.0, 0.0,0.0,0.0,3.9, 0.0, ! 25 emory oak - use gambel oak
     &            0.3, 0.7, 1.4,  0.2, 0.1, 0.0, 0.0,0.0,0.0,3.9, 0.0, ! 26 bur oak - use gambel oak
     &            0.3, 0.7, 1.4,  0.2, 0.1, 0.0, 0.0,0.0,0.0,3.9, 0.0, ! 27 silverleaf oak - use gambel oak
     &            0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, ! 28 paper birch - use quaking aspen
     &            0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, ! 29 alligator juniper - use Utah juniper
     &            0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, ! 30 Rocky Mountain juniper - use Utah juniper
     &            0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, ! 31 oneseed juniper - use Utah juniper
     &            0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, ! 32 Eastern redcedar - use Utah juniper
     &            0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, ! 33 singleleaf pinyon - use pinyon pine
     &            0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, ! 34 border pinyon - use pinyon pine
     &            0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, ! 35 Arizona pinyon - use pinyon pine
     &            0.7, 0.7, 1.6,  2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0, ! 36 Chihuahua pine - use ponderosa pine
     &            0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, ! 37 others softwoods - use lodgepole pine
     &            0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8  ! 38 other hardwoods - use aspen
     &            /
C
C     INITIAL FUEL LOADING FOR 'INITIALIZING' STANDS WITH 10% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD
C
C                  <.25 to1  1-3  3-6  6-12  12-20 20-35 35-50 >50  Lit  Duf
      DATA FUINII /
     &            0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, !  1 subalpine fir
     &            0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, !  1 corkbark fir - use subalpine fir
     &            0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !  3 Douglas-fir
     &            0.5, 0.5, 2.0,  2.8, 2.8, 0.0, 0.0,0.0,0.0,0.3,12.0, !  4 grand fir
     &            0.5, 0.5, 2.0,  2.8, 2.8, 0.0, 0.0,0.0,0.0,0.3,12.0, !  5 white fir
     &            0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, !  6 mountain hemlock (NI)
     &            1.6, 1.6, 3.6,  6.0, 8.0, 6.0, 0.0,0.0,0.0,0.5,12.0, !  7 western redcedar (NI)
     &            0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !  8 western larch (NI)
     &            0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, !  9 bristleoone pine - use pinyon pine / juniper
     &            0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, ! 10 limber pine - use lodgepole pine
     &            0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, ! 11 lodgepole pine
     &            0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, ! 12 pinyon pine - Ottmar, 2000
     &            0.1, 0.1, 0.2,  0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8, ! 13 ponderosa pine
     &            0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, ! 14 whitebark pine - use lodgepole pine
     &            0.6, 0.6, 0.8,  6.0, 6.0, 6.0, 0.0,0.0,0.0,0.4,12.0, ! 15 southwestern white pine - use NI WP
     &            0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, ! 16 Utah juniper - Ottmar, 2000
     &            0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, ! 17 blue spruce - use Engelmann spruce
     &            0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, ! 18 Engelmann spruce
     &            0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, ! 19 white spruce - use Engelmann spruce
     &            0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, ! 20 quaking aspen - Ottmar, 2000
     &            0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, ! 21 narrowleaf cottonwood - use aspen
     &            0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, ! 22 plains cottonwood - use aspen
     &            0.1, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,2.9, 0.0, ! 23 Gambel oak - Ottmar, 2000
     &            0.1, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,2.9, 0.0, ! 24 Arizona white oak - use gambel oak
     &            0.1, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,2.9, 0.0, ! 25 emory oak - use gambel oak
     &            0.1, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,2.9, 0.0, ! 26 bur oak - use gambel oak
     &            0.1, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,2.9, 0.0, ! 27 silverleaf oak - use gambel oak
     &            0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, ! 28 paper birch - use aspen
     &            0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, ! 29 alligator juniper - use Utah juniper
     &            0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, ! 30 Rocky Mountain juniper - use Utah juniper
     &            0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, ! 31 oneseed juniper - use Utah juniper
     &            0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, ! 32 Eastern redcedar - use Utah juniper
     &            0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, ! 33 singleleaf pinyon - use pinyon pine  
     &            0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, ! 34 border pinyon - use pinyon pine
     &            0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, ! 35 Arizona pinyon - use pinyon pine
     &            0.1, 0.1, 0.2,  0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8, ! 36 Chihuahua pine - use ponderosa pine
     &            0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, ! 37 other softwoods - use lodgepole pine
     &            0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6  ! 38 other hardwoods - use aspen
     &            /
C
C     REGION 2 - CHANGES IN R2HABT() **HABTYP** WILL
C     AFFECT THIS
C     
      DATA (COVINI2(I), I=   1,  50) /
     &  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,
     &  5,  5,  5,  5,  5,  5,  5,  5,  5,  5,
     &  5,  5,  5,  5,  5,  1,  1,  1,  1,  1,
     &  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
     &  1,  1,  1,  1,  1,  1,  1,  1,  1,  1/
      DATA (COVINI2(I), I=  51, 100) /
     &  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
     &  1,  1,  1,  1,  1,  1,  1,  1,  1,  1,
     &  1,  1,  1,  1,  1,  1,  1,  1,  1, 18,
     & 18, 18, 18, 18, 18, 18, 18, 18, 18, 18,
     & 18, 18, 18, 18, 18, 18, 18, 18, 18, 19/
      DATA (COVINI2(I), I= 101, 150) /
     & 19, 19, 19, 19, 19, 17, 17, 17, 17, 17,
     & 17, 17, 17, 17, 17, 17, 17, 17, 17, 17,
     & 17, 17, 17, 17, 17, 14, 14, 14, 14, 14,
     & 14, 14, 14, 14, 14, 14, 14,  9,  9,  9,
     &  9,  9, 11, 11, 11, 11, 11, 11, 11, 11/
      DATA (COVINI2(I), I= 151, 200) /
     & 11, 11, 11, 11, 11, 10, 10, 10, 10, 10,
     & 10, 10, 10, 10, 10, 10, 10, 10, 13, 13,
     & 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
     & 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
     & 13, 13, 13, 13, 13, 13, 13, 13, 13, 13/
      DATA (COVINI2(I), I= 201, 250) /
     & 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
     & 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
     & 13, 13, 13, 13, 13, 13, 13, 13, 13, 13,
     & 13, 13, 13, 13,  3,  3,  3,  3,  3,  3,
     &  3,  3,  3,  3,  3,  3,  3,  3,  3,  3/
      DATA (COVINI2(I), I= 251, 300) /
     &  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
     &  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
     &  3,  3,  3,  3,  3,  3,  3,  3,  3, 24,
     & 24, 24, 21, 21, 21, 21, 21, 21, 21, 21,
     & 21, 21, 21, 21, 21, 21, 21, 20, 20, 20/
      DATA (COVINI2(I), I= 301, 350) /
     & 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
     & 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
     & 20, 20, 20, 20, 20, 20, 20, 20, 20, 20,
     & 20, 24, 24, 20, 24, 24, 24, 24, 24, 24,
     & 24, 24,  3, 16, 16, 16, 12, 16, 16, 16/
      DATA (COVINI2(I), I= 351, 376) /
     & 16, 16, 16, 16, 16, 16, 16, 16, 16, 16,
     & 12, 12, 12, 12, 12, 12, 12, 12, 12, 12,
     & 12, 12, 12, 12, 12, 12/
C
C     REGION 3 - CHANGES IN R3HABT() **HABTYP** WILL
C     AFFECT THIS
C     
      DATA (COVINI3(I), I=   1,  50) /
     &  5,  0,  0,  0,  5,  0, 16,  5,  5,  0,
     & 22,  5,  0,  0,  0,  0,  5,  5,  5,  0,
     &  5,  5,  5,  0,  5,  5,  5, 10,  5,  5,
     &  2,  2,  2,  1,  2, 18,  3,  2,  0,  0,
     &  0, 24,  2,  2,  3,  2,  2,  2,  2, 18/
      DATA (COVINI3(I), I=  51, 100) /
     &  0,  3, 18, 18,  2, 18, 18, 18, 18, 18,
     & 18,  2,  5, 18, 17, 17, 17, 17, 13, 17,
     & 17, 17, 17, 13,  0,  0,  0, 24, 22, 13,
     &  0,  0, 22,  5, 13,  0,  0, 12,  0,  0,
     &  0, 13, 13, 13, 13, 22, 13, 13,  0, 13/
      DATA (COVINI3(I), I= 101, 150) /
     & 13, 13,  0, 22, 13, 13,  0, 13, 13, 13,
     & 13, 13, 13,  3,  3,  3,  0,  0,  0,  3,
     &  3,  3,  9, 10, 20,  3, 10,  3,  3, 12,
     & 24,  3,  3,  3,  3,  3, 22, 22, 12, 12,
     & 12, 12, 12, 21, 21, 24, 24, 16,  0, 16/
      DATA (COVINI3(I), I= 151, 200) /
     & 16, 16,  0, 24, 16, 16, 16, 16, 16, 16,
     & 16, 16, 16,  0, 16, 12, 16, 12, 16, 16,
     & 12, 16, 16,  0, 12, 24, 22, 12, 12, 12,
     & 12, 12, 12, 12, 12,  0, 12, 12, 12, 12,
     & 12, 12, 12, 16, 16,  0, 17, 16, 16,  0/
      DATA (COVINI3(I), I= 201, 242) /
     & 16, 16, 16, 12, 12, 12, 12, 12, 12, 12,
     & 12, 12, 16,  0, 12, 12,  0,  0, 12, 12,
     &  9,  9,  9, 10,  0, 24, 17, 17, 24, 24,
     & 24, 24, 24, 24, 24, 24, 24, 16, 16, 12,
     & 24, 24/
C      
      DATA MYACT / 2521, 2548, 2553 /
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMCBA',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
    7 FORMAT(' ENTERING FMCBA CYCLE = ',I2)

C.... Begin routine.

C     Zero out the cummulative variables

      COVTYP = 0
      PERCOV = 0.0
      BIGDBH = 0.0
      TOTBA = 0.0

C     Loop through the tree list

      IF (ITRN.GT.0) THEN

C       Zero out the cummulative variables

        BAMOST = 0.0
        TOTCRA = 0.0

        DO KSP=1,MAXSP
          FMTBA(KSP) = 0.0
        ENDDO

        DO I=1,ITRN
          IF (FMPROB(I) .GT. 0.0) THEN

            KSP = ISP(I)

            FMTBA(KSP) = FMTBA(KSP) +
     &           FMPROB(I) * DBH(I) * DBH(I) * 0.0054542

            IF (DBH(I) .GT. BIGDBH) BIGDBH = DBH(I)

C           Calculate the crown width of the tree and total the area
C           encompassed by all trees

            CWIDTH=CRWDTH(I)

            CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
            TOTCRA = TOTCRA + CAREA*FMPROB(I)
          ENDIF

C         Use this loop to zero this variable, for lack of a better place.

          CURKIL(I) = 0.0
        ENDDO

C        Determine which species has the most basal area
C        -> that will be the cover type

        DO KSP=1,MAXSP
          IF (FMTBA(KSP) .GT. BAMOST) THEN
             BAMOST = FMTBA(KSP)
             COVTYP = KSP
          ENDIF
          TOTBA = TOTBA + FMTBA(KSP)
        ENDDO

C       Use the crown width information to determine the percent cover
C       of the stand. Use the equation sent by Nick which assumes that
C       crowns are randomly distrubuted in the stand:
C
C       PerCov = 100*(1-exp(-total crown areas per acre / sqft in an acre))

        PERCOV = 100. * (1.0 - EXP(-TOTCRA/43560.))

      ENDIF
C
C     If there are no trees (COVTYP=0) in cycle 1, use the
C     optional ITYPE if it is valid. The match is based
C     on whether *KODFOR* shows that the forest is in Region 2 or 3.
C     Otherwise issue a warning and use a default LP cover. After the
C     first cycle, use the previous cover type if no cover is
C     present.
C
      IF (COVTYP .EQ. 0) THEN
        IREGN = KODFOR/100
        IF (IYR .EQ. IY(1)) THEN
          IF (IREGN .EQ. 2) THEN
            IF (ITYPE .GE. 1 .AND. ITYPE .LE. NR2)
     &        COVTYP = COVINI2(ITYPE)
	    ELSEIF (IREGN .EQ. 3) THEN
            IF (ITYPE .GE. 1 .AND. ITYPE .LE. NR3)
     &        COVTYP = COVINI3(ITYPE)
          ENDIF
          IF (COVTYP .EQ. 0) THEN
            WRITE (JOSTND,"(/1X,'*** FFE MODEL WARNING: NO INITIAL ',
     &       'BASAL AREA AND NO VALID HABITAT INFORMATION:',
     &       /1X,'*** COVER TYPE SET TO LODGEPOLE PINE',/1X)")
            CALL RCDSET (2,.TRUE.)
            COVTYP = 11
          ENDIF
        ELSE
          COVTYP = OLDCOVTYP
        ENDIF
      ENDIF
      OLDCOVTYP = COVTYP
C
C     Load live fuels as a function of PERCOV...assume that the initiating
C     stands correspond to about 10% cover and established are 60% or more.
C
      XCOV(1)=10.
      XCOV(2)=60.
      DO I=1,2
        YLOAD(1)=FULIVI(I,COVTYP)
        YLOAD(2)=FULIVE(I,COVTYP)
        FLIVE(I)=ALGSLP(PERCOV,XCOV,YLOAD,2)
      ENDDO

      IF (DEBUG) WRITE(JOSTND,8) COVTYP,PERCOV,FLIVE(1),FLIVE(2)
    8 FORMAT(' IN FMCBA, COVTYP=',I3,' PERCOV=',F6.2,' FLIVE(1&2)=',
     >       2F6.3)

C
C     Initialize the dead fuels only for the first year of the simulation
C
      IF (IYR .EQ. IY(1)) THEN

Csng    IF (IYR .EQ. IY(1)) THEN
Cppe    IF (IYR .EQ. MIY(1)) THEN
C
C       Load dead fuels as a function of PERCOV...assume that the initiating
C       stands correspond to about 10% cover and established are 60% or more.
C
        XCOV(1)=10.
        XCOV(2)=60.
        DO ISZ = 1, MXFLCL
          YLOAD(1)=FUINII(ISZ,COVTYP)
          YLOAD(2)=FUINIE(ISZ,COVTYP)
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
C
C       DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
C       OF BASAL AREA IN THE STAND. ASSUME THE FUELS ARE UNPILED AND HARD.
C       THIS LAST ASSUMPTION MAY CHANGE IN THE FUTURE.
C
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
