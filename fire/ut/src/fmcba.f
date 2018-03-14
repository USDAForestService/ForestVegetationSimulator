      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C FIRE-UT $Id: fmcba.f 0000 2018-02-14 00:00:00Z gedixon $
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
C----------
COMMONS
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'FMPARM.F77'
C
C
Cppe      INCLUDE 'PPEPRM.F77'
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
C     DIMENSION OF THE VEGETATION CODE ARRAY IN HABTYP
C----------
      INTEGER MXVCODE
      PARAMETER (MXVCODE = 363)
C
      REAL BAMOST, TOTCRA, CWIDTH
      INTEGER*2 COVINI(MXVCODE)
      REAL FULIVE(2,MAXSP), FULIVI(2,MAXSP)
      REAL FUINIE(MXFLCL,MAXSP), FUINII(MXFLCL,MAXSP)
      REAL STFUEL(MXFLCL,2),XCOV(2),YLOAD(2), FOTOVAL(MXFLCL)
      REAL PRMS(12), FOTOVALS(9)
      LOGICAL DEBUG
C
      INTEGER MYACT(3)
C
      INTEGER IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC
      REAL    BIGDBH,TOTBA,CAREA,ALGSLP,PRCL,ADD
C----------
C SPECIES ORDER FOR UTAH VARIANT:
C
C  1=WB,  2=LM,  3=DF,  4=WF,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
C 11=PI, 12=WJ, 13=GO, 14=PM, 15=RM, 16=UJ, 17=GB, 18=NC, 19=FC, 20=MC,
C 21=BI, 22=BE, 23=OS, 24=OH
C
C VARIANT EXPANSION:
C GO AND OH USE OA (OAK SP.) EQUATIONS FROM UT
C PM USES PI (COMMON PINYON) EQUATIONS FROM UT
C RM AND UJ USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
C GB USES BC (BRISTLECONE PINE) EQUATIONS FROM CR
C NC, FC, AND BE USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
C MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
C BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
C OS USES OT (OTHER SP.) EQUATIONS FROM UT
C
C NOTE: THE SO VARIANT DOESN'T HAVE FUEL LOADINGS LIKE THESE, SO FOR THOSE
C       TWO SPECIES I LOOKED AT WHAT SURROGATES OTHER VARIANTS ARE USING
C       AND CHOSE UT ASPEN FOR MTN MAH; AND UT DF FOR BIGLEAF MAPLE; THESE
C       MAY NEED TO BE CHANGED.  GED 11/30/09
C----------
C     INITIAL LIVE FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C
C                  herbs, shrubs
C----------
      DATA FULIVE /
     & 0.20,  0.10, !1  whitebark pine - use LP
     & 0.20,  0.10, !2  limber pine - use LP
     & 0.20,  0.20, !3  Douglas-fir
     & 0.15,  0.10, !4  white fir / grand fir
     & 0.15,  0.20, !5  blue spruce - use ES
     & 0.25,  0.25, !6  quaking aspen - Ottmar, 2000
     & 0.20,  0.10, !7  lodgepole pine
     & 0.15,  0.20, !8  Engelmann spruce
     & 0.15,  0.20, !9  subalpine fir
     & 0.20,  0.25, !10 ponderosa pine
     & 0.04,  0.05, !11 common pinyon - Ottmar, 2000
     & 0.04,  0.05, !12 western juniper - Ottmar, 2000
     & 0.23,  0.22, !13 Gambel oak - Ottmar, 2000
     & 0.04,  0.05, !14 singleleaf pinyon - use UT PI
     & 0.04,  0.05, !15 Rocky Mtn juniper - use UT WJ
     & 0.04,  0.05, !16 Utah juniper - use UT WJ
     & 0.04,  0.05, !17 GB bristlecone pine - use CR BC
     & 0.25,  0.25, !18 narrowleaf cottonwood - use CR NC
     & 0.25,  0.25, !19 Fremont cottonwood - use CR NC
     & 0.25,  0.25, !20 curlleaf mtn-mahogany - use CR AS
     & 0.20,  0.20, !21 bigtooth maple - use CR DF
     & 0.25,  0.25, !22 box elder - use CR NC
     & 0.20,  0.10, !23 other softwoods - use whitebark
     & 0.23,  0.22/ !24 other hardwoods - use UT GO
C----------
C     INITIAL LIVE FUEL LOADING FOR 'INTIALIZING STANDS WITH 10% COVER
C
C                  herbs, shrubs
C----------
      DATA FULIVI /
     & 0.40, 1.00,  !1  whitebark pine - use LP
     & 0.40, 1.00,  !2  limber pine - use LP
     & 0.40, 2.00,  !3  Douglas-fir
     & 0.30, 2.00,  !4  white fire / grand fir
     & 0.30, 2.00,  !5  blue spruce - use ES
     & 0.18, 1.32,  !6  quaking aspen - Ottmar, 2000
     & 0.40, 1.00,  !7  lodgepole pine
     & 0.30, 2.00,  !8  Engelmann spruce
     & 0.30, 2.00,  !9  subalpine fir
     & 0.25, 0.10,  !10 ponderosa pine
     & 0.13, 1.63,  !11 common pinyon - Ottmar, 2000
     & 0.13, 1.63,  !12 western juniper - Ottmar, 2000
     & 0.55, 0.35,  !13 Gambel oak - Ottmar, 2000
     & 0.13, 1.63,  !14 singleleaf pinyon - use UT PI
     & 0.13, 1.63,  !15 Rocky Mtn juniper - use UT WJ
     & 0.13, 1.63,  !16 Utah juniper - use UT WJ
     & 0.13, 1.63,  !17 GB bristlecone pine - use CR BC
     & 0.18, 1.32,  !18 narrowleaf cottonwood - use CR NC
     & 0.18, 1.32,  !19 Fremont cottonwood - use CR NC
     & 0.18, 1.32,  !20 curlleaf mtn-mahogany - use CR AS
     & 0.40, 2.00,  !21 bigtooth maple - use CR DF
     & 0.18, 1.32,  !22 box elder - use CR NC
     & 0.40, 1.00,  !23 other softwoods - use whitebark pine
     & 0.55, 0.35/  !24 other hardwoods - use UT GO
C----------
C     INITIAL FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD
C
C                  <.25 to1  1-3  3-6  6-12  12-20 20-35 35-50 >50  Lit  Duf
C----------
      DATA FUINIE /
     & 0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, !1  whitebark pine - use LP
     & 0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, !2  limber pine - use LP
     & 0.9, 0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !3  Douglas-fir
     & 0.7, 0.7, 3.0,  7.0, 7.0, 0.0, 0.0,0.0,0.0,0.6,25.0, !4  white fire / grand fir
     & 1.1, 1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, !5  blue spruce - use ES
     & 0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, !6  quaking aspen - Ottmar, 2000
     & 0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, !7  lodgepole pine
     & 1.1, 1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, !8  Engelmann spruce
     & 1.1, 1.1, 2.2, 10.0,10.0, 0.0, 0.0,0.0,0.0,0.6,30.0, !9  subalpine fir
     & 0.7, 0.7, 1.6,  2.5, 2.5, 0.0, 0.0,0.0,0.0,1.4, 5.0, !10 ponderosa pine
     & 0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, !11 common pinyon - Ottmar, 2000
     & 0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, !12 western juniper - Ottmar, 2000
     & 0.3, 0.7, 1.4,  0.2, 0.1, 0.0, 0.0,0.0,0.0,3.9, 0.0, !13 Gambel oak - Ottmar, 2000
     & 0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, !14 singleleaf pinyon - use UT PI
     & 0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, !15 Rocky Mtn juniper - use UT WJ
     & 0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, !16 Utah juniper - use UT WJ
     & 0.2, 0.8, 2.3,  1.4, 3.0, 0.0, 0.0,0.0,0.0,0.5, 0.0, !17 GB bristlecone pine - use CR BC
     & 0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, !18 narrowleaf cottonwood - use CR NC
     & 0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, !19 Fremont cottonwood - use CR NC
     & 0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, !20 curlleaf mtn-mahogany - use CR AS
     & 0.9, 0.9, 1.6,  3.5, 3.5, 0.0, 0.0,0.0,0.0,0.6,10.0, !21 bigtooth maple - use CR DF
     & 0.2, 0.6, 2.4,  3.6, 5.6, 0.0, 0.0,0.0,0.0,1.4,16.8, !22 box elder - use CR NC
     & 0.9, 0.9, 1.2,  7.0, 8.0, 0.0, 0.0,0.0,0.0,0.6,15.0, !23 other softwoods - use whitebark pine
     & 0.3, 0.7, 1.4,  0.2, 0.1, 0.0, 0.0,0.0,0.0,3.9, 0.0/ !24 other hardwoods - use UT GO
C----------
C     INITIAL FUEL LOADING FOR 'INITIALIZING' STANDS WITH 10% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD
C
C                  <.25 to1  1-3  3-6  6-12  12-20 20-35 35-50 >50  Lit  Duf
C----------
      DATA FUINII /
     & 0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, !1  whitebark pine - use LP
     & 0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, !2  limber pine - use LP
     & 0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !3  Douglas-fir
     & 0.5, 0.5, 2.0,  2.8, 2.8, 0.0, 0.0,0.0,0.0,0.3,12.0, !4  white fire / grand fir
     & 0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, !5  blue spruce - use ES
     & 0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, !6  quaking aspen - Ottmar, 2000
     & 0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, !7  lodgepole pine
     & 0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, !8  Engelmann spruce
     & 0.7, 0.7, 1.6,  4.0, 4.0, 0.0, 0.0,0.0,0.0,0.3,12.0, !9  subalpine fir
     & 0.1, 0.1, 0.2,  0.5, 0.5, 0.0, 0.0,0.0,0.0,0.5, 0.8, !10 ponderosa pine
     & 0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, !11 common pinyon - Ottmar, 2000
     & 0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, !12 western juniper - Ottmar, 2000
     & 0.1, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,2.9, 0.0, !13 Gambel oak - Ottmar, 2000
     & 0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, !14 singleleaf pinyon - use UT PI
     & 0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, !15 Rocky Mtn juniper - use UT WJ
     & 0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, !16 Utah juniper - use UT WJ
     & 0.0, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,0.3, 0.0, !17 GB bristlecone pine - use CR BC
     & 0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, !18 narrowleaf cottonwood - use CR NC
     & 0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, !19 Fremont cottonwood - use CR NC
     & 0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, !20 curlleaf mtn-mahogany - use CR AS
     & 0.5, 0.5, 1.0,  1.4, 1.4, 0.0, 0.0,0.0,0.0,0.3, 5.0, !21 bigtooth maple - use CR DF
     & 0.1, 0.4, 5.0,  2.2, 2.3, 0.0, 0.0,0.0,0.0,0.8, 5.6, !22 box elder - use CR NC
     & 0.6, 0.7, 0.8,  2.8, 3.2, 0.0, 0.0,0.0,0.0,0.3, 7.0, !23 other softwoods - use whitebark pine
     & 0.1, 0.1, 0.0,  0.0, 0.0, 0.0, 0.0,0.0,0.0,2.9, 0.0/ !24 other hardwoods - use UT GO
C----------
C     DOMINANT SPECIES FOR EACH (OPTIONAL) VEGETATION CODE.
C     DERIVED FROM R4 VEG CODES AND VARIANT **HABTYP**
C----------
      DATA (COVINI(I), I=   1,  50) /
     &  2,  2,  2,  2,  2, 10, 10, 10, 10, 10,
     & 10, 10, 10, 10, 10, 10,  2, 10, 10, 10,
     & 10,  3,  5,  8,  9,  9,  9,  9,  9,  9,
     &  9,  9,  9,  9,  4,  4,  7,  7,  7,  7,
     & 10, 10, 10, 10, 10, 10, 10,  3,  3,  3/
      DATA (COVINI(I), I=  51, 100) /
     &  3,  5,  5,  9,  9,  9,  4,  4,  4,  4,
     &  4,  4,  4,  6,  6,  6,  6,  6,  6,  6,
     &  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,
     &  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,
     &  6,  6,  6,  6,  6,  6,  6,  6,  6,  6/
      DATA (COVINI(I), I= 101, 150) /
     &  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,
     &  6,  6,  6,  6,  6,  6,  6,  6,  6,  6,
     &  6,  6,  6,  6,  6,  6,  6,  5,  5,  5,
     &  5,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/
      DATA (COVINI(I), I= 151, 200) /
     & 23, 23, 23, 23, 23, 23, 23, 18, 18, 18,
     & 18, 22, 22,  0,  0,  0,  0,  0,  0,  0,
     &  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &  0,  0,  0,  0,  0,  0,  0,  0,  9,  0/
      DATA (COVINI(I), I= 201, 250) /
     &  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &  0,  0, 20,  0,  0,  0,  0,  0,  0,  0,
     &  0, 23, 23, 18, 18,  0,  0,  0,  0,  0,
     &  0,  0,  0,  0,  0,  0,  0,  0,  0,  0/
      DATA (COVINI(I), I= 251, 300) /
     &  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &  0,  0,  0,  0,  0,  0,  0,  0,  0,  0,
     &  0,  0,  0,  2,  2,  2,  2, 10, 10, 10,
     &  3,  3,  3,  3,  3,  3,  3,  3,  3,  3,
     &  3,  3,  3,  3,  3,  3,  3,  3,  3,  3/
      DATA (COVINI(I), I= 301, 350) /
     &  3,  3,  3,  3,  3,  3,  5,  8,  8,  8,
     &  8,  8,  8,  8,  9,  9,  9,  9,  9,  9,
     &  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
     &  9,  9,  9,  9,  9,  9,  9,  9,  9,  9,
     &  9,  9,  9,  9,  9,  9,  9,  9,  9,  9/
      DATA (COVINI(I), I= 351, 363) /
     &  9,  9,  9,  9,  9,  9,  4,  4,  4,  7,
     &  7,  7,  7/

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
      TOTBA = 0.0
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
C
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
     &       /1X,'*** COVER TYPE SET TO LODGEPOLE PINE',/1X)")
            CALL RCDSET (2,.TRUE.)
            COVTYP = 7
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

Csng    IF (IYR .EQ. IY(1)) THEN
Cppe    IF (IYR .EQ. MIY(1)) THEN
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
C----------
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
C
      ENTRY SNGCOE
C----------
C     ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
C     IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.
C----------
      RETURN
      END
