      SUBROUTINE FMCBA (IYR,ISWTCH)
      IMPLICIT NONE
C----------
C FIRE-CI $Id$
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
***********************************************************************
C----------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'FMPARM.F77'
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
      INCLUDE 'CICOM.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
COMMONS
C----------
C  Variable declarations.
C
C     MAXIMUM NUMBER OF VEGETATION CODES; MUST MATCH THE
C     DIMENSION OF THE ICITYP ARRAY IN CI **BLKDAT**
C----------
      LOGICAL DEBUG
      INTEGER MXVCODE
      PARAMETER (MXVCODE = 130)
      INTEGER*1 COVINI(MXVCODE), MAPPVG(MXVCODE),
     &          MAPS9B(MXVCODE), MAPDRY(MXVCODE)
      INTEGER MYACT(3)
      INTEGER IYR,KSP,I,ISZ,J,NPRM,IACTK,ISWTCH,JYR,IDC,PVG,S9B
      REAL BAMOST, TOTCRA, CWIDTH
      REAL FULIVE(2,MAXSP), FULIVI(2,MAXSP)
      REAL FUINIE(MXFLCL,MAXSP), FUINII(MXFLCL,MAXSP)
      REAL STFUEL(MXFLCL,2),XCOV(2),YLOAD(2), FOTOVAL(MXFLCL)
      REAL PRMS(12), FOTOVALS(9)
      REAL    BIGDBH,TOTBA,CAREA,ALGSLP,PRCL,ADD,DCYMLT
C----------
C     SPECIES LIST FOR CENTRAL IDAHO VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
C     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
C     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
C     4 = GRAND FIR (GF)                   ABIES GRANDIS
C     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
C     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
C     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
C     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
C     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
C    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
C    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
C    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
C    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
C    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
C    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
C    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
C    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
C    18 = OTHER SOFTWOODS (OS)
C    19 = OTHER HARDWOODS (OH)
C
C  SURROGATE EQUATION ASSIGNMENT:
C
C  FROM THE IE VARIANT:
C      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
C      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
C      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
C      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)
C
C  FROM THE UT VARIANT:
C      USE 12(WJ) FOR 14(WJ)
C      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
C                                                  REALLY WC39=OT)
C----------
C     INITIAL LIVE FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C
C                  herbs, shrubs
C----------
C
C
      DATA FULIVE /
     &            0.15,   0.1, !WP
     &             0.2,   0.2, !WL
     &             0.2,   0.2, !DF
     &            0.15,   0.1, !GF
     &             0.2,   0.2, !WH
     &             0.2,   0.2, !RC
     &             0.2,   0.1, !LP
     &            0.15,   0.2, !ES
     &            0.15,   0.2, !AF
     &             0.2,  0.25, !PP
     &             0.2,  0.10, !WB uses LP
     &             0.2,   0.2, !PY uses DF
     &            0.25,  0.25, !AS - Ottmar 2000
     &            0.04,  0.05, !WJ - Ottmar 2000
     &            0.25,  0.25, !MC - use CR AS
     &             0.2,  0.10, !LM uses UT WB/LM
     &            0.25,  0.25, !CW use AS
     &            0.15,   0.2, !OS
     &            0.25,  0.25/ !OH use AS
C----------
C     INITIAL LIVE FUEL LOADING FOR INTIALIZING STANDS WITH 10% COVER
C
C                  herbs, shrubs
C----------
      DATA FULIVI /
     &            0.30,   2.0, !WP
     &             0.4,   2.0, !WL
     &             0.4,   2.0, !DF
     &            0.30,   2.0, !GF
     &             0.4,   2.0, !WH
     &             0.4,   2.0, !RC
     &             0.4,   1.0, !LP
     &            0.30,   2.0, !ES
     &            0.30,   2.0, !AF
     &            0.25,  0.10, !PP
     &             0.4,   1.0, !WB uses LP
     &             0.4,   2.0, !PY uses DF
     &            0.18,  1.32, !AS - Ottmar 2000
     &            0.13,  1.63, !WJ - Ottmar 2000
     &            0.18,  1.32, !MC - use CR AS
     &             0.4,   1.0, !LM uses LP
     &            0.18,  1.32, !CW use AS
     &            0.30,   2.0, !OS
     &            0.18,  1.32/ !OH use AS
C----------
C     INITIAL FUEL LOADING FOR 'ESTABLISHED' STANDS WITH 60% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD
C
C      <.25   to1   1-3   3-6  6-12 12-20 20-35 35-50   >50   Lit  Duf
C----------
      DATA FUINIE /
     &  1.0,  1.0,  1.6, 10.0, 10.0, 10.0,  0.0,  0.0,  0.0,  0.8, 30.0, !WP
     &  0.9,  0.9,  1.6,  3.5,  3.5,  0.0,  0.0,  0.0,  0.0,  0.6, 10.0, !WL
     &  0.9,  0.9,  1.6,  3.5,  3.5,  0.0,  0.0,  0.0,  0.0,  0.6, 10.0, !DF
     &  0.7,  0.7,  3.0,  7.0,  7.0,  0.0,  0.0,  0.0,  0.0,  0.6, 25.0, !GF
     &  2.2,  2.2,  5.2, 15.0, 20.0, 15.0,  0.0,  0.0,  0.0,  1.0, 35.0, !WH
     &  2.2,  2.2,  5.2, 15.0, 20.0, 15.0,  0.0,  0.0,  0.0,  1.0, 35.0, !RC
     &  0.9,  0.9,  1.2,  7.0,  8.0,  0.0,  0.0,  0.0,  0.0,  0.6, 15.0, !LP
     &  1.1,  1.1,  2.2, 10.0, 10.0,  0.0,  0.0,  0.0,  0.0,  0.6, 30.0, !ES
     &  1.1,  1.1,  2.2, 10.0, 10.0,  0.0,  0.0,  0.0,  0.0,  0.6, 30.0, !AF
     &  0.7,  0.7,  1.6,  2.5,  2.5,  0.0,  0.0,  0.0,  0.0,  1.4,  5.0, !PP
     &  0.9,  0.9,  1.2,  7.0,  8.0,  0.0,  0.0,  0.0,  0.0,  0.6, 15.0, !WB use LP
     &  0.9,  0.9,  1.6,  3.5,  3.5,  0.0,  0.0,  0.0,  0.0,  0.6, 10.0, !PY use DF
     &  0.2,  0.6,  2.4,  3.6,  5.6,  0.0,  0.0,  0.0,  0.0,  1.4, 16.8, !AS - Ottmar 2000
     &  0.2,  0.8,  2.3,  1.4,  3.0,  0.0,  0.0,  0.0,  0.0,  0.5,  0.0, !WJ - Ottmar 2000
     &  0.2,  0.6,  2.4,  3.6,  5.6,  0.0,  0.0,  0.0,  0.0,  1.4, 16.8, !MC - use CR AS
     &  0.9,  0.9,  1.2,  7.0,  8.0,  0.0,  0.0,  0.0,  0.0,  0.6, 15.0, !LM use LP
     &  0.2,  0.6,  2.4,  3.6,  5.6,  0.0,  0.0,  0.0,  0.0,  1.4, 16.8, !CW use AS
     &  1.1,  1.1,  2.2, 10.0, 10.0,  0.0,  0.0,  0.0,  0.0,  0.6, 30.0, !OS
     &  0.2,  0.6,  2.4,  3.6,  5.6,  0.0,  0.0,  0.0,  0.0,  1.4, 16.8/ !OH use AS
C----------
C     INITIAL FUEL LOADING FOR 'INITIALIZING' STANDS WITH 10% COVER
C     THIS CAN BE MODIFIED BY THE *FUELINIT** KEYWORD
C
C      <.25   to1   1-3   3-6  6-12  12-20 20-35 35-50  >50   Lit  Duf
C----------
      DATA FUINII /
     &  0.6,  0.6,  0.8,  6.0,  6.0,  6.0,  0.0,  0.0,  0.0,  0.4, 12.0, !WP
     &  0.5,  0.5,  1.0,  1.4,  1.4,  0.0,  0.0,  0.0,  0.0,  0.3,  5.0, !WL
     &  0.5,  0.5,  1.0,  1.4,  1.4,  0.0,  0.0,  0.0,  0.0,  0.3,  5.0, !DF
     &  0.5,  0.5,  2.0,  2.8,  2.8,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0, !GF
     &  1.6,  1.6,  3.6,  6.0,  8.0,  6.0,  0.0,  0.0,  0.0,  0.5, 12.0, !WH
     &  1.6,  1.6,  3.6,  6.0,  8.0,  6.0,  0.0,  0.0,  0.0,  0.5, 12.0, !RC
     &  0.6,  0.7,  0.8,  2.8,  3.2,  0.0,  0.0,  0.0,  0.0,  0.3,  7.0, !LP
     &  0.7,  0.7,  1.6,  4.0,  4.0,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0, !ES
     &  0.7,  0.7,  1.6,  4.0,  4.0,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0, !AF
     &  0.1,  0.1,  0.2,  0.5,  0.5,  0.0,  0.0,  0.0,  0.0,  0.5,  0.8, !PP
     &  0.6,  0.7,  0.8,  2.8,  3.2,  0.0,  0.0,  0.0,  0.0,  0.3,  7.0, !WB use LP
     &  0.5,  0.5,  1.0,  1.4,  1.4,  0.0,  0.0,  0.0,  0.0,  0.3,  5.0, !PY use DF
     &  0.1,  0.4,  5.0,  2.2,  2.3,  0.0,  0.0,  0.0,  0.0,  0.8,  5.6, !AS - Ottmar 2000
     &  0.0,  0.1,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.0,  0.3,  0.0, !WJ - Ottmar 2000
     &  0.1,  0.4,  5.0,  2.2,  2.3,  0.0,  0.0,  0.0,  0.0,  0.8,  5.6, !MC - use CR AS
     &  0.6,  0.7,  0.8,  2.8,  3.2,  0.0,  0.0,  0.0,  0.0,  0.3,  7.0, !LM use LP
     &  0.1,  0.4,  5.0,  2.2,  2.3,  0.0,  0.0,  0.0,  0.0,  0.8,  5.6, !CW use AS
     &  0.7,  0.7,  1.6,  4.0,  4.0,  0.0,  0.0,  0.0,  0.0,  0.3, 12.0, !OS
     &  0.1,  0.4,  5.0,  2.2,  2.3,  0.0,  0.0,  0.0,  0.0,  0.8,  5.6/ !OH use AS
C----------
C     DOMINANT SPECIES FOR EACH (OPTIONAL) VEGETATION CODE.
C     DERIVED FROM VEG CODES IN ICITYP ARRAY IN **BLKDAT**
C----------
      DATA (COVINI(I), I=   1,  50) /
     &16,16,16,16,10,10,10,10,10,10,
     &10,10,10,10, 3, 3, 3, 3, 3, 3,
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3,
     & 3, 3, 3, 3, 3, 3, 3, 3, 3, 3/
      DATA (COVINI(I), I=  51, 100) /
     & 3, 3, 3, 3, 3, 8, 8, 8, 8, 8,
     & 4, 4, 4, 4, 4, 4, 4, 4, 4, 4,
     & 4, 4, 4, 4, 4, 9, 9, 9, 9, 9,
     & 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
     & 9, 9, 9, 9, 9, 9, 9, 9, 9, 9/
      DATA (COVINI(I), I= 101, 130) /
     & 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
     & 9, 9, 9, 9, 9, 9, 9, 9, 9, 9,
     & 9, 9,11,11, 7, 7, 7, 7, 7, 3/
C----------
C     PLANT VEGETATION GROUP FOR EACH OF THE ICITYP VEG
C     CODES (ICITYP ARRAY IN **BLKDAT**); PROVIDED BY
C     KATHY GEIER-HAYES.
C----------
      DATA (MAPPVG(I), I=   1,  50) /
     &10,10,10,10, 1, 1, 1, 1, 1, 1,
     & 1, 2, 2, 1, 1, 1, 1, 1, 1, 3,
     & 2, 2, 2, 4, 3, 3, 2, 4, 2, 2,
     & 4, 2, 4, 2, 4, 4, 2, 2, 4, 4,
     & 2, 4, 4, 4, 4, 4, 1, 1, 3, 3/
      DATA (MAPPVG(I), I=  51, 100) /
     & 3, 4, 4, 4, 4, 7, 7, 7, 9, 7,
     & 5, 5, 6, 6, 6, 6, 6, 6, 6, 6,
     & 5, 6, 6, 6, 6, 7, 9, 8, 8, 8,
     & 9, 9, 9, 8,10, 7, 9, 9, 9, 9,
     & 9, 8, 8, 8,10, 8, 8, 8, 7, 7/
      DATA (MAPPVG(I), I= 101, 130) /
     &10,11, 7, 7, 7, 7, 7, 7, 7,10,
     &11, 8,10, 7, 7,10,10,11,11,10,
     &10,11,11,11,10,10,10,10,10, 4/
C----------
C     HABITAT CODES THAT INCLUDE SNOWBERRY (PHMA) OR
C     NINEBARK (SYAL) ARE 1; ALL OTHERS ARE 0.
C----------
      DATA (MAPS9B(I), I=   1,  50) /
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 1, 1, 0, 0, 0, 0, 0, 0, 0,
     & 1, 1, 1, 1, 0, 0, 1, 1, 1, 0,
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MAPS9B(I), I=  51, 100) /
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 0, 0, 0, 0, 0, 0, 0, 1, 0,
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
      DATA (MAPS9B(I), I= 101, 130) /
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0/
C----------
C     HABITAT MOISTURE CODES PROVIDED BY
C     KATHY GEIER-HAYES. 0 = DRY; 1 = MESIC
C     2 = MOIST
C----------
      DATA (MAPDRY(I), I=   1,  50) /
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 0, 0, 0, 1, 0, 0, 0, 0, 0,
     & 1, 1, 0, 0, 1, 1, 0, 0, 0, 0,
     & 0, 0, 0, 0, 0, 0, 0, 0, 0, 0,
     & 0, 0, 0, 0, 0, 0, 0, 0, 1, 0/
      DATA (MAPDRY(I), I=  51, 100) /
     & 1, 0, 0, 0, 0, 1, 2, 1, 2, 0,
     & 1, 0, 1, 1, 1, 1, 1, 1, 1, 0,
     & 0, 1, 1, 1, 1, 1, 2, 1, 1, 1,
     & 2, 2, 2, 1, 0, 1, 2, 2, 2, 1,
     & 2, 1, 1, 1, 1, 1, 1, 1, 1, 1/
      DATA (MAPDRY(I), I= 101, 130) /
     & 1, 1, 1, 0, 1, 1, 1, 1, 0, 1,
     & 1, 1, 0, 0, 0, 0, 0, 0, 1, 1,
     & 1, 1, 1, 1, 1, 0, 0, 1, 0, 1/
C
      DATA MYACT / 2521, 2548, 2553 /
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'FMCBA',5,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC
    7 FORMAT(' ENTERING FMCBA CYCLE = ',I2)
C-----------
C  Zero out the cumulative variables
C----------
      COVTYP = 0
      PERCOV = 0.0
      BIGDBH = 0.0
      TOTBA  = 0.0
C----------
C  Loop through the tree list
C----------
      IF (ITRN.GT.0) THEN
C----------
C  Zero out the cummulative variables
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
     &           FMPROB(I) * DBH(I) * DBH(I) * 0.0054542
C
            IF (DBH(I) .GT. BIGDBH) BIGDBH = DBH(I)
C----------
C  Calculate the crown width of the tree and total the area
C  encompassed by all trees
C----------
            CWIDTH=CRWDTH(I)
            CAREA = 3.1415927*CWIDTH*CWIDTH/4.0
            TOTCRA = TOTCRA + CAREA*FMPROB(I)
          ENDIF
C----------
C         Use this loop to zero this variable, for lack of a better place.
C----------
          CURKIL(I) = 0.0
        ENDDO
C----------
C  Determine which species has the most basal area
C  -> that will be the cover type
C----------
        DO KSP=1,MAXSP
          IF (FMTBA(KSP) .GT. BAMOST) THEN
            BAMOST = FMTBA(KSP)
            COVTYP = KSP
          ENDIF
          TOTBA = TOTBA + FMTBA(KSP)
        ENDDO
C----------
C  Use the crown width information to determine the percent cover
C  of the stand. Use the equation sent by Nick which assumes that
C  crowns are randomly distrubuted in the stand:
C
C  PerCov = 100*(1-exp(-total crown areas per acre / sqft in an acre))
C----------
        PERCOV = 100.0 * (1.0 - EXP(-TOTCRA/43560.))
C
      ENDIF
C----------
C  If there are no trees (COVTYP=0) in cycle 1,
C  use the optional ICINDX if it is valid. Otherwise
C  issue a warning and use a default LP cover. After the
C  first cycle, use the previous cover type if no cover is
C  present.
C----------
      IF (COVTYP .EQ. 0) THEN
        IF (IYR .EQ. IY(1)) THEN
          IF (ICINDX .GE. 1 .AND. ICINDX .LE. MXVCODE)
     &      COVTYP = COVINI(ICINDX)
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
C  Load live fuels as a function of PERCOV...assume that the initiating
C  stands correspond to about 10% cover and established are 60% or more.
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
     >       2F6.3)
C----------
C  Initialize the dead fuels only for the first year of the simulation
C----------
      IF (IYR .EQ. IY(1)) THEN
C----------
C  MODIFY CWD DECAY RATE BASED ON HABITAT MOISTURE GROUP
C       0 = DRIER - LOWER RATE
C       1 = MESIC - UNCHANGED
C       2 = WETTER - HIGHER RATE
C
C  ONLY DO THIS IF DURING THE NORMAL CALL, NOT FROM SVSTART
C----------
        IF ( ISWTCH .NE. 1 ) THEN
          DCYMLT = 1.0
          SELECT CASE (MAPDRY(ICINDX))
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
              IF (SETDECAY(I,J) .LT. 0) THEN
                DKR(I,J) = DKR(I,J) * DCYMLT
                IF (I .LE. 10) TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J) 
              ENDIF
            ENDDO
          ENDDO
        ENDIF
C
Csng      IF (IYR .EQ. IY(1)) THEN
C----------
C
C  Load dead fuels as a function of PERCOV...assume that the initiating
C  stands correspond to about 10% cover and established are 60% or more.
C----------
         XCOV(1)=10.
         XCOV(2)=60.
         DO ISZ = 1,MXFLCL
            YLOAD(1)=FUINII(ISZ,COVTYP)
            YLOAD(2)=FUINIE(ISZ,COVTYP)
            STFUEL(ISZ,2) = ALGSLP(PERCOV,XCOV,YLOAD,2)
            STFUEL(ISZ,1) = 0
         ENDDO
C----------        
C  CHANGE THE INITIAL FUEL LEVELS BASED ON PHOTO SERIES INFO INPUT
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
C  IF FOTOVAL(1) IS NEGATIVE, THEN AN INVALID CODE WAS ENTERED.
C  DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C  NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.
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
C  CHANGE THE INITIAL FUEL LEVELS BASED ON INPUT FROM THE USER
C  FIRST DO FUELHARD (FUELINIT) THEN FUELSOFT
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
C  DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C  NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.
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
C  DON'T MARK EVENT DONE IF THIS IS A CALL FROM SVSTART--WILL
C  NEED TO REPROCESS EVENT WHEN CALLED FROM FMMAIN.
C----------
          IF ( ISWTCH .NE. 1 ) CALL OPDONE(J,IYR)
C
        ENDIF
C----------
C  DIVIDE THE FUELS INTO DECAY CLASSES ACCORDING TO THE RELATIVE AMOUNTS
C  OF BASAL AREA IN THE STAND.
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
C
C
C----------
C  HOOK TO ALLOW THE MAPPVG() ARRAY TO BE READ BY **FMCFMD*
C----------
      ENTRY CIPVG(PVG)
      PVG = MAPPVG(ICINDX)
      RETURN
C----------
C  HOOK TO ALLOW THE MAPS9B() ARRAY TO BE READ BY **FMCFMD*
C----------
      ENTRY CIS9B(S9B)
      S9B = MAPS9B(ICINDX)
      RETURN
C----------
C  ENTRY POINT FOR SETTING SNAGFALL/DECAY PARAMETERS WHEN FFE
C  IS NOT ACTIVE. CALLED FROM SVSTART. USED ONLY FOR SO.
C----------
      ENTRY SNGCOE
      RETURN
C
      END
