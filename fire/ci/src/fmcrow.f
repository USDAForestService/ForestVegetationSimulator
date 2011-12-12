      SUBROUTINE FMCROW
      IMPLICIT NONE
C----------
C  **FMCROW  FIRE-NI DATE OF LAST REVISION:  09/08/11
C----------
C     CALLED FROM: FMSDIT, FMPRUN
C     CALLS        RDPSRT
C                  PCTILE
*  Purpose:
*     This subroutine calculates CROWNW(tree,size), the weight of
*     various sizes of crown material that is associated with each tree
*     record in the current stand.  These weights depend on tree
*     species, diameter, height, and crown ratio according to the
*     relationships described in Brown & Johnston, 1976, 'Debris
*     Prediction System', Fuel Science RWU 2104, which is itself
*     based primarily on Res Paper INT-197.
*
*     NOTE:  The allocation between crown size class 4 and 5 is
*            somewhat arbitrary, with class 5 currently not receiving any.
*------------------------------------------------------------------------
*
*  Local variable definitions:
*
*     D;        Dbh
*     H:        Height
*     HPCT:     Height PerCenTile ranking of each tree list element
*     HPOINT:   Height POINTer: specifies which tree list element has
*               the tallest trees, next tallest, and so on.
*     IC:       length of live Crown
*     SP:       SPecies
*
***********************************************************************
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
COMMONS
C----------
      LOGICAL DEBUG
      INTEGER I,J,IC,ISPMAP(MAXSP),HPOINT(MAXTRE)
      INTEGER ITR,SPIW,SPIE,SNFOR,SNKOD
      REAL    D,H,HPCT(MAXTRE),HP,SG,JUNK,XV(0:5)
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
C     INDEX TO THE CROWN EQUATIONS USED BY THE WESTERN (FMCROWW) AND
C     EASTERN (FMCROWE) CROWN EQUATION ROUTINES. EASTERN EQUATIONS ARE
C     BASED ON SN-FFE; WESTERN ON CR-FFE (BUT ARE NEARLY UNIFORM ACROSS
C     ALL WESTERN FFE VARIANTS). IN THE TABLE BELOW, A '-' IN THE
C     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF
C
C     I   NAME                     MAPS TO          WEST   EAST
C --------------------------------------------------------------
C     1 = WESTERN WHITE PINE       -                15
C     2 = WESTERN LARCH            -                 8
C     3 = DOUGLAS-FIR              -                 3
C     4 = GRAND FIR                -                 4
C     5 = WESTERN HEMLOCK          -                 6
C     6 = WESTERN REDCEDAR         -                 7
C     7 = LODGEPOLE PINE           -                11
C     8 = ENGELMANN SPRUCE         -                18
C     9 = SUBALPINE FIR            -                 1
C    10 = PONDEROSA PINE           -                13
C    11 = WHITEBARK PINE           -                14
C    12 = PACIFIC YEW              western redcedar  7
C    13 = QUAKING ASPEN            bigtooth aspen           61 
C    14 = WESTERN JUNIPER          R Mtn juniper    16
C    15 = CURLLEAF MTN MAHOGANY    bigtooth aspen           61
C    16 = LIMBER PINE              lodgepole pine   11
C    17 = BLACK COTTONWOOD         cottonwood sp            60
C    18 = OTHER SOFTWOODS          mountain hemlock 24
C    19 = OTHER HARDWOODS          cottonwood sp            60
C----------
      DATA ISPMAP /
     > 15,  8,  3,  4,  6,  7, 11, 18,  1, 13,
     > 14,  7, 61, 16, 61, 11, 60, 24, 60/
C
C     DEFAULT FOREST TYPE & CODE FOR WESTERN VARIANTS CALLING EASTERN
C     EQUATIONS (EASTERN DEFAULT)

      DATA SNFOR / 801 /
      DATA SNKOD /   6 /
C----------
C     CHECK FOR DEBUG
C----------
      CALL DBCHK (DEBUG,'FMCROW',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,ITRN
    7 FORMAT(' ENTERING FMCROW CYCLE = ',I2,' ITRN=',I5)

      IF (ITRN.EQ.0) RETURN
C----------
C  YOU'LL NEED TO KNOW PERCENTILE HEIGHT OF EACH TREE.
C  TO GET THIS, MAKE AN ARRAY THAT LISTS THE TREE LIST ELEMENTS
C  IN DESCENDING ORDER BY THE HEIGHT OF EACH RECORD:
C----------
      CALL RDPSRT(ITRN,HT,HPOINT,.TRUE.)
C----------
C  NOW CALL PCTILE TO GET THE HEIGHT PERCENTILE OF EACH RECORD.
C  NOTE THAT PCTILE ONLY WORKS IF YOU PASS IT THE DENSITY OF TREES IN
C  EACH RECORD RATHER THAN THE HEIGHT OF THE RECORD. NOTE ALSO THAT
C  ANY RECORDS WITH NO TREES WILL NONETHELESS COME BACK WITH THE
C  PERCENTILE RANKING IMPLIED BY THEIR HEIGHT. SUCH RECORDS WILL NOT
C  INFLUENCE THE PERCENTILE RANKING OF TREES IN OTHER RECORDS.
C----------
      CALL PCTILE (ITRN, HPOINT, PROB, HPCT, JUNK)

      DO 999 I = 1,ITRN
C----------
C  INCREMENT GROW TO KEEP TRACK OF WHETHER THIS CROWN IS FREE
C  TO GROW AFTER BEING BURNED IN A FIRE.  SKIP THE REST OF THE LOOP
C  IF GROW IS STILL LESS THAN 1 AFTER THE INCREMENT.
C----------
        IF (GROW(I) .LT. 1) GROW(I) = GROW(I) + 1
        IF (GROW(I) .LT. 1) GOTO 999
C----------
C  ARGUMENTS TO PASS
C----------
        SPIW = ISP(I)
        SPIE = ISPMAP(SPIW)
C
        D   = DBH(I)
        H   = HT(I)
        IC  = ICR(I)
        ITR = ITRUNC(I)
        HP  = HPCT(I)
        SG  = V2T(ISP(I))
C----------
C  INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
C  OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.
C----------
        DO J = 0,5
          XV(J) = 0.0
        ENDDO
C
        SELECT CASE (SPIW)
          CASE (13,15,17,19)
            CALL FMCROWE(SPIE,SPIW,SNFOR,SNKOD,D,H,IC,SG,XV)
          CASE DEFAULT
            CALL FMCROWW(SPIE,D,H,ITR,IC,HP,SG,XV)
        END SELECT
C----------
C  COPY TEMPORARY VALUES TO FFE ARRAY
C----------
        DO J = 0,5
          CROWNW(I,J) = XV(J)
        ENDDO
C
  999 CONTINUE
C
      RETURN
      END

C     PLACEHOLDER FOR UNUSED CALLS IN **FMCROWE**

      SUBROUTINE HTDBH(I10,I11,X10,X11,I12)
      IMPLICIT NONE

      INTEGER I10,I11,I12
      REAL    X10,X11

      I10 = 0
      I11 = 0
      I12 = 0

      X10 = 0.0
      X11 = 0.0

      RETURN
      END

C     PLACEHOLDER FOR UNUSED CALLS IN **FMCROWE**

      SUBROUTINE SEVLHT(X30,X31,L30,L31,X32,I30,L32,I31,C30)
      IMPLICIT NONE

      LOGICAL   L30,L31,L32,L33
      CHARACTER C30*3
      INTEGER   I30,I31
      REAL      X30,X31,X32

      L30 = .FALSE.
      L31 = .FALSE.
      L32 = .FALSE.
      L33 = .FALSE.

      C30 = '---'

      I30 = 0
      I31 = 0

      X30 = 0.0
      X31 = 0.0
      X32 = 0.0

      RETURN
      END
