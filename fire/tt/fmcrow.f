      SUBROUTINE FMCROW
      IMPLICIT NONE
C----------
C FIRE-TT $Id$
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
*     ITR:      truncation height (aspen only)
*     IC:       length of live Crown
*     SP:       SPecies
*
*  Common block variables and parameters:
*
***********************************************************************
C----------
COMMONS
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
      INTEGER ITR,SPIW,SPIE
      REAL    D,H,HPCT(MAXTRE),HP,SG,JUNK,XV(0:5)
C----------
C SPECIES ORDER FOR TETONS VARIANT:
C
C  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
C 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH
C
C VARIANT EXPANSION:
C BS USES ES EQUATIONS FROM TT
C PM USES PI (COMMON PINYON) EQUATIONS FROM UT
C PP USES PP EQUATIONS FROM CI
C UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
C BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
C MM USES MM EQUATIONS FROM IE
C NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
C MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
C OS USES OT (OTHER SP.) EQUATIONS FROM TT
C----------
C
C     INDEX TO THE CROWN EQUATIONS USED BY THE WESTERN (FMCROWW) AND
C     EASTERN (FMCROWE) CROWN EQUATION ROUTINES. EASTERN EQUATIONS ARE
C     BASED ON LS-FFE; WESTERN ON CR-FFE (BUT ARE NEARLY UNIFORM ACROSS
C     ALL WESTERN FFE VARIANTS). IN THE TABLE BELOW, A '-' IN THE
C     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF
C
C     I   NAME                     MAPS TO          WEST   EAST
C --------------------------------------------------------------
C     1 = WHITEBARK PINE           -                14
C     2 = LIMBER PINE              lodgepole pine   11
C     3 = DOUGLAS-FIR              -                 3
C     4 = SINGLELEAF PINYON        -                12
C     5 = BLUE SPRUCE              -                18
C     6 = QUAKING ASPEN            -                        41
C     7 = LODGEPOLE PINE           -                11
C     8 = ENGELMANN SPRUCE         -                18
C     9 = SUBALPINE FIR            -                 1
C    10 = PONDEROSA PINE           -                13
C    11 = UTAH JUNIPER             R Mtn juniper    16
C    12 = ROCKY MTN JUNIPER        R Mtn juniper    16
C    13 = BIGTOOTH MAPLE           bigleaf maple     5
C    14 = ROCKY MTN MAPLE          bigleaf maple     5
C    15 = NARROWLEAF COTTONWOOD    eastern cottonwood       17
C    16 = CURLLEAF MTN-MAHOGANY    quaking aspen            41
C    17 = OTHER SOFTWOODS          whitebark pine   14
C    18 = OTHER HARDWOODS          eastern cottonwood       17
C
C----------
      DATA ISPMAP /
     > 14, 11,  3, 12, 18, 41, 11, 18,  1, 13,
     > 16, 16,  5,  5, 17, 41, 14, 17/

C----------
C     CHECK FOR DEBUG
C----------
      CALL DBCHK (DEBUG,'FMCROW',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,ITRN
    7 FORMAT(' ENTERING FMCROW CYCLE = ',I2,' ITRN=',I5)
C
      IF (ITRN.EQ.0) RETURN
C----------
C     YOU'LL NEED TO KNOW PERCENTILE HEIGHT OF EACH TREE.
C     TO GET THIS, MAKE AN ARRAY THAT LISTS THE TREE LIST ELEMENTS
C     IN DESCENDING ORDER BY THE HEIGHT OF EACH RECORD:
C----------
      CALL RDPSRT(ITRN,HT,HPOINT,.TRUE.)
C----------
C     NOW CALL PCTILE TO GET THE HEIGHT PERCENTILE OF EACH RECORD.
C     NOTE THAT PCTILE ONLY WORKS IF YOU PASS IT THE DENSITY OF TREES IN
C     EACH RECORD RATHER THAN THE HEIGHT OF THE RECORD. NOTE ALSO THAT
C     ANY RECORDS WITH NO TREES WILL NONETHELESS COME BACK WITH THE
C     PERCENTILE RANKING IMPLIED BY THEIR HEIGHT. SUCH RECORDS WILL NOT
C     INFLUENCE THE PERCENTILE RANKING OF TREES IN OTHER RECORDS.
C----------
      CALL PCTILE (ITRN, HPOINT, PROB, HPCT, JUNK)
C
      DO 999 I = 1,ITRN
C----------
C       INCREMENT GROW TO KEEP TRACK OF WHETHER THIS CROWN IS FREE
C       TO GROW AFTER BEING BURNED IN A FIRE.  SKIP THE REST OF THE LOOP
C       IF GROW IS STILL LESS THAN 1 AFTER THE INCREMENT.
C----------
        IF (GROW(I) .LT. 1) GROW(I) = GROW(I) + 1
        IF (GROW(I) .LT. 1) GOTO 999
C----------
C        ARGUMENTS TO PASS
C----------
        SPIW = ISP(I)
        SPIE = ISPMAP(SPIW)
C
        D   = DBH(I)
        H   = HT(I)
        IC  = ICR(I)
        ITR = ITRUNC(I)
        HP  = HPCT(I)
        SG  = V2T(SPIW)
C----------
C       INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
C       OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.
C----------
        DO J = 0,5
          XV(J) = 0.0
        ENDDO
C
        SELECT CASE (ISP(I))
          CASE (6,15,16,18)
            CALL FMCROWE(SPIE,SPIW,D,H,IC,SG,XV)
          CASE DEFAULT
            CALL FMCROWW(SPIE,D,H,ITR,IC,HP,SG,XV)
        END SELECT
C----------
C       COPY TEMPORARY VALUES TO FFE ARRAY
C----------
        DO J = 0,5
          CROWNW(I,J) = XV(J)
          IF (DEBUG) WRITE(JOSTND,*) 'I=',I,' size=',J,
     &    ' CROWNW=',CROWNW(I,J)
        ENDDO
C
  999 CONTINUE
C
      RETURN
      END
C----------
C     PLACEHOLDER FOR UNUSED CALLS IN **FMCROWE**
C----------
      SUBROUTINE HTDBH(I10,I11,X10,X11,I12)
      IMPLICIT NONE
C
      INTEGER I10,I11,I12
      REAL    X10,X11
C
      I10 = 0
      I11 = 0
      I12 = 0
C
      X10 = 0.0
      X11 = 0.0
C
      RETURN
      END

