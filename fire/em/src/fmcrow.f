      SUBROUTINE FMCROW
      IMPLICIT NONE
C----------
C  **FMCROW  FIRE-EM DATE OF LAST REVISION:  12/02/09
C----------
C     CALLED FROM: FMSDIT, FMPRUN
C     CALLS        RDPSRT
C                  PCTILE
C  Purpose:
C     This subroutine calculates CROWNW(tree,size), the weight of
C     various sizes of crown material that is associated with each tree
C     record in the current stand.  These weights depend on tree
C     species, diameter, height, and crown ratio according to the
C     relationships described in Brown & Johnston, 1976, 'Debris
C     Prediction System', Fuel Science RWU 2104, which is itself
C     based primarily on Res Paper INT-197.
C
C     NOTE:  The allocation between crown size class 4 and 5 is
C            somewhat arbitrary, with class 5 currently not receiving any.
C----------
C
C  Local variable definitions:
C
C     D;        Dbh
C     H:        Height
C     HPCT:     Height PerCenTile ranking of each tree list element
C     HPOINT:   Height POINTer: specifies which tree list element has
C               the tallest trees, next tallest, and so on.
C     IC:       length of live Crown
C     SP:       SPecies
C
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
C
COMMONS
C----------
C  VARIABLE DECLARATIONS
C----------
      LOGICAL DEBUG
      INTEGER I,J,IC,ISPMAP(MAXSP),HPOINT(MAXTRE)
      INTEGER ITR,SPIW,SPIE,SNFOR,SNKOD
      REAL    D,H,HPCT(MAXTRE),HP,SG,JUNK,XV(0:5)
C----------
C     INDEX TO THE CROWN EQUATIONS USED BY THE WESTERN (FMCROWW) AND
C     EASTERN (FMCROWE) CROWN EQUATION ROUTINES. EASTERN EQUATIONS ARE
C     BASED ON SN-FFE; WESTERN ON CR-FFE (BUT ARE NEARLY UNIFORM ACROSS
C     ALL WESTERN FFE VARIANTS). IN THE TABLE BELOW, A '-' IN THE
C     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF
C
C     I   NAME                     MAPS TO          WEST   EAST
C --------------------------------------------------------------
C     1 = WHITEBARK PINE           -                14
C     2 = WESTERN LARCH            -                 8
C     3 = DOUGLAS-FIR              -                 3
C     4 = LIMBER PINE              lodgepole pine   11
C     5 = SUBALPINE LARCH          subalpine fir     1
C     6 = ROCKY MOUNTAIN JUNIPER   western juniper  16
C     7 = LODGEPOLE PINE           -                11
C     8 = ENGELMANN SPRUCE         -                18
C     9 = SUBALPINE FIR            -                 1
C    10 = PONDEROSA PINE           -                13
C    11 = GREEN ASH                cottonwood sp            60
C    12 = QUAKING ASPEN            quaking aspen            61
C    13 = BLACK COTTONWOOD         cottonwood sp            60
C    14 = BALSAM POPLAR            cottonwood sp            60
C    15 = PLAINS COTTONWOOD        cottonwood sp            60
C    16 = NARROWLEAF COTTONWOOD    cottonwood sp            60
C    17 = PAPER BIRCH              quaking aspen            61
C    18 = OTHER SOFTWOODS          R Mtn. juniper   16
C    19 = OTHER HARDWOODS          cottonwood sp            60
C----------
      DATA ISPMAP /14, 8, 3,11, 1,16,11,18, 1,13,
     >             60,61,60,60,60,60,61,16,60/
C----------
C     DEFAULT FOREST TYPE & CODE FOR WESTERN VARIANTS CALLING EASTERN
C     EQUATIONS (EASTERN DEFAULT)
C----------
      DATA SNFOR / 801 /
      DATA SNKOD /   6 /
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
        SG  = V2T(ISP(I))
C----------
C       INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
C       OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.
C----------
        DO J = 0,5
          XV(J) = 0.0
        ENDDO
C
        SELECT CASE (SPIW)
          CASE (11:17,19)
            CALL FMCROWE(SPIE,SPIW,SNFOR,SNKOD,D,H,IC,SG,XV)
          CASE DEFAULT
            CALL FMCROWW(SPIE,D,H,ITR,IC,HP,SG,XV)
        END SELECT
C----------
C       COPY TEMPORARY VALUES TO FFE ARRAY
C----------
        DO J = 0,5
          CROWNW(I,J) = XV(J)
        ENDDO
C
  999 CONTINUE
C
      RETURN
      END
C----------
C  PLACEHOLDER FOR UNUSED CALLS IN **FMCROWE**
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
C---------
C     PLACEHOLDER FOR UNUSED CALLS IN **FMCROWE**
C---------
      SUBROUTINE SEVLHT(X30,X31,L30,L31,X32,I30,L32,I31,C30)
      IMPLICIT NONE
C
      LOGICAL   L30,L31,L32,L33
      CHARACTER C30*3
      INTEGER   I30,I31
      REAL      X30,X31,X32
C
      L30 = .FALSE.
      L31 = .FALSE.
      L32 = .FALSE.
      L33 = .FALSE.
C
      C30 = '---'
C
      I30 = 0
      I31 = 0
C
      X30 = 0.0
      X31 = 0.0
      X32 = 0.0
C
      RETURN
      END
