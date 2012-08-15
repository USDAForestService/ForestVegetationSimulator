      SUBROUTINE FMCROW
      IMPLICIT NONE
C----------
C  **FMCROW  FIRE-PN DATE OF LAST REVISION:  01/10/12
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
*
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
*  Common block variables and parameters:
*
***********************************************************************

C.... Parameter include files.
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... Common include files.
      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'

C.... Parameter statements.

      LOGICAL DEBUG
      INTEGER I,J,IC,ISPMAP(MAXSP),HPOINT(MAXTRE)
      INTEGER ITR,SPIW,SPIE
      REAL    D,H,HPCT(MAXTRE),HP,SG,JUNK,XV(0:5)

C     INDEX TO THE CROWN EQUATIONS USED BY THE WESTERN (FMCROWW) AND
C     EASTERN (FMCROWE) CROWN EQUATION ROUTINES. EASTERN EQUATIONS ARE
C     BASED ON LS-FFE; WESTERN ON CR-FFE (BUT ARE NEARLY UNIFORM ACROSS
C     ALL WESTERN FFE VARIANTS). IN THE TABLE BELOW, A '-' IN THE
C     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF

C     I   NAME                     MAPS TO          WEST   EAST
C --------------------------------------------------------------
C     1 = PACIFIC SILVER FIR       grand fir         4
C     2 = WHITE FIR                grand fir         4
C     3 = GRAND FIR                -                 4
C     4 = SUBALPINE FIR            -                 1
C     5 = CALIFORNIA RED FIR/      grand fir         4
C         SHASTA RED FIR
C     6 = SITKA SPRUCE             Engelmann spruce 18
C     7 = NOBLE FIR                grand fir         4
C     8 = ALASKA CEDAR/W LARCH     w larch           8
C     9 = INCENSE-CEDAR            -                20
C    10 = ENGELMANN SPRUCE         -                18
C    11 = LODGEPOLE PINE           -                11
C    12 = JEFFREY PINE             w white pine     15
C    13 = SUGAR PINE               w white pine     15
C    14 = WESTERN WHITE PINE       -                15
C    15 = PONDEROSA PINE           -                13
C    16 = DOUGLAS-FIR              -                 3
C    17 = COAST REDWOOD            giant sequoia    19
C    18 = WESTERN REDCEDAR         -                 7
C    19 = WESTERN HEMLOCK          -                 6
C    20 = MOUNTAIN HEMLOCK         -                24
C    21 = BIGLEAF MAPLE            -                 5
C    22 = RED ALDER                -                23
C    23 = WH. ALDER/PAC.MADRONE    -                10
C    24 = PAPER BIRCH              -                        43
C    25 = GIANT CHINKAPIN/TANOAK   tanoak           17
C    26 = QUAKING ASPEN            -                        41
C    27 = BLACK COTTONWOOD         eastern cottonwood       17
C    28 = OR.WH OAK/CA.BL OAK      tanoak           17
C    29 = WESTERN JUNIPER          R.M. juniper     16
C    30 = SUBALPINE LARCH          subalpine fir     1
C    31 = WHITEBARK PINE           -                14
C    32 = KNOBCONE PINE            lodgepole pine   11
C    33 = PACIFIC YEW              western redcedar  7
C    34 = PACIFIC DOGWOOD          flowering dogwood        56
C    35 = HAWTHORN                 hawthorn sp              57
C    36 = BITTER CHERRY            pin cherry               61
C    37 = WILLOW                   willow sp                64
C    38 = ---                      -                         0
C    39 = OTHER                    quaking aspen            41
C --------------------------------------------------------------

      DATA ISPMAP / 4, 4, 4, 1, 4,18, 4, 8,20,18,
     >             11,15,15,15,13, 3,19, 7, 6,24,
     >              5,23,10,43,17,41,17,17,16, 1,
     >             14,11, 7,56,57,61,64, 0,41/


C     CHECK FOR DEBUG

      CALL DBCHK (DEBUG,'FMCROW',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,ITRN
    7 FORMAT(' ENTERING FMCROW CYCLE = ',I2,' ITRN=',I5)

      IF (ITRN.EQ.0) RETURN

C     YOU'LL NEED TO KNOW PERCENTILE HEIGHT OF EACH TREE.
C     TO GET THIS, MAKE AN ARRAY THAT LISTS THE TREE LIST ELEMENTS
C     IN DESCENDING ORDER BY THE HEIGHT OF EACH RECORD:

      CALL RDPSRT(ITRN,HT,HPOINT,.TRUE.)

C     NOW CALL PCTILE TO GET THE HEIGHT PERCENTILE OF EACH RECORD.
C     NOTE THAT PCTILE ONLY WORKS IF YOU PASS IT THE DENSITY OF TREES IN
C     EACH RECORD RATHER THAN THE HEIGHT OF THE RECORD. NOTE ALSO THAT
C     ANY RECORDS WITH NO TREES WILL NONETHELESS COME BACK WITH THE
C     PERCENTILE RANKING IMPLIED BY THEIR HEIGHT. SUCH RECORDS WILL NOT
C     INFLUENCE THE PERCENTILE RANKING OF TREES IN OTHER RECORDS.

      CALL PCTILE (ITRN, HPOINT, PROB, HPCT, JUNK)

      DO 999 I = 1,ITRN

C       INCREMENT GROW TO KEEP TRACK OF WHETHER THIS CROWN IS FREE
C       TO GROW AFTER BEING BURNED IN A FIRE.  SKIP THE REST OF THE LOOP
C       IF GROW IS STILL LESS THAN 1 AFTER THE INCREMENT.

        IF (GROW(I) .LT. 1) GROW(I) = GROW(I) + 1
        IF (GROW(I) .LT. 1) GOTO 999

C       ARGUMENTS TO PASS

        SPIW = ISP(I)
        SPIE = ISPMAP(SPIW)

        D   = DBH(I)
        H   = HT(I)
        IC  = ICR(I)
        ITR = ITRUNC(I)
        HP  = HPCT(I)
        SG  = V2T(SPIW)

C       INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
C       OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.

        DO J = 0,5
          XV(J) = 0.0
        ENDDO

        SELECT CASE (SPIW)
          CASE (24,26,27,34,35,36,37,39)
            CALL FMCROWE(SPIE,SPIW,D,H,IC,SG,XV)
          CASE DEFAULT
            CALL FMCROWW(SPIE,D,H,ITR,IC,HP,SG,XV)
        END SELECT

C       COPY TEMPORARY VALUES TO FFE ARRAY

        DO J = 0,5
          CROWNW(I,J) = XV(J)
        ENDDO

  999 CONTINUE

      RETURN
      END


