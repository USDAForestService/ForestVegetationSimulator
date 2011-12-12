      SUBROUTINE FMCROW
      IMPLICIT NONE
C----------
C  **FMCROW  FIRE-WS DATE OF LAST REVISION:  02/11/11
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
*  Common block variables and parameters:
*
***********************************************************************
C----------
COMMONS
C
C.... Parameter include files.
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... Common include files.
      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
C
COMMONS
C----------
C.... Parameter statements.

      LOGICAL DEBUG
      INTEGER I,J,IC,ISPMAP(MAXSP),HPOINT(MAXTRE)
      INTEGER ITR,SPIW,SPIE,SNFOR,SNKOD
      REAL    D,H,HPCT(MAXTRE),HP,SG,JUNK,XV(0:5)
C
C     INDEX TO THE CROWN EQUATIONS USED BY THE WESTERN (FMCROWW) AND
C     EASTERN (FMCROWE) CROWN EQUATION ROUTINES. EASTERN EQUATIONS ARE
C     BASED ON SN-FFE; WESTERN ON CR-FFE (BUT ARE NEARLY UNIFORM ACROSS
C     ALL WESTERN FFE VARIANTS). IN THE TABLE BELOW, A '-' IN THE
C     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF
C
C     I   NAME                     MAPS TO          WEST   EAST
C --------------------------------------------------------------
C     1 = SUGAR PINE               w white pine     15
C     2 = DOUGLAS-FIR              -                 3
C     3 = WHITE FIR                grand fir         4
C     4 = GIANT SEQUOIA            -                19
C     5 = INCENSE CEDAR            -                20
C     6 = JEFFREY PINE             w white pine     15
C     7 = RED FIR                  grand fir         4
C     8 = PONDEROSA PINE           -                13
C     9 = LODGEPOLE PINE           -                11
C    10 = WHITEBARK PINE           -                14
C    11 = WESTERN WHITE PINE       -                15
C    12 = SINGLELEAF PINYON        pinyon pine      12
C    13 = PACIFIC SILVER FIR       grand fir         4
C    14 = KNOBCONE PINE            lodgepole pine   11
C    15 = FOXTAIL PINE             lodgepole pine   11
C    16 = COULTER PINE             lodgepole pine   11
C    17 = LIMBER PINE              lodgepole pine   11
C    18 = MONTEREY PINE            ponderosa pine   13
C    19 = GRAY/CA FOOTHILL PINE    lodgepole pine   11
C    20 = WASHOE PINE              lodgepole pine   11
C    21 = GB BRISTLECONE PINE      -                 9
C    22 = BIGCONE DOUGLAS-FIR      Douglas-fir       3
C    23 = REDWOOD                  giant sequoia    19
C    24 = MOUNTAIN HEMLOCK         -                24
C    25 = WESTERN JUNIPER          R.M. juniper     16
C    26 = UTAH JUNIPER             lodgepole pine   16
C    27 = CALIFORNIA JUNIPER       lodgepole pine   16
C    28 = CALIFORNIA LIVE OAK      tanoak           17
C    29 = CANYON LIVE OAK          tanoak           17
C    30 = BLUE OAK                 CA black oak     21
C    31 = CALIFORNIA BLACK OAK     -                21
C    32 = CA WHITE/VALLEY OAK      CA black oak     21
C    33 = INTERIOR LIVE OAK        tanoak           17
C    34 = TANOAK                   tanoak           17
C    35 = GIANT CHINKAPIN          tanoak           17
C    36 = QUAKING ASPEN            big tooth aspen           61
C    37 = CALIFORNIA-LAUREL        tanoak           17
C    38 = PACIFIC MADRONE          -                10
C    39 = PACIFIC DOGWOOD          flowering dogwood         31
C    40 = BIGLEAF MAPLE            bigleaf maple     5
C    41 = CURL-LF. MT. MAHOG.      big tooth aspen           61
C    42 = OTHER SOFTWOODS          lodgepole pine   11
C    43 = OTHER HARDWOODS          CA black oak     21
C
      DATA ISPMAP/
     > 15,  3,  4, 19, 20, 15,  4, 13, 11, 14,
     > 15, 12,  4, 11, 11, 11, 11, 13, 11, 11,
     >  9,  3, 19, 24, 16, 16, 16, 17, 17, 21,
     > 21, 21, 17, 17, 17, 61, 17, 10, 31,  5,
     > 61, 11, 21/
     
C     DEFAULT FOREST TYPE & CODE FOR WESTERN VARIANTS CALLING EASTERN
C     EQUATIONS (EASTERN DEFAULT)

      DATA SNFOR / 801 /
      DATA SNKOD /   6 /

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
        SG  = V2T(ISP(I))

C       INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
C       OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.

        DO J = 0,5
          XV(J) = 0.0
        ENDDO

        SELECT CASE (SPIW)
          CASE (36,39,41)
            CALL FMCROWE(SPIE,SPIW,SNFOR,SNKOD,D,H,IC,SG,XV)
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
