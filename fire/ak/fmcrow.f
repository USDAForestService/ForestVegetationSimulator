      SUBROUTINE FMCROW
      IMPLICIT NONE
C----------
C FIRE-AK $Id$
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
C     BASED ON LS-FFE; IN THE TABLE BELOW, A '-' IN THE
C     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF
C
C                                  MAPPPED TO
C     I   SPECIES                  WEST   EAST
C --------------------------------------------------------------
C     1   pacific silver fir         4      
C     2   subalpine fir              1      
C     3   alaska-cedar               8      
C     4   tamarack                         10
C     5   white spruce                      6
C     6   Lutz's spruce                     6
C     7   black spruce                      9
C     8   sitka spruce              18      
C     9   lodgepole pine            11      
C    10   western redcedar           7      
C    11   western hemlock            6      
C    12   mountain hemlock          24      
C    13   other softwoods                   6
C    14   alder species             23      
C    15   red alder                 23      
C    16   paper birch                      43
C    17   Alaska birch                     43
C    18   balsam poplar                    42
C    19   quaking aspen                    41
C    20   black cottonwood                 17
C    21   willow species                   64
C    22   Scouler's willow                 64
C    23   other hardwoods                  17
C --------------------------------------------------------------

      DATA ISPMAP / 4,  1, 8, 10,  6,  6,  9, 18, 11,  7,
     &              6, 24, 6, 23, 23, 43, 43, 42, 41, 17,
     &             64, 64, 17 /

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
C
C         SPECIES THAT USE CROWN ROUTINE FOR THE EAST
          CASE (4:7,13,16:)
            CALL FMCROWE(SPIE,SPIW,D,H,IC,SG,XV)
C
C         SPECIES THAT USE CROWN ROUTINE FOR THE WEST
          CASE DEFAULT
            CALL FMCROWW(SPIE,D,H,ITR,IC,HP,SG,XV)
        END SELECT

C       COPY TEMPORARY VALUES TO FFE ARRAY

        DO J = 0,5
          CROWNW(I,J) = XV(J)
          IF (DEBUG) WRITE(JOSTND,*) 'I=',I,' size=',J,
     &    ' CROWNW=',CROWNW(I,J)
        ENDDO

  999 CONTINUE

      RETURN
      END
C
C
C----------
C  PLACEHOLDER FOR UNUSED CALLS IN **FMCROWE**
C----------
C      SUBROUTINE HTDBH(I10,I11,X10,X11,I12)
C      IMPLICIT NONE
C
C      INTEGER I10,I11,I12
C      REAL    X10,X11
C
C      I10 = 0
C      I11 = 0
C      I12 = 0
C
C      X10 = 0.0
C      X11 = 0.0
C
C      RETURN
C      END

