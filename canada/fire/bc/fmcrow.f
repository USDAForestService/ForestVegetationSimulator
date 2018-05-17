      SUBROUTINE FMCROW
      IMPLICIT NONE
C
C  $Id$
C
C----------
C  **FMCROW  FIRE-BC
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

C.... Parameter include files.
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... Common include files.
      INCLUDE 'FMCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'

C.... Parameter statements.

      LOGICAL DEBUG
      INTEGER I,J,IC,ISPMAP(MAXSP),SPI,HPOINT(MAXTRE)
      INTEGER ITR
      REAL    D,H,HPCT(MAXTRE),HP,SG,JUNK,XV(0:5)
C
C     INDEX TO THE CROWN EQUATIONS USED BY THE WESTERN (FMCROWW) AND
C     EASTERN (FMCROWE) CROWN EQUATION ROUTINES. EASTERN EQUATIONS ARE
C     BASED ON LS-FFE; WESTERN ON CR-FFE (BUT ARE NEARLY UNIFORM ACROSS
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
C    11 = BIRCH (PB)               aspen                    61
C    12 = ASPEN (AS)                                        61
C    13 = COTTONWOOD (CW)          cottonwood sp            60 
C    14 = OTHER CONIFER (OC)       FD                3
C    15 = OTHER HARDWOOD (OH)      EP                       61
C
      DATA ISPMAP /15, 8, 3, 4, 6, 7,11,18, 1,13,
     >             61,61,60, 3,61/
C
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

        SPI = ISPMAP(ISP(I))

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

C       IN BC, 60/61 map to 17 (alder, cottonwood) in eastern crown equations.
        SELECT CASE (SPI)
	    CASE (60,61)
	      CALL FMCROWE(17,ISP(I),D,H,IC,SG,XV)
	    CASE DEFAULT
	      CALL FMCROWW(SPI,D,H,ITR,IC,HP,SG,XV)
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
C----------
C  PLACEHOLDER FOR UNUSED CALLS IN **FMCROWE**
C----------
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
