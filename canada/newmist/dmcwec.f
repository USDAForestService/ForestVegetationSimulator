      SUBROUTINE DMCW(DMTRCW)
      IMPLICIT NONE
C----------
C CANADA-NEWMIST $Id$
C----------
C **DMCW --NISI Date of last revision: 08/05/94
C This module has been made by modifing the COVER MODEL module CVCW
C  Modified for East Cascades.
C--------------------------------------------------------------------
C Purpose:
C   Computes crown width for individual trees. Widths are measured
C in feet. Further documentation can be found in: Moeur, Melinda.
C 1981. Crown width and foliage weight of northern Rocky Mountain
C Confifers. USDA Forest Service Res. Pap. INT-283.                 
C--------------------------------------------------------------------
C
C Called by:
C
C     DMMTRX
C
C Other routines called:
C
C     DBCHK
C
C Argument list definitions:                        
C
C     REAL    DMTRCW (O)  Predicted maximum crown width (feet).
C
C Local variable definitions (not complete):
C
C     REAL    BH1         COEFFICIENTS FOR HEIGHT TERM FOR CROWN
C                          WIDTH FUNCTION FOR TREES LESS THAN
C                          3.5 INCHES DBH.
C     REAL    BINT2       INTERCEPTS FOR CROWN WIDTH FUNCTION FOR.
C                          TREES 3.5 INCHES AND LARGER
C     REAL    BH2         COEFFICIENTS FOR HEIGHT TERM FOR TREES
C                          .GE. 3.5 INCHES.
C     REAL    BAREA       STAND BASAL AREA (sq feet?).
C     REAL    CL          CROWN LENGTH (feet).
C     REAL    D           TREE DBH (inches).
C     REAL    H           TREE HEIGHT (feet).
C
C Common block variables and parameters:
C
C     [none related to NISI; FVS commons are not documented]
C
C********************************************************************C

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'DMCOM.F77'      

C Argument list variables

      REAL      DMTRCW
      
      DIMENSION DMTRCW(MAXTRE)
      
C Local variables.
      
      LOGICAL   DEBUG
      DIMENSION BH1(MAXSP)
      DIMENSION BINT2(MAXSP)
      DIMENSION BH2(MAXSP)

C Data assignments.      

      DATA BH1   / 0.37031,  0.23846,  0.32874,  0.38503,
     &             0.46452,  0.38503,  0.26342,  0.33089,
     &             0.33722,  0.36380,  0.07049  /

      DATA BINT2 / 4.30800,  2.31359,  3.02271,  2.20611,
     &             2.79784,  2.20611,  1.06804,  3.76535,
     &             1.74558,  1.62365, -0.91984  /

      DATA   BH2 /-1.37265, -0.80919, -1.00486, -0.76936,
     &            -0.89666, -0.76936, -0.55987, -1.18257,
     &            -0.73972, -0.68098, -0.07299  /

C
C  CHECK FOR DEBUG.
C
      CALL DBCHK(DEBUG,'DMCW',4,ICYC)
      IF (DEBUG) WRITE (JOSTND,9000) ICYC
 9000 FORMAT (/' **CALLING DMCW, CYCLE = ',I2 / '         I      ISPI',
     & '         D         H        CL     BAREA   DMTRCW')
C
C     RETURN IF NOTREES OPTION IN EFFECT.
C
      IF (ITRN .GT. 0) GOTO 5
      IF (DEBUG) WRITE (JOSTND,9001) ITRN
 9001 FORMAT (' ITRN =', I5,' : NOTREES : RETURN TO **DMMTRX**')
      RETURN
    5 CONTINUE
C
C  USE PRE-THIN DENSITY STATISTICS IF A THINNING HAS JUST OCCURRED.
C
      BAREA = BA
      ALOGBA = ALOG(BAREA)
C
C  ENTER TREE LOOP
C
      DO 100 I = 1,ITRN
        D = DBH(I)
        H = HT(I)
        ISPI = ISP(I)
        IICR = ICR(I)
        CL = FLOAT(IICR)*H/100.
C
C  BRANCH ON DBH
C
        IF (D .LT. 3.5) GOTO 10
C
C  COMPUTE CROWN WIDTH FOR TREES 3.5 INCHES AND LARGER
C
        DMTRCW(I) = EXP (BINT2(ISPI) +  1.08137*ALOG(D) +
     &                BH2(ISPI)*ALOG(H) + 0.29786*ALOG(CL))
C
C  CORRECT ESTIMATE FOR NEGATIVE BIAS. (BASKERVILLE 1972)
C  BIAS ADJUSTMENT = EXP(.5*MSE) = EXP (.5*.04898)
C
        DMTRCW(I) = DMTRCW(I) * 1.02479
        GOTO 90
C
C  COMPUTE CROWN WIDTH FOR TREES LESS THAN 3.5 INCHES
C
   10   CONTINUE

        DMTRCW(I) = EXP (BH1(ISPI)*ALOG(H) + 0.28283
     &                *ALOG(CL) + 0.04032*ALOGBA)
C----------
C  CORRECT ESTIMATE FOR NEGATIVE BIAS
C  BIAS ADJUSTMENT = EXP(.5*MSE) = EXP(.5*.06036)
C----------
        DMTRCW(I) = DMTRCW(I)*1.03064
C----------
   90   CONTINUE
C----------
        IF (DEBUG) WRITE (JOSTND,9002) I,ISPI,D,H,CL,BAREA,
     &                DMTRCW(I)
 9002   FORMAT (2I10,5F10.1)
C
  100   CONTINUE

      RETURN
      END
