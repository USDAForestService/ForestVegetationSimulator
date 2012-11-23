      SUBROUTINE DMCW(DMTRCW)
      
C********************************************************************C
C **DMCW --NISI Date of last revision: 15-July-2008
C This module has been made by modifing the COVER MODEL module CVCW
C  Modified for Northern Idaho.
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
C  ENTER TREE LOOP
C
      DO 100 I = 1,ITRN
        DMTRCW(I) = CRWDTH(I)
        IF (DEBUG) WRITE (JOSTND,9002) I,ISPI,D,H,CL,BAREA,
     &                DMTRCW(I)
 9002   FORMAT (2I10,5F10.1)
C
  100   CONTINUE

      RETURN
      END
