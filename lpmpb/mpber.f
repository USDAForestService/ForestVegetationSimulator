      SUBROUTINE MPBER (NOER)
      IMPLICIT NONE
C----------
C LPMPB $Id$
C----------
C
C
C     NOER  = TRUE WHEN MINIMUM CONDITIONS MET FOR AN OUTBREAK
C             IE NOT AN 'ERROR'
C
C
C Revision History
C   05/31/00 GEB
C     Adding variable IDXLP, the index of Lodgepole pine in arrays
C     Defined in mpblkd.f, which is variant dependent
C   12/01/98 RNH
C     Adapated to 24 species (CR variant)
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C----------
      LOGICAL NOER
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'MPBCOM.F77'
C
C
COMMONS

      INTEGER I, I1, I2, II
      REAL P, D
C
C     CHECK FOR EXECUTION-ERROR CAUSING CONDITIONS.
C
      NOER=.TRUE.
      ILP = 0
      CNTLP = 0.0
      BALPP = 0.0
      A45DBH = 0.0
      TLP45 = 0.0
C
C     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
C     with new species mapping and new location for LP (RNH Dec98, GEB May2000)
C
      I1 = ISCT(IDXLP,1)
C      I1 = ISCT(7,1)
C
C
      IF ( I1 .EQ. 0 ) GO TO 90
C
C     Changed pointer array ISCT subscript from 7 to IDXLP to correspond
C     with new species mapping and new location for LP (RNH Dec98, GEB May2000)
C
      I2 = ISCT(IDXLP,2)
C      I2 = ISCT(7,2)
C
      ILP = I2-I1+1
      IF ( NCLASS .LE. 0 ) GO TO 90
      IF ( ILP .LT. 1 ) GO TO 90
C
      DO 80 II = I1, I2
      I = IND1(II)
      P=PROB(I)
      D=DBH(I)
      CNTLP = CNTLP + P
      BALPP = BALPP + 0.005454154 * D * D * P
      IF (D .LT. 4.5 )  GO TO 80
      TLP45 = TLP45 + P
      A45DBH = A45DBH + D * P
   80 CONTINUE
      IF ( CNTLP .LT. .01 ) GO TO 90
      IF ( TLP45 .LT. 1. ) GO TO 90
C
C     PROPORTION BASAL AREA IN LPP
C
      PBALPP = BALPP / BA
C
C     AVERAGE LODGEPOLE DBH:
C
      A45DBH = A45DBH  / TLP45
      GO TO 100
C
  90  CONTINUE
      NOER=.FALSE.
 100  CONTINUE
      RETURN
      END
