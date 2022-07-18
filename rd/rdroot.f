      SUBROUTINE RDROOT(I,A,ANS,F1,F2,HEI)
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  FUNCTION FOR CALCULATING THE ROOT RADIUS OF LIVE TREES
C
C  CALLED BY :
C     RDDAM   [ROOT DISEASE]
C     RDESTB  [ROOT DISEASE]
C     RDIN    [ROOT DISEASE]
C     RDPRIN  [ROOT DISEASE]
C     RDSTR   [ROOT DISEASE]
C     RDTREG  [ROOT DISEASE]
C
C  CALLS     :
C     NONE
C
C  PARAMETERS :
C     I      - SPECIES
C     A      - DBH
C     ANS    - ROOT RADIUS
C     F1     - PROOT
C     F2     - RSLOP
C     HEI    - HEIGHT
C
C  Revision History :
C   01/20/94 - Last revision date.
C   09/02/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77' 
      INCLUDE 'RDPARM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'RDADD.F77'
                                 
      INTEGER I
      REAL    A, ANS, EFFECT, F1, F2, HEI, SDINEW
      INTEGER IDANUW
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      IDANUW = I
C
C
C     CALCULATE THE EFFECT OF SDI (IF KEYWORD WAS NOT USED,
C     THERE MAY BE NO EFFECT) 
      
      SDINEW = (OLDTPA / GROSPC) * (ORMSQD / 10.0) ** 1.605
      
      IF (SDINEW .GT. 0.0) THEN
         EFFECT = SDISLP * SDINEW + YINCPT
      ELSE
         EFFECT = 1.0
      ENDIF
      
      EFFECT = MIN(EFFECT,1.5)
      EFFECT = MAX(EFFECT,0.5)
      
C     NOW CALCULATE THE ROOT RADIUS 
      
      IF (A .LT. 3.5) GOTO 1000
      ANS = EFFECT * F2 * F1 * A / 12.0
      GOTO 9999

C
C     ROOT RADIUS FOR SMALL (<3.5 DBH) TREES.
C
 1000 CONTINUE
      ANS = 0.01
      IF (HEI .EQ. 0.0 .OR. BA .EQ. 0.0) GOTO 9999
      ANS = EXP(0.61157 * ALOG(HEI) + 0.04032 * ALOG(BA) - 0.80815)

      ANS = ANS * EFFECT
      
 9999 CONTINUE

      RETURN
      END
