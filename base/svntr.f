      SUBROUTINE SVNTR (XP,NP)
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C
C     STAND VISUALIZATION GENERATION
C     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
C
C     GIVEN XP, THE NUMBER OF TREES PER ACRE, 
C     RETURN NP, THE NUMBER OF TREE RECORDS TO PROCESS
C                 
      REAL XX,XP,X
      INTEGER NP
      NP = 0
      XX = XP
      IF (XX.GT.1.) THEN
         NP = IFIX(XP)
         XX = XX-FLOAT(NP)
      ENDIF
      IF (XX.GT. .00001) THEN
         CALL SVRANN(X)
         IF (X.LT.XX) NP = NP+1
      ENDIF
      RETURN
      END
