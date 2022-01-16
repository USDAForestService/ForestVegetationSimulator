      SUBROUTINE  RDINUP
      IMPLICIT NONE
C----------
C  **RDINUP      LAST REVISION:  08/28/14
C----------
C
C  THIS SUBROUTINE FORMS A WEIGHTED AVERAGE OF INFECTED ROOT
C  CHARACTERISTICS FOR THE ROOT DISEASE MODEL
C
C  CALLED BY :
C     RDCNTL  [ROOT DISEASE]
C     RDSPOR  [ROOT DISEASE]
C
C  CALLS     :
C     NONE
C
C  Revision History :
C   11/06/89 - Last revision date.
C   08/28/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'PLOT.F77'
C
COMMONS
C
      INTEGER  I, IDI, J, K
      
C*    WRITE(6,1)
C*  1 FORMAT(' STARTING RDINUP')

      IDI=IRRSP

      DO 500 I=1, 2
         DO 400 J=1,5
            PROBD(IDI,I,J)  = 0.0
            DBHD(IDI,I,J)   = 0.0
            ROOTD(IDI,I,J)  = 0.0

            DO 300 K=1, ISTEP
               IF (PROBDA(IDI,I,J,K) .LE. 0) GOTO 300
               PROBD(IDI,I,J)  = PROBD(IDI,I,J) + PROBDA(IDI,I,J,K)
               DBHD(IDI,I,J)   = DBHD(IDI,I,J) + DBHDA(IDI,I,J,K) *
     &                           PROBDA(IDI,I,J,K)
               ROOTD(IDI,I,J)  = ROOTD(IDI,I,J) + ROOTDA(IDI,I,J,K) *
     &                           PROBDA(IDI,I,J,K)
  300       CONTINUE

            DBHD(IDI,I,J)   = DBHD(IDI,I,J) / (PROBD(IDI,I,J) + 1E-6)
            ROOTD(IDI,I,J)  = ROOTD(IDI,I,J) / (PROBD(IDI,I,J) + 1E-6)
  400    CONTINUE
  500 CONTINUE

C*    WRITE(6,2)
C*  2 FORMAT(' ENDING RDINUP')

      RETURN
      END
