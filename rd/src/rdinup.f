      SUBROUTINE  RDINUP
C----------
C  **RDINUP      LAST REVISION:  11/06/89
C----------
C
C  THIS SUBROUTINE FORMS A WEIGHTED AVERAGE OF INFECTED ROOT
C  CHARACTERISTICS FOR THE ROOT DISEASE ROOT DISEASE MODEL
C
C  CALLED BY :
C     RDCNTL  [ROOT DISEASE]
C     RDSPOR  [ROOT DISEASE]
C
C  CALLS     :
C     NONE
C
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'RDPARM.F77'
C
C
      INCLUDE 'RDCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C
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
