      SUBROUTINE RDPUSH
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  PROCESSES THE PSTUMP KEYWORD.
C
C  CALLED BY :
C     RDTREG  [ROOT DISEASE]
C
C  CALLS     :
C     OPFIND  (SUBROUTINE)   [PROGNOSIS]
C     OPGET   (SUBROUTINE)   [PROGNOSIS]
C     OPDONE  (SUBROUTINE)   [PROGNOSIS]
C
C  PARAMETERS :
C     NONE
C
C  COMMON BLOCK VARIABLES :
C     xxxxx:   From ANCOM;
C
C
C  LOCAL VARIABLES :
C
C  Revision History :
C   03/24/93 - Last revision date.
C   09/02/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
C.... PARAMETER INCLUDE FILES
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
C
C.... COMMON INCLUDE FILES
C
      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDADD.F77'
C
C.... DATA statements
C
      INTEGER  I, IACTK, IDI, J, K, KDT, MYACT(1), NPS, NTODO
      REAL     DIAM, PROP, PRMS(2)

      DATA MYACT /2403/

      CALL OPFIND (1,MYACT,NTODO)
      IF (NTODO .LE. 0) RETURN

      CALL OPGET (NTODO,2,KDT,IACTK,NPS,PRMS)
      CALL OPDONE (NTODO,IY(ICYC))
      PROP = PRMS(1)
      DIAM = PRMS(2)

      DO 95 IDI=MINRR,MAXRR
         DO 100 I = 1,2
            DO 110 J = 1,5
               DO 115 K = 1,ISTEP
C
C                 PUSH STUMPS
C
C                 WRITE (IRUNIT,999) IDI, I, J, K, PROBDA(IDI,I,J,K),
C    &                  DBHDA(IDI,I,J,K),DIAM,PROP
C 999             FORMAT ('PUSHSTUMPS: ',4I5,2F10.5,2F10.5)
C
                  IF (DBHDA(IDI,I,J,K) .GE. DIAM)
     &               PROBDA(IDI,I,J,K) = PROBDA(IDI,I,J,K) * (1 - PROP)
                  IF (PROBDA(IDI,I,J,K) .NE. 0.0) GOTO 115
C
C                 IF ALL STUMPS WERE REMOVED, ZERO ARRAYS ASSOCIATED
C                 WITH STUMPS
C
                  DBHDA(IDI,I,J,K)  = 0.0
                  ROOTDA(IDI,I,J,K) = 0.0
                  JRAGED(IDI,I,J,K) = 0

C*                DO 1010 J = 1,NCENTS(IDI)
C*                   PCENTS(IDI,J,3) = 0.0
C1010             CONTINUE
C*                NCENTS(IDI)=0

  115          CONTINUE
  110       CONTINUE
  100    CONTINUE
  95  CONTINUE

      RETURN
      END
