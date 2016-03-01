      SUBROUTINE RDESTB(N,ANS)
      IMPLICIT NONE
C----------
C  **RDESTB      LAST REVISION:  O3/01/16
C----------
C
C  SUBROUTINE FOR PUTTING REGENERATION ONTO ROOT DISEASE ROOT DISEASE PATCHES
C
C  CALLED BY :
C     ESTAB   [PROGNOSIS]
C
C  CALLS     :
C     RDROOT  (SUBROUTINE)   [ROOT DISEASE]
C     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
C
C  PARAMETERS :
C     N      -
C     ANS    -
C
C  Revision History :
C   03/07/95 - Last revision date.
C   08/28/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C   03/01/2016 Lance R. David (FMSC)
C     Moved one condition to exit to top.
C
C----------------------------------------------------------------------
C
C
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDADD.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'

      INTEGER  IDI, IT, ISPI, J, N
      REAL     ANS, TPAREA, XXX

      IF (IROOT .EQ. 0) RETURN

      TPAREA = 0.0
      DO 20 IDI=MINRR,MAXRR
         TPAREA = TPAREA + PAREA(IDI)
   20 CONTINUE      
      IF (TPAREA .EQ. 0.0) RETURN
C
C.....PUT TREES INTO OUTSIDE DENSIITY ARRAY (INCLUDING THE FRINGE ARRAY)
C
      FPROB(N) = ANS
      FFPROB(N,2) = ANS

      DO 40 IT=1,ISTEP
         PROBI(N,IT,1) = 0.0
         PROBI(N,IT,2) = 0.0
   40 CONTINUE

      IDI = MAXRR
      IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(ISP(N)))

      PROBL(N) = ANS
      PROBIU(N) = ANS * PAREA(IDI)
      WK22(N) = 0.0
      ISPI = ISP(N)
      CALL RDROOT(ISPI,0.1,XXX,PROOT(IRTSPC(ISPI)),RSLOP(IRTSPC(ISPI)),
     &           HT(N))
      RROOTT(N) = XXX

      DO 1000 J=2,4
         XMTH(J,N) = -1.0
         ROOTH(J,N) = -1.0
 1000 CONTINUE

      CALL RDSUM(ITRN,PROBIT,PROBI,ISTEP)

      RETURN
      END
