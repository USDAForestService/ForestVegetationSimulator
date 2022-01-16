      SUBROUTINE RDGROW
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  SUBROUTINE FOR CALCULATING GROWTH INCREMENT REDUCTION
C  FROM ROOT DISEASE ROOT DISEASE INFECTION
C
C  CALLED BY :
C     RDTREG  [ROOT DISEASE]
C
C  CALLS     :
C     RDSUM   (SUBROUTINE)   [ROOT DISEASE]
C     RDSLP   (FUNCTION)     [ROOT DISEASE]
C
C  Revision History :
C   03/07/95 - Last revision date.
C   08/28/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'

      INCLUDE 'RDCOM.F77'
      INCLUDE 'RDARRY.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'  
      INCLUDE 'RDADD.F77'
C
COMMONS
C
      INTEGER  I, IDI, IP, IS, J
      REAL     BOTTOM, DGIMP, DIFF, OUTNUM, PRADI, RDSLP, TPAREA
      
      TPAREA = 0.0
      DO 150 IDI=MINRR,MAXRR
         TPAREA = TPAREA + PAREA(IDI)
  150 CONTINUE       

      IF (ITRN .LE. 0 .OR. TPAREA .EQ. 0.0) RETURN
      
      CALL RDSUM(ITRN,PROBIT,PROBI,ISTEP)

      IDI = MAXRR
      DO 1000 I=1,ITRN

         IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(ISP(I)))
C
C     Skip operations if non-host species (RNH May98)
C
      IF (IDI .LE. 0) GO TO 1000
C
      IF (PAREA(IDI) .LE. 0.0) GOTO 1000

         IS = ISP(I)
         HTTOT = 0.0
         DGTOT = 0.0       

         DO 550 J=1,ISTEP             
            DO 500 IP=1,2

               IF (PROBI(I,J,IP) .LE. 1E-3) GOTO 500
               
C....          If infection has not yet reached the center, tree is not
C....          affected by the infection (temporarily set radius of
C....          infection to zero).

               PRADI = PROPI(I,J,IP)
               IF (PRADI .LT. 0.0) PRADI = 0.0
             
               HTIMP = RDSLP(PRADI,XHT,YHT,2) * HTIFAC(IRTSPC(IS))
               DGIMP = RDSLP(PRADI,XDBH,YDBH,2) * DBIFAC(IRTSPC(IS))
               HTTOT = HTTOT + HTIMP * PROBI(I,J,IP) /
     &                 (PROBIT(I) + 1E-6)
               DGTOT = DGTOT + DGIMP * PROBI(I,J,IP) /
     &                 (PROBIT(I) + 1E-6)
  500       CONTINUE
  550    CONTINUE

         DIFF = SAREA - PAREA(IDI)
         IF (DIFF .LE. 1E-3 .AND. PROBIU(I) .LE. 1E-3 .AND.
     &       PROBIT(I) .LE. 1E-3) GOTO 999 
         
         OUTNUM = FPROB(I) * (SAREA - PAREA(IDI))
         BOTTOM = OUTNUM + PROBIU(I) + PROBIT(I)

         IF (BOTTOM .LE. 1E-6) THEN
            DG(I) = 0.0001
            HTG(I) = 0.0001
         ELSE
            DG(I) = (DG(I) * (OUTNUM + PROBIU(I)) + 
     &               DG(I) * DGTOT * PROBIT(I)) / BOTTOM
            HTG(I) =(HTG(I) * (OUTNUM + PROBIU(I)) + 
     &               HTG(I) * HTTOT * PROBIT(I)) / BOTTOM
         ENDIF
         
  999    CONTINUE
         IF (DG(I) .LT. 0.0001) DG(I) = 0.0001
         IF (HTG(I) .LT. 0.0001) HTG(I) = 0.0001

 1000 CONTINUE

      RETURN
      END
