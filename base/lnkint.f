      SUBROUTINE LNKINT
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
      INTEGER I
C
COMMONS
C
C
      NUMSP=0
      DO 20 I=1,MAXSP
      ISCT(I,1)=0
      ISCT(I,2)=0
      IBEGIN(I)=0
      IREF(I)=0
      KOUNT(I)=0
   20 CONTINUE
      RETURN
      END
