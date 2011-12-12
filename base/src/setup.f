      SUBROUTINE SETUP
      IMPLICIT NONE
C----------
C  **SETUP  DATE OF LAST REVISION:  07/23/08
C----------
C
C     SET UP THE INDEX POINTERS FOR IND1( SPECIES INDICES ).
C     THE DEFINITION OF MOST VARIABLES OCCURES IN **INTREE**
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
      INTEGER IX,LAST,I,IR,KNT,ISTART,NEXT,J
C
COMMONS
C
C
      IF (IREC1.GT.0) GOTO 10
      ITRN=0
      RETURN
   10 CONTINUE
      IX= 1
      LAST= 0
C
      DO  300  I= 1, MAXSP
C
      IR= IREF(I)
      IF( IR .EQ. 0 ) GO TO 300
      KNT= KOUNT(IR)
      ISCT(I,1)= LAST + 1
      LAST= LAST + KNT
      ISCT(I,2)= LAST
      ISTART= IBEGIN(I)
      IND1(IX)= ISTART
      IX= IX + 1
      IF( KNT .EQ. 1 ) GO TO  300
      NEXT= IND2( ISTART )
C
      DO  299  J= 2, KNT
C
      IND1(IX)= NEXT
      IX= IX + 1
      IF(J. EQ. KNT) GOTO 299
      NEXT= IND2( NEXT )
  299 CONTINUE
  300 CONTINUE
C
      ITRN = LAST
C
C
      RETURN
      END
