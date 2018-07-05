      SUBROUTINE REASS
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
C  **REASS** REALLIGNS THE POINTERS VECTORS IND1 AND IND, AND
C  THE ARRAY ISCT FOLLOWING RECORD TRIPLING.  THE VECTOR IND2
C  IS USED FOR WORK SPACE.
C----------
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
      INTEGER J,ISPC,I1,I2,I,K,ITFN,LAST,IKNT
C
C
COMMONS
C----------
C  COPY IND1 INTO IND2 IN SPECIES ORDER.
C----------
      J=0
      DO 20 ISPC=1,MAXSP
      I1=ISCT(ISPC,1)
      IF(I1.EQ.0) GO TO 20
      I2=ISCT(ISPC,2)
      DO 10 I=I1,I2
      J=J+1
      IND2(J)=IND1(I)
   10 CONTINUE
   20 CONTINUE
C----------
C  REALLIGN THE POINTERS IN IND1.
C----------
      J=1
      DO 30 K=1,ITRN,3
      I=IND2(J)
      ITFN=IREC1+2*I-1
      IND1(K)=ITFN
      IND1(K+1)=I
      IND1(K+2)=ITFN+1
      J=J+1
   30 CONTINUE
C----------
C  REALLIGN POINTERS TO THE SPECIES SEGMENTS IN IND1.
C----------
      LAST=0
      DO 40 ISPC=1,MAXSP
      IF(ISCT(ISPC,1).EQ.0) GO TO 40
      IKNT=ISCT(ISPC,2)-ISCT(ISPC,1)+1
      ISCT(ISPC,1)=LAST+1
      LAST=LAST+IKNT*3
      ISCT(ISPC,2)=LAST
   40 CONTINUE
C----------
C  COPY IND INTO IND2.
C----------
      DO 50 I=1,IREC1
      IND2(I)=IND(I)
   50 CONTINUE
C----------
C  REALLIGN POINTERS IN IND.
C----------
      J=1
      DO 60 K=1,ITRN,3
      I=IND2(J)
      ITFN=IREC1+2*I-1
      IND(K)=ITFN
      IND(K+1)=I
      IND(K+2)=ITFN+1
      J=J+1
   60 CONTINUE
      RETURN
      END
