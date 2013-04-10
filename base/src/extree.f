      SUBROUTINE EXTREE
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     ASSIGNS THE EXAMPLE TREES TO THE OUTPUT ARRAYS.
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
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
COMMONS
C
      INTEGER I,INS1,IMCI,ISP1
      CHARACTER*3 NS
      DATA NS/'---'/
C-------
C  IF THE TREE LIST IS EMPTY, THEN: ZERO OUT THE SAMPLE TREE LIST.
C-------
      IF (ITRN.EQ.0) GOTO 20
      DO 10 I=1,6
      INS1=INS(I)
      IMCI=IMC(INS1)
      ISP1=ISP(INS1)
      IONSP(I)=NSP(ISP1,IMCI)
      DBHIO(I)=DBH(INS1)
      HTIO(I)=HT(INS1)
      IOICR(I)=ICR(INS1)
      DGIO(I)=DG(INS1)
      PCTIO(I)=PCT(INS1)
      PRBIO(I)=PROB(INS1)/TRM
   10 CONTINUE
C
      RETURN
C
   20 CONTINUE
      DO 30 I=1,6
      IONSP(I)=NS
      DBHIO(I)=0.
      HTIO(I)=0.
      IOICR(I)=0
      DGIO(I)=0.
      PCTIO(I)=0.
      PRBIO(I)=0.
   30 CONTINUE
      RETURN
      END
