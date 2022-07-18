      SUBROUTINE GRPSUM (NRECS,IPT,ATR,P,WT)
      IMPLICIT NONE
C----------
C  **GRPSUM  DATE OF LAST REVISION:  06/30/10
C----------
C
C     CALLED FROM GARBEL OR COMPRS    N.L. CROOKSTON JAN 1978
C     MODIFIED MAY 1980 JAN 1981
C
C          NRECS = LENGTH OF POINTER ARRAY
C          IPT   = POINTER ARRAY
C          ATR   = ATTRIBUTE TO BE PROCESSED (REAL)
C          WT    = WEIGHT OF ATTRIBUTE
C          P     = SUM (P(I) * WT * ATR(I))
C          PROB  = WEIGHT OF EACH MEMBER OF ATR
C
C  03-APR-2002 Lance R. David (FHTET)
C     Arrays dimensioned "1" changed to "*" so that array size is
C     inherited from calling routine.
C----------
      INTEGER I, II, IPT, NRECS
      REAL ATR, AVE, P, STDV, WT, XN 
      DIMENSION IPT(*),ATR(*),P(*)
C
      AVE = 0.0
      STDV = 0.0
      XN = FLOAT(NRECS)
C
      DO 17 II = 1, NRECS
      I = IPT(II)
      AVE = AVE + ATR(I)
      STDV=STDV + ATR(I) ** 2
   17 CONTINUE
C
C     USE THE MAXIMUM LIKLIHOOD ESTIMATE OF THE VARIANCE.
C
      STDV=(STDV-AVE*AVE/XN)/XN
      IF ( STDV .GT. 0.000000001 ) GO TO 18
      STDV = 1.0
      GO TO 21
   18 CONTINUE
      STDV = SQRT ( STDV )
   21 CONTINUE
      AVE = AVE/XN
C
      DO 30 II= 1, NRECS
      I = IPT(II)
      P(I) = P(I) + WT * (ATR(I) - AVE ) / STDV
   30 CONTINUE
C
      RETURN
      END
