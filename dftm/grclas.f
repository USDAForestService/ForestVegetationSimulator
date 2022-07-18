      SUBROUTINE GRCLAS (NRECS,IPT,ATR,ISC,NCLAS,NATR,CLAS,
     >                   PROB,IMPROB,IPOS,MAXCLS,ITWO)
      IMPLICIT NONE
C----------
C DFTM $Id$
C----------
C
C     CALLED FROM 'GARBEL'   N.L. CROOKSTON  JAN 1978 & JAN 1981
C
C          NRECS = LENGTH OF POINTER ARRAY
C          IPT   = POINTER ARRAY
C          NCLAS = NUMBER OF CLASSES
C          CLAS  = CLASSIFIED ARRAY (ONE WHICH HOLDS CLASSIFIED DATA)
C          PROB  = WEIGHT OF EACH MEMBER OF ATR
C          IPOS  = THE POSITION WITHIN 'CLAS' OF THIS ATTRIBUTE
C          IMPROB= POSITION WITHIN CLAS OF PROB
C          NATR  = NUMBER OF ATTRIBUTES (NEEDED FOR OBJECT
C                  TIME DIMENSIONING OF CLAS.)
C          ISC   = SECTOR POINTERS TO BE USED WITH 'IPT'
C
C  03-APR-2002 Lance R. David (FHTET)
C     Arrays dimensioned "1" changed to "*" so that array size is
C     inherited from calling routine.
C----------

      INTEGER IPOS,IMPROB,NCLAS,NRECS,MAXCLS,ITWO,ISC,NATR
      INTEGER IPT,I3,I1,I2,J,II,I
      REAL CLAS,PROB,ATR,XP

      DIMENSION IPT(*),ISC(MAXCLS,ITWO),PROB(*),CLAS(MAXCLS,NATR),
     >          ATR(*)
C
      I3 = IPOS
      DO 20 J = 1, NCLAS
      I1 = ISC(J,1)
      I2 = ISC(J,2)
      CLAS(J,I3) = 0.0
C
      DO 10 II = I1, I2
      I = IPT(II)
      CLAS (J,I3) = CLAS (J,I3) + ATR(I) * PROB(I)
   10 CONTINUE
C
      XP = CLAS (J,IMPROB)
      IF ( XP .GT. 1.E-30 ) CLAS (J,I3) = CLAS (J,I3) / XP
   20 CONTINUE
C
      RETURN
      END
