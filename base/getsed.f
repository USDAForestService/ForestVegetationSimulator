      SUBROUTINE GETSED (SED)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     GENERATES A SEED FOR THE RANDOM NUMBER GENERATOR.
C
C
      REAL SED
      INTEGER I1,I2,I3,I4
      CHARACTER DAT*10,TIM*8
C
      SAVE I4
      DATA I4/100/
C
      CALL GRDTIM (DAT,TIM)
      READ (TIM,'(T4,I2,T7,I2,T8,I1)') I1,I2,I3
      I4=I4+I3
      IF (I4.GT.300) I4=100+I3
      I3=MOD(I3,2)+1
      SED=FLOAT(IFIX( (I2*10000.+I1*100.+I2)/(I1+1.)*
     >                FLOAT(I2+I4)/10**I3 ) )
      IF (AMOD(SED,2.0).EQ.0.) SED=SED+1.
      WRITE (DAT,'(F9.0)') SED
      READ  (DAT,'(F10.0)') SED
      RETURN
      END
