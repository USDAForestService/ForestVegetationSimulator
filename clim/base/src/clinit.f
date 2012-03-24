      SUBROUTINE CLINIT 
      IMPLICIT NONE
C----------
C  **CLINIT  CLIMATE--DATE OF LAST REVISION:  03/23/2012
C----------
C
C     CLIMATE EXTENSION 
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CLIMATE.F77'
C
COMMONS
C
      LCLIMATE=.FALSE.
      CLMORTMULT = 1.
      CLGROWMULT = 1.
      CLMXDENMULT = 1.
      NATTRS=0
      NYEARS=0
      INDXSPECIES=0
      IXDD5=0
      IXMTCM=0 
      IXMAT=0 
      IXGSP=0
      IXD100=0 
      IXMMIN=0
      IXDD0=0
      IXPCS=0
      IXPSITE=0
      AESNTREES = 500.
      NESPECIES = 4
      AESTOCK = 40. 
      LAESTB = .FALSE.
      JCLREF = -1
      CLHABINDX = 0
      RETURN
      END
