      SUBROUTINE CLINIT 
      IMPLICIT NONE
C----------
C  **CLINIT  CLIMATE--DATE OF LAST REVISION:  04/14/2012
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
      CLMRTMLT1 = 1.
      CLMRTMLT2 = 1.
      CLGROWMULT = 1.
      CLMXDENMULT = 1.
      NATTRS=0
      NYEARS=0
      INDXSPECIES=0
      IXDD5=0
      IXMTCM=0
      IXMTWM=0 
      IXMAT=0 
      IXGSP=0
      IXGSDD5=0
      IXD100=0 
      IXMMIN=0
      IXDD0=0
      IDEmtwm=0
      IDEmtcm=0
      IDEdd5= 0
      IDEsdi= 0
      IDEdd0= 0
      IDEmapdd5=0 
      IXPSITE=0
      AESNTREES = 500.
      NESPECIES = 4
      AESTOCK = 40. 
      LAESTB = .FALSE.
      JCLREF = -1
      CLHABINDX = 0
      RETURN
      END
