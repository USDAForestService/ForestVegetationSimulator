      SUBROUTINE ORINIT 
      IMPLICIT NONE
C----------
C     **ORINIT  ORGANON--DATE OF LAST REVISION:  11//2011
C     $Id: orinit.f 290 2013-06-06 22:18:41Z jdh $
C     $Revision: 290 $
C     $Date: 2013-06-06 15:18:41 -0700 (Thu, 06 Jun 2013) $
C     $HeadURL: https://www.forestinformatics.com/svn/fvs/trunk/pncor/src/orinit.f $
C----------
C     
C     ORGANON EXTENSION GLOBAL VARIABLE INITIALIZATION

C     DOES THIS MEAN THIS CODE IS CALLED BEFORE EACH STAND, OR WHEN THE SIM 
C     IS LOADED.
C     
C     COMMONS
C     
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ORGANON.F77'

      INTEGER*4 I

      LORGANON  = .FALSE.       ! THE DLL IS NOT LOADED.
      LORGVOLS  = .FALSE.       ! THE VOLUME DLL IS NOT BEING USED.
C      VERSION   = 0             ! THE VERSION, IF NOT SET, DEFAULTS TO SWO
C      IMODTY   = 0             ! THE VERSION, IF NOT SET, DEFAULTS TO SWO
      STAGE     = 0             ! total stand age (even aged only)
      BHAGE     = 0             ! breast height age            
      SITE_1    = 0.0           ! the default site index dependant on variant
      SITE_2    = 0.0           ! the default site index dependant on variant

C     CLEAR OUT THE STAND LEVEL INTEGER AND REAL VARIABLE ARRAYS
C     THIS SHOULD ALSO BE PERFORMED EACH CYCLE TOO.
      DO I=1,30
         INDS(I)   = 0
         RVARS(I)  = 0.0
      ENDDO

C     FOR NOW, ASSIGN THE DBH, HEIGHT, AND CR CALIBRATIONS TO UNITY.
C     TODO: THIS NEED TO BE CONVERTED UPDATED FROM FVS.
      DO I=1,18
         ACALIB(1,I)	= 1.0 
         ACALIB(2,I)	= 1.0
         ACALIB(3,I)	= 1.0
      ENDDO
      
C     CLEAR OUT THE TREE VARIABLES
C     THIS SHOULD ALSO BE PERFORMED EACH CYCLE AS THE TREE
C     RECORDS ARE RE-INITIALIZED FROM THE NATIVE FVS ARRAYS
      DO I=1,2000
         
         TREENO(I)      = 0
         PTNO(I)        = 0
         SPECIES(I)     = 0
         DBH1(I)	= 0.0 
         HT1OR(I)	= 0.0
         CR1(I)		= 0.0
         SCR1B(I)	= 0.0
         EXPAN1(I)	= 0.0
         MGEXP(I)	= 0.0
         USER(I)	= 0

         TWARNING(I) = 0
         
      ENDDO

C     INITIALIZE THE VOLUME VARIABLES NOT NATIVE TO FVS 
C     THESE ONLY APPLY TO THE ORGANON SUBROUTINE VOLCAL_
C     THE DEFAULTS ARE DOCUMENTED IN THE HELP FILE
      LOGTA =  10.0             ! LOG TRIM, IN INCHES.
      LOGML =  12.0             ! MINIMUM LOG LENGTH, IN FEET.
      LOGLL =  32.0             ! TARGET LOG LENGTH, IN FEET.


C     INITIALIZE THE STOR VARIABLES TO ZERO TO ENSURE 
C     THE VALUES ARE NOT CORRUPT FOR EVERY STAND
      DO I=1,30
         STOR(I) = 0.0
      ENDDO



C     NULL OUT THE THINNING VARIABLES.
C     MOVE THE VARIABLES FROM THE PREVIOUS THINNINGS, IF THERE ARE ANY
      DO I=1,5
        YST(I)  = 0
        BART(I) = 0.0
        BABT    = 0.0
	END DO

      
C     NULL OUT THE FERTILIZATION VARIABLES TOO.
      DO I=1,5
        YSF(I)   = 0.0
        PN(I)  = 0.0
	END DO


      RETURN
      END
