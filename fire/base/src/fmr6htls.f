      SUBROUTINE FMR6HTLS (KSP,X)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C     CALLED FROM: FMSNAG
C
*----------------------------------------------------------------------
*  Purpose:
*     This subroutine calculates height loss for a snag record in terms
*     of the proportion of its current height that is lost. 
*     This is based on the species group.
*
*     The calculations are based on work done by Kim Mellen, regional
*     wildlife ecologist for region 6.
*----------------------------------------------------------------------
*
*  Local variable definitions:
*     KSP - SPECIES OF SNAG
*     X - THE PROPORTION OF THE SNAG'S HEIGHT THAT WILL BE LOST.
*     WSSPEC,BMSPEC,ECSPEC,SOSPEC - THE SPECIES GROUP (1 - 15) FOR EACH SPECIES
*     IN EACH VARIANT:
*          1 = cedar
*          2 = western white pine
*          3 = western larch
*          4 = douglas-fir
*          5 = ponderosa pine
*          6 = most hardwoods
*          7 = lodgepole pine
*          8 = spruce
*          9 = western hemlock
*          10 = mountain hemlock
*          11 = pacific silver fir
*          12 = white / grand fir
*          13 = subalpine fir
*          14 = noble fir
*          15 = alders
*
*     SNHTLS- ARRAY WITH HEIGHT LOSS ESTIMATES FOR EACH SPECIES GROUP 
*     PRHTLS- ARRAY WITH THE PROPORTIONS OF SNAGS THAT ACTUALLY LOSE HEIGHT
*
***********************************************************************
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
COMMONS
C----------
C  Variable declarations.
C----------
      LOGICAL DEBUG
      CHARACTER VVER*7
      INTEGER KSP, SPG, I
      INTEGER WSSPEC(39),BMSPEC(18),ECSPEC(32), SOSPEC(33) 
      REAL    X, Y, SNHTLS(15), PRHTLS(15)
C----------
C  DETERMINE WHICH VARIANT IS BEING USED.
C----------
      CALL VARVER(VVER)
C----------
C  CHECK FOR DEBUG.
C----------
      CALL DBCHK (DEBUG,'FMR6HTLS',8,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC, KSP, VVER(1:2)
    7 FORMAT(' FMR6HTLS CYCLE=',I2,' KSP=',I5,' VVER=',A2)
C----------
C  THESE ARE THE WESTSIDE (PN/WC) SPECIES GROUPS (1 - 15) TO USE FOR SNAG HEIGHT LOSS
C----------
      DATA (WSSPEC(I), I= 1, 39) /
     & 11, 12, 12, 13, 12,  8, 14,  1,  1,  8,
     &  7,  5,  2,  2,  5,  4,  1,  1,  9, 10,
     & 15, 15, 15,  6,  6,  6,  6,  6,  1,  3,
     &  2,  7,  1,  6,  6,  6,  6,  6,  6/ 
C----------
C  THESE ARE THE BM SPECIES GROUPS (1 - 15) TO USE FOR SNAG HEIGHT LOSS
C----------
      DATA (BMSPEC(I), I= 1, 18) /
     &  2,  3,  4, 12, 10,  1,  7,  8, 13,  5,
     &  2,  2,  1,  1,  6,  6,  5,  6/
C----------
C  THESE ARE THE EC SPECIES GROUPS (1 - 15) TO USE FOR SNAG HEIGHT LOSS
C----------
      DATA (ECSPEC(I), I= 1, 32) /
     &  2,  3,  4, 11,  1, 12,  7,  8, 13,  5,
     &  9, 10,  1,  2, 14, 12,  3,  1,  1, 15,
     & 15, 15,  6,  6,  6,  6,  6,  6,  6,  6,
     & 10,  6/
C----------
C  THESE ARE THE SO SPECIES GROUPS (1 - 15) TO USE FOR SNAG HEIGHT LOSS
C----------
      DATA (SOSPEC(I), I= 1, 33) /     
     & 2 ,2 ,4 ,12,10,1 ,7 ,8 ,12,5 ,
     & 1 ,12,13,11,14,2 ,3 ,1 ,9 ,1 ,
     & 15,15,15,6 ,6 ,6 ,6 ,6 ,6 ,6 ,
     & 6 ,4 ,6/
C----------
C  PROPORTION OF HEIGHT THAT IS LOST EACH YEAR FOR SNAGS (WHEN THEY ACTUALLY LOSE HEIGHT)
C----------
      DATA (SNHTLS(I), I=1,15) /
     & 0.141,0.202,0.092,0.219,0.172,0.232,0.139,0.199,0.225,0.262,
     & 0.199,0.277,0.119,0.193,0.287/
C----------
C  PROPORTION OF SNAGS THAT ACTUALLY LOSE HEIGHT 
C----------
      DATA (PRHTLS(I), I=1,15) /
     & 0.059,0.122,0.063,0.075,0.053,0.083,0.057,0.054,0.122,0.106,
     & 0.133,0.072,0.042,0.055,0.150/
C----------
C  DETERMINE THE SPECIES GROUP. 
C----------
      SELECT CASE (VVER(1:2))
        CASE('EC')
          SPG = ECSPEC(KSP)      
        CASE('BM')
          SPG = BMSPEC(KSP)       
        CASE('SO')
          SPG = SOSPEC(KSP)         
        CASE DEFAULT
          SPG = WSSPEC(KSP)
      END SELECT
C----------
C  DETERMINE THE AMOUNT OF HEIGHT LOSS.  
C----------
      X = 0.0
      CALL RANN(Y)
      IF (Y .LE. PRHTLS(SPG)) THEN
        X = SNHTLS(SPG)
      ENDIF
C      
      IF (DEBUG) WRITE(JOSTND,8) X, Y, SPG
    8 FORMAT(' FMR6HTLS X=',F6.2,' Y=',F6.2,' SPG=',I3)      
C
      RETURN
      END

