      SUBROUTINE ESNSPE (IFT,PSPE,ITPP)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     PREDICTS THE PROBABILITIES FOR THE NUMBER OF SPECIES 
C     ON STOCKED PLOTS
C----------
C  VARIABLE DEFINITIONS:
C----------
C  IFT      -- STAND FOREST TYPE CATEGORY WHERE:
C                1 = 122
C                2 = 125
C                3 = 270
C                4 = 271
C                5 = 281
C                6 = 301
C                7 = 304
C                8 = 305
C                9 = 703
C               10 = 901
C               11 = 902
C               12 = 904
C               13 = 911
C               14 = OTHER (NO ADVANCED REGENERATION)
C  ITPP     -- NUMBER OF TREES PER STOCKED PLOT (INTEGER)
C  RITPP    -- NUMBER OF TREES PER STOCKED PLOT (REAL)
C  NSSPB1 AND B1 -- B1 COEFFICENT BY FOREST TYPE AND NUMBER OF SPECIES (6)
C  NSSPB2 AND B2 -- B2 COEFFICENT BY NUMBER OF SPECIES (6)
C  PSPA     -- X VARIABLE IN EQN FOR PSPE
C  PSPE     -- PROBABILITY OF 1-6 SPECIES ON A PLOT
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      INTEGER ITPP,I,II,IFT
C
      REAL RITPP,NSSPB1(6,14),NSSPB2(6),B1,B2
C
      REAL PSPE(6),PSPA
C PNFT(13,MAXSP)
C----------   
      RITPP=REAL(ITPP)
C
C----------
      DATA ((NSSPB1(I,II),I=1,6),II=1,14) / 
     &  1.194215,-0.908753,-3.877481,-6.000018, -7.71755,-8.723487,     ! 122 WHITE SPRUCE                     
     &  1.935401,-1.265058,-3.966811,-6.122288,-6.895426, -8.78965,     ! 125 BLACK SPRUCE                     
     &    0.3767,-0.709439,-2.121702, -4.14164,-5.836916,-6.152613,     ! 270 MOUNTAIN HEMLOCK                 
     & -0.695014,-0.740847,-1.390982,-2.576817,-4.566578,-6.823011,     ! 271 ALASKA CEDAR                     
     & -0.508566,-0.651787,  -1.4747,-2.923989,-4.565263,-6.091635,     ! 281 LODGEPOLE PINE                   
     &  0.430023,-0.475483,-2.697713,-4.580122, -7.48335,-8.629654,     ! 301 WESTERN HEMLOCK                  
     & -0.164512,-0.812042,-1.822337,-2.917416,-3.835426,-6.103785,     ! 304 WESTERN REDCEDAR                 
     &  0.771894,-0.692857,-3.136331,-5.891321,-7.274256,-8.011129,     ! 305 SITKA SPRUCE                     
     &  1.249825,-1.135134,-2.769576,-5.516224,-6.895987,-8.004268,     ! 703 COTTONWOOD                       
     &  0.569836,-0.514575,-2.956666,-5.617703,-7.483327,-8.500273,     ! 901 ASPEN                            
     &  0.604791,-0.674578,-2.505112,-5.145078,-7.792646, -8.79403,     ! 902 PAPER BIRCH 
     &  1.249825,-1.135134,-2.769576,-5.516224,-6.895987,-8.004268,     ! 904 BALSAM POPLAR MAPPED TO 703 COTTONWOOD                     
     &  0.090146,-0.625262,-2.668914,-4.807704,-6.431457,-7.705096,     ! 911 RED ALDER                        
     &  1.194215,-0.908753,-3.877481,-6.000018, -7.71755,-8.723487 /    ! OTHER F.T. MAPPED TO 122 WHITE SPRUCE
      DATA NSSPB2 / 
     & -0.043873, 0.009885, 0.026123, 0.042231, 0.048712, 0.054462 /
C     P(1 SPECIES ON STOCKED PLOTS)
      B1 = NSSPB1(1,IFT)
      B2 = NSSPB2(1)
      PSPA = B1 + B2 * RITPP
      PSPE(1) = EXP(PSPA)/(1+EXP(PSPA))
C     P(2 SPECIES ON STOCKED PLOTS)
      IF(ITPP.LT.2) GO TO 78
      B1 = NSSPB1(2,IFT)
      B2 = NSSPB2(2)
      PSPA = B1 + B2 * RITPP
      PSPE(2) = EXP(PSPA)/(1+EXP(PSPA))
C     P(3 SPECIES ON STOCKED PLOTS)
      IF(ITPP.LT.3) GO TO 78
      B1 = NSSPB1(3,IFT)
      B2 = NSSPB2(3)
      PSPA = B1 + B2 * RITPP
      PSPE(3) = EXP(PSPA)/(1+EXP(PSPA))
C     P(4 SPECIES ON STOCKED PLOTS)
      IF(ITPP.LT.4) GO TO 78
      B1 = NSSPB1(4,IFT)
      B2 = NSSPB2(4)
      PSPA = B1 + B2 * RITPP
      PSPE(4) = EXP(PSPA)/(1+EXP(PSPA))
C     P(5 SPECIES ON STOCKED PLOTS)
      IF(ITPP.LT.5) GO TO 78
      B1 = NSSPB1(5,IFT)
      B2 = NSSPB2(5)
      PSPA = B1 + B2 * RITPP
      PSPE(5) = EXP(PSPA)/(1+EXP(PSPA))
C     P(6 SPECIES ON STOCKED PLOTS)
      IF(ITPP.LT.6) GO TO 78
      B1 = NSSPB1(6,IFT)
      B2 = NSSPB2(6)
      PSPA = B1 + B2 * RITPP
      PSPE(6) = EXP(PSPA)/(1+EXP(PSPA))
C
   78 CONTINUE
      RETURN
      END
