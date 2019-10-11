      SUBROUTINE ESNSPE (IFT,PSPE,ITPP)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C     PREDICT PROBS FOR NUMBER OF SPECIES ON STOCKED PLOTS
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
C               12 = 911
C               13 = OTHER (NO ADVANCED REGENERATION)
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
      REAL RITPP,NSSPB1(13,6),NSSPB2(6),B1,B2
C
      REAL PSPE(6),PSPA
C PNFT(13,MAXSP)
C----------   
      RITPP=REAL(ITPP)
C
C----------
      DATA ((NSSPB1(I,II),I=1,13),II=1,6) / 
     &  1.182062,-0.905262,-3.868602,-5.925372,-7.724864,-7.878476,     ! 122 WHITE SPRUCE                     
     &  1.961662,-1.281028,-3.941338,-6.301481, -7.20789,-8.919528,     ! 125 BLACK SPRUCE                     
     &  0.361686,-0.697373,-2.087096,-4.111011, -6.65514,-8.570495,     ! 270 MOUNTAIN HEMLOCK                 
     & -0.714075,-0.728509,-1.401856,-2.574855,-4.437579,-6.761279,     ! 271 ALASKA CEDAR                     
     & -0.484454,-0.611399, -1.54757,-2.974729,-4.310734,-5.972149,     ! 281 LODGEPOLE PINE                   
     &  0.402448,-0.446504,-2.665313,-4.692324,-7.372583,-9.102511,     ! 301 WESTERN HEMLOCK                  
     &  -0.19588,-0.806586,  -1.8159,-2.885034,-3.853085,-5.716377,     ! 304 WESTERN REDCEDAR                 
     &  0.759265,-0.711191,-3.021037,-5.676959,-7.304021,-8.484111,     ! 305 SITKA SPRUCE                     
     &  1.259357, -1.12065,-2.900123,-5.547975,-6.959145,-8.208189,     ! 703 COTTONWOOD                       
     &  0.552359,-0.490424,-2.974516, -5.48252,-7.509753,-8.658547,     ! 901 ASPEN                            
     &  0.606123,-0.666617,-2.515254,-5.176856,-7.805783,-8.930359,     ! 902 PAPER BIRCH                      
     & -0.289107,-0.484229,-2.476832,-4.894519,-6.542142,-7.956111,     ! 911 RED ALDER                        
     &  1.182062,-0.905262,-3.868602,-5.925372,-7.724864,-7.878476 /   ! OTHER F.T. MAPPED TO 122 WHITE SPRUCE
      DATA NSSPB2 / 
     & -0.043772, 0.009329,0.026839,0.041974,0.04734,0.051476 /
C----------
C     P(1 SPECIES ON STOCKED PLOTS)
C----------
      B1 = NSSPB1(IFT,1)
      B2 = NSSPB2(1)
      PSPA = B1 + B2 * RITPP
      PSPE(1) = EXP(PSPA)/(1+EXP(PSPA))
C----------
C     P(2 SPECIES ON STOCKED PLOTS)
C----------
      IF(ITPP.LT.2) GO TO 78
      B1 = NSSPB1(IFT,2)
      B2 = NSSPB2(2)
      PSPA = B1 + B2 * RITPP
      PSPE(2) = EXP(PSPA)/(1+EXP(PSPA))
C----------
C     P(3 SPECIES ON STOCKED PLOTS)
C----------
      IF(ITPP.LT.3) GO TO 78
      B1 = NSSPB1(IFT,3)
      B2 = NSSPB2(3)
      PSPA = B1 + B2 * RITPP
      PSPE(3) = EXP(PSPA)/(1+EXP(PSPA))
C----------
C     P(4 SPECIES ON STOCKED PLOTS)
C----------
      IF(ITPP.LT.4) GO TO 78
      B1 = NSSPB1(IFT,4)
      B2 = NSSPB2(4)
      PSPA = B1 + B2 * RITPP
      PSPE(4) = EXP(PSPA)/(1+EXP(PSPA))
C----------
C     P(5 SPECIES ON STOCKED PLOTS)
C----------
      IF(ITPP.LT.5) GO TO 78
      B1 = NSSPB1(IFT,5)
      B2 = NSSPB2(5)
      PSPA = B1 + B2 * RITPP
      PSPE(5) = EXP(PSPA)/(1+EXP(PSPA))
C----------
C     P(6 SPECIES ON STOCKED PLOTS)
C----------
      IF(ITPP.LT.6) GO TO 78
      B1 = NSSPB1(IFT,6)
      B2 = NSSPB2(6)
      PSPA = B1 + B2 * RITPP
      PSPE(6) = EXP(PSPA)/(1+EXP(PSPA))
C
   78 CONTINUE
      RETURN
      END
