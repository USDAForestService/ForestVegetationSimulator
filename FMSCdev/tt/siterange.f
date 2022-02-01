      SUBROUTINE SITERANGE(IWHO,IS,SILOW,SIHIGH)
      IMPLICIT NONE
C----------
C TT $Id$
C----------
C
C SUBROUTINE TO RETURN THE SITE INDEX RANGE FOR A SPECIES.
C
C THIS SUBROUTINE IS CALLED FROM **SITSET** AND **REGENT**.  
C----------
C SPECIES ORDER FOR TETONS VARIANT:
C
C  1=WB,  2=LM,  3=DF,  4=PM,  5=BS,  6=AS,  7=LP,  8=ES,  9=AF, 10=PP,
C 11=UJ, 12=RM, 13=BI, 14=MM, 15=NC, 16=MC, 17=OS, 18=OH
C
C VARIANT EXPANSION:
C BS USES ES EQUATIONS FROM TT
C PM USES PI (COMMON PINYON) EQUATIONS FROM UT
C PP USES PP EQUATIONS FROM CI
C UJ AND RM USE WJ (WESTERN JUNIPER) EQUATIONS FROM UT
C BI USES BM (BIGLEAF MAPLE) EQUATIONS FROM SO
C MM USES MM EQUATIONS FROM IE
C NC AND OH USE NC (NARROWLEAF COTTONWOOD) EQUATIONS FROM CR
C MC USES MC (CURL-LEAF MTN-MAHOGANY) EQUATIONS FROM SO
C OS USES OT (OTHER SP.) EQUATIONS FROM TT
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
COMMONS
C----------
      REAL SITELO(MAXSP),SITEHI(MAXSP)
      REAL SILOW, SIHIGH
      INTEGER IS,IWHO
C----------
C  DEFINITION OF VARIABLES:
C----------
C      IS -- FVS SPECIES INDEX NUMBER
C    IWHO -- 1 IF CALLED FROM SUBROUTINE **SITSET**
C            2 IF CALLED FROM SUBROUTINE **REGENT**
C  SITEHI -- UPPER SITE INDEX VALUES 
C  SITELO -- LOWER SITE INDEX VALUES 
C----------
C  DATA STATEMENTS:
C----------
C  THESE VALUES SHOULD BE BASED ON THE BASE-AGE OF THE SITE CURVE
C  BEING USED FOR THAT SPECIES.
C----------
      DATA SITELO/
     &  25.,  25.,  20.,   5.,  40.,  30.,  20.,  40.,  40.,  40.,
     &   5.,   5.,   5.,   5.,  30.,   5.,  20.,   5./
C
      DATA SITEHI/
     &  50.,  50.,  60.,  20., 100.,  70., 100., 100.,  90.,  80.,
     &  15.,  15.,  30.,  30., 120.,  15.,  50.,  20./
C----------
      SELECT CASE (IWHO)
C
C  CALLED FROM SUBROUTINES **SITSET** AND **REGENT**
C
        CASE (1,2)
          SILOW = SITELO(IS)
          SIHIGH = SITEHI(IS)
C
C  SPACE FOR ADDITIONAL CALLS
C
        CASE DEFAULT
          SILOW = 0.
          SIHIGH = 999.
C
      END SELECT
C
      RETURN
      END