      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C FIRE-CS $Id$
C----------
C  PURPOSE:
C      INITIALIZE VARIANT-SPECIFIC VARIABLES FOR THE FIRE MODEL
C----------
C  CALLED FROM: FMINIT
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'FMFCOM.F77'
C
C
COMMONS
C
      INTEGER I,J,TFALLCLS(MAXSP),SNAGCLS(MAXSP)

      LVWEST    = .FALSE.  ! EASTERN VARIANT

C
      CANCLS(1) =  5.0
      CANCLS(2) = 17.5
      CANCLS(3) = 37.5
      CANCLS(4) = 75.0
C
      CORFAC(1) =  0.5
      CORFAC(2) =  0.3
      CORFAC(3) =  0.2
      CORFAC(4) =  0.1
C
      SNPRCL(1) =  0
      SNPRCL(2) = 12
      SNPRCL(3) = 18
      SNPRCL(4) = 24
      SNPRCL(5) = 30
      SNPRCL(6) = 36
C
      LOWDBH(1) =  0.0
      LOWDBH(2) =  5.0
      LOWDBH(3) = 10.0
      LOWDBH(4) = 20.0
      LOWDBH(5) = 30.0
      LOWDBH(6) = 40.0
      LOWDBH(7) = 50.0
C----------
C  SET POTENTIAL FIRE TEMPERATURES AND WINDSPEEDS
C----------
      PREWND(1)=20.
      PREWND(2)=8.
      POTEMP(1)=70.
      POTEMP(2)=60.
C----------
C  DECAY RATES FROM ABBOTT AND CROSSLEY / BARBER AND VANLEAR
C----------
      DKR(1,1)  = 0.11
      DKR(2,1)  = 0.11
      DKR(3,1)  = 0.09
      DKR(4,1)  = 0.07
      DKR(5,1)  = 0.07
      DKR(6,1)  = 0.07
      DKR(7,1)  = 0.07
      DKR(8,1)  = 0.07
      DKR(9,1)  = 0.07
C
      DO I = 1,9
        DO J = 2,4
          DKR(I,J) = DKR(I,1)
        ENDDO
      ENDDO
C----------
C  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
C  LITTER RATES FROM SHARPE ET AL. AND WITKAMP
C----------
      DO J = 1,4
        DKR(10,J) = 0.65
        DKR(11,J) = 0.002
      ENDDO
C----------
C  DUFF PRODUCTION RATES 'PRDUFF' ARE A PROPORTION OF THE OVERALL
C  DECAY RATE: 'DKR'.
C----------
      DO I = 1,MXFLCL
        DO J = 1,4
          PRDUFF(I,J) = 0.02
          TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J)
        ENDDO
      ENDDO
C----------
C  SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
C  ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.
C
C  NO SNAG HEIGHT LOSS
C----------
      NZERO  =  0.01
      LIMBRK =  0.01
      HTXSFT =  2.0
      HTR1   =  0.01  !from bm fmvinit, the way to get 0 ht. loss is
      HTR2   =  0.01  !to set these low (can't be 0) and set htx to 0
      DO I= 1,MAXSP
        PSOFT(I) = 0.0
      ENDDO
C----------
C  ** SPECIES-LEVEL VARIABLE ASSIGNMENT **
C----------
C  V2T() - DERIVED BY USING TABLE 4-3A OF THE
C  'WOOD HANDBOOK' USDA FOREST PRODUCTS LAB. 1999.  FPL-GTR-113.
C  AS WELL AS 'HARDWOODS OF NORTH AMERICA' 1995. FPL-GTR-83 AND
C  JENKINS ET. AL. 2004. GTR-NE-319.
C
C  LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES - TAKEN FROM SN-FFE
C  LEAFLF WAS SET WITH INFO FROM HARLOW AND HARRAH'S TEXTBOOK
C  OF DENDROLOGY, 9TH EDITION
C  EXCEPTIONS:
C    EASTERN REDCEDAR FOUND IN MICHIGAN TREES, BARNES AND WAGNER
C
C  TFALL() - TIME TO FALL FOR CROWN COMPONENTS;
C  CATEGORIES ARE
C  0 -  FOLIAGE
C  1 -   0 - 0.25 IN
C  2 -   0.25 - 1.00 IN
C  3 -   1.00 - 3.00 IN
C  4 -   3.00 - 6.00 IN
C  5 -   6.00 - 12.00 IN
C
C  TFALL IS ASSIGNED FOR 6 DIFFERENT TFALLCLS GROUPS, AS DEFINED
C  AT THE SN-FFE WORKSHOP.  SPECIES NOT IN SN-FFE WERE CLASSES AS 5,
C  EXCEPT OSAGE-ORANGE, WHICH WAS SET TO 4.
C
C  NOTE: IF ANY TFALL VALUE IS GT 25, YOU SHOULD RE-DIM TFMAX IN FMPARM.
C
C  THE SNAG VARIABLES DESCRIBED BELOW ARE SET BY FIRST DEFINING A
C  SNAGCLS (1, 2, OR 3) FOR EACH SPECIES, AND THEN SETTING THE
C  VARIABLES FOR THE 3 SNAG CLASSES.  SNAGCLS IS DEFINED AS FOLLOWS:
C  1 - SPECIES DECAYS AND FALLS FASTER THAN AVERAGE
C  2 - SPECIES DECAYS AND FALLS AT AN AVERAGE RATE
C  3 - SPECIES DECAYS AND FALLS SLOWER THAN AVERAGE
C  SNAGCLS WAS TAKEN FROM SN-FFE.
C  SNAGCLS IS USED TO SET THE FOLLOWING VARIABLES:
C  ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
C  DECAYX() - DECAY RATE MULTIPLIER
C  FALLX()  - FALL RATE MULTIPLIER
C  HTX()    - HEIGHT-LOSS RATE MULTIPLIER
C
C  DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
C  CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
C  CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS
C
C  DKRCLS WAS ASSIGNED FROM INFO IN THE WOOD HANDBOOK:
C  HTTP://WWW.FPL.FS.FED.US/DOCUMNTS/FPLGTR/FPLGTR113/FPLGTR113.HTM
C  WHEN SPECIES WERE CLASSIFIED DIFFERENTLY FOR YOUNG OR OLD GROWTH,
C  YOUNG GROWTH WAS ASSUMED.
C  SPECIES NOT LISTED IN THE BOOK WERE ASSIGNED BASED ON INFO
C  PROVIDED AT THE SN WORKSHOP
C----------
      DO I= 1,MAXSP
C
        SELECT CASE (I)
C----------
C  EASTERN REDCEDAR
C----------
          CASE (1)
            V2T(I)     =  27.4
            TFALLCLS(I) = 1
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  JUNIPER SPECIES
C----------
          CASE (2)
            V2T(I)     =  27.4
            TFALLCLS(I) = 1
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  SHORTLEAF PINE
C----------
          CASE (3)
            V2T(I)     =  29.3
            TFALLCLS(I) = 6
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  VIRGINIA PINE
C----------
          CASE (4)
            V2T(I)     =  28.1
            TFALLCLS(I) = 6
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  LOBLOLLY PINE
C----------
          CASE (5)
            V2T(I)     =  29.3
            TFALLCLS(I) = 6
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  OTHER SOFTWOOD SPECIES
C----------
          CASE (6)
            V2T(I)     =  21.2
            TFALLCLS(I) = 6
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  EASTERN WHITE PINE
C----------
          CASE (7)
            V2T(I)     =  21.2
            TFALLCLS(I) = 6
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  BLACK WALNUT
C----------
          CASE (8)
            V2T(I)     =  31.8
            TFALLCLS(I) = 4
            DKRCLS(I)  =  2
            SNAGCLS(I) =  2
C----------
C  BUTTERNUT
C----------
          CASE (9)
            V2T(I)     =  22.5
            TFALLCLS(I) = 4
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  TUPELO SPECIES
C----------
          CASE (10)
            V2T(I)     =  28.7
            TFALLCLS(I) = 3
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  SWAMP TUPELO
C----------
          CASE (11)
            V2T(I)     =  28.7
            TFALLCLS(I) = 3
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  WATER TUPELO
C----------
          CASE (12)
            V2T(I)     =  28.7
            TFALLCLS(I) = 3
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  BLACKGUM / BLACK TUPELO
C----------
          CASE (13)
            V2T(I)     =  28.7
            TFALLCLS(I) = 3
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  SELECT HICKORY
C----------
          CASE (14)
            V2T(I)     =  39.9
            TFALLCLS(I) = 2
            DKRCLS(I)  =  4
            SNAGCLS(I) =  3
C----------
C  SHAGBARK HICKORY
C----------
          CASE (15)
            V2T(I)     =  39.9
            TFALLCLS(I) = 2
            DKRCLS(I)  =  4
            SNAGCLS(I) =  3
C----------
C  SHELLBARK HICKORY
C----------
          CASE (16)
            V2T(I)     =  38.7
            TFALLCLS(I) = 2
            DKRCLS(I)  =  4
            SNAGCLS(I) =  3
C----------
C  MOCKERNUT HICKORY
C----------
          CASE (17)
            V2T(I)     =  39.9
            TFALLCLS(I) = 2 
            DKRCLS(I)  =  4
            SNAGCLS(I) =  3
C----------
C  PIGNUT HICKORY
C----------
          CASE (18)
            V2T(I)     =  41.2
            TFALLCLS(I) = 2  
            DKRCLS(I)  =  4
            SNAGCLS(I) =  3
C----------
C  HICKORY SPECIES
C----------
          CASE (19)
            V2T(I)     =  39.9
            TFALLCLS(I) = 2
            DKRCLS(I)  =  4
            SNAGCLS(I) =  3
C----------
C  WATER HICKORY
C----------
          CASE (20)
            V2T(I)     =  38.0
            TFALLCLS(I) = 2 
            DKRCLS(I)  =  4
            SNAGCLS(I) =  3
C----------
C  BITTERNUT HICKORY
C----------
          CASE (21)
            V2T(I)     =  37.4
            TFALLCLS(I) = 2
            DKRCLS(I)  =  4
            SNAGCLS(I) =  3
C----------
C  PECAN
C----------
          CASE (22)
            V2T(I)     =  37.4
            TFALLCLS(I) = 2
            DKRCLS(I)  =  4
            SNAGCLS(I) =  3
C----------
C  BLACK HICKORY
C----------
          CASE (23)
            V2T(I)     =  39.9
            TFALLCLS(I) = 2
            DKRCLS(I)  =  4
            SNAGCLS(I) =  3
C----------
C  AMERICAN BEECH
C----------
          CASE (24)
            V2T(I)     =  34.9
            TFALLCLS(I) = 4 
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  BLACK ASH
C----------
          CASE (25)
            V2T(I)     =  28.1
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  PUMPKIN ASH
C----------
          CASE (26)
            V2T(I)     =  29.9
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  BLUE ASH
C----------
          CASE (27)
            V2T(I)     =  33.1
            TFALLCLS(I) = 5 
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  EASTERN COTTONWOOD
C----------
          CASE (28)
            V2T(I)     = 23.1
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  RED MAPLE
C----------
          CASE (29)
            V2T(I)     =  30.6
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  BOXELDER
C----------
          CASE (30)
            V2T(I)     =  25.9
            TFALLCLS(I) = 5 
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  SILVER MAPLE
C----------
          CASE (31)
            V2T(I)     =  27.4
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  BLACK CHERRY
C----------
          CASE (32)
            V2T(I)     =  29.3
            TFALLCLS(I) = 4  
            DKRCLS(I)  =  2
            SNAGCLS(I) =  2
C----------
C  AMERICAN ELM
C----------
          CASE (33)
            V2T(I)     =  28.7
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  SUGARBERRY
C----------
          CASE (34)
            V2T(I)     =  30.6
            TFALLCLS(I) = 4  
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  HACKBERRY
C----------
          CASE (35)
            V2T(I)     =  30.6
            TFALLCLS(I) = 4
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  WINGED ELM
C----------
          CASE (36)
            V2T(I)     =  37.4
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  ELM SPECIES
C----------
          CASE (37)
            V2T(I)     =  28.7
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  SIBERIAN ELM
C----------
          CASE (38)
            V2T(I)     =  28.7
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  SLIPPERY ELM
C----------
          CASE (39)
            V2T(I)     =  29.9
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  ROCK ELM
C----------
          CASE (40)
            V2T(I)     =  35.6
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  YELLOW-POPLAR
C----------
          CASE (41)
            V2T(I)     =  24.9
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  AMERICAN BASSWOOD
C----------
          CASE (42)
            V2T(I)     =  20.0
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  SUGAR MAPLE
C----------
          CASE (43)
            V2T(I)     =  34.9
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  ASH SPECIES
C----------
          CASE (44)
            V2T(I)     =  33.1
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  WHITE ASH
C----------
          CASE (45)
            V2T(I)     =  34.3
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  GREEN ASH
C----------
          CASE (46)
            V2T(I)     =  33.1
            TFALLCLS(I) = 5 
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  WHITE OAK
C----------
          CASE (47)
            V2T(I)     =  37.4
            TFALLCLS(I) = 3
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  NORTHERN RED OAK
C----------
          CASE (48)
            V2T(I)     =  34.9
            TFALLCLS(I) = 4
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------
C  SOUTHERN RED OAK
C----------
          CASE (49)
            V2T(I)     =  32.4
            TFALLCLS(I) = 4
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------
C  BLACK OAK
C----------
          CASE (50)
            V2T(I)     =  34.9 
            TFALLCLS(I) = 4
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------
C  SCARLET OAK
C----------
          CASE (51)
            V2T(I)     =  37.4
            TFALLCLS(I) = 4
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------
C  BLACKJACK OAK
C----------
          CASE (52)
            V2T(I)     =  34.9
            TFALLCLS(I) = 2
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  CHINKAPIN OAK
C----------
          CASE (53)
            V2T(I)     =  37.4
            TFALLCLS(I) = 3
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  SWAMP WHITE OAK
C----------
          CASE (54)
            V2T(I)     =  39.9
            TFALLCLS(I) = 3
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  BUR OAK
C----------
          CASE (55)
            V2T(I)     =  36.2
            TFALLCLS(I) = 3
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  SWAMP CHESTNUT OAK
C----------
          CASE (56)
            V2T(I)     =  37.4
            TFALLCLS(I) = 3 
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------
C  POST OAK
C----------
          CASE (57)
            V2T(I)     =  37.4
            TFALLCLS(I) = 3 
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  DELTA POST OAK
C----------
          CASE (58)
            V2T(I)     =  37.4
            TFALLCLS(I) = 3 
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  CHESTNUT OAK
C----------
          CASE (59)
            V2T(I)     =  35.6
            TFALLCLS(I) = 3 
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------
C  PIN OAK
C----------
          CASE (60)
            V2T(I)     =  36.2
            TFALLCLS(I) = 4
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------
C  CHERRYBARK OAK
C----------
          CASE (61)
            V2T(I)     =  38.0
            TFALLCLS(I) = 4
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------
C  SHINGLE OAK
C----------
          CASE (62)
            V2T(I)     =  34.9
            TFALLCLS(I) = 4 
            DKRCLS(I)  =  2
            SNAGCLS(I) =  2
C----------
C  OVERCUP OAK
C----------
          CASE (63)
            V2T(I)     =  35.6
            TFALLCLS(I) = 3
            DKRCLS(I)  =  3        
            SNAGCLS(I) =  2
C----------
C  WATER OAK
C----------
          CASE (64)
            V2T(I)     =  34.9
            TFALLCLS(I) = 3
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  NUTTALL OAK
C----------
          CASE (65)
            V2T(I)     =  34.9
            TFALLCLS(I) = 4
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------
C  WILLOW OAK
C----------
          CASE (66)
            V2T(I)     =  34.9
            TFALLCLS(I) = 4
            DKRCLS(I)  =  2
            SNAGCLS(I) =  2
C----------
C  SHUMARD OAK
C----------
          CASE (67)
            V2T(I)     =  34.9
            TFALLCLS(I) = 4 
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------
C  OTHER UPLAND HARDWOODS
C----------
          CASE (68)
            V2T(I)     =  37.4
            TFALLCLS(I) = 2
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  SASSAFRAS
C----------
          CASE (69)
            V2T(I)     =  26.2
            TFALLCLS(I) = 4 
            DKRCLS(I)  =  2
            SNAGCLS(I) =  2
C----------
C  OHIO BUCKEYE
C----------
          CASE (70)
            V2T(I)     =  20.6
            TFALLCLS(I) = 5 
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  CATALPA
C----------
          CASE (71)
            V2T(I)     =  23.7
            TFALLCLS(I) = 4
            DKRCLS(I)  =  2
            SNAGCLS(I) =  2
C----------
C  COMMON PERSIMMON
C----------
          CASE (72)
            V2T(I)     =  39.9
            TFALLCLS(I) = 4
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  HONEYLOCUST
C----------
          CASE (73)
            V2T(I)     =  37.4
            TFALLCLS(I) = 2
            DKRCLS(I)  =  2
            SNAGCLS(I) =  3
C----------
C  BALSAM POPLAR
C----------
          CASE (74)
            V2T(I)     =  19.3
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  BIGTOOTH ASPEN
C----------
          CASE (75)
            V2T(I)     =  22.5
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  QUAKING ASPEN
C----------
          CASE (76)
            V2T(I)     =  21.8
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  BLACK LOCUST
C----------
          CASE (77)
            V2T(I)     =  41.2
            TFALLCLS(I) = 2 
            DKRCLS(I)  =  1
            SNAGCLS(I) =  3
C----------
C  OTHER LOWLAND SPECIES
C----------
          CASE (78)
            V2T(I)     =  28.7
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  SYCAMORE
C----------
          CASE (79)
            V2T(I)     =  28.7
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  BALDCYPRESS
C----------
          CASE (80)
            V2T(I)     =  26.2
            TFALLCLS(I) = 1
            DKRCLS(I)  =  3
            SNAGCLS(I) =  3
C----------
C  RIVER BIRCH
C----------
          CASE (81)
            V2T(I)     =  30.6
            TFALLCLS(I) = 5 
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  SWEETGUM
C----------
          CASE (82)
            V2T(I)     =  28.7
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  WILLOW SPECIES
C----------
          CASE (83)
            V2T(I)     =  22.5
            TFALLCLS(I) = 6
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  BLACK WILLOW
C----------
          CASE (84)
            V2T(I)     =  22.5
            TFALLCLS(I) = 6
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  NON-COMMERCIAL HARDWOODS
C----------
          CASE (85)
            V2T(I)     =  39.9
            TFALLCLS(I) = 5 
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------
C  AMERICAN HORNBEAM
C----------
          CASE (86)
            V2T(I)     =  36.2
            TFALLCLS(I) = 4
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------
C  EASTERN REDBUD
C----------
          CASE (87)
            V2T(I)     =  36.2
            TFALLCLS(I) = 5
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------
C  FLOWERING DOGWOOD
C----------
          CASE (88)
            V2T(I)     =  39.9
            TFALLCLS(I) = 5
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------
C  HAWTHORN SPECIES
C----------
          CASE (89)
            V2T(I)     =  38.7
            TFALLCLS(I) = 5 
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  KENTUCKY COFFEETREE
C----------
          CASE (90)
            V2T(I)     =  33.1
            TFALLCLS(I) = 5  
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------               
C  OSAGE-ORANGE  
C----------               
          CASE (91)       
            V2T(I)     =  47.4
            TFALLCLS(I) = 4 
            DKRCLS(I)  =  1
            SNAGCLS(I) =  2
C----------               
C  CUCUMBERTREE  
C----------               
          CASE (92)       
            V2T(I)     =  27.4
            TFALLCLS(I) = 4
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------               
C  SWEETBAY  
C----------               
          CASE (93)       
            V2T(I)     =  26.2
            TFALLCLS(I) = 4
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------               
C  MULBERRY SPECIES  
C----------               
          CASE (94)       
            V2T(I)     =  36.8
            TFALLCLS(I) = 5
            DKRCLS(I)  =  1
            SNAGCLS(I) =  2
C----------               
C  EASTERN HOPHORNBEAM  
C----------               
          CASE (95)       
            V2T(I)     =  39.3
            TFALLCLS(I) = 4
            DKRCLS(I)  =  3
            SNAGCLS(I) =  2
C----------               
C  SOURWOOD  
C----------               
          CASE (96)       
            V2T(I)     =  31.2
            TFALLCLS(I) = 5
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C
        END SELECT
C
        SELECT CASE (I)
          CASE (1:7)
            LSW(I) = .TRUE.
          CASE DEFAULT
            LSW(I) = .FALSE.
        END SELECT
C
        SELECT CASE (I)
          CASE (1,2)
            LEAFLF(I) = 5
          CASE (3)
            LEAFLF(I) = 4
          CASE (4,5)
            LEAFLF(I) = 3
          CASE (6,7)
            LEAFLF(I) = 2
          CASE DEFAULT
            LEAFLF(I) = 1
        END SELECT
C
        IF (I .LE. 2) THEN !redcedar /juniper species
          TFALL(I,0) = 3.0  ! foliage
        ELSE
          TFALL(I,0) = 1.0
        ENDIF
C
        SELECT CASE (TFALLCLS(I))
C
          CASE (1)  ! baldcypress/redcedar group
            TFALL(I,1) = 5.0
            TFALL(I,3) = 10.0
            TFALL(I,4) = 25.0
C
          CASE (2)  ! hickory/blackjack oak group
            TFALL(I,1) = 3.0
            TFALL(I,3) = 6.0
            TFALL(I,4) = 12.0
C
          CASE (3)  ! white oak group
            TFALL(I,1) = 2.0
            TFALL(I,3) = 5.0
            TFALL(I,4) = 10.0
C
          CASE (4)  ! red oak group
            TFALL(I,1) = 1.0
            TFALL(I,3) = 4.0
            TFALL(I,4) = 8.0
C
          CASE (5)  ! ash/elm/cottonwood group
            TFALL(I,1) = 1.0
            TFALL(I,3) = 3.0
            TFALL(I,4) = 6.0
C
          CASE (6)  ! pines
            TFALL(I,1) = 1.0
            TFALL(I,3) = 2.0
            TFALL(I,4) = 4.0
C
        END SELECT
C
        TFALL(I,2) = TFALL(I,1)
        TFALL(I,5) = TFALL(I,4)
C
        SELECT CASE (SNAGCLS(I))
C
          CASE (1)  ! pines and others
                    ! this group decays and falls faster than average
            DECAYX(I)  = 0.07 ! 12 inch tree is soft in 2 years
            FALLX(I)   = 7.17 ! 95% of 12-inchers are down in 3 years
            ALLDWN(I)  = 6.0
            IF ((I .GE. 3) .AND. (I .LE. 7)) THEN ! pines
              ALLDWN(I) = 50.0
            ENDIF
C
          CASE (2)  ! black oak and others
                    ! this group decays and falls at average rate
            DECAYX(I)  = 0.21  ! 12 inch tree is soft in 6 years
            FALLX(I)   = 3.07  ! 95% of 12-inchers are down in 7 years
            ALLDWN(I)  = 15.0
C
          CASE (3)  ! white oak, redcedar, and others
                    ! this group decays and falls slower than average
            DECAYX(I)  = 0.35  ! 12 inch tree is soft in 10 years
            FALLX(I)   = 1.96  ! 95% of 12-inchers are down in 11 years
            ALLDWN(I)  = 25.0
            IF (I .LE. 2) THEN  ! redcedar
              ALLDWN(I) = 100.0
            ENDIF
C
        END SELECT
C
        DO J= 1,4
          HTX(I,J) =   0.0 ! no height loss
        ENDDO
C----------
C  CONVERT LB/FT**3 TO TONS/FT**3
C----------
        V2T(I) = V2T(I) / 2000.0
C
      ENDDO
C----------
C  PARAMETERS FOR POST-BURN SNAG FALL RATES
C----------
      PBSCOR =  0.0
      PBSOFT =  1.0
      PBSMAL =  0.9
      PBSIZE = 12.0
      PBTIME =  7.0 !may need to make this shorter??
C----------
C  PARAMETERS FOR FUEL MODEL SELECTION
C  COVER TYPE 
C----------
      OLDICT = 0
C----------
C  DROUGHT START AND END YEARS
C----------
      IDRYB  = 0 
      IDRYE  = 0
C----------
C  CRITICAL % CHANGE REQUIERED TO TRIGGER ACTIVITY FUELS
C----------
      SLCRIT = 10.0
C
      RETURN
      END
