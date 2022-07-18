      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C FIRE-SN $Id$
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
C  DECAY RATES FOR DECAY CLASS 1 (PINES) BASED ON PHIL RADTKE'S ANALYSIS
C----------
      DKR(1,1)  = 0.11
      DKR(2,1)  = 0.11
      DKR(3,1)  = 0.11
      DKR(4,1)  = 0.11
      DKR(5,1)  = 0.11
      DKR(6,1)  = 0.11
      DKR(7,1)  = 0.11
      DKR(8,1)  = 0.11
      DKR(9,1)  = 0.11
C----------
C  DECAY RATES FOR DECAY CLASSES 2 - 4 
C  THESE WERE THE ORIGINAL RATES SET FOR SN FROM ABBOTT AND CROSSLEY / BARBER AND VANLEAR
C----------
      DKR(1,2)  = 0.11
      DKR(2,2)  = 0.11
      DKR(3,2)  = 0.09
      DKR(4,2)  = 0.07
      DKR(5,2)  = 0.07
      DKR(6,2)  = 0.07
      DKR(7,2)  = 0.07
      DKR(8,2)  = 0.07
      DKR(9,2)  = 0.07
C 
      DO I = 1,9
        DO J = 3,4
          DKR(I,J) = DKR(I,2)
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
C  AT WORKSHOP, PARTICIPANTS WANTED TO SET IT SO 25% OF SNAGS WILL
C  LOSE 50% OF THEIR HEIGHT IN 10 YEARS.  CAN WE JUST APPLY THIS TO A
C  QUARTER OF THE SNAGS THOUGH? IN SPOKANE, DECIDED TO HAVE NO HEIGHT
C  LOSS--SNAGS FALL QUICKLY ANYHOW
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
C
C  LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES
C  LEAFLF WAS SET WITH INFO FROM HARLOW AND HARRAH'S TEXTBOOK
C  OF DENDROLOGY, 9TH EDITION
C  EXCEPTIONS:
C    EASTERN REDCEDAR FOUND IN MICHIGAN TREES, BARNES AND WAGNER
C    HOLLY FOUND AT HTTP://WWW.AMERICANFORESTS.ORG/
C    PRODUCTSANDPUBS/MAGAZINE/ARCHIVES/2002WINTER/INPROFILE.PHP
C    LOBLOLLY BAY FOUND AT HTTP://WWW.FL-DOF.COM/
C    PUBS/TREES_OF_FLORIDA/LOBLOLLYBAY.HTML
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
C  AT THE WORKSHOP.  SPECIES NOT IN THE OZARKS WERE CLASSED AS 5.
C
C  NOTE: IF ANY TFALL VALUE IS GT 25, YOU SHOULD RE-DIM TFMAX IN FMPARM.
C
C  THE SNAG VARIABLES DESCRIBED BELOW ARE SET BY FIRST DEFINING A
C  SNAGCLS (1, 2, OR 3) FOR EACH SPECIES, AND THEN SETTING THE
C  VARIABLES FOR THE 3 SNAG CLASSES.  SNAGCLS IS DEFINED AS FOLLOWS:
C  1 - SPECIES DECAYS AND FALLS FASTER THAN AVERAGE
C  2 - SPECIES DECAYS AND FALLS AT AN AVERAGE RATE
C  3 - SPECIES DECAYS AND FALLS SLOWER THAN AVERAGE
C  SPECIES NOT PRESENT IN THE OZARKS WERE PUT IN SNAGCLS 2 (AVERAGE)
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
C  PROVIDED AT THE WORKSHOP ON SNAG DECAY (OAKS CLASSED THIS WAY)
C  SPECIES NOT PRESENT IN THE OZARKS WERE CLASSED AS 4 IF NOT IN
C  WOOD HANDBOOK
C----------
      DO I= 1,MAXSP
C
        SELECT CASE (I)
C----------
C  FIR SP.
C----------
            CASE (1)
                V2T(I)     =  20.6 ! used balsam fir
            TFALLCLS(I) =  5  ! not in ozarks
            LEAFLF(I)  =   8.0  !7-10
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2  ! not in ozarks
C----------
C  REDCEDAR
C----------
          CASE (2)
            V2T(I)     =  27.4
            TFALLCLS(I) =  1
            LEAFLF(I)  =   5.0  !5-6
            DKRCLS(I)  =   2
            SNAGCLS(I) =   3
C----------
C  SPRUCE
C----------
          CASE (3)
            V2T(I)     =  23.1 ! used red spruce
            TFALLCLS(I) =  5  ! not in ozarks
            LEAFLF(I)  =   8.0 !7-10
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2  ! not in ozarks
C----------
C  SANDPINE
C----------
          CASE (4)
            V2T(I)     =  28.7
            TFALLCLS(I) =  6
            LEAFLF(I)  =   2.0 !2-3
            DKRCLS(I)  =   1
            SNAGCLS(I) =   1
C----------
C  SHORTLEAF PINE
C----------
          CASE (5)
            V2T(I)     =  29.3
            TFALLCLS(I) =  6
            LEAFLF(I)  =   4.0 !3-5
            DKRCLS(I)  =   1
            SNAGCLS(I) =   1
C----------
C  SLASH PINE
C----------
          CASE (6)
            V2T(I)     =  33.7
            TFALLCLS(I) =  6
            LEAFLF(I)  =   2.0
            DKRCLS(I)  =   1  
            SNAGCLS(I) =   1
C----------
C  SPRUCE PINE
C----------
          CASE (7)
            V2T(I)     =  25.6
            TFALLCLS(I) =  6
            LEAFLF(I)  =   2.0 ! 2-3
            DKRCLS(I)  =   1
            SNAGCLS(I) =   1
C----------
C  LONGLEAF PINE
C----------
          CASE (8)
            V2T(I)     =  33.7
            TFALLCLS(I) =  6
            LEAFLF(I)  =   2.0
            DKRCLS(I)  =   1  
            SNAGCLS(I) =   1
C----------
C  TABLE MOUNTAIN PINE
C----------
          CASE (9)
            V2T(I)     =  28.1 ! used virginia pine
            TFALLCLS(I) =  6
            LEAFLF(I)  =   3.0
            DKRCLS(I)  =   1
            SNAGCLS(I) =   1
C----------
C  PITCH PINE
C----------
          CASE (10)
            V2T(I)     =  29.3
            TFALLCLS(I) =  6
            LEAFLF(I)  =   2.0
            DKRCLS(I)  =   1
            SNAGCLS(I) =   1
C----------
C  POND PINE
C----------
          CASE (11)
            V2T(I)     =  31.8
            TFALLCLS(I) =  6
            LEAFLF(I)  =   2.0 !2-3
            DKRCLS(I)  =   1
            SNAGCLS(I) =   1
C----------
C  EASTERN WHITE PINE
C----------
          CASE (12)
            V2T(I)     =  21.2
            TFALLCLS(I) =  6
            LEAFLF(I)  =   2.0
            DKRCLS(I)  =   1
            SNAGCLS(I) =   1
C----------
C  LOBLOLLY PINE
C----------
          CASE (13)
            V2T(I)     =  29.3
            TFALLCLS(I) =  6
            LEAFLF(I)  =   3.0
            DKRCLS(I)  =   1
            SNAGCLS(I) =   1
C----------
C  VIRGINIA PINE
C----------
          CASE (14)
            V2T(I)     =  28.1
            TFALLCLS(I) =  6
            LEAFLF(I)  =   3.0 !3-4
            DKRCLS(I)  =   1
            SNAGCLS(I) =   1
C----------
C  BALDCYPRESS
C----------
          CASE (15)
            V2T(I)     =  26.2
            TFALLCLS(I) =  1
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3 ! assuming young growth
            SNAGCLS(I) =   3
C----------
C  PONDCYPRESS
C----------
          CASE (16)
            V2T(I)     =  26.2 !used baldcypress
            TFALLCLS(I) =  1
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3 ! assuming young growth
            SNAGCLS(I) =   3
C----------
C  HEMLOCK
C----------
          CASE (17)
            V2T(I)     =  23.7
            TFALLCLS(I) =  5  ! not in ozarks
            LEAFLF(I)  =   4.0 ! 3-6
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2  ! not in ozarks
C----------
C  FLORIDA MAPLE
C----------
          CASE (18)
            V2T(I)     =  34.9 !used sugar maple
      TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  BOXELDER
C----------
          CASE (19)
            V2T(I)     =  30.6  !used red maple
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  RED MAPLE
C----------
          CASE (20)
            V2T(I)     =  30.6
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  SILVER MAPLE
C----------
          CASE (21)
            V2T(I)     =  27.4
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  SUGAR MAPLE
C----------
          CASE (22)
            V2T(I)     =  34.9
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  BUCKEYE/HORSECHESTNUT
C----------
          CASE (23)
            V2T(I)     =  34.9 !used black oak
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  BIRCH SP.
C----------
          CASE (24)
            V2T(I)     =  34.3 !used yellow birch
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   1
C----------
C  SWEET BIRCH
C----------
          CASE (25)
            V2T(I)     =  37.4
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   1
C----------
C  AMERICAN HORNBEAM/MUSCLEWOOD
C----------
          CASE (26)
            V2T(I)     =  34.9 !used black oak
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3  ! not in wood book, used workshop notes
            SNAGCLS(I) =   2
C----------
C  HICKORY SP.
C----------
          CASE (27)
            V2T(I)     =  39.9 !used shagbark/mockernut
            TFALLCLS(I) =  2
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   3
C----------
C  CATALPA
C----------
          CASE (28)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2
            SNAGCLS(I) =   2
C----------
C  HACKBERRY
C----------
          CASE (29)
            V2T(I)     =  30.6
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  EASTERN REDBUD
C----------
          CASE (30)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3 ! not in wood book, used workshop notes
            SNAGCLS(I) =   2
C----------
C  FLOWERING DOGWOOD
C----------
          CASE (31)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3 ! not in wood book, used workshop notes
            SNAGCLS(I) =   2
C----------
C  PERSIMMON
C----------
          CASE (32)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2 ! not in wood book, used workshop notes
            SNAGCLS(I) =   3
C----------
C  AMERICAN BEECH
C----------
          CASE (33)
            V2T(I)     =  34.9
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  ASH
C----------
          CASE (34)
            V2T(I)     =  33.1 ! used green ash
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  WHITE ASH
C----------
          CASE (35)
            V2T(I)     =  34.3
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  BLACK ASH
C----------
          CASE (36)
            V2T(I)     =  28.1
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  GREEN ASH
C----------
          CASE (37)
            V2T(I)     =  33.1
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  HONEYLOCUST
C----------
          CASE (38)
            V2T(I)     =  37.4
            TFALLCLS(I) =  2
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2
            SNAGCLS(I) =   3
C----------
C  LOBLOLLY BAY
C----------
          CASE (39)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  5  ! not in ozarks
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4 ! not in wood handbook, not in ozarks
            SNAGCLS(I) =   2  ! not in ozarks
C----------
C  SILVERBELL
C----------
          CASE (40)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  5  ! not in ozarks
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4 ! not in wood handbook, not in ozarks
            SNAGCLS(I) =   2  ! not in ozarks
C----------
C  AMERICAN HOLLY
C----------
          CASE (41)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  4
            LEAFLF(I)  =   3.0
            DKRCLS(I)  =   3 ! not in wood book, used workshop notes
            SNAGCLS(I) =   2
C----------
C  BUTTERNUT
C----------
          CASE (42)
            V2T(I)     =  22.5
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  BLACK WALNUT
C----------
          CASE (43)
            V2T(I)     =  31.8
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2
            SNAGCLS(I) =   2
C----------
C  SWEET GUM
C----------
          CASE (44)
            V2T(I)     =  28.7
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  YELLOW-POPLAR
C----------
          CASE (45)
            V2T(I)     =  24.9
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  MAGNOLIA SP.
C----------
          CASE (46)
            V2T(I)     =  27.4 ! used cucumbertree
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0  ! magnolias as a group are mostly
                                ! deciduous
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  CUCUMBERTREE
C----------
          CASE (47)
            V2T(I)     =  27.4
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  SOUTHERN MAGNOLIA
C----------
          CASE (48)
            V2T(I)     =  28.7
            TFALLCLS(I) =  4
            LEAFLF(I)  =   2.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  SWEETBAY
C----------
          CASE (49)
            V2T(I)     =  27.4 !used cucumbertree
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  BIGLEAF MAGNOLIA
C----------
          CASE (50)
            V2T(I)     =  27.4 ! used cucumbertree
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  APPLE SP.
C----------
          CASE (51)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3 ! not in wood book, used workshop notes
            SNAGCLS(I) =   2
C----------
C  MULBERRY SP.
C----------
          CASE (52)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2
            SNAGCLS(I) =   2
C----------
C  WATER TUPELO
C----------
          CASE (53)
            V2T(I)     =  28.7
            TFALLCLS(I) =  3
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2 ! not in wood book, used workshop notes
            SNAGCLS(I) =   3
C----------
C  BLACKGUM
C----------
          CASE (54)
            V2T(I)     =  28.7
            TFALLCLS(I) =  3
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2 ! not in wood book, used workshop notes
            SNAGCLS(I) =   3
C----------
C  SWAMP TUPELO
C----------
          CASE (55)
            V2T(I)     =  28.7
            TFALLCLS(I) =  3
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2 ! not in wood book, used workshop notes
            SNAGCLS(I) =   3
C----------
C  EASTERN HOPHORNBEAM/IRONWOOD
C----------
          CASE (56)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3 ! not in wood book, used workshop notes
            SNAGCLS(I) =   2
C----------
C  SOURWOOD
C----------
          CASE (57)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  5  ! not in ozarks
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4 ! not in wood handbook, not in ozarks
            SNAGCLS(I) =   2  ! not in ozarks
C----------
C  REDBAY
C----------
          CASE (58)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  5  ! not in ozarks
            LEAFLF(I)  =   1.0 ! not sure about this one
            DKRCLS(I)  =   4 ! not in wood handbook, not in ozarks
            SNAGCLS(I) =   2  ! not in ozarks
C----------
C  SYCAMORE
C----------
          CASE (59)
            V2T(I)     =  28.7
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  COTTONWOOD
C----------
          CASE (60)
            V2T(I)     =  23.1
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   1
C----------
C  BIGTOOTH ASPEN
C----------
          CASE (61)
            V2T(I)     =  22.5
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   1
C----------
C  BLACK CHERRY
C----------
          CASE (62)
            V2T(I)     =  29.3
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2
            SNAGCLS(I) =   2
C----------
C  WHITE OAK
C----------
          CASE (63)
            V2T(I)     =  37.4
            TFALLCLS(I) =  3
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2 ! mapping oaks based on workshop input on
                             ! decay. most red oaks are generally class
                             ! 3, white oaks are generally class 2.
                             ! done because w.h. only classifies
                             ! white oak group, not red
            SNAGCLS(I) =   3
C----------
C  SCARLET OAK
C----------
          CASE (64)
            V2T(I)     =  37.4
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3
            SNAGCLS(I) =   2
C----------
C  SOUTHERN RED OAK
C----------
          CASE (65)
            V2T(I)     =  32.4
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3
            SNAGCLS(I) =   2
C----------
C  CHERRYBARK OAK/SWAMP RED OAK
C----------
          CASE (66)
            V2T(I)     =  38.0
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3
            SNAGCLS(I) =   2
C----------
C  TURKEY OAK
C----------
          CASE (67)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3
            SNAGCLS(I) =   2
C----------
C  LAUREL OAK
C----------
          CASE (68)
            V2T(I)     =  34.9
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3
            SNAGCLS(I) =   2
C----------
C  OVERCUP OAK
C----------
          CASE (69)
            V2T(I)     =  35.6
            TFALLCLS(I) =  3
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3
            SNAGCLS(I) =   2
C----------
C  BLACKJACK OAK
C----------
          CASE (70)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  2
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2
            SNAGCLS(I) =   3
C----------
C  SWAMP CHESTNUT OAK
C----------
          CASE (71)
            V2T(I)     =  37.4
            TFALLCLS(I) =  3
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3
            SNAGCLS(I) =   2
C----------
C  CHINKAPIN OAK
C----------
          CASE (72)
            V2T(I)     =  37.4 ! used white oak
            TFALLCLS(I) =  3
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2
            SNAGCLS(I) =   3
C----------
C  WATER OAK
C----------
          CASE (73)
            V2T(I)     =  34.9
            TFALLCLS(I) =  3
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2
            SNAGCLS(I) =   3
C----------
C  CHESTNUT OAK
C----------
          CASE (74)
            V2T(I)     =  35.6
            TFALLCLS(I) =  3
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3
            SNAGCLS(I) =   2
C----------
C  NORTHERN RED OAK
C----------
          CASE (75)
            V2T(I)     =  34.9
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3
            SNAGCLS(I) =   2
C----------
C  SHUMARK OAK
C----------
          CASE (76)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3
            SNAGCLS(I) =   2
C----------
C  POST OAK
C----------
          CASE (77)
            V2T(I)     =  37.4
           TFALLCLS(I) =  3
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2
            SNAGCLS(I) =   3
C----------
C  BLACK OAK
C----------
          CASE (78)
            V2T(I)     =  34.9
            TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   3
            SNAGCLS(I) =   2
C----------
C  LIVE OAK
C----------
          CASE (79)
            V2T(I)     =  49.9
            TFALLCLS(I) =  5  ! not in ozarks
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2
            SNAGCLS(I) =   2  ! not in ozarks
C----------
C  BLACK LOCUST
C----------
          CASE (80)
            V2T(I)     =  41.2
            TFALLCLS(I) =  2
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2
            SNAGCLS(I) =   3
C----------
C  WILLOW
C----------
          CASE (81)
            V2T(I)     =  22.5
            TFALLCLS(I) =  6
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   1
C----------
C  SASSAFRAS
C----------
          CASE (82)
            V2T(I)     =  26.2
             TFALLCLS(I) =  4
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   2
            SNAGCLS(I) =   2
C----------
C  BASSWOOD
C----------
          CASE (83)
            V2T(I)     =  20.0
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   1
C----------
C  ELM
C----------
          CASE (84)
            V2T(I)     =  28.7 ! used amer. elm
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   1
C----------
C  WINGED ELM
C----------
          CASE (85)
            V2T(I)     =  28.7 ! used amer. elm
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   1
C----------
C  AMERICAN ELM
C----------
          CASE (86)
            V2T(I)     =  28.7
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   1
C----------
C  SLIPPERY ELM
C----------
          CASE (87)
            V2T(I)     =  29.9
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   1
C----------
C  SOFTWOODS, MISC.
C----------
          CASE (88)
            V2T(I)     =  27.4 ! used redcedar
            TFALLCLS(I) =  5
            LEAFLF(I)  =   2.0 !not sure who this should be mapped to
            DKRCLS(I)  =   1
            SNAGCLS(I) =   1
C----------
C  HARDWOODS, MISC.
C----------
          CASE (89)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C----------
C  UNKNOWN OR NOT LISTED
C----------
          CASE (90)
            V2T(I)     =  34.9 ! used black oak
            TFALLCLS(I) =  5
            LEAFLF(I)  =   1.0
            DKRCLS(I)  =   4
            SNAGCLS(I) =   2
C
        END SELECT
C
        SELECT CASE (I)
          CASE (1:17,88)
            LSW(I) = .TRUE.
          CASE DEFAULT
            LSW(I) = .FALSE.
        END SELECT
C
        IF (I .EQ. 2) THEN !redcedar
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
            IF ((I .GE. 4) .AND. (I .LE. 14)) THEN ! pines
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
            IF (I .EQ. 2) THEN  ! redcedar
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
C  COVER TYPE (NOT USED IN OZ-FFE)
C----------
      OLDICT = 0
C----------
C  DROUGHT START AND END YEARS
C----------
      IDRYB  = 0 ! not used in OZ-FFE, but still set
      IDRYE  = 0
C----------
C  CRITICAL % CHANGE REQUIERED TO TRIGGER ACTIVITY FUELS
C----------
      SLCRIT = 10.0
C
      RETURN
      END
