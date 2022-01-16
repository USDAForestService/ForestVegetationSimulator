      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C FIRE-LS $Id: fmvinit.f 0000 2018-02-14 00:00:00Z gedixon $
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
      PREWND(1)=25.
      PREWND(2)=15.
      POTEMP(1)=80.
      POTEMP(2)=50.
C----------
C  DECAY RATES - FROM ABBOTT AND CROSSLEY, ECOLOGY, 1982
C                     ALBAN AND PASTOR, CAN J FOR RES, 1993
C                     TYRRELL AND CROW, CAN J FOR RES, 1994
C----------
      DKR(1,1)  = 0.11
      DKR(2,1)  = 0.11
      DKR(3,1)  = 0.09
      DKR(4,1)  = 0.06
      DKR(5,1)  = 0.06
      DKR(6,1)  = 0.02
      DKR(7,1)  = 0.02
      DKR(8,1)  = 0.02
      DKR(9,1)  = 0.02
C
      DO I = 1,9
        DO J = 2,4
          DKR(I,J) = DKR(I,1)
        ENDDO
      ENDDO
C----------
C  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
C  FOR LITTER DECAY - PERALA AND ALBAN SUGGESTS 0.18, BUT THIS INCLUDES
C  TWIGS, SO USED INFO FROM MELILLO (NH) INSTEAD.  THIS IS CONSISTENT WITH
C  DISCUSSIONS WITH COELI HOOVER.
C----------
      DO J = 1,4
        DKR(10,J) = 0.31
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
C----------
      NZERO  =  0.01
      LIMBRK =  0.01
      HTXSFT =  2.0
      HTR1   =  0.1  ! based on LS workshop info
      HTR2   =  0.01
      DO I= 1,MAXSP
        PSOFT(I) = 0.0
      ENDDO
C----------
C  ** SPECIES-LEVEL VARIABLE ASSIGNMENT **
C----------
C  V2T() - DERIVED BY USING THE SPECIFIC GRAVITY VALUES IN TABLE 4-3A IN
C  THE 'WOOD HANDBOOK' USDA FOREST PRODUCTS LAB. 1999.  FPL-GTR-113.
C
C  LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES
C  LEAFLF WAS SET WITH INFO FROM HARLOW AND HARRAH'S TEXTBOOK
C  OF DENDROLOGY, 9TH EDITION (EASTERN REDCEDAR AND NORTHERN WHITE CEDAR
C  FOUND IN MICHIGAN TREES, BARNES AND WAGNER)
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
C  TFALL IS ASSIGNED FOR 4 DIFFERENT TFALLCLS GROUPS.  THIS INFO IS FROM
C  THE SN VARIANT, SINCE WE FORGOT TO DISCUSS IT AT THE LS WORKSHOP.  SOME
C  CHANGES WERE MADE BASED ON THE RELATIVE FALL RATES OF VARIOUS SPECIES
C  AND GENERAL LS WORKSHOP NOTES.
C
C  THE SNAG VARIABLES DESCRIBED BELOW ARE SET BY FIRST DEFINING A
C  SNAGCLS (1 - 6) FOR EACH SPECIES, AND THEN SETTING THE
C  VARIABLES FOR THE 6 SNAG CLASSES.  SNAGCLS IS DEFINED AS FOLLOWS:
C  1 - ASPEN, BIRCH, SPRUCE, FIR, POPLAR, BASSWOOD (FASTEST FALLERS)
C  2 - JACK PINE
C  3 - WHITE PINE
C  4 - RED PINE
C  5 - ASH, MAPLE, BEECH, ELM
C  6 - CEDAR, TAMARACK, OAK, HICKORY, HEMLOCK (SLOWEST FALLERS)
C  SPECIES WERE PUT IN SNAGCLS 5 BY DEFAULT
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
C  SPECIES NOT LISTED WERE CLASSED AS 4 IF NOT IN
C  WOOD HANDBOOK
C----------
      DO I= 1,MAXSP
C
        SELECT CASE (I)
C----------
C  JACK PINE
C----------
          CASE (1)
            V2T(I)     =  24.9
            TFALLCLS(I) = 4
            LEAFLF(I)  =  2.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  2
C----------
C  SCOTCH PINE
C----------
          CASE (2)
            V2T(I)     =  25.6
            TFALLCLS(I) = 4
            LEAFLF(I)  =  3.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  4
C----------
C  RED PINE (NATURAL)
C----------
          CASE (3)
            V2T(I)     =  25.6
            TFALLCLS(I) = 4
            LEAFLF(I)  =  4.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  4
C----------
C  RED PINE PLANTATION
C----------
          CASE (4)
            V2T(I)     =  25.6
            TFALLCLS(I) = 4
            LEAFLF(I)  =  4.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  4
C----------
C  EASTERN WHITE PINE
C----------
          CASE (5)
            V2T(I)     =  21.2
            TFALLCLS(I) = 4
            LEAFLF(I)  =  2.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  3
C----------
C  WHITE SPRUCE
C----------
          CASE (6)
            V2T(I)     =  23.1
            TFALLCLS(I) = 4
            LEAFLF(I)  =  8.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  NORWAY SPRUCE
C----------
          CASE (7)
            V2T(I)     =  23.1
            TFALLCLS(I) = 4
            LEAFLF(I)  =  8.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  BALSAM FIR
C----------
          CASE (8)
            V2T(I)     =  20.6
            TFALLCLS(I) = 4
            LEAFLF(I)  =  8.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  BLACK SPRUCE
C----------
          CASE (9)
            V2T(I)     =  23.7
            TFALLCLS(I) = 4
            LEAFLF(I)  =  8.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  TAMARACK
C----------
          CASE (10)
            V2T(I)     =  30.6
            TFALLCLS(I) = 1
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  3
            SNAGCLS(I) =  6
C----------
C  NORTHERN WHITE-CEDAR
C----------
          CASE (11)
            V2T(I)     =  18.1
            TFALLCLS(I) = 1
            LEAFLF(I)  =  2.0
            DKRCLS(I)  =  2
            SNAGCLS(I) =  6
C----------
C  EASTERN HEMLOCK
C----------
          CASE (12)
            V2T(I)     =  23.7
            TFALLCLS(I) = 1
            LEAFLF(I)  =  3.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  6
C----------
C  OTHER SOFTWOOD
C----------
          CASE (13)
            V2T(I)     =  27.4
            TFALLCLS(I) = 1
            LEAFLF(I)  = 5.0 !used eastern redcedar
            DKRCLS(I)  = 2 !used eastern redcedar
            SNAGCLS(I) = 6 !used eastern redcedar
C----------
C  EASTERN REDCEDAR
C----------
          CASE (14)
            V2T(I)     =  27.4
            TFALLCLS(I) = 1
            LEAFLF(I)  =  5.0
            DKRCLS(I)  =  2
            SNAGCLS(I) =  6
C----------
C  BLACK ASH
C----------
          CASE (15)
            V2T(I)     =  28.1
            TFALLCLS(I) = 3
            LEAFLF(I)  = 1.0
            DKRCLS(I)  = 4
            SNAGCLS(I) = 5
C----------
C  GREEN ASH
C----------
          CASE (16)
            V2T(I)     =  33.1
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  EASTERN COTTONWOOD
C----------
          CASE (17)
            V2T(I)     =  23.1
            TFALLCLS(I) = 4
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  SILVER MAPLE
C----------
          CASE (18)
            V2T(I)     =  27.4
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  RED MAPLE
C----------
          CASE (19)
            V2T(I)     =  30.6
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  BLACK CHERRY
C----------
          CASE (20)
            V2T(I)     =  29.3
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  2
            SNAGCLS(I) =  5
C----------
C  AMERICAN ELM
C----------
          CASE (21)
            V2T(I)     =  28.7
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  SLIPPERY ELM
C----------
          CASE (22)
            V2T(I)     =  29.9
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  ROCK ELM
C----------
          CASE (23)
            V2T(I)     =  35.6
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  YELLOW BIRCH
C----------
          CASE (24)
            V2T(I)     =  34.3
            TFALLCLS(I) = 4
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  AMERICAN BASSWOOD
C----------
          CASE (25)
            V2T(I)     = 20.0
            TFALLCLS(I) = 4
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  SUGAR MAPLE
C----------
          CASE (26)
            V2T(I)     =  34.9
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  BLACK MAPLE
C----------
          CASE (27)
            V2T(I)     =  32.4
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  AMERICAN BEECH
C----------
          CASE (28)
            V2T(I)     =  34.9
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  WHITE ASH
C----------
          CASE (29)
            V2T(I)     =  34.3
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  WHITE OAK
C----------
          CASE (30)
            V2T(I)     =  37.4
            TFALLCLS(I) = 1
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  2
            SNAGCLS(I) =  6
C----------
C  SWAMP WHITE OAK
C----------
          CASE (31)
            V2T(I)     =  39.9
            TFALLCLS(I) = 1
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  2
            SNAGCLS(I) =  6
C----------
C  BUR OAK
C----------
          CASE (32)
            V2T(I)     =  36.2
            TFALLCLS(I) = 1
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  2
            SNAGCLS(I) =  6
C----------
C  CHINKAPIN OAK
C----------
          CASE (33)
            V2T(I)     =  37.4
            TFALLCLS(I) = 1
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  2
            SNAGCLS(I) =  6
C----------
C  NORTHERN RED OAK
C----------
          CASE (34)
            V2T(I)     =  34.9
            TFALLCLS(I) = 2
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  2 ! used white oak
            SNAGCLS(I) =  6
C----------
C  BLACK OAK
C----------
          CASE (35)
            V2T(I)     =  34.9
            TFALLCLS(I) = 2
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  2 ! used white oak
            SNAGCLS(I) =  6
C----------
C  NORTHERN PIN OAK
C----------
          CASE (36)
            V2T(I)     =  36.2
            TFALLCLS(I) = 2
            LEAFLF(I)  = 1.0
            DKRCLS(I)  = 2 ! used white oak
            SNAGCLS(I) = 6
C----------
C  BITTERNUT HICKORY
C----------
          CASE (37)
            V2T(I)     =  37.4
            TFALLCLS(I) = 1
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  6
C----------
C  PIGNUT HICKORY
C----------
          CASE (38)
            V2T(I)     =  41.2
            TFALLCLS(I) = 1
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  6
C----------
C  SHAGBARK HICKORY
C----------
          CASE (39)
            V2T(I)     =  39.9
            TFALLCLS(I) = 1
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  6
C----------
C  BIGTOOTH ASPEN
C----------
          CASE (40)
            V2T(I)     =  22.5
            TFALLCLS(I) = 4
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  QUAKING ASPEN
C----------
          CASE (41)
             V2T(I)     =  21.8
            TFALLCLS(I) = 4
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  BALSAM POPLAR
C----------
          CASE (42)
            V2T(I)     =  19.3
            TFALLCLS(I) = 4
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  PAPER BIRCH
C----------
          CASE (43)
            V2T(I)     =  29.9
            TFALLCLS(I) = 4
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  1
C----------
C  COMMERICAL HARDWOOD
C----------
          CASE (44)
            V2T(I)     =  31.8
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  BUTTERNUT
C----------
          CASE (45)
            V2T(I)     =  22.5
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  BLACK WALNUT
C----------
          CASE (46)
            V2T(I)     =  31.8
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  2
            SNAGCLS(I) =  5
C----------
C  EASTERN HOPHORNBEAM
C----------
          CASE (47)
            V2T(I)     =  31.8
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  BLACK LOCUST
C----------
          CASE (48)
            V2T(I)     =  41.2
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  1
            SNAGCLS(I) =  5
C----------
C  NONCOMMERCIAL HARDWOOD
C----------
          CASE (49)
            V2T(I)     =  28.7
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  BOXELDER
C----------
          CASE (50)
            V2T(I)     =  30.6
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  STRIPED MAPLE
C----------
          CASE (51)
            V2T(I)     =  30.6
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  MOUNTAIN MAPLE
C----------
          CASE (52)
            V2T(I)     =  30.6
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  AMERICAN HORNBEAM / MUSCLEWOOD
C----------
          CASE (53)
            V2T(I)     =  28.7
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  AMERICAN CHESTNUT
C----------
          CASE (54)
            V2T(I)     =  28.7
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  2
            SNAGCLS(I) =  5
C----------
C  HACKBERRY
C----------
          CASE (55)
             V2T(I)     =  30.6
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  FLOWERING DOGWOOD
C----------
          CASE (56)
            V2T(I)     =  28.7
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  HAWTHORN
C----------
          CASE (57)
            V2T(I)     =  28.7
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  APPLE
C----------
          CASE (58)
            V2T(I)     =  29.3
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  2 ! used black cherry
            SNAGCLS(I) =  5
C----------
C  BLACKGUM
C----------
          CASE (59)
            V2T(I)     =  28.7
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  AMERICAN SYCAMORE
C----------
          CASE (60)
            V2T(I)     =  28.7
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  PIN CHERRY
C----------
          CASE (61)
            V2T(I)     =  29.3
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  2 ! used black cherry
            SNAGCLS(I) =  5
C----------
C  COMMON CHOKECHERRY
C----------
          CASE (62)
            V2T(I)     =  29.3
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  2 ! used black cherry
            SNAGCLS(I) =  5
C----------
C  CHERRY AND PLUM SPECIES
C----------
          CASE (63)
            V2T(I)     =  29.3
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  2 ! used black cherry
            SNAGCLS(I) =  5
C----------
C  WILLOW
C----------
          CASE (64)
            V2T(I)     =  22.5
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  BLACK WILLOW
C----------
          CASE (65)
            V2T(I)     =  22.5
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  DIAMOND WILLOW
C----------
          CASE (66)
            V2T(I)     =  22.5
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C----------
C  SASSAFRAS
C----------
          CASE (67)
            V2T(I)     =  26.2
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  2
            SNAGCLS(I) =  5
C----------
C  AMERICAN MOUNTAIN ASH
C----------
          CASE (68)
            V2T(I)     =  28.7
            TFALLCLS(I) = 3
            LEAFLF(I)  =  1.0
            DKRCLS(I)  =  4
            SNAGCLS(I) =  5
C
        END SELECT
C
        SELECT CASE (I)
          CASE (1:14)
            LSW(I) = .TRUE.
          CASE DEFAULT
            LSW(I) = .FALSE.
        END SELECT
C
        SELECT CASE (TFALLCLS(I))
          CASE (1)  ! hickory/white oak/hemlock/cedar/tamarack group
            TFALL(I,1) = 2.0
            TFALL(I,3) = 5.0
            TFALL(I,4) = 10.0
C
          CASE (2)  ! red oak group
            TFALL(I,1) = 1.0
            TFALL(I,3) = 4.0
            TFALL(I,4) = 8.0
C
          CASE (3)  ! ash/elm/maple/beech/other group
            TFALL(I,1) = 1.0
            TFALL(I,3) = 3.0
            TFALL(I,4) = 6.0
C
          CASE (4)  ! pine/spruce/fir/aspen/poplar/birch/basswood
            TFALL(I,1) = 1.0
            TFALL(I,3) = 2.0
            TFALL(I,4) = 4.0
C
        END SELECT
C
        TFALL(I,0) = 1.0
        TFALL(I,2) = TFALL(I,1)
        TFALL(I,5) = TFALL(I,4)
C
        SELECT CASE (SNAGCLS(I))
C
          CASE (1)  ! aspen, birch, spruce, fir, poplar, basswood
                    ! this group decays and falls really fast
            DECAYX(I)  = 0.4
            FALLX(I)   = 1.66
            ALLDWN(I)  = 10
            DO J= 1,4
              HTX(I,J) =   3.0
            ENDDO
C
          CASE (2)  ! jack pine
            DECAYX(I)  = 0.8
            FALLX(I)   = 1.33
            ALLDWN(I)  = 30
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
C
          CASE (3)  ! white pine
            DECAYX(I)  = 1.0
            FALLX(I)   = 1.16
            ALLDWN(I)  = 50
            DO J= 1,4
              HTX(I,J) =   0.0 ! no height loss
            ENDDO
C
          CASE (4)  ! red pine
            DECAYX(I)  = 1.2
            FALLX(I)   = 1.0
            ALLDWN(I)  = 50
            DO J= 1,4
              HTX(I,J) =   0.0 ! no height loss
            ENDDO
C
          CASE (5)  ! ash, maple, beech, elm
            DECAYX(I)  = 1.5
            FALLX(I)   = .83
            ALLDWN(I)  = 50
            DO J= 1,4
              HTX(I,J) =   0.65
            ENDDO
C
          CASE (6)  ! cedar, tamarack, oak, hickory, hemlock
                    ! this group decays and falls very slowly
            DECAYX(I)  = 2.3
            FALLX(I)   = .53
            ALLDWN(I)  = 50
            DO J= 1,4
              HTX(I,J) =   0.45
              IF (I .EQ. 12) HTX(I,J) = 0.0 ! hemlock gets no ht loss
            ENDDO
        END SELECT
C
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
      OLDICT = 3
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
