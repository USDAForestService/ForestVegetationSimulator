      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C FIRE-NE $Id: fmvinit.f 0000 2018-02-14 00:00:00Z gedixon $
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
C  DECAY RATES - FROM FOSTER AND LANG, CAN J FOR RES, 1982
C                     ARTHUR, TRITTON, AND FAHEY, CAN J FOR RES, 1993
C                     FAHEY, HUGHES, PU, AND ARTHUR, FOREST SCIENCE, 1988
C  THIS IS BASED ON DISCUSSIONS WITH COELI HOOVER.
C----------
      DKR(1,1)  = 0.19 ! Fahey et. al.
      DKR(2,1)  = 0.19 ! Fahey et. al.
      DKR(3,1)  = 0.11 ! Arthur et. al. and Fahey et. al.
      DKR(4,1)  = 0.07 ! gradient
      DKR(5,1)  = 0.03 ! Foster and Lang
      DKR(6,1)  = 0.03 ! Foster and Lang
      DKR(7,1)  = 0.03 ! Foster and Lang      
      DKR(8,1)  = 0.03 ! Foster and Lang      
      DKR(9,1)  = 0.03 ! Foster and Lang      
C
      DO I = 1,9
        DO J = 2,4
          DKR(I,J) = DKR(I,1)
        ENDDO
      ENDDO
C----------
C  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
C  FOR LITTER DECAY - USED INFO FROM MELILLO, ECOLOGY, 1982.  THIS IS
C  BASED ON DISCUSSIONS WITH COELI HOOVER.
C  *** MAY 2006 - I BUMPED UP THE LITTER DECAY RATE SINCE TOO MUCH LITTER WAS
C  ACCUMULATING OVER TIME - SAR
C----------
      DO J = 1,4
        DKR(10,J) = 0.40
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
      HTR1   =  0.015  ! ht loss info from Coeli Hoover & Linda Heath
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
C  TFALL IS ASSIGNED FOR 6 DIFFERENT TFALLCLS GROUPS.  THIS INFO IS BASED
C  ON THE SN AND LS VARIANTS, SINCE I FORGOT TO DISCUSS IT AT THE MEETING
C  WITH COELI HOOVER AND LINDA HEATH.
C
C  THE SNAG DECAY RATE IS SET BY FIRST DEFINING A
C  SNAGCLS (1, 2, OR 3) FOR EACH SPECIES, AND THEN SETTING THE DECAY RATE
C  VARIABLE FOR THE 3 SNAG CLASSES.  SNAGCLS IS DEFINED AS FOLLOWS:
C  1 - SPECIES DECAYS FASTER THAN AVERAGE
C  2 - SPECIES DECAYS AT AN AVERAGE RATE
C  3 - SPECIES DECAYS SLOWER THAN AVERAGE
C  SNAGCLS IS USED TO SET THE FOLLOWING VARIABLE:
C  DECAYX() - DECAY RATE MULTIPLIER
C  THESE CLASSES AND THE DECAY RATES WERE TAKEN FROM THE SN-FFE (WITH SOME
C  MODIFICATION) BECAUSE LINDA THOUGHT THEY SEEMED REASONABLE.
C
C  THE FOLLOWING VARIABLES ARE ALSO SET, BUT ARE NOT SPECIES-SPECIFIC
C  ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
C  FALLX()  - FALL RATE MULTIPLIER
C  HTX()    - HEIGHT-LOSS RATE MULTIPLIER
C  THESE WERE BASED ON DISCUSSIONS WITH COELI HOOVER AND LINDA
C  HEATH AND A PAPER BY YAMASAKI AND LEAK (IN PRESS, NJAF)

C  DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
C  CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
C  CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS
C
C  DKRCLS WAS ASSIGNED FROM INFO IN THE WOOD HANDBOOK:
C  HTTP://WWW.FPL.FS.FED.US/DOCUMNTS/FPLGTR/FPLGTR113/FPLGTR113.HTM
C  WHEN SPECIES WERE CLASSIFIED DIFFERENTLY FOR YOUNG OR OLD GROWTH,
C  YOUNG GROWTH WAS ASSUMED.
C  SPECIES NOT LISTED WERE CLASSED AS 4 IF NOT IN THE WOOD HANDBOOK
C----------
      DO I= 1,MAXSP
C
        SELECT CASE (I)
C----------
C  BALSAM FIR
C----------
       CASE (1)
         V2T(I)     = 20.6
         TFALLCLS(I) = 6
         LEAFLF(I)  = 8
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  TAMARACK
C----------
       CASE (2)
         V2T(I)     = 30.6
         TFALLCLS(I) = 1
         LEAFLF(I)  = 1
         DKRCLS(I)  = 3
         SNAGCLS(I) = 3
C----------
C  WHITE SPRUCE
C----------
       CASE (3)
         V2T(I)     =  23.1
         TFALLCLS(I) = 6
         LEAFLF(I)  = 8
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  RED SPRUCE
C----------
       CASE (4)
         V2T(I)     = 23.1
         TFALLCLS(I) = 6
         LEAFLF(I)  = 8
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  NORWAY SPRUCE
C----------
       CASE (5)
         V2T(I)     = 23.1
         TFALLCLS(I) = 6
         LEAFLF(I)  = 8
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  BLACK SPRUCE
C----------
       CASE (6)
         V2T(I)     = 23.7
         TFALLCLS(I) = 6
         LEAFLF(I)  = 8
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  OTHER SPRUCE
C----------
       CASE (7)
         V2T(I)     = 23.1
         TFALLCLS(I) = 6
         LEAFLF(I)  = 8
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  RED PINE
C----------
       CASE (8)
         V2T(I)     =  25.6
         TFALLCLS(I) = 6
         LEAFLF(I)  = 4
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  EASTERN WHITE PINE
C----------
       CASE (9)
         V2T(I)     = 21.2
         TFALLCLS(I) = 6
         LEAFLF(I)  = 2
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  LOBLOLLY PINE
C----------
       CASE (10)
         V2T(I)     = 29.3
         TFALLCLS(I) = 6
         LEAFLF(I)  = 3
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  VIRGINIA PINE
C----------
       CASE (11)
         V2T(I)     = 28.1
         TFALLCLS(I) = 6
         LEAFLF(I)  = 3
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  NORTHERN WHITE CEDAR
C----------
       CASE (12)
         V2T(I)     = 18.1
         TFALLCLS(I) = 1
         LEAFLF(I)  = 2
         DKRCLS(I)  = 2
         SNAGCLS(I) = 3
C----------
C  ATLANTIC WHITE CEDAR
C----------
       CASE (13)
         V2T(I)     = 19.3
         TFALLCLS(I) = 1
         LEAFLF(I)  = 3
         DKRCLS(I)  = 2
         SNAGCLS(I) = 3
C----------
C  EASTERN REDCEDAR
C----------
       CASE (14)
         V2T(I)     = 27.4
         TFALLCLS(I) = 1
         LEAFLF(I)  = 5
         DKRCLS(I)  = 2
         SNAGCLS(I) = 3
C----------
C  JUNIPER SPECIES
C----------
       CASE (15)
         V2T(I)     = 27.4
         TFALLCLS(I) = 1
         LEAFLF(I)  = 5
         DKRCLS(I)  = 2
         SNAGCLS(I) = 3
C----------
C  EASTERN HEMLOCK
C----------
       CASE (16)
         V2T(I)     = 23.7
         TFALLCLS(I) = 3
         LEAFLF(I)  = 3
         DKRCLS(I)  = 4
         SNAGCLS(I) = 3
C----------
C  HEMLOCK SPECIES
C----------
       CASE (17)
         V2T(I)     = 26.2
         TFALLCLS(I) = 3
         LEAFLF(I)  = 3
         DKRCLS(I)  = 4
         SNAGCLS(I) = 3
C----------
C  OTHER PINE
C----------
       CASE (18)
         V2T(I)     = 25.6
         TFALLCLS(I) = 6
         LEAFLF(I)  = 2
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  JACK PINE
C----------
       CASE (19)
         V2T(I)     = 24.9
         TFALLCLS(I) = 6
         LEAFLF(I)  = 2
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  SHORTLEAF PINE
C----------
       CASE (20)
         V2T(I)     = 29.3
         TFALLCLS(I) = 6
         LEAFLF(I)  = 4
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C TABLE MOUNTAIN PINE
C----------
       CASE (21)
         V2T(I)     = 28.1
         TFALLCLS(I) = 6
         LEAFLF(I)  = 3
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  PITCH PINE
C----------
       CASE (22)
         V2T(I)     = 29.3
         TFALLCLS(I) = 6
         LEAFLF(I)  = 2
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  POND PINE
C----------
       CASE (23)
         V2T(I)     = 31.8
         TFALLCLS(I) = 6
         LEAFLF(I)  = 2
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  SCOTCH PINE
C----------
       CASE (24)
         V2T(I)     =  25.6
         TFALLCLS(I) = 6
         LEAFLF(I)  = 3
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  OTHER SOFTWOOD SPECIES
C----------
       CASE (25)
         V2T(I)     = 25.6
         TFALLCLS(I) = 6
         LEAFLF(I)  = 2
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  RED MAPLE
C----------
       CASE (26)
         V2T(I)     = 30.6
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  SUGAR MAPLE
C----------
       CASE (27)
         V2T(I)     = 34.9
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  BLACK MAPLE
C----------
       CASE (28)
         V2T(I)     = 32.4
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  SILVER MAPLE
C----------
       CASE (29)
         V2T(I)     = 27.4
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  YELLOW BIRCH
C----------
       CASE (30)
         V2T(I)     = 34.3
         TFALLCLS(I) = 6
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  SWEET BIRCH
C----------
       CASE (31)
         V2T(I)     = 37.4
         TFALLCLS(I) = 6
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  RIVER BIRCH
C----------
       CASE (32)
         V2T(I)     = 29.9
         TFALLCLS(I) = 6
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  PAPER BIRCH
C----------
       CASE (33)
         V2T(I)     = 29.9
         TFALLCLS(I) = 6
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  GRAY BIRCH
C----------
       CASE (34)
         V2T(I)     = 29.9
         TFALLCLS(I) = 6
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  HICKORY SPECIES
C----------
       CASE (35)
         V2T(I)     = 39.9
         TFALLCLS(I) = 2
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 3
C----------
C  PIGNUT HICKORY
C----------
       CASE (36)
         V2T(I)     = 41.2
         TFALLCLS(I) = 2
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 3
C----------
C  SHELLBARK HICKORY
C----------
       CASE (37)
         V2T(I)     = 38.7
         TFALLCLS(I) = 2
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 3
C----------
C  SHAGBARK HICKORY
C----------
       CASE (38)
         V2T(I)     = 39.9
         TFALLCLS(I) = 2
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 3
C----------
C  MOCKERNUT HICKORY
C----------
       CASE (39)
         V2T(I)     = 39.9
         TFALLCLS(I) = 2
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 3
C----------
C  AMERICAN BEECH
C----------
       CASE (40)
         V2T(I)     = 34.9
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  ASH SPECIES
C----------
       CASE (41)
         V2T(I)     = 33.1
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  WHITE ASH
C----------
       CASE (42)
         V2T(I)     = 34.3
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  BLACK ASH
C----------
       CASE (43)
         V2T(I)     = 28.1
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  GREEN ASH
C----------
       CASE (44)
         V2T(I)     = 33.1
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  PUMPKIN ASH
C----------
       CASE (45)
         V2T(I)     = 33.1
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  YELLOW-POPLAR
C----------
       CASE (46)
         V2T(I)     = 24.9
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  SWEETGUM
C----------
       CASE (47)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  CUCUMBERTREE
C----------
       CASE (48)
         V2T(I)     = 27.4
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  QUAKING ASPEN
C----------
       CASE (49)
         V2T(I)     = 21.8
         TFALLCLS(I) = 6
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  BALSAM POPLAR
C----------
       CASE (50)
         V2T(I)     = 19.3
         TFALLCLS(I) = 6
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  EASTERN COTTONWOOD
C----------
       CASE (51)
         V2T(I)     = 23.1
         TFALLCLS(I) = 6
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  BIGTOOTH ASPEN
C----------
       CASE (52)
         V2T(I)     = 22.5
         TFALLCLS(I) = 6
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  SWAMP COTTONWOOD
C----------
       CASE (53)
         V2T(I)     = 23.1
         TFALLCLS(I) = 6
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  BLACK CHERRY
C----------
       CASE (54)
         V2T(I)     = 29.3
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C----------
C  WHITE OAK
C----------
       CASE (55)
         V2T(I)     = 37.4
         TFALLCLS(I) = 3
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 3
C----------
C  BUR OAK
C----------
       CASE (56)
         V2T(I)     = 36.2
         TFALLCLS(I) = 3
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 3
C----------
C  CHINKAPIN OAK
C----------
       CASE (57)
         V2T(I)     = 37.4
         TFALLCLS(I) = 3
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 3
C----------
C  POST OAK
C----------
       CASE (58)
         V2T(I)     = 37.4
         TFALLCLS(I) = 3
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 3
C----------
C  OAK SPECIES
C----------
       CASE (59)
         V2T(I)     = 37.4
         TFALLCLS(I) = 3
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 3
C----------
C  SCARLET OAK
C----------
       CASE (60)
         V2T(I)     = 37.4
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C----------
C  SHINGLE OAK
C----------
       CASE (61)
         V2T(I)     = 34.9
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C----------
C  WATER OAK
C----------
       CASE (62)
         V2T(I)     = 34.9
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C----------
C  NORTHERN PIN OAK
C----------
       CASE (63)
         V2T(I)     = 36.2
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C----------
C  CHESTNUT OAK
C----------
       CASE (64)
         V2T(I)     = 35.6
         TFALLCLS(I) = 3
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 3
C----------
C  SWAMP WHITE OAK
C----------
       CASE (65)
         V2T(I)     = 39.9
         TFALLCLS(I) = 3
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 3
C----------
C  SWAMP CHESTNUT OAK
C----------
       CASE (66)
         V2T(I)     = 37.4
         TFALLCLS(I) = 3
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 3
C----------
C  NORTHERN RED OAK
C----------
       CASE (67)
         V2T(I)     = 34.9
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C----------
C  SOUTHERN RED OAK
C----------
       CASE (68)
         V2T(I)     = 32.4
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C----------
C  BLACK OAK
C----------
       CASE (69)
         V2T(I)     = 34.9
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C----------
C  CHERRYBARK OAK
C----------
       CASE (70)
         V2T(I)     = 38.0
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C----------
C  OTHER HARDWOODS
C----------
       CASE (71)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  BUCKEYE SPECIES
C----------
       CASE (72)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  YELLOW BUCKEYE
C----------
       CASE (73)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  WATER BIRCH
C----------
       CASE (74)
         V2T(I)     = 29.9
         TFALLCLS(I) = 6
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  COMMON HACKBERRY
C----------
       CASE (75)
         V2T(I)     = 30.6
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  COMMON PERSIMMON
C----------
       CASE (76)
         V2T(I)     = 28.7
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 3
C----------
C  AMERICAN HOLLY
C----------
       CASE (77)
         V2T(I)     = 28.7
         TFALLCLS(I) = 4
         LEAFLF(I)  = 3
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  BUTTERNUT
C----------
       CASE (78)
         V2T(I)     = 22.5
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  BLACK WALNUT
C----------
       CASE (79)
         V2T(I)     = 31.8
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C----------
C  OSAGE-ORANGE
C----------
       CASE (80)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 1
         SNAGCLS(I) = 2
C----------
C  MAGNOLIA
C----------
       CASE (81)
         V2T(I)     = 28.7
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  SWEETBAY
C----------
       CASE (82)
         V2T(I)     = 28.7
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  APPLE SPECIES
C----------
       CASE (83)
         V2T(I)     = 29.3
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C----------
C  WATER TUPELO
C----------
       CASE (84)
         V2T(I)     = 28.7
         TFALLCLS(I) = 3
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 3
C----------
C  BLACK GUM
C----------
       CASE (85)
         V2T(I)     = 28.7
         TFALLCLS(I) = 3
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 3
C----------
C  SOURWOOD
C----------
       CASE (86)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  PAULOWNIA
C----------
       CASE (87)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  =  1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  SYCAMORE
C----------
       CASE (88)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  WILLOW OAK
C----------
       CASE (89)
         V2T(I)     = 34.9
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C----------
C  BLACK LOCUST
C----------
       CASE (90)
         V2T(I)     = 41.2
         TFALLCLS(I) = 2
         LEAFLF(I)  = 1
         DKRCLS(I)  = 1
         SNAGCLS(I) = 3
C----------
C  BLACK WILLOW
C----------
       CASE (91)
         V2T(I)     = 22.5
         TFALLCLS(I) = 6
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  SASSAFRAS
C----------
       CASE (92)
         V2T(I)     = 26.2
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C----------
C  AMERICAN BASSWOOD
C----------
       CASE (93)
         V2T(I)     = 20.0
         TFALLCLS(I) = 6
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  WHITE BASSWOOD
C----------
       CASE (94)
         V2T(I)     = 20.0
         TFALLCLS(I) = 6
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  ELM SPECIES
C----------
       CASE (95)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  AMERICAN ELM
C----------
       CASE (96)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  SLIPPERY ELM
C----------
       CASE (97)
         V2T(I)     = 29.9
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 1
C----------
C  NON-COMMERCIAL HARDWOODS
C----------
       CASE (98)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  BOX ELDER
C----------
       CASE (99)
         V2T(I)     = 30.6
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  STRIPED MAPLE
C----------
       CASE (100)
         V2T(I)     = 30.6
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  TREE OF HEAVEN - AILANTHUS
C----------
       CASE (101)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  SERVICEBERRY
C----------
       CASE (102)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  AMERICAN HORNBEAM
C----------
       CASE (103)
         V2T(I)     = 28.7
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  FLOWERING DOGWOOD
C----------
       CASE (104)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  HAWTHORN SPECIES
C----------
       CASE (105)
         V2T(I)     = 28.7
         TFALLCLS(I) = 5
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  HOPHORNBEAM
C----------
       CASE (106)
         V2T(I)     = 28.7
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 4
         SNAGCLS(I) = 2
C----------
C  PLUM SPECIES
C----------
       CASE (107)
         V2T(I)     = 29.3
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C----------
C  PIN CHERRY
C----------
       CASE (108)
         V2T(I)     = 29.3
         TFALLCLS(I) = 4
         LEAFLF(I)  = 1
         DKRCLS(I)  = 2
         SNAGCLS(I) = 2
C
        END SELECT
C
        SELECT CASE (I)
          CASE (1:25)
            LSW(I) = .TRUE.
          CASE DEFAULT
            LSW(I) = .FALSE.
        END SELECT
C
        IF ((I .GE. 12) .AND. (I .LE. 15)) THEN !cedars
          TFALL(I,0) = 3.0  ! foliage
        ELSE
          TFALL(I,0) = 1.0
        ENDIF
C
        SELECT CASE (TFALLCLS(I))
C
          CASE (1)  ! cedar / tamarack group
            TFALL(I,1) = 5.0
            TFALL(I,3) = 10.0
            TFALL(I,4) = 25.0
C
          CASE (2)  ! hickory / black locust group
            TFALL(I,1) = 3.0
            TFALL(I,3) = 6.0
            TFALL(I,4) = 12.0
C
          CASE (3)  ! white oak group / hemlock / other
            TFALL(I,1) = 2.0
            TFALL(I,3) = 5.0
            TFALL(I,4) = 10.0
C
          CASE (4)  ! red oak group / other
            TFALL(I,1) = 1.0
            TFALL(I,3) = 4.0
            TFALL(I,4) = 8.0
C
          CASE (5)  ! ash/elm/maple/other group
            TFALL(I,1) = 1.0
            TFALL(I,3) = 3.0
            TFALL(I,4) = 6.0
C
          CASE (6)  ! pine/spruce/fir/aspen/poplar/birch/basswood
            TFALL(I,1) = 1.0
            TFALL(I,3) = 2.0
            TFALL(I,4) = 4.0
C
        END SELECT
C
        TFALL(I,2) = TFALL(I,1)
        TFALL(I,5) = TFALL(I,4)
C
        FALLX(I)   = 1.0
        ALLDWN(I)  = 50.0

        SELECT CASE (SNAGCLS(I))
C
          CASE (1)  ! pines (except white), fir, spruce and others
                    ! this group decays faster than average
            DECAYX(I)  = 0.07 ! 12 inch tree is soft in 2 years
C
          CASE (2)  ! black oak and others
                    ! this group decays at an average rate
            DECAYX(I)  = 0.21  ! 12 inch tree is soft in 6 years

C
          CASE (3)  ! white oak, redcedar, and others
                    ! this group decays slower than average
            DECAYX(I)  = 0.35  ! 12 inch tree is soft in 10 years
        END SELECT
C
        DO J= 1,4
          HTX(I,J) =   1.0 ! all species get base rate of
                           ! 1.5% ht. loss/year
        ENDDO
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
C  COVER TYPE  - CURRENTLY NOT USED IN NE-FFE
C----------
      OLDICT = 0
C----------
C  DROUGHT START AND END YEARS - CURRENTLY NOT USED IN NE-FFE
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
