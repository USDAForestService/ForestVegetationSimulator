      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C  **FMVINIT  FIRE-EC-DATE OF LAST REVISION: 02/24/12
C----------
*  Purpose:
*      Initialize variant-specific variables for the Fire Model
*----------------------------------------------------------------------
*
*  Called from: INITRE
*
*  Call list definitions:
*
*  Local variable definitions:
*
***********************************************************************
C----------
COMMONS
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
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'FMFCOM.F77'
C
COMMONS
C----------
      INTEGER I,J
C----------
      LVWEST    = .TRUE.  ! WESTERN VARIANT
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
      PREWND(2)=6.
      POTEMP(1)=70.
      POTEMP(2)=70.
C----------
C  EC DECAY RATES BASED ON NI
C----------
      DKR(1,1)  =  0.12
      DKR(2,1)  =  0.12
      DKR(3,1)  =  0.09
      DKR(4,1)  =  0.015
      DKR(5,1)  =  0.015
      DKR(6,1)  =  0.015
      DKR(7,1)  =  0.015
      DKR(8,1)  =  0.015
      DKR(9,1)  =  0.015
C
      DO I = 1,9
        DO J = 2,4
          DKR(I,J) = DKR(I,1)
        ENDDO
      ENDDO
C----------
C  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
C----------
      DO J = 1,4
        DKR(10,J) = 0.50
        DKR(11,J) = 0.002
      ENDDO
C----------
C  DUFF PRODUCTION RATES 'PRDUFF' ARE A PROPORTION OF THE OVERALL
C  DECAY RATE: 'DKR'.
C----------
      DO I = 1,10
        PRDUFF(I) = 0.02
        DO J = 1,4
          TODUFF(I,J) = DKR(I,J) * PRDUFF(I)
        ENDDO
      ENDDO
C----------
C  SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
C  ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.
C
C  HTR1 and HTR2 are set below, but are not currently used in this variant,
C  unless a user modifies the snag height loss rates thru the SNAGBRK keyword.
C  See fmsnag for details.
C----------
      NZERO  =  0.01
      LIMBRK =  0.01
      HTXSFT =  1.0
      HTR1   =  0.0228
      HTR2   =  0.01
      DO I= 1,MAXSP
        PSOFT(I)  =  0.0
      ENDDO
C----------
C  ** SPECIES-LEVEL VARIABLE ASSIGNMENT **
C
C  V2T() - UNITS ARE LB/CUFT BY SPECIES FROM NI/SO VARIANTS,
C  AND 'WOOD HANDBOOK' USDA FOREST PRODUCTS AG. HANDBOOK 72.
C  1974. DENSITY OF PINYON, JUNIPER, GAMBEL FROM CHOJNACKY 1992.
C
C  [TO CONVERT G/CM**3 TO LB/FT**3, MULTIPLY 'X' G/CM**3 * 62.372]
C
C  LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES
C
C  TFALL() - TIME TO FALL FOR DEAD CROWN COMPONENTS. THIS VARIABLE
C  IS NOT UNDER USER-CONTROL BUT MIGHT BE SOMEDAY.
C  [LITTERFALL AND SMALL TWIG FALL VALUES CHANGED 2/97. SB&ER]
C  TFALL INDEXING USES CROWNW() DIMENSIONS, I.E.
C
C     0 :  FOLIAGE
C     1 : <0.25"
C     2 :  0.25" -   1"
C     3 :  1"    -   3"
C     4 :  3"    -   6"
C     5 :  6"    -  12"
C
C  IF THE VALUE OF TFALL(I,-) IS LARGER THAN 20, PARAMETER TFMAX IN
C  **FMPARM.F77** MUST BE ADJUSTED TO EQUAL THE NEW VALUE, AND LOOPS
C  INVOLVING THE VALUE (SEE FMSCRO) MUST BE RE-EXAMINED TO INSURE
C  THAT THEY BEHAVE PROPERLY.
C
C  ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
C  DECAYX() - DECAY RATE MULTIPLIER       (PP = 1.0 in NI var)
C  FALLX()  - FALL RATE MULTIPLIER        (PP = 1.0 in NI var)
C  HTX()    - HEIGHT-LOSS RATE MULTIPLIER (PP = 1.0 in NI var)
C
C  ** NOTE THAT SNAG RATE PARAMETERS ARE ALL RELATIVE TO PP **
C
C  DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
C  CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
C  CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS
C----------
      DO I= 1,MAXSP

         SELECT CASE (I)
C----------
C  white pine (NI)
C----------
          CASE (1)
            V2T(I)     =  22.5
            LEAFLF(I)  =   4.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 110.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  western larch (NI)
C----------
          CASE (2)
            V2T(I)     =  29.9
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 150.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   3
            LSW(I)     = .TRUE.
C----------
C  Douglas-fir (NI)
C----------
          CASE (3)
            V2T(I)     =  28.7  ! west DF in Wood Handbook
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  75.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   3
            LSW(I)     = .TRUE.
C----------
C  Pacific silver fir (based in part on GF-NI)
C----------
          CASE (4)
            V2T(I)     =  24.9
            LEAFLF(I)  =   7.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  30.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  western redcedar (NI)
C----------
          CASE (5)
            V2T(I)     =  19.3
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 300.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.
C----------
C  grand fir (NI)
C----------
          CASE (6)
            V2T(I)     =  21.8
            LEAFLF(I)  =   7.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  90.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  lodgepole pine (NI)
C----------
          CASE (7)
            V2T(I)     =  23.7
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  35.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  Engelmann spruce (NI)
C----------
          CASE (8)
            V2T(I)     =  20.6
            LEAFLF(I)  =   6.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  subalpine fir (NI)
C----------
          CASE (9)
            V2T(I)     =  19.3
            LEAFLF(I)  =   7.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  40.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  ponderosa pine (NI)
C----------
          CASE (10)
            V2T(I)     =  23.7
            LEAFLF(I)  =   4.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  western hemlock; from WC
C----------
          CASE (11)
            V2T(I)     =  26.2
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   3.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  mountain hemlock; from SO via WC
C----------
          CASE (12)
            V2T(I)     =  26.2
            LEAFLF(I)  =   4.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  =  30.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  Pacific yew; from CA via WC
C----------
          CASE (13)
            V2T(I)     =  26.2 ! baldcypress
            LEAFLF(I)  =   7.0
            TFALL(I,0) =   3.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   1
            LSW(I)     = .TRUE.
C----------
C  whitebark pine; from CA via WC
C----------
          CASE (14)
            V2T(I)     =  22.5 ! white pine
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   3.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  noble fir; DF from SO, via WC
C----------
          CASE (15)
            V2T(I)     =  23.1
            LEAFLF(I)  =   7.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  90.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  white fir; use WC grand fir
C----------
          CASE (16)
            V2T(I)     =  21.8
            LEAFLF(I)  =   7.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  90.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  subalpine larch; WL from EC, via WC
C----------
          CASE (17)
            V2T(I)     =  29.9 ! western larch
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 150.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   3
            LSW(I)     = .TRUE.
C----------
C  Alaska cedar; RC from EC, via WC
C----------
          CASE (18)
            V2T(I)     =  26.2
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 300.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.
C----------
C  western juniper; from SO via WC
C----------
          CASE (19)
            V2T(I)     =  34.9
            LEAFLF(I)  =   4.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.
C----------
C  bigleaf maple; from CA via WC
C----------
          CASE (20)
            V2T(I)     =  27.4
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  vine maple; use bigleaf maple
C----------
          CASE (21)
            V2T(I)     =  27.4
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  red alder; from CA via WC
C----------
          CASE (22)
            V2T(I)     =  23.1
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  paper birch; from WC
C----------
          CASE (23)
            V2T(I)     =  29.9
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  golden chinkapin; from CA via WC
C----------
          CASE (24)
            V2T(I)     =  36.2
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  Pacific dogwood; from CA via WC
C----------
          CASE (25)
            V2T(I)     =  27.4 ! bigleaf maple
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  quaking aspen; from CA via WC
C----------
          CASE (26)
            V2T(I)     =  21.8
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  black cottonwood; from CA via WC
C----------
          CASE (27)
            V2T(I)     =  19.3
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  Oregon white oak; from CA via WC
C----------
          CASE (28)
            V2T(I)     =  37.4  ! Or white oak
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   2
            LSW(I)     = .FALSE.
C----------
C  cherry and plum species; BC from WC
C----------
          CASE (29)
            V2T(I)     =   29.3 ! black cherry
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   2
            LSW(I)     = .FALSE.
C----------
C  willow species; from CA via WC
C----------
          CASE (30)
            V2T(I)     =  22.5
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  other softwoods (HM - based in part on PP-NI)
C----------
          CASE (31)
            V2T(I)     =  26.2
            LEAFLF(I)  =   4.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  =  30.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  other hardwoods; use aspen
C----------
          CASE (32)
            V2T(I)     =  21.8
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C
        END SELECT
C----------
C  TIME-TO-FALL FOR OTHER CROWN CATEGORIES (NI)
C----------
        TFALL(I,4) = TFALL(I,3)
        TFALL(I,5) = TFALL(I,3)
C----------
C  DEAD LEAF FALL CANNOT BE > LIVE
C----------
        TFALL(I,0) = MIN(TFALL(I,0), LEAFLF(I))
C----------
C  TFALL(I,3) CANNOT BE < TFALL(I,2)
C----------
        IF (TFALL(I,2) .GT. TFALL(I,3)) THEN
          TFALL(I,2) = TFALL(I,3)
        ENDIF
C----------
C  CONVERT LB/FT**3 TO TONS/FT**3
C----------
        V2T(I) = V2T(I) / 2000.0
C
      ENDDO
C----------
C  parameters for post-burn snag fall rates:
C----------
      PBSCOR =  0.0
      PBSOFT =  0.0
      PBSMAL =  0.0
      PBSIZE = 12.0
      PBTIME =  7.0
C----------
C  PARAMETERS FOR FUEL MODEL SELECTION
C
C  THE DOUGLAS-FIR COVER TYPE METAGROUP **FMCFMD**
C----------
      OLDICT = 1
C----------
C  DROUGHT START AND END YEARS
C----------
      IDRYB  = 0
      IDRYE  = 0
C----------
C  CRITICAL % CHANGE REQUIRED TO TRIGGER ACTIVITY FUELS
C----------
      SLCRIT = 10.0
C----------
      RETURN
      END
