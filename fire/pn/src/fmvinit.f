      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C  **FMVINIT  FIRE-PN-DATE OF LAST REVISION: 09/21/09
C----------
*  Purpose:
*      Initialize variant-specific variables for the Fire Model
*
*
*  Called from: INITRE
*
*  Call list definitions:
*
*  Local variable definitions:
*
***********************************************************************
COMMONS

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'

COMMONS

      INTEGER I,J

      LVWEST    = .TRUE.  ! WESTERN VARIANT

      CANCLS(1) =  5.0
      CANCLS(2) = 17.5
      CANCLS(3) = 37.5
      CANCLS(4) = 75.0

      CORFAC(1) =  0.5
      CORFAC(2) =  0.3
      CORFAC(3) =  0.2
      CORFAC(4) =  0.1

      SNPRCL(1) =  0
      SNPRCL(2) = 12
      SNPRCL(3) = 18
      SNPRCL(4) = 24
      SNPRCL(5) = 30
      SNPRCL(6) = 36

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
      PREWND(1) = 20.0
      PREWND(2) =  6.0
      POTEMP(1) = 70.0
      POTEMP(2) = 70.0

C     DECAY RATES BASED ON SO-FFE (closest neighbour)

      DKR(1,1) = 0.12  ! < 0.25"
      DKR(2,1) = 0.12  ! 0.25 - 1"
      DKR(3,1) = 0.09  ! 1 - 3"
      DKR(4,1) = 0.015 ! 3 - 6"
      DKR(5,1) = 0.015 ! 6 - 12"
      DKR(6,1) = 0.015 ! 12 - 20"
      DKR(7,1) = 0.015 ! 20 - 35"
      DKR(8,1) = 0.015 ! 35 - 50"
      DKR(9,1) = 0.015 !  > 50"

      DO I = 1,9
        DO J = 2,4
          DKR(I,J) = DKR(I,1)
        ENDDO
      ENDDO

C  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)

      DO J = 1,4
        DKR(10,J) = 0.5
        DKR(11,J) = 0.002
      ENDDO

C     Duff production rates 'PRDUFF' are a proportion of the overall
C     decay rate: 'DKR'.

      DO I = 1,10
        PRDUFF(I) = 0.02
        DO J = 1,4
          TODUFF(I,J) = DKR(I,J) * PRDUFF(I)
        ENDDO
      ENDDO

C     SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
C     ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.

      NZERO  =  0.01
      LIMBRK =  0.01
      HTXSFT =  1.0
      DO I= 1,MAXSP
        PSOFT(I)  =  0.0
      ENDDO

C     HTR1 and HTR2 are set below, but are not currently used in this variant,
C     unless a user modifies the snag height loss rates thru the SNAGBRK keyword.
C     See fmsnag for details.

      HTR1   =  0.03406
      HTR2   =  0.01

C     ** SPECIES-LEVEL VARIABLE ASSIGNMENT **

C     V2T() - UNITS ARE LB/CUFT BY SPECIES FROM NI/SO VARIANTS,
C     AND 'WOOD HANDBOOK' USDA FOREST PRODUCTS AG. HANDBOOK 72.
C     1974. DENSITY OF PINYON, JUNIPER, GAMBEL FROM CHOJNACKY 1992.

C     [TO CONVERT G/CM**3 TO LB/FT**3, MULTIPLY 'X' G/CM**3 * 62.372]

C     LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES

C     TFALL() - TIME TO FALL FOR DEAD CROWN COMPONENTS. THIS VARIABLE
C     IS NOT UNDER USER-CONTROL BUT MIGHT BE SOMEDAY.
C     [LITTERFALL AND SMALL TWIG FALL VALUES CHANGED 2/97. SB&ER]
C     TFALL INDEXING USES CROWNW() DIMENSIONS, I.E.

C     0 :  FOLIAGE
C     1 : <0.25"
C     2 :  0.25" -   1"
C     3 :  1"    -   3"
C     4 :  3"    -   6"
C     5 :  6"    -  12"

C     IF THE VALUE OF TFALL(I,-) IS LARGER THAN 20, PARAMETER TFMAX IN
C     **FMPARM.F77** MUST BE ADJUSTED TO EQUAL THE NEW VALUE, AND LOOPS
C     INVOLVING THE VALUE (SEE FMSCRO) MUST BE RE-EXAMINED TO INSURE
C     THAT THEY BEHAVE PROPERLY.

C     ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
C     DECAYX() - DECAY RATE MULTIPLIER       (PP = 1.0 in NI var)
C     FALLX()  - FALL RATE MULTIPLIER        (PP = 1.0 in NI var)
C     HTX()    - HEIGHT-LOSS RATE MULTIPLIER (always set to 1.0,
C                                     but users can change thru keyword)

C     ** NOTE THAT SNAG RATE PARAMETERS ARE ALL RELATIVE TO PP **

C     DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
C     CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
C     CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS

C     DECAYX() MULTIPLIER = 999.0; INITIALLY HARD SNAGS NEVER BECOME SOFT

      DO I = 1,MAXSP

         SELECT CASE (I)

C         Pacific silver fir - EC
          CASE (1)
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

C         white fir - SO
          CASE (2)
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

C         grand fir - SO
          CASE (3)
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

C         subalpine fir - SO
          CASE (4)
            V2T(I)     =  19.3
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

C         California red fir/Shasta red fir - SO
          CASE (5)
            V2T(I)     =  22.5
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


C         Sitka spruce - ES
          CASE (6)
            V2T(I)     =  20.6
            LEAFLF(I)  =   5.0
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

C         noble fir - DF, SO
          CASE (7)
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

C         Alaska cedar/western larch - EC,RC
          CASE (8)
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


C         incense cedar - SO
          CASE (9)
            V2T(I)     =  21.8
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   1.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  =  90.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.

C         Engelmann spruce - SO
          CASE (10)
            V2T(I)     =  20.6
            LEAFLF(I)  =   6.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   5.0
            TFALL(I,2) =   5.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  =  90.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.

C         lodgepole pine - SO
          CASE (11)
            V2T(I)     =  23.7
            LEAFLF(I)  =   3.0
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

C         Jeffrey pine - CA
          CASE (12)
            V2T(I)     =  21.2
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   2.0
            TFALL(I,1) =   3.0
            TFALL(I,2) =  10.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.

C         sugar pine - SO
          CASE (13)
            V2T(I)     =  21.2
            LEAFLF(I)  =   3.0
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

C         western white pine - SO
          CASE (14)
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

C         ponderosa pine - SO
          CASE (15)
            V2T(I)     =  23.7
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
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.

C         Douglas-fir, "not used"
          CASE (16,38)
            V2T(I)     =  28.1  ! coast
            LEAFLF(I)  =   5.0
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
            DKRCLS(I)  =   3
            LSW(I)     = .TRUE.

C         coast redwood - EC, sequoia
          CASE (17)
            V2T(I)     =  21.2
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   3.0
            TFALL(I,1) =  10.0
            TFALL(I,2) =  15.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 150.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.

C         western redcedar - EC
          CASE (18)
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

C         western hemlock
          CASE (19)
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

C         mountain hemlock - SO
          CASE (20)
            V2T(I)     =  26.2
            LEAFLF(I)  =   4.0
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

C         bigleaf maple - CA
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

C         red alder - CA
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

C         white alder/pacific madrone - CA
          CASE (23)
            V2T(I)     =  36.2 ! tanoak
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
            DKRCLS(I)  =   3
            LSW(I)     = .FALSE.

C         paper birch
          CASE (24,39)
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

C         giant chinkapin/tanoak - CA
          CASE (25)
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

C         quaking aspen - CA
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

C         black cottonwood - CA
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

C         Oregon white oak/California black oak - CA
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

C         juniper - SO
          CASE (29)
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

C         subalpine larch -  EC, western larch
          CASE (30)
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

C         whitebark pine - CA
          CASE (31)
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

C         knobcone pine - CA
          CASE (32)
            V2T(I)     =  23.7 ! lodgepole pine
            LEAFLF(I)  =   4.0
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

C         Pacific yew - CA
          CASE (33)
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

C         Pacific dogwood - CA
          CASE (34)
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

C         hawthorn (r4 definition)
          CASE (35)
            V2T(I)     =   27.4 ! bigleaf maple
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

C         bitter cherry
          CASE (36)
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

C         willow - CA
          CASE (37)
            V2T(I)     =   22.5
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

        END SELECT

C       TIME-TO-FALL FOR OTHER CROWN CATEGORIES

        TFALL(I,4) = TFALL(I,3)
        TFALL(I,5) = TFALL(I,3)

C       DEAD LEAF FALL CANNOT BE > LIVE

        TFALL(I,0) = MIN(TFALL(I,0), LEAFLF(I))

C       TFALL(I,3) CANNOT BE < TFALL(I,2)

        IF (TFALL(I,2) .GT. TFALL(I,3)) THEN
          TFALL(I,2) = TFALL(I,3)
        ENDIF

C       CONVERT LB/FT**3 TO TONS/FT**3

        V2T(I) = V2T(I) / 2000.0

      ENDDO

C     parameters for post-burn snag fall rates:

      PBSCOR =  0.0
      PBSOFT =  0.0
      PBSMAL =  0.0
      PBSIZE = 12.0
      PBTIME =  7.0

C     PARAMETERS FOR FUEL MODEL SELECTION

C     THE DOUGLAS-FIR DEFAULT COVER GROUP **FMCFMD**
      OLDICT  = 2
      OLDICT2 = 0
      OLDICTWT(1) = 1.0
      OLDICTWT(2) = 0.0

C     DROUGHT START AND END YEARS
      IDRYB  = 0
      IDRYE  = 0

C     CRITICAL % CHANGE REQUIRED TO TRIGGER ACTIVITY FUELS
      SLCRIT = 10.0

C     CRITICAL % CHANGE IN %CC REQUIRED TO TRIGGER SHRUB MODEL DELAY
      CCCRIT = 10.0

      RETURN
      END
