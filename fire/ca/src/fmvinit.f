      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C  **FMVINIT  FIRE-CA-DATE OF LAST REVISION: 09/21/09
C----------
*  Purpose:
*      Initialize variant-specific variables for the Fire Model

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
      PREWND(1)=20.
      PREWND(2)=6.
      POTEMP(1)=70.
      POTEMP(2)=70.

C     CA DECAY RATES BASED ON CA DESIGN WORKSHOP (NOTES FROM
C     STEPHANIE REBAIN, 2003) EACH VARIANT MAY USE DIFFERENT
C     RATES AND CITE DIFFERENT DATA SOURCES

C     DECAY RATES IN THE NC ARE SUBSEQUENTLY MODIFIED BY A
C     MULTIPLIER BASED ON DUNNING-CODE (SEE CA **FMCBA**)

      DKR(1,1) =  0.025   ! < 0.25"
      DKR(2,1) =  0.025   ! 0.25 - 1"
      DKR(3,1) =  0.025   ! 1 - 3"
      DKR(4,1) =  0.0125  ! 3 - 6"
      DKR(5,1) =  0.0125  ! 6 - 12"
      DKR(6,1) =  0.0125  ! 12 - 20"
      DKR(7,1) =  0.0125  ! 20 - 35"
      DKR(8,1) =  0.0125  ! 35 - 50"
      DKR(9,1) =  0.0125  !  > 50"

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

C     VALUE OF HTXSFT BASED ON CONVERTING PL HEIGHT-LOSS RATE TO DF (19.5)
C     AND PP RATE TO ~DF/SPRUCE (10.2, 2.1); USE 10.0

      NZERO  =  0.01
      LIMBRK =  0.01
      HTXSFT = 10.0
      DO I= 1,MAXSP
        PSOFT(I)  =  0.0
      ENDDO

C     VALUE OF HTR1 BASED ON 50% HEIGHT LOSS IN 20 YEARS; ALL SPECIES
C     VALUE OF HTR2 BASED ON HEIGHT LOSS CEASING AFTER 50% LOSS
C        NOTE: HTR2 CAN NO LONGER BE 0 (FEB 2002) SO WAS RESET TO DEFAULT 0.01
C              THE CEASING OF HEIGHT-LOSS AFTER 50% IS NOW GIVEN BY HTX (BELOW)

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
C     HTX()    - HEIGHT-LOSS RATE MULTIPLIER (PP = 1.0 in NI var)

C     ** NOTE THAT SNAG RATE PARAMETERS ARE ALL RELATIVE TO PP **

C     DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
C     CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
C     CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS

C     DECAYX() MULTIPLIER = 999.0; INITIALLY HARD SNAGS NEVER BECOME SOFT

      DO I = 1,MAXSP

         SELECT CASE (I)

C         Port Orford cedar (borrows from western redcedar)
          CASE (1)
            V2T(I)     =  24.3
            LEAFLF(I)  =   4.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 150.0
            FALLX(I)   =   1.235 ! 20", 95% down @ 25 yrs
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.

C         incense-cedar
          CASE (2)
            V2T(I)     =  21.8
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.687
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.

C         western redcedar (uses DF for some RC attributes)
          CASE (3)
            V2T(I)     =  19.3
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 150.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.

C         white fir
          CASE (4)
            V2T(I)     =  23.1
            LEAFLF(I)  =   7.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.882
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.

C         California red fir, Shasta red fir
          CASE (5,6)
            V2T(I)     =  22.5
            LEAFLF(I)  =   7.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.882
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.

C         Douglas-fir
          CASE (7)
            V2T(I)     =  28.7 ! interior west DF in Wood Handbook
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.882
            DKRCLS(I)  =   3
            LSW(I)     = .TRUE.

C         western hemlock
          CASE (8)
            V2T(I)     =  26.2
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.

C         mountain hemlock
          CASE (9)
            V2T(I)     =  26.2
            LEAFLF(I)  =   4.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.

C         pines: whitebark (10), knobcone (11), lodgepole (12),
C         Coulter (13), limber (14), Jeffrey (15), sugar (16),
C         western white (17), ponderosa (18), Monterey (19),
C         gray (20), other softwoods (25)
          CASE (10:20,25)
            SELECT CASE (I)
              CASE (10,14,17)
                V2T(I)  =  22.5 ! white
              CASE (11,12,13,18,19,20,25)
                V2T(I)  =  23.7 ! lodgepole/ponderosa
              CASE (15,16)
                V2T(I)  =  21.2 ! sugar
            END SELECT
            SELECT CASE (I)
              CASE (11)
                LEAFLF(I)  =   4.0
              CASE DEFAULT
                LEAFLF(I)  =   3.0
            END SELECT
            TFALL(I,0) =   3.0
            SELECT CASE (I)
              CASE (18,25)
                TFALL(I,3) =  10.0
              CASE DEFAULT
                TFALL(I,3) =  15.0
            END SELECT
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.

c         western juniper
          CASE (21)
            V2T(I)     =  34.9
            LEAFLF(I)  =   4.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 150.0
            FALLX(I)   =   0.687
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.

c         Brewer spruce
          CASE (22)
            V2T(I)     =  20.6 ! Engelmann
            LEAFLF(I)  =   8.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.687
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.

C         giant sequoia
          CASE (23)
            V2T(I)     =  21.2 ! use redwood
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 150.0
            FALLX(I)   =   0.687
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.

C         Pacific yew
          CASE (24)
            V2T(I)     =  26.2 ! use baldcypress
            LEAFLF(I)  =   7.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.687
            DKRCLS(I)  =   1
            LSW(I)     = .TRUE.

C         oaks: coast live oak (26), canyon live oak (27),
C         blue oak (28), Engelmann oak (29), Oregon white
C         oak (30), California black oak (31), valley white
C         oak (32), interior live oak (33), other hardwoods (49)

          CASE (26:33,49)
            SELECT CASE (I)
              CASE (26,27,33)
                V2T(I)  =  49.9  ! live oak
              CASE (28,29,30,32)
                V2T(I)  =  37.4  ! white oak
              CASE (31,49)
                V2T(I)  =  34.9  ! black oak
            END SELECT
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   2
            LSW(I)     = .FALSE.

C         bigleaf maple
          CASE (34)
            V2T(I)     =  27.4
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.

C         California buckeye
          CASE (35)
            V2T(I)     =  37.4 ! use white oak
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.

C         red alder
          CASE (36)
            V2T(I)     =  23.1
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.

C         Pacific madrone (37)
          CASE (37)
            V2T(I)     =  36.2 ! tanoak
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   3
            LSW(I)     = .FALSE.

C         Golden chinkapin (38), tanoak (42)
          CASE (38,42)
            V2T(I)     =  36.2 ! tanoak
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.

C         Pacific dogwood
          CASE (39)
            V2T(I)     =  27.4 ! bigleaf maple
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.

C         Oregon ash
          CASE (40)
            V2T(I)     =  31.2
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.

C         walnut
          CASE (41)
            V2T(I)     =  31.8
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   2
            LSW(I)     = .FALSE.

C         California sycamore
          CASE (43)
            V2T(I)     =  28.7 ! American sycamore
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.

C         quaking aspen
          CASE (44)
            V2T(I)     =  21.8
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.

C         black cottonwood
          CASE (45)
            V2T(I)     =  19.3
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.

C         willow
          CASE (46)
            V2T(I)     =  22.5 ! black willow
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.

C         California nutmeg
          CASE (47)
            V2T(I)     =  34.9 ! hickory-nutmeg
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.

C         California laurel
          CASE (48)
            V2T(I)     =  36.2 ! tanoak
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   2
            LSW(I)     = .FALSE.

        END SELECT

C       HARD SNAGS NEVER BECOME SOFT
C       HEIGHT LOSS IS THE SAME FOR ALL SPP; SEE 'HTR1' (ABOVE)
C       HEIGHT LOSS CEASES FOR THE LAST 50% (USED TO BE SET BY HTR2, CHANGED FEB 2002)

        DECAYX(I) = 999.0
        HTX(I,1)  =   1.0  ! 20 yrs for all species
        HTX(I,3)  =   1.0
        HTX(I,2)  =   0.0
        HTX(I,4)  =   0.0

C       TIME-TO-FALL FOR OTHER CROWN CATEGORIES

        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
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
      PBSOFT =  1.0
      PBSMAL =  0.9
      PBSIZE = 12.0
      PBTIME =  7.0

C     PARAMETERS FOR FUEL MODEL SELECTION

C     THE LODGEPOLE PINE COVER TYPE METAGROUP **FMCFMD**
      OLDICT = 0

C     DROUGHT START AND END YEARS
      IDRYB  = 0
      IDRYE  = 0

C     CRITICAL % CHANGE REQUIRED TO TRIGGER ACTIVITY FUELS
      SLCRIT = 10.0

C     CRITICAL % CHANGE IN %CC REQUIRED TO TRIGGER SHRUB MODEL DELAY
      CCCRIT = 10.0

      RETURN
      END
