      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C  **FMVINIT  FIRE-NC-DATE OF LAST REVISION: 04/23/13
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
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
C
COMMONS
C
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
C
C     NC DECAY RATES BASED ON NC DESIGN WORKSHOP (NOTES FROM
C     STEPHANIE REBAIN, 2003) EACH VARIANT MAY USE DIFFERENT
C     RATES AND CITE DIFFERENT DATA SOURCES
C
C     DECAY RATES IN THE NC ARE SUBSEQUENTLY MODIFIED BY A
C     MULTIPLIER BASED ON DUNNING-CODE (SEE NC **FMCBA**)
C
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
C
C  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
C
      DO J = 1,4
        DKR(10,J) = 0.5
        DKR(11,J) = 0.002
      ENDDO

C     Duff production rates 'PRDUFF' are a proportion of the overall
C     decay rate: 'DKR'.

      DO I = 1,10
        DO J = 1,4
          PRDUFF(I,J) = 0.02
          TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J)
        ENDDO
      ENDDO

C     SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
C     ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.
C
C     VALUE OF HTXSFT BASED ON CONVERTING PL HEIGHT-LOSS RATE TO DF (19.5)
C     AND PP RATE TO ~DF/SPRUCE (10.2, 2.1); USE 10.0
C
      NZERO  =  0.01
      LIMBRK =  0.01
      HTXSFT = 10.0
      DO I= 1,MAXSP
        PSOFT(I)  =  0.0
      ENDDO
C
C     VALUE OF HTR1 BASED ON 50% HEIGHT LOSS IN 20 YEARS; ALL SPECIES
C     VALUE OF HTR2 BASED ON HEIGHT LOSS CEASING AFTER 50% LOSS
C        NOTE: HTR2 CAN NO LONGER BE 0 (FEB 2002) SO WAS RESET TO DEFAULT 0.01
C              THE CEASING OF HEIGHT-LOSS AFTER 50% IS NOW GIVEN BY HTX (BELOW)
C
      HTR1   =  0.03406
      HTR2   =  0.01
C
C     ** SPECIES-LEVEL VARIABLE ASSIGNMENT **
C
C     V2T() - UNITS ARE LB/CUFT BY SPECIES FROM NI/SO VARIANTS,
C     AND 'WOOD HANDBOOK' USDA FOREST PRODUCTS AG. HANDBOOK 72.
C     1974. DENSITY OF PINYON, JUNIPER, GAMBEL FROM CHOJNACKY 1992.
C
C     [TO CONVERT G/CM**3 TO LB/FT**3, MULTIPLY 'X' G/CM**3 * 62.372]
C
C     LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES
C
C     TFALL() - TIME TO FALL FOR DEAD CROWN COMPONENTS. THIS VARIABLE
C     IS NOT UNDER USER-CONTROL BUT MIGHT BE SOMEDAY.
C     [LITTERFALL AND SMALL TWIG FALL VALUES CHANGED 2/97. SB&ER]
C     TFALL INDEXING USES CROWNW() DIMENSIONS, I.E.
C
C     0 :  FOLIAGE
C     1 : <0.25"
C     2 :  0.25" -   1"
C     3 :  1"    -   3"
C     4 :  3"    -   6"
C     5 :  6"    -  12"
C
C     IF THE VALUE OF TFALL(I,-) IS LARGER THAN 20, PARAMETER TFMAX IN
C     **FMPARM.F77** MUST BE ADJUSTED TO EQUAL THE NEW VALUE, AND LOOPS
C     INVOLVING THE VALUE (SEE FMSCRO) MUST BE RE-EXAMINED TO INSURE
C     THAT THEY BEHAVE PROPERLY.
C
C     ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
C     DECAYX() - DECAY RATE MULTIPLIER       (PP = 1.0 in NI var)
C     FALLX()  - FALL RATE MULTIPLIER        (PP = 1.0 in NI var)
C     HTX()    - HEIGHT-LOSS RATE MULTIPLIER (PP = 1.0 in NI var)
C
C     ** NOTE THAT SNAG RATE PARAMETERS ARE ALL RELATIVE TO PP **
C
C     DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
C     CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
C     CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS
C
C     DECAYX() MULTIPLIER = 999.0; INITIALLY HARD SNAGS NEVER BECOME SOFT
C
      DO I= 1,MAXSP

         SELECT CASE (I)

C         sugar pine
          CASE (2)
            V2T(I)     =  21.2
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.

C         Douglas-fir, other conifers
          CASE (3,1)
            V2T(I)     =  28.7 ! interior west DF in Wood Handbook
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.882
            DKRCLS(I)  =   3
            LSW(I)     = .TRUE.

C         white fir & red fir
          CASE (4,9)
            SELECT CASE (I)
              CASE (4)
                V2T(I) =  23.1
              CASE (9)
                V2T(I) =  22.5
            END SELECT
            LEAFLF(I)  =   7.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.882
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.

C         madrone
          CASE (5)
            V2T(I)     =  36.2   ! use tanoak
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0   ! like tanoak and black oak
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545 ! like tanoak and black oak
            DKRCLS(I)  =   3
            LSW(I)     = .FALSE.

C         incense cedar
          CASE (6)
            V2T(I)     =  21.8
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.687
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.

C         California black oak
          CASE (7)
            V2T(I)     =  34.9
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0 ! wild guess
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   2
            LSW(I)     = .FALSE.

C         tanoak & other hardwoods
          CASE (8,11)
            V2T(I)     =  36.2
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0 ! wild guess
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.

C         ponderosa pine
          CASE (10)
            V2T(I)     =  23.7
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.

        END SELECT
C
C       HARD SNAGS NEVER BECOME SOFT
C       HEIGHT LOSS IS THE SAME FOR ALL SPP; SEE 'HTR1' (ABOVE)
C       HEIGHT LOSS CEASES FOR THE LAST 50% (USED TO BE SET BY HTR2, CHANGED FEB 2002)
C
        DECAYX(I) = 999.0
        HTX(I,1)  =   1.0
        HTX(I,3)  =   1.0
        HTX(I,2)  =   0.0
        HTX(I,4)  =   0.0
C
C       TIME-TO-FALL FOR OTHER CROWN CATEGORIES
C
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,4) = TFALL(I,3)
        TFALL(I,5) = TFALL(I,3)
C
C       DEAD LEAF FALL CANNOT BE > LIVE
C
        TFALL(I,0) = MIN(TFALL(I,0), LEAFLF(I))
C
C       TFALL(I,3) CANNOT BE < TFALL(I,2)
C
        IF (TFALL(I,2) .GT. TFALL(I,3)) THEN
          TFALL(I,2) = TFALL(I,3)
        ENDIF
C
C       CONVERT LB/FT**3 TO TONS/FT**3
C
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
