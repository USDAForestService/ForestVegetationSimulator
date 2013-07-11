      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C  **FMVINIT  FIRE-BM-DATE OF LAST REVISION: 04/25/13
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
C     DECAY RATES BASED ON WORKSHOP RESULTS FOR KIM MELLEN-MCLEAN'S CWD MODEL
C     FIRST BASE RATES ARE SET (BY DECAY RATE CLASS) AND THEN THEY ARE ADJUSTED
C     BASED ON HABITAT TYPE (TEMPERATURE AND MOISTURE CATEGORY)

      DKR(1,1) = 0.076 ! < 0.25"
      DKR(2,1) = 0.076 ! 0.25 - 1"
      DKR(3,1) = 0.076 ! 1 - 3"
      DKR(4,1) = 0.019 ! 3 - 6"
      DKR(5,1) = 0.019 ! 6 - 12"
      DKR(6,1) = 0.019  ! 12 - 20"
      DKR(7,1) = 0.019  ! 20 - 35"
      DKR(8,1) = 0.019  ! 35 - 50"
      DKR(9,1) = 0.019  !  > 50"

      DKR(1,2) = 0.081 ! < 0.25"
      DKR(2,2) = 0.081 ! 0.25 - 1"
      DKR(3,2) = 0.081 ! 1 - 3"
      DKR(4,2) = 0.025 ! 3 - 6"
      DKR(5,2) = 0.025 ! 6 - 12"
      DKR(6,2) = 0.025  ! 12 - 20"
      DKR(7,2) = 0.025  ! 20 - 35"
      DKR(8,2) = 0.025  ! 35 - 50"
      DKR(9,2) = 0.025  !  > 50"
      
      DKR(1,3) = 0.090 ! < 0.25"
      DKR(2,3) = 0.090 ! 0.25 - 1"
      DKR(3,3) = 0.090 ! 1 - 3"
      DKR(4,3) = 0.033 ! 3 - 6"
      DKR(5,3) = 0.033 ! 6 - 12"
      DKR(6,3) = 0.033  ! 12 - 20"
      DKR(7,3) = 0.033  ! 20 - 35"
      DKR(8,3) = 0.033  ! 35 - 50"
      DKR(9,3) = 0.033  !  > 50"      

      DKR(1,4) = 0.113 ! < 0.25"
      DKR(2,4) = 0.113 ! 0.25 - 1"
      DKR(3,4) = 0.113 ! 1 - 3"
      DKR(4,4) = 0.058 ! 3 - 6"
      DKR(5,4) = 0.058 ! 6 - 12"
      DKR(6,4) = 0.058  ! 12 - 20"
      DKR(7,4) = 0.058  ! 20 - 35"
      DKR(8,4) = 0.058  ! 35 - 50"
      DKR(9,4) = 0.058  !  > 50"

C
C  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
C
      DO J = 1,4
        DKR(10,J) = 0.65
        DKR(11,J) = 0.002
      ENDDO
C
C     DUFF PRODUCTION RATES 'PRDUFF' ARE A PROPORTION OF THE OVERALL
C     DECAY RATE: 'DKR'.
C
      DO I = 1,10
        DO J = 1,4
          PRDUFF(I,J) = 0.02
          TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J)
        ENDDO
      ENDDO
C
C     SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
C     ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.
C
      NZERO  =  0.01
      LIMBRK =  0.01
      HTXSFT =  1.0
      DO I = 1,MAXSP
        PSOFT(I) = 0.0
      ENDDO
C
C     HTR1 and HTR2 are set below, but are not currently used in this variant,
C     unless a user modifies the snag height loss rates thru the SNAGBRK keyword.
C     See fmsnag for details.
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
C     FALL RATE MULTIPLIER FALLX() = 1.79 FOR ALL PINES. THIS GIVES 7%/YR
C     FOR DBH=15"; FALLX() = 1.02 FOR ALL OTHERS; THIS GIVE 4%/YR FOR
C     DBH=15"
C
C     DECAYX() MULTIPLIER = 999.0; INITIALLY HARD SNAGS NEVER BECOME SOFT
C
      DO I= 1,MAXSP

         SELECT CASE (I)
C
C  WESTERN WHITE PINE (1 = WP) (uses PP for some WP attributes)
C
          CASE (1)
            V2T(I)     =  22.5
            LEAFLF(I)  =   4.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  75.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            LSW(I)     = .TRUE.
C
C  WESTERN LARCH (2 = WL) (uses similarity to PP for some WL attributes)
C
          CASE (2)
            V2T(I)     =  29.9
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            LSW(I)     = .TRUE.
C
C  DOUGLAS-FIR (3 = DF)
C
          CASE (3)
            V2T(I)     =  28.7  ! west DF in Wood Handbook
            LEAFLF(I)  =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  75.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            LSW(I)     = .TRUE.
C
C  GRAND FIR (4 = GF)
C
          CASE (4)
            V2T(I)     =  21.8
            LEAFLF(I)  =   7.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  60.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            LSW(I)     = .TRUE.
C
C  MOUNTAIN HEMLOCK (5 = MH) (mostly from western hemlock)
C
          CASE (5)
            V2T(I)     =  26.2
            LEAFLF(I)  =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  70.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            LSW(I)     = .TRUE.
C
C  WESTERN JUNIPER (6 = WJ) -- USE SO WJ
C
          CASE (6)
            V2T(I)     =  34.9 
            LEAFLF(I)  =   4.0 
            TFALL(I,3) =  15.0 
            ALLDWN(I)  = 100.0 
            DECAYX(I)  =   1.0 
            FALLX(I)   =   1.0 
            DO J= 1,4
              HTX(I,J) =   1.0 
            ENDDO
            LSW(I)     = .TRUE.
C
C  LODGEPOLE PINE (7 = LP)
C
          CASE (7)
            V2T(I)     =  23.7
            LEAFLF(I)  =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            LSW(I)     = .TRUE.
C
C  ENGELMANN SPRUCE (8 = ES)
C
          CASE (8)
            V2T(I)     =  20.6
            LEAFLF(I)  =   6.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  =  75.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            LSW(I)     = .TRUE.
C
C  SUBALPINE FIR (9 = AF)
C
          CASE (9)
            V2T(I)     =  19.3
            LEAFLF(I)  =   7.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  60.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            LSW(I)     = .TRUE.
C
C  PONDEROSA PINE (10 = PP)
C
          CASE (10)
            V2T(I)     =  23.7
            LEAFLF(I)  =   4.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  =  80.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            LSW(I)     = .TRUE.
C
C  WHITEBARK PINE (11 = WB) USE SO WB VALUES
C
          CASE (11)
            V2T(I)     =  22.5  ! from white pine
            LEAFLF(I)  =   3.0 
            TFALL(I,3) =  15.0 
            ALLDWN(I)  = 110.0 
            DECAYX(I)  =   1.0 
            FALLX(I)   =   1.0 
            DO J= 1,4
              HTX(I,J) =   1.0 
            ENDDO
            LSW(I)     = .TRUE.
C
C  LIMBER PINE (12 = LM) USE SO WB VALUES
C
          CASE (12)
            V2T(I)     =  22.5  ! from white pine
            LEAFLF(I)  =   3.0 
            TFALL(I,3) =  15.0 
            ALLDWN(I)  = 110.0 
            DECAYX(I)  =   1.0 
            FALLX(I)   =   1.0 
            DO J= 1,4
              HTX(I,J) =   1.0 
            ENDDO
            LSW(I)     = .TRUE.
C
C  PACIFIC YEW (13 = PY) USE SO PY VALUES
C
          CASE (13)
            V2T(I)     =  26.2 ! baldcypress
            LEAFLF(I)  =   7.0 
            TFALL(I,3) =  20.0 
            ALLDWN(I)  =  90.0 
            DECAYX(I)  =   1.0 
            FALLX(I)   =   1.0 
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO 
            LSW(I)     = .TRUE.
C
C  ALASKA CEDAR (14 = YC) USE WC YC VALUES
C
          CASE (14)
            V2T(I)     =  26.2
            LEAFLF(I)  =   5.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 300.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            LSW(I)     = .TRUE.
C
C  QUAKING ASPEN (15 = AS) USE SO AS VALUES
C
          CASE (15)
            V2T(I)     =  21.8 
            LEAFLF(I)  =   1.0 
            TFALL(I,3) =  15.0 
            ALLDWN(I)  =  90.0 
            DECAYX(I)  =   1.0 
            FALLX(I)   =   1.0 
            DO J= 1,4
              HTX(I,J) =   1.0 
            ENDDO
            LSW(I)     = .FALSE.
C
C  BLACK COTTONWOOD (16 = CW) USE SO CW VALUES
C
          CASE (16)
            V2T(I)     =  19.3 
            LEAFLF(I)  =   1.0 
            TFALL(I,3) =  15.0 
            ALLDWN(I)  =  90.0 
            DECAYX(I)  =   1.0 
            FALLX(I)   =   1.0 
            DO J= 1,4
              HTX(I,J) =   1.0 
            ENDDO
            LSW(I)     = .FALSE.
C
C  OTHER SOFTWOODS (17 = OS) (USE BM PONDEROSA PINE)
C
          CASE (17)
            V2T(I)     =  23.7
            LEAFLF(I)  =   4.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  =  80.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            LSW(I)     = .TRUE.
C
C  OTHER HARDWOODS (18 = OH) USE SO OH VALUES
C
          CASE (18)
            V2T(I)     =  21.8 
            LEAFLF(I)  =   1.0 
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  90.0 
            DECAYX(I)  =   1.0 
            FALLX(I)   =   1.0 
            DO J= 1,4
              HTX(I,J) =   1.0 
            ENDDO
            LSW(I)     = .FALSE.
C
        END SELECT
C
C
C       TIME-TO-FALL FOR OTHER CROWN CATEGORIES
C
        SELECT CASE (I)
        CASE(6,11:16,18)
          TFALL(I,0) =  2.0
          TFALL(I,1) =  5.0
          TFALL(I,2) =  5.0
        CASE DEFAULT
          TFALL(I,0) =  3.0
          TFALL(I,1) = 10.0
          TFALL(I,2) = 15.0
        END SELECT
C
        TFALL(I,4) = TFALL(I,3)
        TFALL(I,5) = TFALL(I,3)
C
C       DEAD LEAF FALL CANNOT BE > LIVE
C
        TFALL(I,0) = MIN(3.0, LEAFLF(I))
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

C
C       SET THE DECAY RATE CLASS (DKRCLS)
C
        SELECT CASE (I)

C         some pines, doug-fir, cedars 
          CASE (1:3,6,11:14)
            DKRCLS(I)  =   1

C         lodgepole, spruce, hemlock
          CASE (5,7,8)
            DKRCLS(I)  =   2

C         firs, some pines, oak
          CASE (4,9,10,17)
            DKRCLS(I)  =   3

C         aspen, cottonwood, other hardwoods
          CASE (15,16,18)
            DKRCLS(I)  =   4
            
        END SELECT

      ENDDO
C
C     PARAMETERS FOR POST-BURN SNAG FALL RATES:
C
      PBSCOR =  0.0
      PBSOFT =  0.0
      PBSMAL =  0.0
      PBSIZE = 12.0
      PBTIME =  7.0
C
C     THE LODGEPOLE PINE COVER TYPE METAGROUP (NOT USED IN BM-FFE)
C
      OLDICT = 0
C
C     DROUGHT START AND END YEARS
C
      IDRYB  = 0
      IDRYE  = 0
C
C     CRITICAL % CHANGE REQUIRED TO TRIGGER ACTIVITY FUELS
C
      SLCRIT = 10.0

      RETURN
      END
