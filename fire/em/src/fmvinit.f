      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C  **FMVINIT  FIRE-EM-DATE OF LAST REVISION: 09/21/09
C----------
C  Purpose:
C  Initialize variant-specific variables for the Fire Model
C  Called from: FMINIT
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
C----------
C  VARIABLE DECLARATIONS
C----------
      INTEGER I,J
C----------
C  SPECIES ORDER:
C   1=WB,  2=WL,  3=DF,  4=LM,  5=LL,  6=RM,  7=LP,  8=ES,
C   9=AF, 10=PP, 11=GA, 12=AS, 13=CW, 14=BA, 15=PW, 16=NC,
C  17=PB, 18=OS, 19=OH
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
C     NEW DECAY RATES (ADDED 2/97) BASED ON VALUES FROM ABBOTT AND
C     CROSSLEY, ECOLOGY 1982. THESE ARE SIZE CLASS DEPENDENT, BUT
C     DO NOT HAVE DIFFERENT DECAY RATES FOR DIFFERENT SPECIES.
C----------
      DKR(1,1) = 0.12
      DKR(2,1) = 0.12
      DKR(3,1) = 0.09
      DKR(4,1) = 0.015
      DKR(5,1) = 0.015
      DKR(6,1) = 0.015
      DKR(7,1) = 0.015
      DKR(8,1) = 0.015
      DKR(9,1) = 0.015
C
      DO I = 1,9
        DO J = 2,4
          DKR(I,J) = DKR(I,1)
        ENDDO
      ENDDO
      
C  LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
      DO J = 1,4
        DKR(10,J) = .5          
        DKR(11,J) = .002        
      ENDDO
C----------
C     DUFF PRODUCTION RATES 'PRDUFF' ARE A PROPORTION OF THE OVERALL
C     DECAY RATE: 'DKR'.
C----------
      DO I = 1,10
        PRDUFF(I)=0.02
        DO J = 1,4
          TODUFF(I,J) = DKR(I,J) * PRDUFF(I)
        ENDDO
      ENDDO
C----------
C     SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
C     ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.
C----------
      NZERO  = 0.01
      LIMBRK = 0.01
      HTXSFT = 2.0
      HTR1   = 0.0228
      HTR2   = 0.01
      DO I= 1,MAXSP
        PSOFT(I) = 0.0
      ENDDO
C----------
C  ** SPECIES-LEVEL VARIABLE ASSIGNMENT **
C----------
C  V2T() - UNITS ARE LB/CUFT BY SPECIES FROM 'WOOD HANDBOOK' USDA
C  FOREST PRODUCTS AG. HANDBOOK 72.  1974. DENSITY OF PINYON, JUNIPER,
C  GAMBEL FROM CHOJNACKY 1992.
C  
C  FOR THE ORIGINAL 8 SPECIES 1-3, 7-10, AND 18, DATA IS 
C  FROM JIM BROWN. 1977. HANDBOOK FOR PREDICTING SLASH WEIGHTS OF
C  WESTERN CONIFERS.
C
C  [TO CONVERT G/CM**3 TO LB/FT**3, MULTIPLY 'X' G/CM**3 * 62.372]
C
C  LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES
C
C  TFALL() - TIME TO FALL FOR DEAD CROWN COMPONENTS. THIS VARIABLE
C  IS NOT UNDER USER-CONTROL BUT MIGHT BE SOMEDAY.
C  TFALL INDEXING USES CROWNW() DIMENSIONS, I.E.
C
C  0 :  FOLIAGE
C  1 : <0.25"
C  2 :  0.25" -   1"
C  3 :  1"    -   3"
C  4 :  3"    -   6"
C  5 :  6"    -  12"
C
C  NOTE: IF ANY TFALL VALUE IS GT 25, YOU SHOULD RE-DIM TFMAX IN FMPARM.
C
C  ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
C  DECAYX() - DECAY RATE MULTIPLIER       (PP = 1.0)
C  FALLX()  - FALL RATE MULTIPLIER        (PP = 1.0)
C  HTX()    - HEIGHT-LOSS RATE MULTIPLIER (PP = 1.0)
C
C  ** NOTE THAT SNAG RATE PARAMETERS ARE ALL RELATIVE TO PP **
C     DEFAULT SNAG RATE PARAMETERS ARE SET RELATIVE TO THE VALUES FOR
C     PP (SPECIES 10), WHICH WAS ORIGINALLY IN THE 'MEDIUM' SPECIES
C     GROUP. ALLDWN & DECAYX VALUES:
C     OTHER SOFTWOODS = PP
C     WHITE BARK PINE, WESTERN LARCH, DOUGLAS-FIR = 1.1 * PP
C     LODGEPOLE PINE, ENGELMANN SPRUCE, SUBALPINE FIR = 0.9 * PP
C
C  DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
C  CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
C  CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS
C
C  LOG MINERALIZATION RATES, BASED ON HARMON ET AL. (DATE?)
C  ADV. ECOL. RES. 15. THERE ARE FOUR DEFAULT RATES, AND EACH OF
C  THOSE SPECIES TAKES ONE OF THE FOUR RATES. THE ABSOLUTE RATES
C  ARE ALSO MAPPED DIRECTLY ONTO EACH OF THE 1-6 SIZE CATEGORIES.
C  THIS IS MORE COMPLICATED THAN NECESSARY, BUT ALLOWS SIZE-DEPENDENT
C  RATES TO BE ADDED EASILY.
C
C----------
      DO I= 1,MAXSP

        SELECT CASE (I)
C----------
C  WHITEBARK PINE 
C----------
          CASE (1)
            V2T(I)     =  22.5    ! uses IE white pine
            LEAFLF(I)  =   7.0    ! uses EM subalpine fir
            TFALL(I,3) =  15.0    ! uses IE white pine
            ALLDWN(I)  = 110.0    ! uses IE white pine
            DECAYX(I)  =   1.1    ! uses IE white pine
            FALLX(I)   =   0.9    ! uses IE white pine
            DO J= 1,4
              HTX(I,J) =   0.9    ! uses IE white pine
            ENDDO
            DKRCLS(I)  =   4      ! uses IE white pine
            LSW(I)     = .TRUE.   ! uses IE white pine
C----------
C  WESTERN LARCH (USES SIMILARITY TO PP FOR SOME WL ATTRIBUTES)
C----------
          CASE (2)
            V2T(I)     =  29.9
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 110.0
            DECAYX(I)  =   1.1
            FALLX(I)   =   0.9
            DO J= 1,4
              HTX(I,J) =   0.9
            ENDDO
            DKRCLS(I)  =   3
            LSW(I)     = .TRUE.
C----------
C  DOUGLAS-FIR
C----------
          CASE (3)
            V2T(I)     =  28.1    ! use DF north; Wood Handbook
            LEAFLF(I)  =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 110.0
            DECAYX(I)  =   1.1
            FALLX(I)   =   0.9
            DO J= 1,4
              HTX(I,J) =   0.9
            ENDDO
            DKRCLS(I)  =   3
            LSW(I)     = .TRUE.
C----------
C  LIMBER PINE -- USE IE LIMBER PINE
C----------
          CASE (4)
            V2T(I)     =  22.5    !w. white pine
            LEAFLF(I)  =   3.0
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
C  SUBALPINE LARCH -- USE IE SUBALPINE LARCH
C----------
          CASE (5)
            V2T(I)     =  19.3    !subalpine fir
            LEAFLF(I)  =   1.0
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
C  ROCKY MOUNTAIN JUNIPER -- USE IE ROCKY MTN JUNIPER
C----------
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
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.
C----------
C  LODGEPOLE PINE
C----------
          CASE (7)
            V2T(I)     =  23.7
            LEAFLF(I)  =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  90.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   1.1
            DO J= 1,4
              HTX(I,J) =   1.1
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  ENGELMANN SPRUCE
C----------
          CASE (8)
            V2T(I)     =  20.6
            LEAFLF(I)  =   6.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  =  90.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   1.1
            DO J= 1,4
              HTX(I,J) =   1.1
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  SUBALPINE FIR
C----------
          CASE (9)
            V2T(I)     =  19.3
            LEAFLF(I)  =   7.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  90.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   1.1
            DO J= 1,4
              HTX(I,J) =   1.1
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C  PONDEROSA PINE
C----------
          CASE (10)
            V2T(I)     =  23.7
            LEAFLF(I)  =   4.0
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
C  GREEN ASH -- USE IE COTTONWOOD
C----------
          CASE (11)
            V2T(I)     =  19.3    !black cottonwood
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  QUAKING ASPEN -- USE IE QUAKING ASPEN
C----------
          CASE (12)
            V2T(I)     =  21.8
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  BLACK COTTONWOOD -- USE IE COTTONWOOD
C----------
          CASE (13)
            V2T(I)     =  19.3    !black cottonwood
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  BALSAM POPLAR -- USE IE COTTONWOOD
C----------
          CASE (14)
            V2T(I)     =  19.3    !black cottonwood
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  PLAINS COTTONWOOD -- USE IE COTTONWOOD
C----------
          CASE (15)
            V2T(I)     =  19.3    !black cottonwood
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  NARROWLEAF COTTONWOOD -- USE IE COTTONWOOD
C----------
          CASE (16)
            V2T(I)     =  19.3    !black cottonwood
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  PAPER BIRCH -- USE IE PAPER BIRCH
C----------
          CASE (17)
            V2T(I)     =  29.9
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C----------
C  OTHER SOFTWOODS
C----------
          CASE (18)
            V2T(I)     =  34.9
            LEAFLF(I)  =   4.0    ! other softwoods (juniper)
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.
C----------
C  OTHER HARDWOODS -- USE IE COTTONWOOD
C----------
          CASE (19)
            V2T(I)     =  19.3    !black cottonwood
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C
        END SELECT
C
        TFALL(I,1) = 5.0
        TFALL(I,2) = TFALL(I,1)
        TFALL(I,4) = TFALL(I,3)
        TFALL(I,5) = TFALL(I,3)
C----------
C  DEAD LEAF FALL CANNOT BE > LIVE
C----------
        TFALL(I,0) = MIN(2.0, LEAFLF(I))
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
C     PARAMETERS FOR POST-BURN SNAG FALL RATES:
C----------
      PBSCOR = 0.0
      PBSOFT = 1.0
      PBSMAL = 0.9
      PBSIZE = 10.0
      PBTIME = 7.0
C----------
C     THE LODGEPOLE PINE COVER TYPE METAGROUP (NOT USED IN EM-FFE)
C----------
      OLDICT = 0
C----------
C     DROUGHT START AND END YEARS
C----------
      IDRYB  = 0
      IDRYE  = 0
C----------
C     CRITICAL % CHANGE REQUIRED TO TRIGGER ACTIVITY FUELS
C----------
      SLCRIT = 10.0
C
      RETURN
      END
