      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C  **FMVINIT  FIRE-CI-DATE OF LAST REVISION: 04/23/13
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
C
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
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'FMFCOM.F77'
C
COMMONS
C----------
      INTEGER I,J
C----------
C     SPECIES LIST FOR CENTRAL IDAHO VARIANT.
C
C     1 = WESTERN WHITE PINE (WP)          PINUS MONTICOLA
C     2 = WESTERN LARCH (WL)               LARIX OCCIDENTALIS
C     3 = DOUGLAS-FIR (DF)                 PSEUDOTSUGA MENZIESII
C     4 = GRAND FIR (GF)                   ABIES GRANDIS
C     5 = WESTERN HEMLOCK (WH)             TSUGA HETEROPHYLLA
C     6 = WESTERN REDCEDAR (RC)            THUJA PLICATA
C     7 = LODGEPOLE PINE (LP)              PINUS CONTORTA
C     8 = ENGLEMANN SPRUCE (ES)            PICEA ENGELMANNII
C     9 = SUBALPINE FIR (AF)               ABIES LASIOCARPA
C    10 = PONDEROSA PINE (PP)              PINUS PONDEROSA
C    11 = WHITEBARK PINE (WB)              PINUS ALBICAULIS
C    12 = PACIFIC YEW (PY)                 TAXUS BREVIFOLIA
C    13 = QUAKING ASPEN (AS)               POPULUS TREMULOIDES
C    14 = WESTERN JUNIPER (WJ)             JUNIPERUS OCCIDENTALIS
C    15 = CURLLEAF MOUNTAIN-MAHOGANY (MC)  CERCOCARPUS LEDIFOLIUS
C    16 = LIMBER PINE (LM)                 PINUS FLEXILIS
C    17 = BLACK COTTONWOOD (CW)            POPULUS BALSAMIFERA VAR. TRICHOCARPA
C    18 = OTHER SOFTWOODS (OS)
C    19 = OTHER HARDWOODS (OH)
C
C  SURROGATE EQUATION ASSIGNMENT:
C
C  FROM THE IE VARIANT:
C      USE 17(PY) FOR 12(PY)             (IE17 IS REALLY TT2=LM)
C      USE 18(AS) FOR 13(AS)             (IE18 IS REALLY UT6=AS)
C      USE 13(LM) FOR 11(WB) AND 16(LM)  (IE13 IS REALLY TT2=LM)
C      USE 19(CO) FOR 17(CW) AND 19(OH)  (IE19 IS REALLY CR38=OH)
C
C  FROM THE UT VARIANT:
C      USE 12(WJ) FOR 14(WJ)
C      USE 20(MC) FOR 15(MC)             (UT20 = SO30=MC, WHICH IS
C                                                  REALLY WC39=OT)
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
C
      DKR(1,1)  = 0.12
      DKR(2,1)  = 0.12
      DKR(3,1)  = 0.09
      DKR(4,1)  = 0.015
      DKR(5,1)  = 0.015
      DKR(6,1)  = 0.015
      DKR(7,1)  = 0.015
      DKR(8,1)  = 0.015
      DKR(9,1)  = 0.015
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
        DKR(10,J) = 0.5
        DKR(11,J) = 0.002
      ENDDO
C----------
C  DUFF PRODUCTION RATES 'PRDUFF' ARE A PROPORTION OF THE OVERALL
C  DECAY RATE: 'DKR'.
C----------
      DO I = 1,10
        DO J = 1,4
          PRDUFF(I,J) = 0.02
          TODUFF(I,J) = DKR(I,J) * PRDUFF(I,J)
        ENDDO
      ENDDO
C----------
C     SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
C     ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.
C
C     VALUE OF HTXSFT BASED ON CONVERTING PL HEIGHT-LOSS RATE TO DF (19.5)
C     AND PP RATE TO ~DF/SPRUCE (10.2, 2.1); USE 10.0
C----------
      NZERO  =  0.01
      LIMBRK =  0.01
      HTXSFT =  2.0
      HTR1   =  0.0228
      HTR2   =  0.01
      DO I= 1,MAXSP
        PSOFT(I)  =  0.0
      ENDDO
C----------
C     ** SPECIES-LEVEL VARIABLE ASSIGNMENT **
C
C     V2T() - UNITS ARE LB/CUFT BY SPECIES FROM NI/SO VARIANTS,
C     AND 'WOOD HANDBOOK' USDA FOREST PRODUCTS AG. HANDBOOK 72.
C     1974.
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
C----------
      DO I= 1,MAXSP

        SELECT CASE (I)
C
C  1=WP, western white pine (NI uses PP for most WP attributes)
C
          CASE (1)
            V2T(I)     =  22.5
            LEAFLF(I)  =   4.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 110.0
            DECAYX(I)  =   1.1
            FALLX(I)   =   0.9   ! NI default
            DO J= 1,4
              HTX(I,J)=    0.4   ! 25% loss 30 yr -> 0.42
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C  2=WL, western larch (NI)
C
          CASE (2)
            V2T(I)     =  29.9
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 150.0
            DECAYX(I)  =   1.1
            FALLX(I)   =   1.0   ! 95% down 15"/25yr -> 1.0
            DO J= 1,4
              HTX(I,J)=    0.001 ! no height loss
            ENDDO
            DKRCLS(I)  =   3
            LSW(I)     = .TRUE.
C
C  3=DF, Douglas-fir (NI)
C
          CASE (3)
            V2T(I)     =  28.1   ! north DF in Wood Handbook
            LEAFLF(I)  =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  75.0
            DECAYX(I)  =   1.1
            FALLX(I)   =   0.9   ! 95% down 18"/30yr -> 0.9
            DO J= 1,4
              HTX(I,J) =   1.0   ! 50% loss 30 yr -> 1.0
            ENDDO
            DKRCLS(I)  =   3
            LSW(I)     = .TRUE.
C
C  4=GF, grand fir
C
          CASE (4)
            V2T(I)     =  21.8
            LEAFLF(I)  =   7.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  90.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   1.1
            DO J= 1,4
              HTX(I,J) =   1.5   ! 50% loss 20 yr -> 1.52
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C  5=WH, western hemlock
C
          CASE (5)
            V2T(I)     =  26.2
            LEAFLF(I)  =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 150.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   0.9   ! 95% down 18"/28 yr -> 0.87
            DO J= 1,4
              HTX(I,J) =   0.9   ! 50% loss 35 yr -> 0.87
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C  6=RC, western redcedar
C
          CASE (6)
            V2T(I)     =  19.3
            LEAFLF(I)  =   5.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 300.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   0.3   ! 95% down 18"/100yr -> 0.27
            DO J= 1,4
              HTX(I,J) =   0.3   ! 25% loss 50 yr -> 0.25
            ENDDO
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.
C
C  7=LP, lodgepole pine
C
          CASE (7)
            V2T(I)     =  23.7
            LEAFLF(I)  =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  35.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   1.6   ! 95% down 15"/15yr -> 1.6
            DO J= 1,4
              HTX(I,J) =   0.001 ! no height loss
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C  8=ES, Engelmann spruce
C  special handling of FALLX(8) in **FMSNAG**
C
          CASE (8)
            V2T(I)     =  20.6
            LEAFLF(I)  =   6.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   1.2   ! 95% 15"/20yr -> 1.2; 24"/95yr -> 0.4
            DO J= 1,4
              HTX(I,J) =   0.001 ! no height loss
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C  9=AF, subalpine fir
C
          CASE (9)
            V2T(I)     =  19.3
            LEAFLF(I)  =   7.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  40.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   0.8   ! 95% 15"/30yr -> 0.8
            DO J= 1,4
              HTX(I,J) =   0.001 ! no height loss
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C  10=PP, ponderosa pine (NI)
C
          CASE (10)
            V2T(I)     =  23.7
            LEAFLF(I)  =   4.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  =  90.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0   ! NI default
            DO J= 1,4
              HTX(I,J) =   0.001 ! no height loss
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C 11=WB, whitebark pine
C 16=LM, limber pine
C
          CASE (11,16)
            V2T(I)     =  22.5  !w. white pine
            LEAFLF(I)  =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  90.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   0.41
            DO J= 1,4
              HTX(I,J) =   0.001 ! no height loss
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C 12=PY, Pacific yew
C
          CASE (12)
            V2T(I)     =  26.2  ! bald cypress
            LEAFLF(I)  =   7.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   1
            LSW(I)     = .TRUE.
C
C 13=AS, quaking aspen
C
          CASE (13)
            V2T(I)     =  21.8
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  =   5.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   4.0
            DO J= 1,4
              HTX(I,J) =   0.001 ! no height loss
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C
C 14=WJ, western juniper
C
          CASE (14)
            V2T(I)     =  34.9
            LEAFLF(I)  =   4.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 150.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   0.0978
            ENDDO
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.
C
C 15=MC, curlleaf mtn-mahogany
C
          CASE (15)
            V2T(I)     =  21.8
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  90.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =   1.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C
C 17=CW, black cottonwood
C 19=OH, other hardwoods
C
          CASE (17,19)
            V2T(I)     =  19.3 !black cottonwood
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  =   5.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   4.0
            DO J= 1,4
              HTX(I,J) =   0.001 ! no height loss
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C
C 18=OS, other softwoods (mountain hemlock (snag params from EC-PSF))
C
          CASE (18)
            V2T(I)     =  26.2
            LEAFLF(I)  =   4.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  30.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   0.9  ! 95% 18"/25yr -> 0.9
            DO J= 1,4
              HTX(I,J) =   1.5  ! 50% 20 yr -> 1.52
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
        END SELECT
C
        SELECT CASE (I)
C 14=WJ
        CASE (14)
          TFALL(I,1) = 10.0
          TFALL(I,2) = 15.0
          TFALL(I,4) = TFALL(I,3)
          TFALL(I,5) = TFALL(I,3)
C
        CASE DEFAULT
          TFALL(I,1) = 5.0
          TFALL(I,2) = TFALL(I,1)
          TFALL(I,4) = TFALL(I,3)
          TFALL(I,5) = TFALL(I,3)
        END SELECT
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

      ENDDO
C----------
C  PARAMETERS FOR POST-BURN SNAG FALL RATES:
C----------
      PBSCOR =  0.0
      PBSOFT =  1.0
      PBSMAL =  0.9
      PBSIZE = 12.0
      PBTIME =  7.0
C----------
C  THE LODGEPOLE PINE COVER TYPE METAGROUP **FMCFMD**
C----------
      OLDICT = 0
C----------
C  DROUGHT START AND END YEARS
C----------
      IDRYB  = 0
      IDRYE  = 0
C----------
C  CRITICAL % CHANGE REQUIRED TO TRIGGER ACTIVITY FUELS
C----------
      SLCRIT = 10.0
C
      RETURN
      END
