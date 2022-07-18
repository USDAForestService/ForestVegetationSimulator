      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C CANADA-FIRE-BC $Id$
C----------
C  PURPOSE:
C  INITIALIZE VARIANT-SPECIFIC VARIABLES FOR THE FIRE MODEL
C  CALLED FROM: FMINIT
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
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'FMFCOM.F77'
C
C
      INCLUDE 'BCPLOT.F77'
C
COMMONS
C
      INTEGER I,J

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
      DKR(1,1)  = 0.12      ! <0.25"
      DKR(2,1)  = 0.12      ! 0.25 - 1"
      DKR(3,1)  = 0.09      ! 1 - 3"
      DKR(4,1)  = 0.03      ! 3 - 6"
      DKR(5,1)  = 0.03      ! 6-12"  Advice of Walt Klenner July 2021: 95% of CWD gone in 100y => 0.025
      DKR(6,1)  = 0.03      ! 12-20"ma
      DKR(7,1)  = 0.03      ! 20-35"
      DKR(8,1)  = 0.03      ! 35-50"
      DKR(9,1)  = 0.03      ! >50"
C
      DO I = 1,9
        DO J = 2,4
          DKR(I,J) = DKR(I,1)
        ENDDO
      ENDDO
C----------
C     LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
C     Annual decay rates (proportion lost/yr) are taken from Kurz et al. 2009
C     Ecological Modelling 220: 480-504, Table 4
C     LITTER ... AG very fast 
C     DUFF ... BG slow
C----------
      DO J = 1,4
        DKR(10,J) = 0.355
        DKR(11,J) = 0.0033
      ENDDO
C----------
C     DUFF PRODUCTION RATES 'PRDUFF' ARE A PROPORTION OF THE OVERALL
C     DECAY RATE: 'DKR'.
C----------
      DO I = 1,MXFLCL
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
        PSOFT(I) = 0.0
      ENDDO
C----------
C     ** SPECIES-LEVEL VARIABLE ASSIGNMENT **
C----------
C  V2T() - UNITS ARE LB/CUFT BY SPECIES FRIN 'WOOD HANDBOOK' USDA
C  FOREST PRODUCTS AG. HANDBOOK 72.  1974. DENSITY OF PINYON, JUNIPER,
C  GAMBEL FROM CHOJNACKY 1992.
C
C     [TO CONVERT G/CM**3 TO LB/FT**3, MULTIPLY 'X' G/CM**3 * 62.372]
C
C     LEAFLF() - LIFETIME (YRS) OF LEAVES/NEEDLES
C
C     TFALL() - TIME TO FALL FOR DEAD CROWN COMPONENTS. THIS VARIABLE
C     IS NOT UNDER USER-CONTROL BUT MIGHT BE SOMEDAY.
C     TFALL INDEXING USES CROWNW() DIMENSIONS, I.E.
C
C     0 :  FOLIAGE
C     1 : <0.25"
C     2 :  0.25" -   1"
C     3 :  1"    -   3"
C     4 :  3"    -   6"
C     5 :  6"    -  12"
C
C  NOTE: IF ANY TFALL VALUE IS GT 25, YOU SHOULD RE-DIM TFMAX IN FMPARM.
C
C     ALLDWN() - YEARS DEAD AT WHICH ALL SNAGS WILL BE FALLEN
C     DECAYX() - DECAY RATE MULTIPLIER       (PP = 1.0)
C     FALLX()  - FALL RATE MULTIPLIER        (PP = 1.0)
C     HTX()    - HEIGHT-LOSS RATE MULTIPLIER (PP = 1.0)
C
C     ** NOTE THAT SNAG RATE PARAMETERS ARE ALL RELATIVE TO PP **
C
C     DKRCLS() - DECAY RATE CLASS 1 (V.SLOW) TO 4 (FAST). MODEL USERS
C     CAN USE THE FUELDCAY KEYWORD TO REASSIGN RATES WITHIN THE 4
C     CLASSES, AND THE FUELPOOL KEYWORD TO REASSIGN CLASS
C
      DO I= 1,MAXSP

        SELECT CASE (I)
C----------
C  WESTERN WHITE PINE (USES PP FOR MOST WP ATTRIBUTES)
C----------
          CASE (1)
            V2T(I)     =  22.5
            LEAFLF(I)  =   4.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 110.0
            DECAYX(I)  =   1.1
            FALLX(I)   =   0.9
            DO J= 1,4
              HTX(I,J) =   0.9
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
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
            V2T(I)     =  28.1
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
C  GRAND FIR
C----------
          CASE (4)
            V2T(I)     =  21.8
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
C  WESTERN HEMLOCK
C----------
          CASE (5)
            V2T(I)     =  26.2
            LEAFLF(I)  =   5.0
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
C  WESTERN REDCEDAR (USES DF FOR SOME RC ATTRIBUTES)
C----------
          CASE (6)
            V2T(I)     =  19.3
            LEAFLF(I)  =   5.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  =  90.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   1.1
            DO J= 1,4
              HTX(I,J) =   1.1
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
C  PAPER BIRCH
C----------
          CASE (11)
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
C  QUAKING ASPEN
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
C  COTTONWOOD
C----------
          CASE (13)
            V2T(I)     =  19.3 !black cottonwood
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
C  OTHER SOFTWOODS (USE DOUGLAS-FIR)
C----------
          CASE (14)
            V2T(I)     =  28.1
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
C  OTHER HARDWOODS (USE BIRCH)
C----------
          CASE (15)
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
C
        END SELECT
C
        TFALL(I,1) = 5.0
        TFALL(I,2) = TFALL(I,1)
        TFALL(I,4) = TFALL(I,3)
        TFALL(I,5) = TFALL(I,3)
C----------
C       DEAD LEAF FALL CANNOT BE > LIVE
C----------
        TFALL(I,0) = MIN(2.0, LEAFLF(I))
C----------
C       TFALL(I,3) CANNOT BE < TFALL(I,2)
C----------
        IF (TFALL(I,2) .GT. TFALL(I,3)) THEN
          TFALL(I,2) = TFALL(I,3)
        ENDIF
C----------
C       CONVERT LB/FT**3 TO TONS/FT**3
C----------
        V2T(I) = V2T(I) / 2000.0
C
      ENDDO
C----------
C     PARAMETERS FOR POST-BURN SNAG FALL RATES:
C----------
      PBSCOR =  0.0
      PBSOFT =  1.0
      PBSMAL =  0.9
      PBSIZE = 12.0
      PBTIME =  7.0
C----------
C     THE LODGEPOLE PINE COVER TYPE METAGROUP **FMCFMD**
C----------
      OLDICT = 0
C----------
C     DROUGHT START AND END YEARS
C----------
      IDRYB  = 0
      IDRYE  = 0
C----------
C     CRITICAL % CHANGE REQUIERED TO TRIGGER ACTIVITY FUELS
C----------
      SLCRIT = 10.0
C
C----------
C     CFIM INITIALIZATIONS
C----------
C     SET TO TRUE VIA KEYWORD ONLY
      CFIM_ON = .FALSE.
C     BULK DENSITY
      CFIM_BD = 80.0
C     DROUGHT CODE
      CFIM_DC = 300.0
C     fuel_density(kg/m^3)                rho_surf     398.0
      CFIM_INPUT(17) = 398.0
C     surface_area_to_volume_ratio(1/m)   sigma_surf   3092.0
      CFIM_INPUT(16) = 3092
C     surface_area_to_volume_ratio(1/m)   sigma_can    5401.0
      CFIM_INPUT(19) = 5401.0
C     canopy_fuel_particle_diameter(m)    diameter        0.00074
      CFIM_INPUT(21) = 0.00074
C     canopy_fuel_particle_length(m)      length          0.10
      CFIM_INPUT(23) = 0.10
C     time_step(s)                        tstep           1
      CFIM_INPUT(7) = 1.0
C     iterations                          iters         150
      CFIM_INPUT(8) = 150.0
C     x_starting_position(m)              xstart          8.0
      CFIM_INPUT(9) = 8.0
C----------
C     END OF CFIM INITIALIZATIONS
C----------
      RETURN
      END
