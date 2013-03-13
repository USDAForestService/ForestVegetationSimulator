      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C  **FMVINIT  FIRE-SEI-DATE OF LAST REVISION: 09/21/09
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
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'
      INCLUDE 'FMCLCOM.F77'
      INCLUDE 'BCPLOT.F77'
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

c     Annual decay rates (proportion lost/yr) are taken from Kurz et al. 2009
c     Ecological Modelling 220: 480-504, Table 4

      DKR(1,1)  = 0.12       ! <0.25"
      DKR(2,1)  = 0.12       ! 0.25 - 1"
      DKR(3,1)  = 0.09       ! 1 - 3"
      DKR(4,1)  = 0.015      ! 3 - 6"
      DKR(5,1)  = 0.015      ! 6-12"
      DKR(6,1)  = 0.015      ! 12-20"ma
      DKR(7,1)  = 0.015      ! 20-35"
      DKR(8,1)  = 0.015      ! 35-50"
      DKR(9,1)  = 0.015      ! >50"
                             ! 10: Litter (includes foliage)
                             ! 11: Duff

      DO I = 1,9
        DO J = 2,4
          DKR(I,J) = DKR(I,1)
        ENDDO
      ENDDO
C
C     LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
C
      DO J = 1,4
        DKR(10,J) = 0.355   ! Kurz Table 4: AG very fast = 0.355
        DKR(11,J) = 0.0033  ! Kurz Table 4: BG slow      = 0.0033
      ENDDO

C
C     DUFF PRODUCTION RATES 'PRDUFF' ARE A PROPORTION OF THE OVERALL
C     DECAY RATE: 'DKR'.
C
      DO I = 1,10
        PRDUFF(I) = 0.02
        DO J = 1,4
          TODUFF(I,J) = DKR(I,J) * PRDUFF(I)
        ENDDO
      ENDDO

C     Initialize CWD decay rate sensitivy. 
C     The dimensions of these arrays are MXFLCL from FMPARM.F77  
C     Set Q10 and reference decay temperature for each of size category.
C     Ref: Kurz et al. 2009. Ecol. Mod. 220:480-504

      DO I = 1, 9
        Q10CWD(I) = 2.0  ! Kurz Table 4: snag stems, branches
      ENDDO
      Q10CWD(10)  = 2.65 ! litter - Kurz Table 4: AG very fast 
      Q10CWD(11)  = 1.0  ! duff   - Kurz Table 4: BG slow
      Q10CWD(12)  = 2.0  ! Kurz Table 4: snag stems, branches      

      DO I = 1,(MXFLCL+1)
        REFMATCWD(I) = 10.0
      ENDDO

C     SET ALL THE SNAG PARAMETERS HERE (THOSE WHICH ARE UNDER USER CONTROL).
C     ALSO SET LIMBRK.  NZERO COULD BE UNDER USER-CONTROL ONE DAY.
C
C     VALUE OF HTXSFT BASED ON CONVERTING PL HEIGHT-LOSS RATE TO DF (19.5)
C     AND PP RATE TO ~DF/SPRUCE (10.2, 2.1); USE 10.0
C
      NZERO  =  0.01
      LIMBRK =  0.01
      HTXSFT =  2.0
      HTR1   =  0.0228
      HTR2   =  0.01
      DO I= 1,MAXSP
        PSOFT(I) = 0.0
      ENDDO
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

C         western white pine (uses PP for most WP attributes)
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

C         western larch (uses similarity to PP for some WL attributes)
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

C         Douglas-fir
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

C         grand fir
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

C         western hemlock
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

C         western redcedar (uses DF for some RC attributes)
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

C         lodgepole pine
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

C         Engelmann spruce
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

C         subalpine fir
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

C         ponderosa pine
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

C         BIRCH - paper birch from ie variant
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

C         aspen   - quaking aspen from ie variant
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

C         cottonwood - cottonwood from ie variant
          CASE (13)
            V2T(I)     =  19.3
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

C         other conifer (use douglas-fir)
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

C         other hardwood (use birch from above)
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

        END SELECT

        TFALL(I,1) = 5.0
        TFALL(I,2) = TFALL(I,1)
        TFALL(I,4) = TFALL(I,3)
        TFALL(I,5) = TFALL(I,3)
C
C       DEAD LEAF FALL CANNOT BE > LIVE
C
        TFALL(I,0) = MIN(2.0, LEAFLF(I))
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
C
C     PARAMETERS FOR POST-BURN SNAG FALL RATES:
C
      PBSCOR =  0.0
      PBSOFT =  1.0
      PBSMAL =  0.9
      PBSIZE = 12.0
      PBTIME =  7.0
C
C     THE LODGEPOLE PINE COVER TYPE METAGROUP **FMCFMD**
C
      OLDICT = 0
C
C     DROUGHT START AND END YEARS
C
      IDRYB  = 0
      IDRYE  = 0
C
C     CRITICAL % CHANGE REQUIERED TO TRIGGER ACTIVITY FUELS
C
      SLCRIT = 10.0
C
C     CFIM INITIALIZATIONS
C
C     SET TO TRUE VIA KEYWORD ONLY
      CFIM_ON = .FALSE.

C     BULK DENSITY
      CFIM_BD = 80.0
C     DROUGHT CODE
      CFIM_DC = 300.0
C     fuel_density(kg/m^3)---rho_surf   				398.0
      CFIM_INPUT(17) = 398.0
C     surface_area_to_volume_ratio(1/m)---sigma_surf			3092.0
      CFIM_INPUT(16) = 3092
C     surface_area_to_volume_ratio(1/m)---sigma_can				5401.0
      CFIM_INPUT(19) = 5401.0
C     canopy_fuel_particle_diameter(m)---diameter				0.00074
      CFIM_INPUT(21) = 0.00074
C     canopy_fuel_particle_length(m)---length				0.10
      CFIM_INPUT(23) = 0.10

C     time_step(s)---tstep						1
      CFIM_INPUT(7) = 1.0
C     iterations---iters						150
      CFIM_INPUT(8) = 150.0
C     x_starting_position(m)---xstart					8.0
      CFIM_INPUT(9) = 8.0
C
C     END OF CFIM INITIALIZATIONS
C
      RETURN
      END
