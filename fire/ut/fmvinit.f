      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C FIRE-UT $Id$
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
C  SET FLAG FOR WESTERN VARIANT
C----------
      LVWEST    = .TRUE.
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
C     LOG MINERALIZATION RATES, BASED ON HARMON ET AL. (DATE?)
C     ADV. ECOL. RES. 15. THERE ARE FOUR DEFAULT RATES, AND EACH OF
C     THOSE SPECIES TAKES ONE OF THE FOUR RATES. THE ABSOLUTE RATES
C     ARE ALSO MAPPED DIRECTLY ONTO EACH OF THE 1-6 SIZE CATEGORIES.
C     THIS IS MORE COMPLICATED THAN NECESSARY, BUT ALLOWS SIZE-DEPENDENT
C     RATES TO BE ADDED EASILY.
C
CSB      DKRDEF(1)=.007
CSB      DKRDEF(2)=.010
CSB      DKRDEF(3)=.011
CSB      DKRDEF(4)=.012
C
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
C----------
C     LITTER LOSS/YR (10) AND DUFF LOSS/YR (11)
C----------
      DO J = 1,4
        DKR(10,J) = 0.5
        DKR(11,J) = 0.002
      ENDDO
C----------
C     MODIFER FOR CR/UT - NO SPECIES MODIFIER
C     THIS AGREES WITH PUBLISHED VALUES FOR LARGE CWD
C     LP LOGS AT 100 YR: 0.0067/YR
C     SOURCE: BROWN ET AL. 1998. CJFR 28:932-936
C----------
      DO I = 1,MXFLCL
        DO J = 2,4
          DKR(I,J) = DKR(I,J) * 0.45
        ENDDO
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
      HTXSFT = 10.0
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
C----------
      DO I= 1,MAXSP
C
        SELECT CASE (I)
C
C         whitebark & limber pine, other softwoods
C
          CASE (1,2,23)
            V2T(I)     =  22.5 ! use white pine
            LEAFLF(I)  =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 150.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =  0.0978
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C         Douglas-fir
C
          CASE (3)
            V2T(I)     =  26.8  ! use DF South from Wood Handbook
            LEAFLF(I)  =   5.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   1.1
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =  0.9
            ENDDO
            DKRCLS(I)  =   3
            LSW(I)     = .TRUE.
C
C         white fir
C
          CASE (4)
            V2T(I)     =  23.1
            LEAFLF(I)  =   7.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  40.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   2.5
            DO J= 1,4
              HTX(I,J) =  1.494
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C         Blue & Engelmann spruce
C
          CASE (5,8)
            V2T(I)     =  20.6
            LEAFLF(I)  =   6.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  = 100.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =  0.0462
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C         quaking aspen
C
          CASE (6)
            V2T(I)     =  21.8
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  =   5.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   4.0
            DO J= 1,4
              HTX(I,J) =   0.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C
C         lodgepole pine
C
          CASE (7)
            V2T(I)     =  23.7
            LEAFLF(I)  =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 150.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =  0.0462
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C         subalpine fir
C
          CASE (9)
            V2T(I)     =  19.3
            LEAFLF(I)  =   7.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  40.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   2.5
            DO J= 1,4
              HTX(I,J) =  1.494
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C         ponderosa pine
C
          CASE (10)
            V2T(I)     =  23.7
            LEAFLF(I)  =   4.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  = 150.0
            DECAYX(I)  =   1.0
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =  0.0978
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C         common pinyon, singleleaf pinyon
C
          CASE (11,14)
            V2T(I)     =  31.8
            LEAFLF(I)  =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 150.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =  0.0978
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C         western juniper, RM juniper, Utah juniper
C
          CASE (12,15,16)
            V2T(I)     =  34.9
            LEAFLF(I)  =   4.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 150.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =  0.0978
            ENDDO
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.
C
C         Gambel oak, other hardwoods
C
          CASE (13,24)
            V2T(I)     =  39.6
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  40.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   2.5
            DO J= 1,4
              HTX(I,J) =  1.494
            ENDDO
            DKRCLS(I)  =   2
            LSW(I)     = .FALSE.
C
C         GB bristlecone pine (based on LP)
C
          CASE (17)
            V2T(I)     =  23.7
            LEAFLF(I)  =   3.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 999.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   0.001
            DO J= 1,4
              HTX(I,J) =   0.0462
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
C         narrowleaf cottonwood, Fremont cottonwood, box elder
C
          CASE (18,19,22)
            V2T(I)  =  19.3
            LEAFLF(I)  =   1.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  =   5.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   4.0
            DO J= 1,4
              HTX(I,J) =   0.0
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .FALSE.
C
C         curlleaf mtn mahogany, bigtooth maple
C
          CASE (20,21)
            SELECT CASE (I)
            CASE(20)
              V2T(I)  = 21.8
            CASE(21)
              V2T(I)  = 27.4
            END SELECT
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
        END SELECT
C
        SELECT CASE (I)
        
C----------
C       TIME-TO-FALL FOR OTHER CROWN CATEGORIES
C----------
        CASE (20,21)
          TFALL(I,1) =   5.0
          TFALL(I,2) =   5.0
          TFALL(I,4) = TFALL(I,3)
          TFALL(I,5) = TFALL(I,3)
C----------
C       DEAD LEAF FALL CANNOT BE > LIVE
C----------
          TFALL(I,0) = MIN(TFALL(I,0), LEAFLF(I))
C
        CASE DEFAULT
          TFALL(I,1) = 10.0
          TFALL(I,2) = 15.0
          TFALL(I,4) = TFALL(I,3)
          TFALL(I,5) = TFALL(I,3)
C----------
C       DEAD LEAF FALL CANNOT BE > LIVE
C----------
          TFALL(I,0) = MIN(2.0, LEAFLF(I))
C
        END SELECT
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
      OLDICT = 6
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
