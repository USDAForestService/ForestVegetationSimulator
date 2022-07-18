      SUBROUTINE FMVINIT
      IMPLICIT NONE
C----------
C FIRE-WS $Id$
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
C----------
      INTEGER I,J
C----------
C     SPECIES LIST FOR WESTERN SIERRAS VARIANT.
C
C     1 = SUGAR PINE (SP)                   PINUS LAMBERTIANA
C     2 = DOUGLAS-FIR (DF)                  PSEUDOTSUGA MENZIESII
C     3 = WHITE FIR (WF)                    ABIES CONCOLOR
C     4 = GIANT SEQUOIA (GS)                SEQUOIADENDRON GIGANTEAUM
C     5 = INCENSE CEDAR (IC)                LIBOCEDRUS DECURRENS
C     6 = JEFFREY PINE (JP)                 PINUS JEFFREYI
C     7 = CALIFORNIA RED FIR (RF)           ABIES MAGNIFICA
C     8 = PONDEROSA PINE (PP)               PINUS PONDEROSA
C     9 = LODGEPOLE PINE (LP)               PINUS CONTORTA
C    10 = WHITEBARK PINE (WB)               PINUS ALBICAULIS
C    11 = WESTERN WHITE PINE (WP)           PINUS MONTICOLA
C    12 = SINGLELEAF PINYON (PM)            PINUS MONOPHYLLA
C    13 = PACIFIC SILVER FIR (SF)           ABIES AMABILIS
C    14 = KNOBCONE PINE (KP)                PINUS ATTENUATA
C    15 = FOXTAIL PINE (FP)                 PINUS BALFOURIANA
C    16 = COULTER PINE (CP)                 PINUS COULTERI
C    17 = LIMBER PINE (LM)                  PINUS FLEXILIS
C    18 = MONTEREY PINE (MP)                PINUS RADIATA
C    19 = GRAY PINE (GP)                    PINUS SABINIANA
C         (OR CALIFORNIA FOOTHILL PINE)
C    20 = WASHOE PINE (WE)                  PINUS WASHOENSIS
C    21 = GREAT BASIN BRISTLECONE PINE (GB) PINUS LONGAEVA
C    22 = BIGCONE DOUGLAS-FIR (BD)          PSEUDOTSUGA MACROCARPA
C    23 = REDWOOD (RW)                      SEQUOIA SEMPERVIRENS
C    24 = MOUNTAIN HEMLOCK (MH)             TSUGA MERTENSIANA
C    25 = WESTERN JUNIPER (WJ)              JUNIPERUS OCIDENTALIS
C    26 = UTAH JUNIPER (UJ)                 JUNIPERUS OSTEOSPERMA
C    27 = CALIFORNIA JUNIPER (CJ)           JUNIPERUS CALIFORNICA
C    28 = CALIFORNIA LIVE OAK (LO)          QUERCUS AGRIFOLIA
C    29 = CANYON LIVE OAK (CY)              QUERCUS CHRYSOLEPSIS
C    30 = BLUE OAK (BL)                     QUERCUS DOUGLASII
C    31 = CALIFORNIA BLACK OAK (BO)         QUERQUS KELLOGGII
C    32 = VALLEY OAK (VO)                   QUERCUS LOBATA
C         (OR CALIFORNIA WHITE OAK)
C    33 = INTERIOR LIVE OAK (IO)            QUERCUS WISLIZENI
C    34 = TANOAK (TO)                       LITHOCARPUS DENSIFLORUS
C    35 = GIANT CHINKAPIN (GC)              CHRYSOLEPIS CHRYSOPHYLLA
C    36 = QUAKING ASPEN (AS)                POPULUS TREMULOIDES
C    37 = CALIFORNIA-LAUREL (CL)            UMBELLULARIA CALIFORNICA
C    38 = PACIFIC MADRONE (MA)              ARBUTUS MENZIESII
C    39 = PACIFIC DOGWOOD (DG)              CORNUS NUTTALLII
C    40 = BIGLEAF MAPLE (BM)                ACER MACROPHYLLUM
C    41 = CURLLEAF MOUNTAIN-MAHOGANY (MC)   CERCOCARPUS LEDIFOLIUS
C    42 = OTHER SOFTWOODS (OS)
C    43 = OTHER HARDWOODS (OH)
C
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
C     WS DECAY RATES BASED ON WS DESIGN DOCUMENT (BEUKEMA, 2000)
C     MODIFIED AT CA-REVIEW MEETING (FEBRUARY 2003) EACH VARIANT
C     MAY USE DIFFERENT RATES AND CITE DIFFERENT DATA SOURCES
C
C     DECAY RATES IN THE WS ARE SUBSEQUENTLY MODIFIED BY A
C     MULTIPLIER BASED ON DUNNING-CODE (SEE WS **FMCBA**)
C----------
      DKR(1,1) =  0.025   ! < 0.25"
      DKR(2,1) =  0.025   ! 0.25 - 1"
      DKR(3,1) =  0.025   ! 1 - 3"
      DKR(4,1) =  0.0125  ! 3 - 6"
      DKR(5,1) =  0.0125  ! 6 - 12"
      DKR(6,1) =  0.0125  ! 12 - 20"
      DKR(7,1) =  0.0125  ! 20 - 35"
      DKR(8,1) =  0.0125  ! 35 - 50"
      DKR(9,1) =  0.0125  !  > 50"
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
C     Duff production rates 'PRDUFF' are a proportion of the overall
C     decay rate: 'DKR'.
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
      DO I= 1,MAXSP
        PSOFT(I)  =  0.0
      ENDDO
C----------
C     VALUE OF HTR1 BASED ON 50% HEIGHT LOSS IN 20 YEARS; ALL SPECIES
C     VALUE OF HTR2 BASED ON HEIGHT LOSS CEASING AFTER 50% LOSS
C        NOTE: HTR2 CAN NO LONGER BE 0 (FEB 2002) SO WAS RESET TO DEFAULT 0.01
C              THE CEASING OF HEIGHT-LOSS AFTER 50% IS NOW GIVEN BY HTX (BELOW)
C----------
      HTR1   =  0.03406
      HTR2   =  0.01
C----------
C     ** SPECIES-LEVEL VARIABLE ASSIGNMENT **
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
C----------
      DO I= 1,MAXSP
C----------
C  FOR SPECIES USING EQNS FROM THE WS VARIANT:
C       HARD SNAGS NEVER BECOME SOFT
C       HEIGHT LOSS IS THE SAME FOR ALL SPP; SEE 'HTR1' (ABOVE)
C       HEIGHT LOSS CEASES FOR THE LAST 50% (USED TO BE SET BY HTR2, CHANGED FEB 2002)
C----------
        DECAYX(I) = 999.0
        HTX(I,1)  =   1.0
        HTX(I,3)  =   1.0
        HTX(I,2)  =   0.0
        HTX(I,4)  =   0.0
C
        SELECT CASE (I)
C----------
C         sugar pine
C----------
          CASE (1)
            V2T(I)     =  21.2
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         Douglas-fir
C----------
          CASE (2)
            V2T(I)     =  28.7 ! interior west DF in Wood Handbook
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.882
            DKRCLS(I)  =   3
            LSW(I)     = .TRUE.
C----------
C         white fir
C----------
          CASE (3)
            V2T(I)     =  23.1
            LEAFLF(I)  =   7.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.882
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         giant sequoia
C----------
          CASE (4)
            V2T(I)     =  21.2 ! use redwood
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 150.0 ! Nate Stevenson, 'a century or more'
            FALLX(I)   =   0.687
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.
C----------
C         incense cedar
C----------
          CASE (5)
            V2T(I)     =  21.8
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.687
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.
C----------
C         jeffrey pine
C----------
          CASE (6)
            V2T(I)     =  21.2
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         red fir
C----------
          CASE (7)
            V2T(I)     =  22.5
            LEAFLF(I)  =   7.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.882
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         ponderosa pine
C----------
          CASE (8)
            V2T(I)     =  23.7
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  10.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         lodgepole pine
C----------
          CASE (9)
            V2T(I)     =  23.7 ! lodgepole/ponderosa
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         whitebark pine
C----------
          CASE (10)
            V2T(I)     =  22.5 ! white
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         western white pine
C----------
          CASE (11)
            V2T(I)     =  22.5
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         singleleaf pinyon
C----------
          CASE (12)
            V2T(I)     =  31.8
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   2.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 150.0
            DECAYX(I)  =   0.9
            FALLX(I)   =   1.0
            DO J= 1,4
              HTX(I,J) =  0.0978
            ENDDO
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         pacific silver fir
C----------
          CASE (13)
            V2T(I)     =  24.9
            LEAFLF(I)  =   7.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.882
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         knobcone pine
C----------
          CASE (14)
            V2T(I)     =  23.7 ! lodgepole/ponderosa
            LEAFLF(I)  =   4.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         foxtail pine
C----------
          CASE (15)
            V2T(I)     =  23.7 ! lodgepole/ponderosa
            LEAFLF(I)  =   4.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         Coulter pine
C----------
          CASE (16)
            V2T(I)     =  23.7 ! lodgepole/ponderosa
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         limber pine
C----------
          CASE (17)
            V2T(I)     =  22.5 ! white
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         monterey pine
C----------
          CASE (18)
            V2T(I)     =  23.7
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         gray or California foothill pine
C----------
          CASE (19)
            V2T(I)     =  23.7 ! lodgepole/ponderosa
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         washoe pine
C----------
          CASE (20)
            V2T(I)     =  23.7 ! lodgepole/ponderosa
            LEAFLF(I)  =   4.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         Great Basin bristlecone pine
C----------
          CASE (21)
            TFALL(I,0) = 3.0   
            DKRCLS(I)  = 4  
            V2T(I)     = 23.7
            LEAFLF(I)  = 3.0
            TFALL(I,3) = 20.0
            ALLDWN(I)  = 999.0
            DECAYX(I)  = 0.9
            FALLX(I)   = 0.001
            DO J= 1,4
              HTX(I,J) = 0.0462
            ENDDO
            LSW(I)     = .TRUE.
C----------
C         bigcone Douglas-fir
C----------
          CASE (22)
            V2T(I)     =  28.7 ! interior west DF in Wood Handbook
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   0.882
            DKRCLS(I)  =   3
            LSW(I)     = .TRUE.
C----------
C         redwood
C----------
          CASE (23)
            V2T(I)     =  21.2 ! use redwood
            LEAFLF(I)  =   5.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 150.0 ! Nate Stevenson, 'a century or more'
            FALLX(I)   =   0.687
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.
C----------
C         mountain hemlock 
C----------
          CASE (24)
            V2T(I)     =  26.2
            LEAFLF(I)  =   4.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C----------
C         western juniper, Utah juniper, California juniper
C----------
          CASE (25:27)
            V2T(I)     =  34.9
            LEAFLF(I)  =   4.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  20.0
            ALLDWN(I)  = 150.0
            FALLX(I)   =   0.687
            DKRCLS(I)  =   2
            LSW(I)     = .TRUE.
C----------
C         California live oak, canyon live oak, blue oak, California black oak,
C         California white/valley oak, interior live oak, bigleaf maple,
C         other hardwoods
C----------
          CASE (28:33,40,43)
            SELECT CASE (I)
              CASE (28,29,33)
                V2T(I)  =  49.9  ! live oak
              CASE (30,32)
                V2T(I)  =  37.4  ! white oak
              CASE (31,43)
                V2T(I)  =  34.9  ! black oak
              CASE (40)
                V2T(I)  =  27.4
            END SELECT
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            DKRCLS(I)  =   2
            LSW(I)     = .FALSE.
C----------
C         tanoak, giant chinkapin, quaking aspen, California-laurel,
C         pacific madrone, pacific dogwood
C----------
          CASE (34:39)
            SELECT CASE (I)
            CASE(34,35,37,38)
              V2T(I)   =  36.2
            CASE(36)
              V2T(I)   =  21.8
            CASE(39)
              V2T(I)   =  27.4 ! bigleaf maple
            END SELECT
            LEAFLF(I)  =   1.0
            TFALL(I,0) =   1.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  =  50.0
            FALLX(I)   =   1.545
            SELECT CASE (I)
            CASE(34:36,39)
              DKRCLS(I)  =   4
            CASE(37)
              DKRCLS(I)  =   2
            CASE(38)
              DKRCLS(I)  =   3
            END SELECT
            LSW(I)     = .FALSE.
C----------
C         curlleaf mountain-mahogany
C----------
          CASE (41)
             V2T(I)     = 21.8
             LEAFLF(I)  = 1.0
             TFALL(I,0) = 1.0 
             TFALL(I,3) = 15.0 
             ALLDWN(I)  = 90.0
             FALLX(I)   = 1.0 
             DKRCLS(I)  = 4
             LSW(I)     = .FALSE.
             DO J= 1,4
              HTX(I,J)  = 1.0
             ENDDO
             DECAYX(I)  = 1.0
C----------
C         other softwoods: use LP
C----------
          CASE (42)
            V2T(I)     =  23.7
            LEAFLF(I)  =   3.0
            TFALL(I,0) =   3.0
            TFALL(I,3) =  15.0
            ALLDWN(I)  = 100.0
            FALLX(I)   =   1.235
            DKRCLS(I)  =   4
            LSW(I)     = .TRUE.
C
        END SELECT
C----------
C       TIME-TO-FALL FOR OTHER CROWN CATEGORIES
C----------
        TFALL(I,1) = 10.0
        TFALL(I,2) = 15.0
        TFALL(I,4) = TFALL(I,3)
        TFALL(I,5) = TFALL(I,3)
C----------
C       DEAD LEAF FALL CANNOT BE > LIVE
C----------
        TFALL(I,0) = MIN(TFALL(I,0), LEAFLF(I))
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
C     parameters for post-burn snag fall rates:
C----------
      PBSCOR =  0.0
      PBSOFT =  1.0
      PBSMAL =  0.9
      PBSIZE = 12.0
      PBTIME =  7.0
C----------
C     PARAMETERS FOR FUEL MODEL SELECTION
C
C     THE LODGEPOLE PINE COVER TYPE METAGROUP **FMCFMD**
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
C----------
C     CRITICAL % CHANGE IN %CC REQUIRED TO TRIGGER SHRUB MODEL DELAY
C----------
      CCCRIT = 10.0
C
      RETURN
      END
