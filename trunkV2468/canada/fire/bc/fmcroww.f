      SUBROUTINE FMCROWW (SPI,D,H,ITRNC,IC,HP,SG,XV)
      IMPLICIT NONE
C
C  $Id$
C
C----------
C  **FMCROWW  FIRE-BC
C----------
C     CALLED FROM: FMCROW

*  Purpose:
*     This subroutine calculates CROWNW(tree,size), the weight of
*     various sizes of crown material that is associated with each tree
*     record in the current stand.  These weights depend on tree
*     species, diameter, height, and crown ratio according to the
*     relationships described in Brown & Johnston, 1976, 'Debris
*     Prediction System', Fuel Science RWU 2104, which is itself
*     based primarily on Res Paper INT-197.
*
*     NOTE:  whitebark pine from Brown
*     NOTE:  oak from Chojnacky (1992)
*     NOTE:  pinyon and juniper from Grier (1992)
*     NOTE:  bigleaf maple from Snell & Little (1983)
*     NOTE:  The allocation between crown size class 4 and 5 is
*            somewhat arbitrary, with class 5 currently not receiving any.

C     SPECIES MAPPING (BASED ON CR AND ELABORATED FROM THERE)

C     1 = SUBALPINE FIR, corkbark, subalpine larch
C     2 = ASPEN
C     3 = DOUGLAS-FIR
C     4 = GRAND FIR, white, Cal red, shasta red, red, Pacific silver, noble
C     5 = BIGLEAF MAPLE, mountain
C     6 = WESTERN HEMLOCK
C     7 = WESTERN REDCEDAR, pacific yew
C     8 = WESTERN LARCH, Alaska cedar
C     9 = BRISTLECONE PINE
C    10 = MADRONE, white alder
C    11 = LODGEPOLE PINE, limber, knobcone
C    12 = PINYON PINE
C    13 = PONDEROSA PINE, Monterey
C    14 = WHITEBARK PINE
C    15 = WESTERN WHITE PINE, southwestern white, Jeffrey, sugar
C    16 = ROCKY MTN JUNIPER, western
C    17 = TANOAK, chinkapin, coast live oak, canyon live oak, Engelmann oak
C                 interior live oak, nutmeg, laurel
C    18 = ENGELMANN SPRUCE, blue, white, brewer, Sitka
C    19 = GIANT SEQUOIA, coast redwood
C    20 = INCENSE CEDAR
C    21 = CALIFORNIA BLACK OAK - based on tanoak
C    22 = GAMBEL OAK
C    23 = RED ALDER
C    24 = MOUNTAIN HEMLOCK
C    25 = PONDEROSA PINE - BLACK HILLS
*------------------------------------------------------------------------
*
*  Local variable definitions:
*     C:        length of live Crown
*     D;        Dbh
*     DBR1-4:   DRY WT (KG) OF BRANCH COMPONENTS
*     DEADWT:   WeighT of DEAD crown of the focal tree
*     DOMPCT:   Percentile at which trees are considered dominant or co-dom.
*     DP1-3:    Dead crown Proportions, each being the sum of the pro-
*               portions of all smaller material.
*     DBHCUT:   Breakpoint for sm. and lg. tree crown biomass estimates
*     DCTLOW:   point where interpolation between sm. and lg. tree
*               estimates starts
*     DCTHGH:   point where interpolation between sm. and lg. tree
*               estimates stops
*     H:        Height
*     HPCT:     Height PerCenTile ranking of each tree list element
*     HPOINT:   Height POINTer: specifies which tree list element has
*               the tallest trees, next tallest, and so on.
*     ITRNC     truncation height (aspen only)
*     LIVEWT:   WeighT of LIVE crown of the focal tree
*     P1-4:     live crown Proportions, each being the sum of the
*               proportions of all smaller material.
*     R:        crown Ratio
*     SP:       SPecies
*     SMWGT:    WeiGT give to SMall tree estimate during interpolation
*     TOTWT:    TOTal crown WeighT of the focal tree
*     XVAL,YVAL:used to get SMWGT with ALGSLP
*
*  Common block variables and parameters:
*
***********************************************************************

C.... Parameter statements.
C.... Parameter include files.
C.... Variable declarations.

      INTEGER SPI,IC,ITRNC
      REAL    D,H,HP,SG,XV(0:5)

      INTEGER J

      LOGICAL LDUM1,LDUM2
      REAL    C, DEADWT, DOMPCT, DP1, DP2, DP3
      REAL    LIVEWT, TEMP, P1, P2, P3, P4, R, TOTWT, DRC, X, X0
      REAL    V, DBHCUT, LDM, D2H
      REAL    VM,XDUM1,XDUM2,XDUM3
      REAL    DBR1, DBR2, DBR3, DBR4, DBR5, DFOL
      REAL    DCTLOW, DCTHGH, SMWGT, XVAL(3), YVAL(3), ALGSLP

C.... Variables for new Black Hills PP equations
      REAL    TFOL, T1HRL, T1HRD, TCR

      DATA DOMPCT /60.0/

      DP1 = 0.0
      DP2 = 0.0
      DP3 = 1.0

      P1  = 0.0
      P2  = 0.0
      P3  = 0.0
      P4  = 0.0

      DEADWT = 0.0
      LIVEWT = 0.0
      TOTWT  = 0.0

      TCR = (REAL(IC)/100)
      C = (REAL(IC) / 100.0) * H
      IF (H .GT. 0.0) R = (C / H) * 10.0

C     INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
C     OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.

      DO J = 0,5
        XV(J) = 0.0
      ENDDO

      IF ((D .EQ. 0.0) .OR. (H .EQ. 0.0) .OR. (C .EQ. 0.0)) GOTO 999

C***********************1.  CANOPIES OF SMALL TREES*********************

C  THE SMALL/LARGE BREAPOINT IS NORMALLY 1 INCH DBH BUT SOME SPECIES
C  REQUIRE DIFFERENT BREAKPOINTS.  BETWEEN DCTLOW AND DCTHGH
C  WEIGHTING BETWEEN THE SMALL/LARGE EQUATIONS IS DONE.

        SELECT CASE (SPI)
          CASE  (9,12,16) ! bristlecone pine, pinyon pine, R.M juniper
            DBHCUT = 2.0
            DCTLOW = 1.0
            DCTHGH = 3.0
          CASE  (22)      ! Gambel oak
            DBHCUT = 4.0
            DCTLOW = 3.0
            DCTHGH = 5.0
          CASE  (11)      ! lodgepole pine
            DBHCUT = 1.0
            DCTLOW = 0.5
            DCTHGH = 3.0
          CASE DEFAULT    ! ALL OTHERS
            DBHCUT = 1.0
            DCTLOW = 0.5
            DCTHGH = 2.0
        END SELECT
C----------
C  SET WEIGHTS BETWEEN LG. AND SM. MODELS FOR TOTAL CROWN BIOMASS
C----------
          XVAL(1) = DCTLOW
          XVAL(2) = DBHCUT
          XVAL(3) = DCTHGH
          YVAL(1) = 1.0
          YVAL(2) = 0.5
          YVAL(3) = 0.0
          SMWGT   = ALGSLP(D,XVAL,YVAL,3)

        IF (D .GT. DCTHGH) GOTO 299

C       Calculate the total crown weight depending on tree species:
C       (Use formulas from Brown 1978, page 14)

        SELECT CASE (SPI)

C         subalpine fir, corkbark fir
          CASE (1)
            TOTWT = EXP(-3.335 + 2.303*LOG(H))

C         aspen - no height-based eqn available; set D=0.75"
          CASE (2)
            TOTWT = EXP(0.1942 + 2.8426*LOG(0.75))

C         Douglas-fir
          CASE (3)
            TOTWT = EXP(-4.212 + 2.7168*LOG(H))

C         grand fir
          CASE (4)
            TOTWT = 0.4284 * H

C         bigleaf maple - predict weight of 1" dbh tree (.977)
C         and scale back that based on the height of the 1"
C         tree (7.728)
          CASE (5)
            TOTWT = 0.977 * H / 7.728

C         western hemlock, mountain hemlock
          CASE (6,24)
            TOTWT = EXP(-5.126 + 2.563*LOG(H))

C         western redcedar, giant sequoia, incense cedar
          CASE (7,19,20)
            TOTWT = 0.04833 * H * H

C         western larch
          CASE (8)
            TOTWT = 0.1128*H + 0.00813 * H * H

c         madrone -  weight predicted from height (8.2176 ft) of
c         1" dbh tree and scaled from that based on the weight
c         (0.54599 lbs) of the 1" tree
          CASE (10)
            TOTWT = 0.54599 * H / 8.2176

C         lodgepole pine
          CASE (11)
            TOTWT = 0.03111 * H * H

C         ponderosa pine
          CASE (13,25)
            TOTWT = 0.3451 * H

C         whitebark pine
          CASE (14)
            TOTWT = 0.070 + 0.02446 * H * H

C         western white pine
          CASE (15)
            TOTWT = 0.3292 * H

c         tanoak - predicted from height (5.1213 ft) of 1" dbh tree
c         (WS-FFE) and scaled from that based on the computed weight
c         (0.81135 lbs) of the 1" tree
          CASE (17)
            TOTWT = 0.81135 * H / 5.1213

C         Engelmann spruce
          CASE (18)
            TOTWT = EXP(-3.932 + 2.571*LOG(H))

c         california black oak
c         hardwood predicted from height (5.3539 ft, WS-FFE) of 1" dbh tree
c         and scaled from that based on the weight (0.81135 lbs) of
c         the 1" tree (derived from tanoak large tree)
          CASE (21)
            TOTWT = 0.81135 * H / 5.3539

c         red alder -  predict weight of 1" dbh tree (.277)
c         and scale back that based on the height of
c         the 1" tree (7.728)
          CASE (23)
            TOTWT = 0.277 * H / 7.728

        END SELECT

        SELECT CASE (SPI)

C         subalpine fir, grand fir;
C         western hemlock, western redcedar
C         Engelmann spruce, mountain hemlock
          CASE (1,4,6,7,18,24)
            XV(0) = 0.62 * TOTWT
            XV(1) = 0.26 * TOTWT
            XV(2) = 0.12 * TOTWT

C         quaking aspen - assume half in foliage; quarter in others
          CASE (2)
            XV(0) = 0.50 * TOTWT
            XV(1) = 0.25 * TOTWT
            XV(2) = 0.25 * TOTWT

C         Douglas-fir, lodgepole pine, whitebark pine, white pine
          CASE (3,11,14,15)
            XV(0) = 0.52 * TOTWT
            XV(1) = 0.27 * TOTWT
            XV(2) = 0.21 * TOTWT

c         bigleaf maple - use proportions at 1" dbh
          CASE (5)
            XV(0) = 0.20 * TOTWT
            XV(1) = 0.09 * TOTWT
            XV(2) = 0.71 * TOTWT

C         western larch
          CASE (8)
            XV(0) = 0.40 * TOTWT
            XV(1) = 0.42 * TOTWT
            XV(2) = 0.18 * TOTWT

C         bristlecone pine
          CASE (9)
            X = (H / 16.3) ! 16.3 is the ht of a 2in dbh bc (9.8 in drc)
            XV(0) = 8.8839 * X
            XV(1) = 3.4332 * X
            XV(2) = 4.4363 * X
            XV(3) = 0.5615 * X

C         madrone -  predicted from partitioning of 1" dbh tree using
C         large tree equation
          CASE (10)
            XV(0) = 0.43 * TOTWT
            XV(1) = 0.35 * TOTWT
            XV(2) = 0.22 * TOTWT

C         ponderosa pine
          CASE (13,25)
            XV(0) = 0.57 * TOTWT
            XV(1) = 0.14 * TOTWT
            XV(2) = 0.29 * TOTWT

c         pinyon, bristlecone, juniper, oak based on plugging in a
c         2 or 4" drc tree (CR-FFE) and computing biomass components
c         (as per large trees), the scaling result by the height of
c         a 2 or 4" drc tree. these values are already in lbs and don't
c         need further conversion.

c         pinyon pine
          CASE (12)
            X = (H / 16.3) ! 16.3 is the height of a 2in drc pinyon
            XV(0) = .73866 * X
            XV(1) = .16971 * X
            XV(2) = .15328 * X
            XV(3) = .00474 * X

C         juniper
          CASE (16)
            X = (H / 16.3) ! 16.3 is the height of a 2in drc juniper
            XV(0) = .07054 * X
            XV(1) = .03474 * X
            XV(2) = .25751 * X
            XV(3) = .08457 * X

c         tanoak, California black oak use proportions at 1" dbh
          CASE (17,21)
            XV(0) = 0.38 * TOTWT
            XV(1) = 0.32 * TOTWT
            XV(2) = 0.30 * TOTWT

C         giant sequoia, incense cedar: use proportions at 1" dbh
          CASE (19,20)
            XV(0) = 0.52 * TOTWT
            XV(1) = 0.28 * TOTWT
            XV(2) = 0.20 * TOTWT

C         Gambel oak
          CASE (22)
            X = (H / 28) !28 is the height of a 4in drc oak
            XV(0) =  3.2119 * X
            XV(1) =  1.606  * X
            XV(2) = 15.8115 * X
            XV(3) =  7.7877 * X

C         red alder
          CASE (23)
            XV(0) = 0.32 * TOTWT
            XV(1) = 0.39 * TOTWT
            XV(2) = 0.29 * TOTWT

        END SELECT

        DO J = 0,5
          XV(J) = MAX(0.0, XV(J) * SMWGT)
        ENDDO

        IF (D .LT. DCTLOW) GOTO 999

C***********************2.  CANOPIES OF LARGE TREES*********************

C       Limit D as instructed in Brown & Johnson:

  299   IF (D .GT. 40.0) D = 40.0

C       Using relationships which depend on species, calculate the live
C       and dead crown weights.  Scale these back for intermediate or
C       suppressed trees (trees below DOMPCT percentile in height; only done
C       for some species).  Also calculate the necessary partitioning
C       coefficients.  Species found in the NI are taken from Brown (1978,
C       page 10-11); other species are as noted << MAKE SURE TO REFERENCE >>

        SELECT CASE (SPI)

C         subalpine fir
          CASE (1)

            LIVEWT = 0.1862*D*D*R + 1.066
            IF (D .LE. 16.0) THEN
              DEADWT = EXP(4.0365*LOG(D) - 6.5431)
            ELSE
              DEADWT = 0.31 * LIVEWT
            ENDIF
            P1 = 0.5966 * EXP(-0.04247*D)
            P2 = 0.8643 * EXP(-0.03733*D)
            IF (D .LE. 2.9) THEN
              P3 = 1.0
            ELSE
              P3 = 1.0221 - 0.01083*D
            ENDIF
            P4 = 1.0
            IF (D .LT. 1.5) THEN
              DP1 = 1.0
            ELSE
              DP1 = 1.2105 * D**(-0.565)
            ENDIF
            DP2 = 1.0

C         aspen
          CASE (2)

	      D2H = D*D*H
            CALL CFVOL (SPI,D,H,D2H,V,VM,XDUM3,.FALSE.,.FALSE.,XDUM2,
     1          ITRNC,LDUM1)


C           INPUT   D (IN) CONVERTED TO (M)
C           INPUT   V (FT**3)CONVERTED TO (M**3);
C           OUTPUT  DBR1, DBR2, ETC ALL METRIC (KG)

C           DBR1: BRANCH BIOMIASS  >10CM (    >3.94") - LARGE
C           DBR2: BRANCH BIOMIASS 6-10CM (2.36-3.94")
C           DBR3: BRANCH BIOMIASS 2- 6CM (0.78-2.36") - SMALL
C           DFOL: FOLIAGE

            D = D * 2.54 / 100.
            V = V * 0.028317

            DBR1 = -3.3 - 56.9*V + 581.8*D*D
            DBR2 = -1.8 - 42.2*V + 634.3*D*D
            DBR3 = -1.2 - 29.0*V + 271.1*D*D
            DFOL =  0.5 +  0.2*V +  89.2*D*D

C         Douglas-fir
          CASE (3)

            IF (HP .LT. DOMPCT) THEN
              LIVEWT = EXP(0.1508 + 1.8621*LOG(D))
              DEADWT = EXP(-1.928 + 2.353*LOG(D))
            ELSE
              IF (D .LT. 17.0) THEN
                LIVEWT = EXP(1.1368 + 1.5819*LOG(D))
              ELSE
                LIVEWT = 1.0237*D*D - 20.74
              ENDIF
              DEADWT = 0.01094*D*D*D
            ENDIF
            IF (D .GT. 36.0) THEN
              P1 = 0.227
              P2 = 0.315
              P3 = 0.465
            ELSE
              P1 = 0.484  * EXP(-0.02102*D)
              P2 = 0.7289 * EXP(-0.02332*D)
              IF (D .LE. 2.9) THEN
                P3 = 1.0
              ELSE
                P3 = 1.0342 - 0.01584*D
              ENDIF
            ENDIF
            IF (D .LE. 14.0) THEN
              P4 = 1.0
            ELSE
              P4 = 1.0221 - 0.001821*D
            ENDIF
            IF (D .LT. 1.8) THEN
              DP1 = 1.0
            ELSE
              DP1 = 0.08355 + (1.5893/D)
            ENDIF
            IF (D .LT. 9.0) THEN
              DP2 = 1.0
            ELSE
              DP2 = 1.5673 * EXP(-0.05232*D)
            ENDIF

C         grand fir
          CASE (4)

            LIVEWT = EXP(1.3094 + 1.6076*LOG(D))
            IF (D .LE. 18.0) THEN
              DEADWT = EXP(3.5638*LOG(D) - 5.3154)
            ELSE
              DEADWT = 0.38*LIVEWT
            ENDIF
            IF (D .GT. 36.0) THEN
              P1 = 0.286
              P2 = 0.378
              P3 = 0.488
            ELSE
              P1 = 1.0 / (1.5916 + 0.05294*D)
              P2 = 1.0 / (1.1495 + 0.04165*D)
              IF (D .LE. 2.9) THEN
                P3 = 1.0
              ELSE
                P3 = 1.0267 - 0.01495*D
              ENDIF
            ENDIF
            P4 = 1.0
            IF (D .LT. 3.0) THEN
              DP1 = 1.0
            ELSE
               IF (D .GT. 27.0) THEN
                 DP1 = 0.01
               ELSE
                 DP1 = 1.4336 * EXP(-0.1816*D)
               ENDIF
            ENDIF
            IF (D .LT. 8.0) THEN
              DP2 = 1.0
            ELSE
              DP2 = 1.2623 * EXP(-0.0347*D)
            ENDIF

C         bigleaf maple (Snell & Little 1983)
          CASE (5)
           LIVEWT = EXP(-0.0582 + 2.1505 * LOG(D))
           DEADWT = EXP(-3.3678 + 2.5033 * LOG(D))

           P1 = 1.0/(4.6762 + 0.1091 * D**2.0390)
           P2 = 1.0/(3.3212 + 0.0777 * D**2.0496)
           IF (D .LT. 1.9) THEN
             P3 = 1.0
           ELSE
             P3 = 1.0/(0.9341 + 0.0158 * D**2.1627)
           ENDIF
           IF (D .LT. 4.8) THEN
             P4 = 1.0
           ELSE
             P4 = 1.0/(0.8625 + 0.0093 * D**1.7070)
           ENDIF

           DP1 = EXP(-1.0444 - 0.1892 * D)
           IF (D .LT. 1.0) THEN
             DP2 = 1.0
           ELSE
             DP2 = EXP(0.0553 - 0.0660 * D)
           ENDIF
           IF (D .LT. 2.5) THEN
             DP3 = 1.0
           ELSE
             DP3 = EXP(0.0083 - 0.0033 * D)
           ENDIF

C         western hemlock
          CASE (6)
            LIVEWT = 0.3729*D*D + 0.284*D*C - 0.005525*D*D*C - 4.501
            DEADWT = EXP(3.3664*LOG(D) - 6.6768)

            IF (D .LE. 40.0) THEN
              P1 = 0.5474 * EXP(-0.03697*D)
              P2 = 0.8352* EXP(-0.03802*D)
              IF (D .LE. 2.9) THEN
                P3 = 1.0
              ELSE
                P3 = 1.0781 * EXP(-0.02735*D)
              ENDIF
            ELSE
              P1 = 0.125
              P2 = 0.183
              P3 = 0.361
            ENDIF
            P4 = 1.0
            IF (D .LT. 4.0) THEN
              DP1 = 1.0
            ELSE
              IF (D .GT. 28.0) THEN
                DP1 = 0.005
              ELSE
                DP1 = 1.9608 * EXP(-0.2064*D)
              ENDIF
            ENDIF
            IF (D .LT. 12.0) THEN
              DP2 = 1.0
            ELSE
              DP2 = 1.0 / (0.2772 + 0.06141*D)
            END IF

C         western redcedar, incense cedar
          CASE (7,20)

            LIVEWT = EXP(1.7273*LOG(D*R) - 2.8086)
            DEADWT = 0.01063*D*D*D
            P1 = 0.6174 * EXP(-0.02326*D)
            P2 = 0.7562 * EXP(-0.02411*D)
            IF (D .LE. 2.9) THEN
              P3 = 1.0
            ELSE
              P3 = 1.0602 * EXP(-0.02226*D)
            ENDIF
            P4 = 1.0
            IF (D .LT. 1.5) THEN
              DP1 = 1.0
            ELSE
              DP1 = -0.01578 + (1.4673/D)
            ENDIF
            IF (D .LT. 8.0) THEN
               DP2 = 1.0
            ELSE
               DP2 = 1.4534 * EXP(-0.05395*D)
            ENDIF

C         western larch
          CASE (8)

            LIVEWT = EXP(0.4373 + 1.6786*LOG(D))
            IF (HP .LT. DOMPCT) THEN
              IF (D .LE. 7.5) THEN
                LIVEWT = 0.5 * LIVEWT
              ELSE
                LIVEWT = 0.6 * LIVEWT
              ENDIF
            ENDIF
            P1 = 0.3468 * EXP(-0.04343*D)
            P2 = 0.745  * EXP(-0.03622*D)
            IF (D .LE. 2.9) THEN
              P3 = 1.0
            ELSE
              P3 = 1.05448 * EXP(-0.0213*D)
            ENDIF
            IF (D .LE. 11) THEN
              P4 = 1.0
            ELSE
              P4 = 0.9223 + 0.7197/D
            ENDIF
            DEADWT = 0.0
            DP1 = 0.0
            DP2 = 0.0

C         bristlecone pine, pinyon pine (single stem)
          CASE (9,12)

C         COEFFICIENTS TO DERIVE DRC FROM DBH ARE
C         FROM CHOJNACKY (1999) WJAF 14:14-16, TABLE 2.
C         (REARRANGED, SINCE CHOJNACKY FITS DBH FROM DRC). REGRESSIONS
C         ARE FROM GRIER ET AL. 1992 FOR. ECOL. MGMT. 50:331, TABLE 2.

C         ONLY BRISTLECONE NEEDS TO HAVE ITS DBH CONVERTED TO DRC,
C         SINCE FOR PINYON, DRC IS WHAT USERS ENTER

            DRC = D
            IF (SPI .EQ. 9) DRC = 2.54 * (D + 1.9410) / 1.0222

C           INPUT   DRC (CM) - DIAMETER AT ROOT COLLAR
C           OUTPUT  DBR1, DBR2, ETC ALL METRIC (KG)

C           DFOL: FOLIAGE
C           DBR1: BRANCH BIOMIASS <1/4"  - SMALL
C           DBR2: BRANCH BIOMIASS 1/4-1"
C           DBR3: BRANCH BIOMIASS 1-3"   - LARGE
C           DBR4: 0.0 - IN BASE FVS EQNS
C           DBR5: 0.0 - IN BASE FVS EQNS

C           GRIER ET AL <1 CATEGORY SPLIT TO GIVE TWICE AS MUCH
C           IN 1/4-1" (67%), COMPARED TO <1/4" CATEGORY (33%)
C           1-3" CATEGORY IS SPLIT SO 25% GOES INTO DBR3; REMAINDER
C           ASSUMED TO BE COUNTED BY FVS BASE EQNS (>1.5" BRANCH).
C           STEMWOOD IS ASSUMED TO BE INCLUDED IN THE BASE FVS
C           VOLUME EQUATIONS, EVEN FOR <1.5" UTILIZATION. SO NO
C           STEMWOOD IS INCLUDED.

C           TRIAL CALCULATION COMPARING WITH GRIER STAND SUMMARIES
C           GIVES ESTIMATED TOTAL BIOMASS 1.83 TIMES OBSERVED
C           ** THIS IS CAUSE FOR CONCERN **

            DFOL = 10**(-0.946 + 1.565 * LOG10(DRC))
            DBR1 = 10**(-1.613 + 2.088 * LOG10(DRC)) * 0.33
            DBR2 = 10**(-1.613 + 2.088 * LOG10(DRC)) * 0.67
            DBR3 = 10**(-2.971 + 3.007 * LOG10(DRC)) * 0.25
            DBR4 = 0.0
            DBR5 = 0.0

C           OLD TWIGS ADDED INTO <1/4"

            DBR1 = DBR1 + 10**(-1.873 + 1.675 * LOG10(DRC))

C           DEAD BRANCHES DIVIDED ACCORDING TO DISTRIBUTION
C           OF ALL LIVING BRANCHES 0.25-1.5"; IN STRANGE CASE
C           OF NO BRANCH BIOMASS, PUT IN SMALL

            X =  DBR2 + DBR3
            TEMP = 10**(-5.400 + 4.470 * LOG10(DRC))
            IF (X .GT. 1.0E-6) THEN
              DBR2 = DBR2 + (TEMP * DBR2/X)
              DBR3 = DBR3 + (TEMP * DBR3/X)
            ELSE
              DBR2 = TEMP
            ENDIF

C         madrone (Snell & Little 1983. lbs ovendry weight)
          CASE (10)

            LIVEWT = EXP(-0.7881 + 2.4839 * LOG(D))
            DEADWT = EXP(-2.3938 + 2.2936 * LOG(D))

            P1 = 1.0/(1.6013 + 0.3591 * D**1.3090)
            P2 = 1.0/(1.0357 + 0.2263 * D**1.3567)
            P3 = 1.0/(1.0281 + 0.0084 * D**2.1850)
            P4 = 1.0
            IF (D .GE. 4.2) THEN
              P4 = 1.0/(0.8778 + 0.0115 * D**1.6394)
            ENDIF

            DP1 = -0.0632 + (0.7214 * D**0.25) - (0.4655 * LOG(D))

            DP2 = 1.0
            IF (D .GE. 2.5) THEN
              DP2 = 1.2671 - (0.1686 * D**0.5)
            ENDIF
            DP3 = 1.0
            IF (D .GE. 7.6) THEN
              DP3 = EXP(0.0281 - (0.0004714 * D * D))
            ENDIF

C         lodgepole pine
          CASE (11)

            LIVEWT = 0.02238*D*D*D + 0.1233*D*D*R - 2.0
            IF (D .LE. 10.0) THEN
              DEADWT = (0.026*D - 0.025) * LIVEWT
            ELSE
              DEADWT = 0.235 * LIVEWT
            ENDIF
            IF (HP .LT. DOMPCT) THEN
              IF (D .LE. 7.5) THEN
                LIVEWT = 0.5 * LIVEWT
                DEADWT = 0.5 * DEADWT
              ELSE
                LIVEWT = 0.6 * LIVEWT
                DEADWT = 0.6 * DEADWT
              ENDIF
            ENDIF
            P1 = 0.4933 - 0.01167*D
            P2 = 0.7767 - 0.01464*D
            IF (D .LE. 3.9) THEN
              P3 = 1.0
            ELSE
              P3 = 1.0494 - 0.01402*D
            ENDIF
            P4 = 1.0
            IF (D .GT. 20.0) THEN
              DP1 = 0.139
              DP2 = 0.226
            ELSE
              IF (D .LT. 1.5) THEN
                DP1 = 1.0
              ELSE
                DP1 = 1.3527 * D**(-0.7585)
              ENDIF
              IF (D .LT. 9.0) THEN
                DP2 = 1.0
              ELSE
                DP2 = 2.7979 * EXP(-0.1257*D)
              ENDIF
            ENDIF

C         ponderosa pine
          CASE (13)

            IF (HP .LT. DOMPCT) THEN
              LIVEWT = EXP(-0.7572 + 2.216*LOG(D))
              DEADWT = EXP(-2.5176 + 2.51*LOG(D))
              P1 = 0.6501 * EXP(-0.1544*D)
              P2 = 0.8435 * EXP(-0.1665*D)
              P3 = 1.0865 * EXP(-0.0833*D)
              P4 = 1.0
            ELSE
              LIVEWT = EXP(2.2812*LOG(D) + 1.5098*LOG(R) - 3.0957)
              DEADWT = EXP(2.8376*LOG(D) - 3.7398)
              P1 = 0.5578 * EXP(-0.04754*D)
              IF (D .GE. 31.0) THEN
                P2 = P1 + 0.01
              ELSE
                P2 = 0.6254 * EXP(-0.05114*D)
              ENDIF
              IF (D .LE. 1.0) THEN
                P3 = 1.0
                P4 = 1.0
              ELSE
                P3 = 0.985 * EXP(-0.03102*D)
                IF (D .LE. 6.5) THEN
                  P4 = 1.0
                ELSE
                  P4 = 1.083 - 0.01306*D
                ENDIF
              ENDIF
            ENDIF
            IF (D .GT. 30.0) THEN
              DP1 = 0.004
              DP2 = 0.06
            ELSE
              DP1 = (1.4114 / D) - 0.04345
              DP2 = 1.0621 - 0.03342*D
            ENDIF

C         whitebark pine
          CASE (14)

            LIVEWT = 0.06056*D*D*D + 0.05477*D*D*R + 0.646
            DEADWT = 0.001713*D*D*C + 0.33

            IF (D .GT. 20.0) THEN
              P1 = 0.242
            ELSE
              P1 = 0.5120 * EXP(-0.03737*D)
            ENDIF

            IF (D .GT. 20.0) THEN
              P2 = 0.268
            ELSE
              P2 = 0.8644 * EXP(-0.05854*D)
            ENDIF

            IF (D .LE. 3.9) THEN
              P3 = 1.0
            ELSEIF (D .GT. 20.0) THEN
              P3 = 0.670
            ELSE
              P3 = 1.0733 * EXP(-0.02376*D)
            ENDIF

            P4 = 1.0

            IF (D .LT. 1.4) THEN
              DP1 = 1.0
            ELSE
              DP1 = 0.268 + (1.1733/D)
            ENDIF
            DP2 = 1.0

C         white pine
          CASE (15)

            LIVEWT = 0.0947 * D * D * R
            DEADWT = EXP(2.6076*LOG(D) - 4.397)
            P1 = 0.5497 * EXP(-0.0345*D)
            P2 = 0.9138 - 0.0978*SQRT(D)
            IF (D .LE. 3.9) THEN
              P3 = 1.0
            ELSE
              P3 = 1.0564 * EXP(-0.0181*D)
            ENDIF
            P4 = 1.0
            DP1 = 1.0077 * D**(-0.4556)
            IF (D .LT. 7.0) THEN
              DP2 = 1.0
            ELSE
              DP2 = 1.0291 - 0.004964*D
            ENDIF

C         western juniper (single stem)
          CASE (16)

C           METHODOLOGY AS PER PINYON PINE, EXCEPT <1" CATEGORY
C           ALL PLACED IN .25-1" CATEGORY; "FOLIAGE TWIGS" SPLIT
C           INTO FOLIAGE (67%), <.25" CATEGORIES. (33%). 1-3"
C           CATEGORY IS SPLIT SO 25% GOES INTO DBR3; REMAINDER
C           ASSUMED TO BE COUNTED BY FVS BASE EQNS (>1.5" BRANCH).

C           TRIAL CALCULATION COMPARING WITH GRIER STAND SUMMARIES
C           GIVES ESTIMATED TOTAL BIOMASS 1.14 TIMES OBSERVED

C           LINE BELOW IS NOT NEEDED SINCE USERS ENTER DRC--SAR 4/03
C           DRC = 2.54 * (D + 1.1841) / 0.9823
            DRC = D

            DFOL = 10**(-1.737 + 1.382 * LOG10(DRC)) * 0.67
            DBR1 = 10**(-1.737 + 1.382 * LOG10(DRC)) * 0.33
            DBR2 = 10**(-1.476 + 1.787 * LOG10(DRC))
            DBR3 = 10**(-1.356 + 1.782 * LOG10(DRC)) * 0.25
            DBR4 = 0.0
            DBR5 = 0.0

C           DEAD BRANCHES DIVIDED ACCORDING TO DISTRIBUTION
C           OF ALL LIVING BRANCHES 0.25-1.5"; IN STRANGE CASE
C           OF NO BRANCH BIOMASS, PUT IN SMALL

            X = DBR2 + DBR3
            TEMP = 10**(-3.543 + 2.774 * LOG10(DRC))
            IF (X .GT. 1.0E-6) THEN
              DBR2 = DBR2 + (TEMP * DBR2/X)
              DBR3 = DBR3 + (TEMP * DBR3/X)
            ELSE
              DBR2 = TEMP
            ENDIF

c        tanoak, black oak
c        snell & little 1983, and snell 1979 (to justify
c        lumping black oak with tanoak; equations use
c        coefficients from *tanoak* and predict ovendry
c        weight in pounds
         CASE (17,21)

           LIVEWT = EXP(-0.3169 + 2.2774 * LOG(D))
           DEADWT = EXP(-2.4895 + 2.0374 * LOG(D))

           P1 = 1.0/(1.7936 + 0.5952 * D**0.7239)
           P2 = 1.0/(0.9940 + 0.4229 * D**0.6520)
           IF (D .LT. 1.5) THEN
             P3 = 1.0
           ELSE
             P3 = 1.0/(0.8759 + 0.0927 * D**0.7843)
           ENDIF
           P4 = 1.0

           DP1 = -0.1424 + (0.7684 * D**0.25) - (0.4730 * LOG(D))
           IF (D .LT. 4.1) THEN
             DP2 = 1.0
           ELSE
             DP2 = EXP(-2.810 + (4.379 * D**0.25) - (1.691 * D**0.5))
           ENDIF
           IF (D .LT. 7.9) THEN
             DP3 = 1.0
           ELSE
             DP3 = 1.027 - (0.003439 * D)
           ENDIF

C         Engelmann spruce
          CASE (18)

            LIVEWT = EXP(1.0404 + 1.7096*LOG(D))
            DEADWT = EXP(3.6172*LOG(D) - 6.686)
            IF (D .LT. 40.0) THEN
              P1 = 0.5738 * EXP(-0.0325*D)
              P2 = 0.8519* EXP(-0.02811*D)
              IF (D .LE. 2.9) THEN
                P3 = 1.0
              ELSE
                P3 = 1.03781 - 0.01537*D
              ENDIF
            ELSE
              P1 = 0.158
              P2 = 0.277
              P3 = 0.423
            ENDIF
            P4 = 1.0
            IF (D .LT. 1.8) THEN
              DP1 = 1.0
            ELSE
              DP1 = 1.4657 * D**(-0.6454)
            ENDIF
            IF (D .LT. 10.0) THEN
              DP2 = 1.0
            ELSE
              DP2 = 1 / (0.847 + 0.01678*D)
            ENDIF

c         sequoia - use cedar for weights, and western hemlock
c         for proportions - mike landram
          CASE (19)

            LIVEWT = EXP(1.7273*LOG(D*R) - 2.8086)
            DEADWT = 0.01063*D**3

            P1 = 0.5474 * EXP(-0.03697*D)
            P2 = 0.8352* EXP(-0.03802*D)
            IF (D .LE. 2.9) THEN
               P3 = 1.0
            ELSE
               P3 = 1.0781 * EXP(-0.02735*D)
            ENDIF
            P4 = 1.0

            IF (D .LT. 4.0) THEN
              DP1 = 1.0
            ELSE
              IF (D .GT. 28.0) THEN
                DP1 = 0.005
              ELSE
                DP1 = 1.9608 * EXP(-0.2064*D)
              ENDIF
            ENDIF
            IF (D .LT. 12.0) THEN
              DP2 = 1.0
            ELSE
              DP2 = 1.0 / (0.2772 + 0.06141*D)
            END IF

C         Gambel oak - imperial equation
          CASE (22)

C         USING CHOJNACKY 1992 (TABLE 2) WITH COEFFICIENTS
C         FOR Q. SPP, Q. GAMBELLI AND Q. ILEX
C         (CHOJNACKY PERS. COMM.) - DROBINSON OCT/2000
C         SOME DOUBLE COUNTING IS PROBABLY PRESENT HERE

C           LINE BELOW IS NOT NEEDED SINCE USERS ENTER DRC--SAR 4/03
C           DRC = (D + 0.5766) / 0.8841
            DRC = D
            X = DRC * DRC * H / 1000.

C           VOLUME IN PLANT & BRANCHES > 1.5" (FT**3)

            X0 = 7.1046
            IF (X .LE. X0) THEN
              V = -0.0534 + (2.3077 * X) + (0.0467 * X**2)
            ELSE
              V = -0.0534 + (2.3077 * X) +
     &            (0.0467 * 3.0 * (X0**2 - X0**3/X))
            ENDIF
C
            IF (V .LE. .01) THEN
              V = .01
            ENDIF

C           WT OF PLANT & BRANCHES > 1.5" (POUNDS)

            V = V * SG * 2000.0

C           WT OF FOLIAGE AND BRANCHES < 2" (POUNDS)
C           ** NOTE 2" VS 1.5" DISCREPANCY **
C           WEIGHT OF TWIGS SET TO HALF OF FOLIAGE (LIKE PINYON & JUNIPER);
C           REMAINING BRANCHES DIVIDED BETWEEN 0.25-1" AND 1-3" BY
C           PUTTING 2/3 IN THE SMALLER CATEGORY AND 1/3 IN THE LARGER
C           (SIMILAR TO PINYON AND JUNIPER RELATIVE PREDICTIONS)

            DFOL = 10**(-0.5655 + 0.8382 * LOG10(V) - 0.0094 * H)
            DBR3 = 10**( 0.3036 + 0.7752 * LOG10(V) - 0.0049 * H)

            DBR1 = DFOL * 0.5
            DBR2 = MAX(0.0, ((DBR3-DBR1) * 0.67))
            DBR3 = MAX(0.0, ((DBR3-DBR1) * 0.33))
            DBR4 = 0.0
            DBR5 = 0.0

c         red alder (Snell & Little 1983)
          CASE (23)

            LIVEWT = EXP(-1.3290 + 2.6232 * LOG(D))
            DEADWT = EXP(-4.3788 + 2.6243 * LOG(D))

            P1 = 1.0/(2.7638 + 0.2155 * D**1.3364)
            P2 = 1.0/(1.286 + 0.1016 * D**1.3525)
            IF (D .LT. 2.1) THEN
              P3 = 1.0
            ELSE
              P3 = 1.0/(0.8847 + 0.0441 * D**1.3021)
            ENDIF
            IF (D .LT. 6.1) THEN
              P4 = 1.0
            ELSE
              P4 = 1.0/(0.995 + 0.0013 * D**1.9736)
            ENDIF

            DP1 = EXP(-.6880 - 0.1532 * D)
            IF (D .LT. 2.5) THEN
             DP2 = 1.0
            ELSE
             DP2 = EXP(0.2134 - 0.0869 * D)
            ENDIF
            IF (D .LT. 11.0) THEN
             DP3 = 1.0
            ELSE
             DP3 = EXP(0.3473 - 0.0315 * D)
            ENDIF

c         mountain hemlock - total from Gholz (1979)
c         partitioning from western hemlock
          CASE (24)
            LDM = LOG(D * 2.54)
            LIVEWT = EXP(-3.8169 + 1.9756 * LDM) +  ! foliage
     >               EXP(-5.2581 + 2.6045 * LDM)    ! live branches
            DEADWT = EXP(-9.9449 + 3.2845 * LDM)

            LIVEWT = LIVEWT*2.2046         ! convert kg to lbs
            DEADWT = DEADWT*2.2046
            
            IF (D .LE. 40.0) THEN
              P1 = 0.5474 * EXP(-0.03697*D)
              P2 = 0.8352* EXP(-0.03802*D)
              IF (D .LE. 2.9) THEN
                P3 = 1.0
              ELSE
                P3 = 1.0781 * EXP(-0.02735*D)
              ENDIF
            ELSE
              P1 = 0.125
              P2 = 0.183
              P3 = 0.361
            ENDIF
            P4 = 1.0
            IF (D .LT. 4.0) THEN
              DP1 = 1.0
            ELSE
              IF (D .GT. 28.0) THEN
                DP1 = 0.005
              ELSE
                DP1 = 1.9608 * EXP(-0.2064*D)
              ENDIF
            ENDIF
            IF (D .LT. 12.0) THEN
              DP2 = 1.0
            ELSE
              DP2 = 1.0 / (0.2772 + 0.06141*D)
            END IF

C         ponderosa pine - black hills
C         based on Keyser and Smith, Forest Science 56(2) 2010
          CASE (25)
C           INSERT NEW CODE FOR CROWN BIOMASS OF NEEDLE AND 1-HOUR FUELS
            TFOL = (0.0865*((D*2.54)**1.8916)*(TCR**1.1358))*2.2046
            T1HRL = 1.5439*(TCR**5.6131)*2.2046
            T1HRD = 0.0

C           REST IS SAME AS 13 ABOVE
            IF (HP .LT. DOMPCT) THEN
              LIVEWT = EXP(-0.7572 + 2.216*LOG(D))
              DEADWT = EXP(-2.5176 + 2.51*LOG(D))
              P1 = 0.6501 * EXP(-0.1544*D)
              P2 = 0.8435 * EXP(-0.1665*D)
              P3 = 1.0865 * EXP(-0.0833*D)
              P4 = 1.0
            ELSE
              LIVEWT = EXP(2.2812*LOG(D) + 1.5098*LOG(R) - 3.0957)
              DEADWT = EXP(2.8376*LOG(D) - 3.7398)
              P1 = 0.5578 * EXP(-0.04754*D)
              IF (D .GE. 31.0) THEN
                P2 = P1 + 0.01
              ELSE
                P2 = 0.6254 * EXP(-0.05114*D)
              ENDIF
              IF (D .LE. 1.0) THEN
                P3 = 1.0
                P4 = 1.0
              ELSE
                P3 = 0.985 * EXP(-0.03102*D)
                IF (D .LE. 6.5) THEN
                  P4 = 1.0
                ELSE
                  P4 = 1.083 - 0.01306*D
                ENDIF
              ENDIF
            ENDIF
            IF (D .GT. 30.0) THEN
              DP1 = 0.004
              DP2 = 0.06
            ELSE
              DP1 = (1.4114 / D) - 0.04345
              DP2 = 1.0621 - 0.03342*D
            ENDIF

        END SELECT

C       CALCULATE FINAL CROWN WEIGHT FOR TREES > 1" DBH

        X = 1 - SMWGT
        SELECT CASE (SPI)

C         MOST SPECIES
          CASE DEFAULT

C           MOST CALCULATIONS ARE DERIVED FROM BROWN'S METHODOLOGY

C           Check whether anything came out negative that shouldn't have, and
C           if so, make it be zero.  Check also that none of the proportions
C           are greater than 1, or less than the preceding one.

            IF (LIVEWT .LT. 0.0) LIVEWT = 0.0
            IF (DEADWT .LT. 0.0) DEADWT = 0.0

            IF (P1 .LT. 0.0)   P1 = 0.0
            IF (P2 .LT. 0.0)   P2 = 0.0
            IF (P3 .LT. 0.0)   P3 = 0.0
            IF (P4 .LT. 0.0)   P4 = 0.0
            IF (DP1 .LT. 0.0) DP1 = 0.0
            IF (DP2 .LT. 0.0) DP2 = 0.0
            IF (DP3 .LT. 0.0) DP3 = 0.0

            IF (P1 .GT. 1.0)   P1 = 1.0
            IF (P2 .GT. 1.0)   P2 = 1.0
            IF (P3 .GT. 1.0)   P3 = 1.0
            IF (P4 .GT. 1.0)   P4 = 1.0
            IF (DP1 .GT. 1.0) DP1 = 1.0
            IF (DP2 .GT. 1.0) DP2 = 1.0
            IF (DP3 .GT. 1.0) DP3 = 1.0

            IF (P2 .LT. P1)    P2 = P1
            IF (P3 .LT. P2)    P3 = P2
            IF (P4 .LT. P3)    P4 = P3
            IF (DP2 .LT. DP1) DP2 = DP1
            IF (DP3 .LT. DP2) DP3 = DP2

C           Divide the live and dead crown weights among the different size
C           classes according to the appropriate proportions.  Size Class 5
C           does not get anything under the current rules (Jim says this is
C           right).

            IF (SPI .EQ. 25) THEN  ! ponderosa pine - black  hills
              IF (TFOL  .LT. 0.0)  TFOL  = 0.0
              IF (T1HRL .LT. 0.0)  T1HRL = 0.0
              IF (T1HRD .LT. 0.0)  T1HRD = 0.0
                                      
              XV(0) = XV(0) + (TFOL) * X
              XV(1) = XV(1) + (T1HRL + T1HRD) * X
            ELSE
              XV(0) = XV(0) + (LIVEWT *  P1) * X
              XV(1) = XV(1) + (LIVEWT * (P2-P1) + DEADWT * DP1) * X
            ENDIF
            
            XV(2) = XV(2) + (LIVEWT * (P3-P2) + DEADWT * (DP2-DP1)) * X
            XV(3) = XV(3) + (LIVEWT * (P4-P3) + DEADWT * (DP3-DP2)) * X
            XV(4) = XV(4) + (LIVEWT * (1.0-P4)+ DEADWT * (1.0-DP3)) * X

C         quaking aspen
          CASE (2)

C           weights to shift breakpoints; values for <0.25" and .25-1"
C           are guesses based on fig 4.2 of ffe model desc. feb/99

C           august 2003 - further modification: comparison with bigtooth
C           aspen (eastern species) showed that foliage biomass was approximately
C           equal, but XV(1,2,3) were markedly lower (3-5 fold!). XV(4) was
C           similar. This demonstrates that the assumptions used to fill the
C           gaps in the quaking aspen crown were probably very wrong. The fix is:
C           set <.25" (xv(1)) to 0.75 of foliage (xv(0)); set 0.25-1" (xv(2)) to
C           2x foliage (similar to value in bigtooth); leave 1-3" alone (xv(3),
C           since quaking aspen has a subjectively lighter crown than bigtooth,
C           and since the data for xv(3) are more quantitative, while xv(1) and
C           xv(2) are linked to computed foliage biomass only through a comparison
C           with bigtooth aspen.

            XV(0) = XV(0) + MAX(0.0,DFOL*2.2046)  ! modified
            XV(1) = XV(1) + MAX(0.0,DFOL*0.75*2.2046)
            XV(2) = XV(2) + MAX(0.0,(DBR3*0.139+DFOL*2.0)*2.2046)
            XV(3) = XV(3) +
     &                      MAX(0.0,(DBR3*0.861+DBR2*0.405)*2.2046)
            XV(4) = XV(4) +
     &                     MAX(0.0,(DBR1+DBR2*0.595)*2.2046)

c            XV(0) = XV(0) + MAX(0.0,DFOL*2.2046)  ! original
c            XV(1) = XV(1) + MAX(0.0,DFOL*0.25*2.2046)
c            XV(2) = XV(2) + MAX(0.0,(DBR3*0.139+DFOL*0.50)*2.2046)
c            XV(3) = XV(3) +
c     &                      MAX(0.0,(DBR3*0.861+DBR2*0.405)*2.2046)
c            XV(4) = XV(4) +
c     &                     MAX(0.0,(DBR1+DBR2*0.595)*2.2046)

C         bristlecone pine, pinyon pine, juniper
C         convert kg->lb (dry weight)
          CASE (9,12,16)
            XV(0) = XV(0) + MAX(0.0,DFOL * 2.2046) * X
            XV(1) = XV(1) + MAX(0.0,DBR1 * 2.2046) * X
            XV(2) = XV(2) + MAX(0.0,DBR2 * 2.2046) * X
            XV(3) = XV(3) + MAX(0.0,DBR3 * 2.2046) * X
            XV(4) = XV(4) + MAX(0.0,DBR4 * 2.2046) * X
            XV(5) = XV(5) + MAX(0.0,DBR5 * 2.2046) * X

C         Gambel oak
          CASE (22)

            XV(0) = XV(0) + MAX(0.0,DFOL) * X
            XV(1) = XV(1) + MAX(0.0,DBR1) * X
            XV(2) = XV(2) + MAX(0.0,DBR2) * X
            XV(3) = XV(3) + MAX(0.0,DBR3) * X
            XV(4) = XV(4) + MAX(0.0,DBR4) * X
            XV(5) = XV(5) + MAX(0.0,DBR5) * X

        END SELECT

      DO J = 0,5
        XV(J) = MAX(0.0,XV(J))
      ENDDO

  999 CONTINUE

      RETURN
      END
