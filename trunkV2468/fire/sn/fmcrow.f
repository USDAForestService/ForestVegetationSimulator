      SUBROUTINE FMCROW
      IMPLICIT NONE
C----------
C FIRE-SN $Id: fmcrow.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C     CALLED FROM: FMSDIT, FMPRUN
C     CALLS:
C                 
C  PURPOSE:
C     THIS SUBROUTINE CALCULATES CROWNW(TREE,SIZE), THE WEIGHT OF
C     VARIOUS SIZES OF CROWN MATERIAL THAT IS ASSOCIATED WITH EACH TREE
C     RECORD IN THE CURRENT STAND.  
C----------
C  LOCAL VARIABLE DEFINITIONS:
C     D:        DBH
C     H:        HEIGHT
C     IC:       LENGTH OF LIVE CROWN
C     SP:       SPECIES
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
      INCLUDE 'FMCOM.F77'
C
C      
      INCLUDE 'CONTRL.F77'
C
C      
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'SNCOM.F77'
C
C
COMMONS
C----------
C  VARIABLE DECLARATIONS
C----------
      LOGICAL DEBUG
      INTEGER I,J,IC,ISPMAP(MAXSP),SPI
      REAL    D,H,SG,XV(0:5)


C     INDEX TO THE CROWN EQUATIONS USED IN FMCROWE. EASTERN EQUATIONS ARE
C     BASED ON LS-FFE SPECIES NUMBERING;  IN THE TABLE BELOW, A '-' IN THE
C     "MAPS TO" COLUMN INDICATES A SPECIES THAT MAPS TO ITSELF
C
C     I   NAME                     MAPS TO          FMCROWE
C --------------------------------------------------------------
C     1 = fir sp.                  balsam fir           8
C     2 = redcedar                      -              14
C     3 = spruce sp.               black spruce         9
C     4 = sand pine                red pine natural     3
C     5 = shortleaf pine           red pine natural     3
C     6 = slash pine               red pine natural     3
C     7 = spruce pine              red pine natural     3
C     8 = longleaf pine            red pine natural     3
C     9 = table mountain pine      red pine natural     3
C    10 = pitch pine               red pine natural     3
C    11 = pond pine                red pine natural     3
C    12 = eastern white pine            -               5
C    13 = loblolly pine            red pine natural     3
C    14 = virginia pine            red pine natural     3
C    15 = baldcypress              eastern redcedar    14
C    16 = pondcypress              eastern redcedar    14
C    17 = hemlock                       -              12
C    18 = Florida maple            sugar maple         26
C    19 = boxelder                      -              50
C    20 = red maple                     -              19
C    21 = silver maple                  -              18
C    22 = sugar maple                   -              26
C    23 = buckeye/horsechestnut    comm. hardwoods     44
C    24 = birch sp.                yellow birch        24
C    25 = sweet birch              yellow birch        24
C    26 = american hornbeam             -              53
C    27 = hickory sp.              shagbark hickory    39
C    28 = catalpa                  comm. hardwoods     44
C    29 = hackberry sp.                 -              55
C    30 = eastern redbud           comm. hardwoods     44
C    31 = flowering dogwood             -              56
C    32 = common persimmon         comm. hardwoods     44
C    33 = american beech                -              28
C    34 = ash sp.                  green ash           16
C    35 = white ash                     -              29
C    36 = black ash                     -              15
C    37 = green ash                     -              16
C    38 = honeylocust              comm. hardwoods     44
C    39 = loblolly-bay             comm. hardwoods     44
C    40 = silverbell               comm. hardwoods     44
C    41 = american holly           comm. hardwoods     44
C    42 = butternut                     -              45
C    43 = black walnut                  -              46
C    44 = sweet gum                comm. hardwoods     44
C    45 = yellow-poplar            comm. hardwoods     44
C    46 = magnolia sp.             comm. hardwoods     44
C    47 = cucumbertree             comm. hardwoods     44
C    48 = southern magnolia        comm. hardwoods     44
C    49 = sweetbay                 comm. hardwoods     44
C    50 = bigleaf magnolia         comm. hardwoods     44
C    51 = apple sp.                     -              58
C    52 = mulberry sp.             apple sp.           58
C    53 = water tupelo                  -              59
C    54 = black gum                     -              59
C    55 = swamp tupelo                  -              59
C    56 = e. hophornbeam                -              47
C    57 = sourwood                 comm. hardwoods     44
C    58 = redbay                   comm. hardwoods     44
C    59 = sycamore                      -              60
C    60 = cottonwood                    -              17
C    61 = bigtooth aspen                -              40
C    62 = black cherry                  -              20
C    63 = white oak                     -              30
C    64 = scarlet oak              northern red oak    34
C    65 = southern red oak         northern red oak    34
C    66 = cherrybark oak           northern red oak    34
C    67 = turkey oak               northern red oak    34
C    68 = laurel oak               northern red oak    34
C    69 = overcup oak              white oak           30
C    70 = blackjack oak            northern red oak    34
C    71 = swamp chestnut oak       white oak           30
C    72 = chinkapin oak                 -              33
C    73 = water oak                northern red oak    34
C    74 = chestnut oak             white oak           30
C    75 = northern red oak              -              34
C    76 = shumard oak              northern red oak    34
C    77 = post oak                 white oak           30
C    78 = black oak                     -              35
C    79 = live oak                 white oak           30
C    80 = black locust                  -              48
C    81 = willow                        -              64
C    82 = sassafras                     -              67
C    83 = basswood                      -              25
C    84 = elm sp.                  slippery elm        22
C    85 = winged elm               slippery elm        22
C    86 = american elm                  -              21
C    87 = slippery elm                  -              22
C    88 = softwoods, misc.         eastern redcedar    14
C    89 = hardwoods, misc.         comm. hardwoods     44
C    90 = unknown/not listed       comm. hardwoods     44

  
      DATA ISPMAP /8,14, 9, 3, 3, 3, 3, 3, 3, 3,
     &             3, 5, 3, 3,14,14,12,26,50,19,
     &            18,26,44,24,24,53,39,44,55,44,
     &            56,44,28,16,29,15,16,44,44,44,
     &            44,45,46,44,44,44,44,44,44,44,
     &            58,58,59,59,59,47,44,44,60,17,
     &            40,20,30,34,34,34,34,34,30,34,
     &            30,33,34,30,34,34,30,35,30,48,
     &            64,67,25,22,22,21,22,14,44,44/

C
C----------
C  CHECK FOR DEBUG
C----------
      CALL DBCHK (DEBUG,'FMCROW',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,ITRN
    7 FORMAT(' ENTERING FMCROW CYCLE = ',I2,' ITRN=',I5)
C
      IF (ITRN.EQ.0) RETURN
C
      DO 999 I = 1,ITRN
C----------
C  INCREMENT GROW TO KEEP TRACK OF WHETHER THIS CROWN IS FREE
C  TO GROW AFTER BEING BURNED IN A FIRE.  SKIP THE REST OF THE LOOP
C  IF GROW IS STILL LESS THAN 1 AFTER THE INCREMENT.
C----------
        IF (GROW(I) .LT. 1) GROW(I) = GROW(I) + 1
        IF (GROW(I) .LT. 1) GOTO 999
C----------        
C  ARGUMENTS TO PASS
C----------
        SPI = ISPMAP(ISP(I))
        D   = DBH(I)
        H   = HT(I)
        IC  = ICR(I)
        SG  = V2T(ISP(I))
C----------
C  INITIALIZE ALL THE CANOPY COMPONENTS TO ZERO, AND SKIP THE REST
C  OF THIS LOOP IF THE TREE HAS NO DIAMETER, HEIGHT, OR LIVE CROWN.
C----------
        DO J = 0,5
          XV(J) = 0.0
        ENDDO
C
        CALL FMCROWE(SPI,ISP(I),D,H,IC,SG,XV)
C----------
C  COPY TEMPORARY VALUES TO FFE ARRAY
C----------
        DO J = 0,5
          CROWNW(I,J) = XV(J)
          IF (DEBUG) WRITE(JOSTND,*) 'I=',I,' size=',J,
     &    ' CROWNW=',CROWNW(I,J)
        ENDDO
C
  999 CONTINUE
C
      RETURN
      END
