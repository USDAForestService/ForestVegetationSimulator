      SUBROUTINE HTCALC (N,ISPC,AGET,H,HTMAX,HTG,JOSTND,DEBUG)
      IMPLICIT NONE
C----------
C AK $Id$
C----------
C  THIS SUBROUTINE COMPUTES ONE OF THE FOLLOWING:
C
C  IF N .EQ. 0 THEN CALCULATE AGE USING HEIGHT AND SITE INDEX
C  IF N .EQ. 1 THEN CALCULATE HEIGHT USING AGE AND SITE INDEX
C  IF N .EQ. 2 CALCULATE HEIGHT INCREMENT USING AGE AND SITE INDEX
C  HTMAX IS CALCULATED ON EVERY CALL TO HTCALC
C  CALLED FROM: REGENT, ESSADV, ESSUBH, ESXCSH
C----------

C----------
C SPECIES LIST FOR ALASKA VARIANT.
C Number  V  Code  Common Name         FIA  PLANTS Scientific Name
C   1        SF   Pacific silver fir  011  ABAM   Abies amabilis
C   2        AF   subalpine fir       019  ABLA   Abies lasiocarpa
C   3        YC   Alaska cedar        042  CANO9  Callitropsis nootkatensis
C   4        TA   tamarack            071  LALA   Larix laricina
C   5     P  WS   white spruce        094  PIGL   Picea glauca
C   6     P  LS   Lutz’s spruce            PILU   Picea lutzii
C   7     P  BE   black spruce        095  PIMA   Picea mariana
C   8        SS   Sitka spruce        098  PISI   Picea sitchensis
C   9        LP   lodgepole pine      108  PICO   Pinus contorta
C  10        RC   western redcedar    242  THPL   Thuja plicata
C  11        WH   western hemlock     263  TSHE   Tsuga heterophylla
C  12        MH   mountain hemlock    264  TSME   Tsuga mertensiana
C  13     P  OS   other softwoods     298  2TE
C  14        AD   alder species       350  ALNUS  Alnus species
C  15        RA   red alder           351  ALRU2  Alnus rubra
C  16     P  PB   paper birch         375  BEPA   Betula papyrifera
C  17     P  AB   Alaska birch        376  BENE4  Betula neoalaskana
C  18     P  BA   balsam poplar       741  POBA2  Populus balsamifera
C  19     P  AS   quaking aspen       746  POTR5  Populus tremuloides
C  20     P  CW   black cottonwood    747  POBAT  Populus trichocarpa
C  21     P  WI   willow species      920  SALIX  Salix species
C  22     P  SU   Scouler’s willow    928  SASC   Salix scouleriana
C  23     P  OH   other hardwoods     998  2TD
C----------

C----------
COMMONS
C----------
C
      INCLUDE 'PRGPRM.F77'
C
      INCLUDE 'PLOT.F77'
C
      INCLUDE 'VARCOM.F77'

C----------
C  VARIABLE DEFINITIONS:
C---------

C  N     -- MODE FOR CALCULATION
C  ISPC  -- SPECIES
C  AGET  -- TREE AGE
C  H     -- CURRENT TREE HEIGHT
C  XSITE -- SITE INDEX FOR SPECIES
C  *RETURN VARIABLES
C  HTG  -- HEIGHT INCREMENT FOR CYCLE
C  HTMAX -- MAXIMUM HEIGHT FOR SPECIES AT SITE SI
C  AGET  -- TREE AGE RETURNED IF N <= 0

C------------
C  VARIABLE DECLARATIONS:
C----------

      LOGICAL DEBUG
      INTEGER JOSTND,N,ISPC
      REAL HTMAX,H,H1,H2,AGET,XSITE,HTG
      REAL B1,B2,B3,B4,B5,C1,C2,C3
      REAL HEGYI1(MAXSP),HEGYI2(MAXSP),HEGYI3(MAXSP)

C----------
C  DATA STATEMENTS
C----------

      DATA HEGYI1/1.1945, 1.3832, 1.1243, 1.1637, 1.2883,
     &            1.2883, 1.1637, 1.0458, 1.0236, 1.1243,
     &            1.1514, 1.1514, 1.2883, 1.1318, 1.0142,
     &            1.1580, 1.1580, 1.1318, 1.2025, 1.1318,
     &            1.1318, 1.1318, 1.1318/

      DATA HEGYI2/-0.0236, -0.0155, -0.0263, -0.0215, -0.0181,
     &            -0.0181, -0.0215, -0.0380, -0.0465, -0.0263,
     &            -0.0237, -0.0237, -0.0181, -0.0226, -0.0421,
     &            -0.0175, -0.0175, -0.0226, -0.0158, -0.0226,
     &            -0.0226, -0.0226, -0.0226/

      DATA HEGYI3/1.7918, 1.3597, 1.5662, 1.2243, 1.4177,
     &            1.4177, 1.2243, 1.9804, 2.4269, 1.5662,
     &            1.4365, 1.4365, 1.4177, 1.1233, 0.9422,
     &            0.7687, 0.7687, 1.1233, 0.7994, 1.1233,
     &            1.1233, 1.1233, 1.1233/

C----------
C NOTES ABOUT SITE INDEX EQUATIONS USED TO CALCULATE HEIGHT, AGE, AND
C HEIGHT INCREMENT.
C
C ALL SPECIES EXCEPT SS, WH, AND MH:
C USE SITE INDEX EQUATIONS FROM HEGYI 1981:SITE INDEX EQUATIONS
C AND CURVES FOR THE MAJOR TREE SPECIES OF BRITISH COLUMBIA.
C
C SS, WH, WH:
C USE SITE INDEX EQUATION FROM PAYANDEH 1974: NONLINEAR SITE INDEX EQUATIONS
C FOR SEVERAL MAJOR CANADIAN TIMBER SPECIES.
C----------

C  SET SITE INDEX FOR SPECIES
      XSITE=SITEAR(ISPC)

C  LOAD PAYANDEH COEFFICIENTS
      B1 = 1.5469
      B2 = 1.0018
      B3 = -0.0114
      B4 = 1.0883
      B5 = 0.0072

C  LOAD HEGYI COEFFICIENTS
      C1 = HEGYI1(ISPC)
      C2 = HEGYI2(ISPC)
      C3 = HEGYI3(ISPC)

C  CALCULATE HTMAX
      SELECT CASE (ISPC)
        CASE(8,11,12)
          HTMAX =(B1*XSITE**B2)
        CASE DEFAULT
          HTMAX = (C1*XSITE)
      END SELECT

C------
C  IF DIFFERENCE BETWEEN MAX SITE INDEX HEIGHT AND H IS LE
C  1 THEN BYPASS CALCULATIONS AND EXIT SUBROUTINE.
C
C  OTHERWISE DETERMINE TYPE OF CALCULATION TO BE MADE BASED ON N
C------

      IF(HTMAX-H.LE.1.) GOTO 900

C------
C  CHOICE 1
C  N IS 0 AND AN AGE WILL BE CALCULATED FROM H AND XSITE
C  CALL IS FROM REGENT
C------
      IF(N .EQ. 0) THEN

C  PAYANDEH CALCULATION
        SELECT CASE(ISPC)
          CASE(8,11,12)
            AGET = 1./B3*(ALOG(1-((H)/B1/XSITE**B2)**(1./B4/XSITE**B5)))
            IF(DEBUG)WRITE(JOSTND,*)' IN HTCALC GETTING AGE (PAYANDEH)',
     &      ' N=',N,' ISPC=',ISPC, ' HT=',H,' AGET=',AGET,
     &      ' XSITE=',XSITE

C  HEGYI CALCULATION
          CASE DEFAULT
            AGET = 1/C2*LOG(1-((H)/C1/XSITE)**(1/C3))
            IF(DEBUG)WRITE(JOSTND,*)' IN HTCALC GETTING AGE (HEGYI)',
     &      ' N=',N,' ISPC=',ISPC, ' HT=',H,' AGET=',AGET,
     &      ' XSITE=',XSITE
        END SELECT

C------
C  CHOICE 2
C  N IS 1 AND HEIGHT IS BEING CALCULATED FROM AGET AND XSITE
C  CALL IS FROM ESADVH, ESSUBH, ESXCSH
C------
      ELSE IF(N .EQ. 1) THEN

C  PAYANDEH CALCULATION
        SELECT CASE(ISPC)
          CASE(8,11,12)
            H = B1*XSITE**B2*(1-EXP(B3*AGET))**(B4*XSITE**B5)
            IF(DEBUG)WRITE(JOSTND,*)' IN HTCALC GETTING HT (PAYANDEH)',
     &      ' N=',N,' ISPC=',ISPC, ' HT=',H,' AGET=',AGET,
     &      ' XSITE=',XSITE

C  HEGYI CALCULATION
          CASE DEFAULT
            H = C1*XSITE*(1-EXP(C2*AGET))**C3

C  DO DEBUG
            IF(DEBUG)WRITE(JOSTND,*)' IN HTCALC GETTING HT (HEGYI)',
     &     ' N=',N,' ISPC=',ISPC, ' HT=',H,' AGET=',AGET,
     &     ' XSITE=',XSITE
        END SELECT

C------
C  CHOICE 3
C  MODE IS 2 AND HEIGHT INCREMENT IS BEING CALCULATED FROM AGET AND XSITE
C  CALL IS FROM REGENT
C------
      ELSE

C  PAYANDEH CALCULATION
        SELECT CASE(ISPC)
          CASE(8,11,12)
            H1 = B1*XSITE**B2*(1-EXP(B3*AGET))**(B4*XSITE**B5)
            H2 = B1*XSITE**B2*(1-EXP(B3*(AGET + 10.0)))**(B4*XSITE**B5)
            HTG = H2 - H1

C  DO DEBUG
            IF(DEBUG)WRITE(JOSTND,*)' IN HTCALC GETTING HI (PAYANDEH)',
     &      ' N=',N,' ISPC=',ISPC,' AGET=',AGET,' XSITE=',XSITE,
     &      ' H1=',H1, ' H2=',H2, ' HTG=', HTG

C  HEGYI CALCULATION
          CASE DEFAULT
            H1 = C1*XSITE*(1-EXP(C2*AGET))**C3
            H2 = C1*XSITE*(1-EXP(C2*(AGET + 10.0)))**C3
            HTG = H2 - H1

C  DO DEBUG
            IF(DEBUG)WRITE(JOSTND,*)' IN HTCALC GETTING HI (HEGYI)',
     &      ' N=',N,' ISPC=',ISPC, ' AGET=',AGET,' XSITE=',XSITE,
     &      ' H1=',H1, ' H2=',H2,' HTG=', HTG
        END SELECT

C  END OF CHOICES
      ENDIF

  900 CONTINUE
      RETURN
      END