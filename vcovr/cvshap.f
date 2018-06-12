      SUBROUTINE CVSHAP (LTHIN)
      IMPLICIT NONE
C----------
C  **CVSHAP--COVR DATE OF LAST REVISION:  06/16/16
C----------
C  SUBROUTINE CVSHAP ASSIGNS EACH TREE CROWN TO ONE OF FIVE SHAPES.
C  SHAPES ARE:
C     ISHAPE(I)    PLANE SHAPE     SOLID FORM
C     ---------    -----------     ----------
C          1       CIRCLE          SPHERE
C          2       TRIANGLE        CONE
C          3       NEILOID         NEILOID
C          4       PARABOLA        PARABOLOID
C          5       ELLIPSE         ELLIPSOID
C
C  THE CLASSIFICATION FUNCTION COEFFICIENTS WERE ESTIMATED USING
C  FISHER'S LINEAR DISCRIMINANT FUNCTIONS, INDIVIDUALLY FOR SPECIES.
C
C  FOR DOCUMENTATION SEE:
C     MOEUR, M.  MODELS FOR PREDICTING CROWN SHAPES OF NORTHERN ROCKY
C        MOUNTAIN CONIFERS.  REPORT ON FILE, NOVEMBER 1983.
C
C 1-11 REFERRING TO ORIGINAL 11 NI SPECIES:
C  CONST(5,11), BCL(5,11), BCR(5,11), BRAD(5,11),
C  BDBH(5,11), BHT(5,11), BTPA(5,11) -- COEFFICIENTS
C  CL      -- CROWN LENGTH
C  CR      -- CROWN RATIO
C  ISHAPE(MAXTRE) -- PREDICTED CROWN SHAPE
C  RAD     -- CROWN RADIUS
C  TPA     -- TREES PER ACRE
C  SCORE(5) -- DISCRIMINANT FUNCTION SCORE
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'CVCOM.F77'
C
C
COMMONS
C----------
      LOGICAL LTHIN,DEBUG
      INTEGER I,ISPI,J
      INTEGER MAPISP(49),MAPAK(49),MAPCA(49),MAPBM(49),MAPCI(49),
     &        MAPCR(49),MAPEC(49),MAPEM(49),MAPIE(49),MAPNI(49),
     &        MAPSO(49),MAPTT(49),MAPUT(49),MAPWC(49),MAPWS(49)
      REAL TPA,RAD,CR,CL,SCORM1
      REAL CONST(5,11),BCR(5,11),BHT(5,11),BRAD(5,11),
     &          BDBH(5,11),BCL(5,11),BTPA(5,11),SCORE(5)         
C----------
C  DATA STATEMENTS
C----------
C   SHAPE      1        2       3       4       5
      DATA CONST/
     &      -13.943,-37.395,-99.000,-32.104,-32.042,
     &      -99.000,-38.278,-99.000,-44.875,-32.742,
     &      -25.380,-31.308,-35.906,-31.435,-28.185,
     &      -19.633,-23.785,-20.062,-22.012,-22.962,
     &      -19.633,-23.785,-20.062,-22.012,-22.962,
     &      -99.000,-27.573,-99.000,-22.434,-26.275,
     &      -22.322,-37.190,-99.000,-38.316,-31.988,
     &      -99.000,-46.116,-99.000,-49.990,-43.456,
     &      -24.002,-37.611,-99.000,-30.234,-36.183,
     &      -20.296,-24.195,-36.526,-26.622,-22.397,
     &      -24.002,-37.611,-99.000,-30.234,-36.183/
      DATA BCR/
     &      49.794,100.240,-99.00, 86.383, 94.554,
     &      -99.00,123.805,-99.00,134.780,116.554,
     &      52.715, 60.761,63.156, 61.744, 59.922,
     &      45.992, 54.464,38.042, 51.755, 53.598,
     &      45.992, 54.464,38.042, 51.755, 53.598,
     &      -99.00, 59.560,-99.00, 53.273, 55.799,
     &      73.920,104.413,-99.00,102.380, 99.877,
     &      -99.00,106.784,-99.00,110.115,100.844,
     &      56.463, 80.909,-99.00, 67.587, 78.258,
     &      45.808, 57.540,60.760, 59.567, 57.457,
     &      56.463, 80.909,-99.00, 67.587, 78.258/
      DATA BHT/
     &      0.48348,0.92054,-99.000,0.81786,0.85462,
     &      -99.000,1.03836,-99.000,1.00712,1.02891,
     &      1.02304,1.13332,1.28061,1.13364,1.16673,
     &      0.72015,0.88508,0.56218,0.84169,0.88962,
     &      0.72015,0.88508,0.56218,0.84169,0.88962,
     &      -99.000,1.08626,-99.000,0.98588,0.98314,
     &      0.83497,1.16782,-99.000,1.17028,1.15645,
     &      -99.000,1.60533,-99.000,1.62982,1.59203,
     &      1.25278,1.59728,-99.000,1.38587,1.58670,
     &      0.71361,0.91473,0.98645,0.89231,0.91047,
     &      1.25278,1.59728,-99.000,1.38587,1.58670/
     &
      DATA BRAD/
     &      0.56507,1.11080, -99.000,1.82564, 1.32317,
     &      -99.000,1.08869, -99.000,1.81120, 0.79969,
     &     -0.10457,0.28897,-0.49614,0.07177,-0.04803,
     &      0.13463,0.35420, 0.24447,0.32306, 0.18187,
     &      0.13463,0.35420, 0.24447,0.32306, 0.18187,
     &      -99.000,0.92215, -99.000,0.91886, 0.48536,
     &      0.62301,1.69840, -99.000,1.57036, 1.08601,
     &      -99.000,1.12257, -99.000,0.86087, 0.89170,
     &      1.64765,1.72301, -99.000,1.83248, 1.53816,
     &      0.98573,0.60433, 2.22885,0.89341, 0.58180,
     &      1.64765,1.72301, -99.000,1.83248, 1.53816/
     &
      DATA BCL/
     &      -0.8373,-1.5604,-99.000,-1.3933,-1.4959,
     &      -99.000,-1.9316,-99.000,-2.1712,-1.8977,
     &      -1.3082,-1.4288,-1.5055,-1.4393,-1.4367,
     &      -1.1754,-1.3304,-0.9574,-1.3074,-1.3250,
     &      -1.1754,-1.3304,-0.9574,-1.3074,-1.3250,
     &      -99.000,-1.2529,-99.000,-1.1295,-1.1502,
     &      -1.6455,-2.2164,-99.000,-2.1892,-2.1148,
     &      -99.000,-2.2233,-99.000,-2.3029,-2.1475,
     &      -1.6686,-2.0343,-99.000,-1.8193,-2.0154,
     &      -1.0722,-1.3127,-1.3903,-1.3079,-1.2973,
     &      -1.6686,-2.0343,-99.000,-1.8193,-2.0154/
     &
      DATA BTPA/
     &      0.00437,0.00911,-99.000,0.00603,0.00899,
     &      -99.000,0.01548,-99.000,0.02342,0.02134,
     &      0.00634,0.00706,0.00673,0.00683,0.00673,
     &      0.00782,0.00841,0.00813,0.00778,0.00816,
     &      0.00782,0.00841,0.00813,0.00778,0.00816,
     &      -99.000,0.00896,-99.000,0.00736,0.00810,
     &      0.00600,0.00406,-99.000,0.00542,0.00613,
     &      -99.000,0.04348,-99.000,0.04908,0.05410,
     &      0.01644,0.01891,-99.000,0.01809,0.01923,
     &      0.01233,0.01068,0.01136,0.01190,0.01070,
     &      0.01644,0.01891,-99.000,0.01809,0.01923/
      DATA BDBH/
     &       0.05236, 0.08377,-99.0000,-0.17327, 0.22765,
     &      -99.0000,-0.14770,-99.0000, 0.25077, 0.01210,
     &      -0.40495,-0.66563,-0.42355,-0.50661,-0.54780,
     &       0.14819,-0.02081, 0.14503, 0.14760, 0.00202,
     &       0.14819,-0.02081, 0.14503, 0.14760, 0.00202,
     &      -99.0000,-0.81991,-99.0000,-0.99031,-0.33308,
     &       0.21973,-0.17842,-99.0000, 0.00182,-0.04777,
     &      -99.0000, 0.06301,-99.0000, 0.41333, 0.02430,
     &      -0.61101,-0.80165,-99.0000,-0.74084,-0.72773,
     &      -0.39061,-0.33475,-0.99571,-0.40740,-0.37839,
     &      -0.61101,-0.80165,-99.0000,-0.74084,-0.72773/
C----------
C  MAP VARIANT SPECIES = ORIGINAL 11 SPECIES
C  IF VARIANT SPECIES NUMBER OR ORDER CHANGES
C  THE FOLLOWING MAPPING SHOULD BE ADJUSTED HERE AND IN **CVSHAP**
C----------
      DATA MAPNI /
C     WP=WP,WL=WL,DF=DF,GF=GF,WH=WH,RC=RC,LP=LP,ES=ES,AF=AF,PP=PP
     &    1,    2,    3,    4,    5,    6,    7,    8,    9,   10,
C     MH=MH
     &   11, 38*0/
C
C
      DATA MAPAK /
C     WS=ES,RC=RC,SF=GF,MH=MH,WH=WH,YC=RC,LP=LP,SS=ES,AF=AF,RA=WL
     &    8,    6,    4,   11,    5,    6,    7,    8,    9,    2,
C     CW=WL,OH=WL,OS=MH
     &    2,    2,   11, 36*0/
C
C
      DATA MAPBM /
C     WP=WP,WL=WL,DF=DF,GF=GF,MH=MH,WJ=RC,LP=LP,ES=ES,AF=AF,PP=PP
     &    1,    2,    3,    4,   11,    6,    7,    8,    9,   10,
C     WB=WP,LM=WP,PY=WH,YC=RC,AS=WL,CW=WL,OS=MH,OH=WL
     &    1,    1,    5,    6,    2,    2,   11,    2, 31*0/
C
C
      DATA MAPCA /
C     PC=RC,IC=RC,RC=RC,WF=GF,RF=AF,SH=AF,DF=DF,WH=WH,MH=MH,WB=WP
     &    6,    6,    6,    4,    9,    9,    3,    5,   11,    1,
C     KP=PP,LP=LP,CP=PP,LM=WP,JP=PP,SP=WP,WP=WP,PP=PP,MP=PP,GP=PP
     &   10,    7,   10,    1,   10,    1,    1,   10,   10,   10,
C     WJ=RC,BR=ES,GS=RC,PY=WH,OS=MH,LO=WL,CY=WL,BL=WL,EO=WL,WO=WL
     &    6,    8,    6,    5,   11,    2,    2,    2,    2,    2,
C     BO=WL,VO=WL,IO=WL,BM=WL,BU=WL,RA=WL,MA=WL,GC=WL,DG=WL,FL=WL
     &    2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
C     WN=WL,TO=WL,SY=WL,AS=WL,CW=WL,WI=WL,CN=WL,CL=WL,OH=WL      
     &    2,    2,    2,    2,    2,    2,    2,    2,    2/
C
C
      DATA MAPCI /
C     WP=WP,WL=WL,DF=DF,GF=GF,WH=WH,RC=RC,LP=LP,ES=ES,AF=AF,PP=PP
     &    1,    2,    3,    4,    5,    6,    7,    8,    9,   10,
C     WB=WP,PY=WH,AS=WL,WJ=RC,MC=WL,LM=WP,CW=WL,OS=MH,OH=WL
     &    1,    5,    2,    6,    2,    1,    2,   11,    2, 30*0/
C
C
      DATA MAPCR /
C     AF=AF,CB=AF,DF=DF,GF=GF,WF=GF,MH=MH,RC=RC,WL=WL,BC=WP,LM=WP
     &    9,    9,    3,    4,    4,   11,    6,    2,    1,    1,
C     LP=LP,PI=PP,PP=PP,WB=WP,SW=WP,UJ=RC,BS=ES,ES=ES,WS=ES,AS=WL
     &    7,   10,   10,    1,    1,    6,    8,    8,    8,    2,
C     NC=WL,PW=WL,GO=WL,AW=WL,EM=WL,BK=WL,SO=WL,PB=WL,AJ=RC,RM=RC
     &    2,    2,    2,    2,    2,    2,    2,    2,    6,    6,
C     OJ=RC,ER=RC,PM=PP,PD=PP,AZ=PP,CI=LP,OS=MH,OH=WL
     &    6,    6,   10,   10,   10,    7,   11,    2, 11*0/
C
C
      DATA MAPEC /
C     WP=WP,WL=WL,DF=DF,SF=GF,RC=RC,GF=GF,LP=LP,ES=ES,AF=AF,PP=PP
     &    1,    2,    3,    4,    6,    4,    7,    8,    9,   10,
C     WH=WH,MH=MH,PY=WH,WB=WP,NF=GF,WF=GF,LL=WL,YC=RC,WJ=RC,BM=WL
     &    5,   11,    5,    1,    4,    4,    2,    6,    6,    2,
C     VN=WL,RA=WL,PB=WL,GC=WL,DG=WL,AS=WL,CW=WL,WO=WL,PL=WL,WI=WL
     &    2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
C     OS=MH,OH=WL
     &   11,    2, 17*0/
C
C
      DATA MAPEM /
C     WB=WP,WL=WL,DF=DF,LM=WP,LL=WL,RM=RC,LP=LP,ES=ES,AF=AF,PP=PP
     &    1,    2,    3,    1,    2,    6,    7,    8,    9,   10,
C     GA=WL,AS=WL,CW=WL,BA=WL,PW=WL,NC=WL,PB=WL,OS=MH,OH=WL
     &    2,    2,    2,    2,    2,    2,    2,   11,    2, 30*0/
C
C
      DATA MAPIE /
C     WP=WP,WL=WL,DF=DF,GF=GF,WH=WH,RC=RC,LP=LP,ES=ES,AF=AF,PP=PP
     &    1,    2,    3,    4,    5,    6,    7,    8,    9,   10,
C     MH=MH,WB=WP,LM=WP,LL=WL,PI=PP,RM=RC,PY=WH,AS=WL,CO=WL,MM=WL
     &   11,    1,    1,    2,   10,    6,    5,    2,    2,    2,
C     PB=WL,OH=WL,OS=MH
     &    2,    2,   11, 26*0/
C
C 
       DATA MAPSO /
C     WP=WP,SP=WP,DF=DF,WF=GF,MH=MH,IC=RC,LP=LP,ES=ES,SH=AF,PP=PP
     &    1,    1,    3,    4,   11,    6,    7,    8,    9,   10,
C     WJ=RC,GF=GF,AF=AF,SF=GF,NF=GF,WB=WP,WL=WL,RC=RC,WH=WH,PY=WH
     &    6,    4,    9,    4,    4,    1,    2,    6,    5,    5,
C     WA=WL,RA=WL,BM=WL,AS=WL,CW=WL,CH=WL,WO=WL,WI=WL,GC=WL,MC=WL
     &    2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
C     MB=WL,OS=MH,OH=WL
     &    2,   11,    2, 16*0/
C
C
      DATA MAPTT /
C     WB=WP,LM=WP,DF=DF,PM=PP,BS=ES,AS=WL,LP=LP,ES=ES,AF=AF,PP=PP
     &    1,    1,    3,   10,    8,    2,    7,    8,    9,  10,
C     UJ=RC,RM=RC,BI=WL,MM=WL,NC=WL,MC=WL,OS=MH,OH=WL
     &    6,    6,    2,    2,    2,    2,   11,    2, 31*0/
C
C
      DATA MAPUT /
C     WB=WP,LM=WP,DF=DF,WF=GF,BS=ES,AS=WL,LP=LP,ES=ES,AF=AF,PP=PP,
     &    1,    1,    3,    4,    8,    2,    7,    8,    9,   10,
C     PI=PP,WJ=RC,GO=WL,PM=PP,RM=RC,UJ=RC,GB=WP,NC=WL,FC=WL,MC=WL,
     &   10,    6,    2,   10,    6,    6,    1,    2,    2,    2,
C    &BI=WL,BE=WL,OS=MH,OH=WL
     &    2,    2,   11,    2, 25*0/
C
C
      DATA MAPWC /
C     SF=GF,WF=GF,GF=GF,AF=AF,RF=AF,  =MH,NF=GF,YC=RC,IC=RC,ES=ES
     &    4,    4,    4,    9,    9,   11,    4,    6,    6,    8,
C     LP=LP,JP=PP,SP=WP,WP=WP,PP=PP,DF=DF,RW=RC,RC=RC,WH=WH,MH=MH
     &    7,   10,    1,    1,   10,    3,    6,    6,    5,   11,
C     BM=WL,RA=WL,WA=WL,PB=WL,GC=WL,AS=WL,CW=WL,WO=WL,WJ=RC,LL=WL
     &    2,    2,    2,    2,    2,    2,    2,    2,    6,    2,
C     WB=WP,KP=PP,PY=WH,DG=WL,HT=WL,CH=WL,WI=WL,  =MH,OT=MH
     &    1,   10,    5,    2,    2,    2,    2,   11,   11, 10*0/
C
C
      DATA MAPWS /
C     SP=WP,DF=DF,WF=GF,GS=RC,IC=RC,JP=PP,RF=AF,PP=PP,LP=LP,WB=WP
     &    1,    3,    4,    6,    6,   10,    9,   10,    7,    1,
C     WP=WP,PM=PP,SF=GF,KP=PP,FP=PP,CP=PP,LM=WP,MP=PP,GP=PP,WE=PP,
     &    1,   10,    4,   10,   10,   10,    1,   10,   10,   10,
C     GB=WP,BD=DF,RW=RC,MH=MH,WJ=RC,UJ=RC,CJ=RC,LO=WL,CY=WL,BL=WL,
     &    1,    3,    6,   11,    6,    6,    6,    2,    2,    2,
C     BO=WL,VO=WL,IO=WL,TO=WL,GC=WL,AS=WL,CL=WL,MA=WL,DG=WL,BM=WL,
     &    2,    2,    2,    2,    2,    2,    2,    2,    2,    2,
C     MC=WL,OS=MH,OH=WL
     &    2,   11,    2,  6*0/
C
C----------
C    
      SELECT CASE (VARACD)
C  ORIGINAL 11 SPECIES VARIANTS
      CASE('KT','NC')
        MAPISP=MAPNI
      CASE('AK')
        MAPISP=MAPAK
      CASE('BM')
        MAPISP=MAPBM
C
C  SPECIES 4=GF IN OC VARIANT SO USING CA MAPPING IS OKAY FOR OC 
C
      CASE('CA','OC')
        MAPISP=MAPCA
      CASE('CI')
        MAPISP=MAPCI
      CASE('CR')
        MAPISP=MAPCR
      CASE('EC')
        MAPISP=MAPEC
      CASE('EM')
        MAPISP=MAPEM
      CASE('IE')
        MAPISP=MAPIE
      CASE('SO')
        MAPISP=MAPSO
      CASE('TT')
        MAPISP=MAPTT
      CASE('UT')
        MAPISP=MAPUT
C
C  SPECIES 23=MA, 24=TO, 25=GC IN OP VARIANT; ALL MAP TO WL. SO USING 
C  WC MAPPING IS OKAY FOR OP 
C
      CASE('WC','PN','OP')
        MAPISP=MAPWC
      CASE('WS')
        MAPISP=MAPWS
      CASE DEFAULT
        MAPISP=1
      END SELECT
C----------
C  CHECK FOR DEBUG.
C----------
      CALL DBCHK(DEBUG,'CVSHAP',6,ICYC)
      IF (DEBUG) WRITE (JOSTND,7001) ICYC
 7001 FORMAT (/'**CALLING CVSHAP, CYCLE = ',I2,/,
     &'   I  ISPI  DBH        HT        CR        CL       RAD',
     &'       TPA                          SCORE(J)              ',
     &'            ISHAPE')
C----------
C  RETURN IF NOTREES OPTION IN EFFECT.
C----------
      IF (ITRN .GT. 0) GO TO 5
      IF (DEBUG) WRITE (JOSTND,9001) ITRN
 9001 FORMAT ('ITRN =', I5,' : NOTREES : RETURN TO **CVCNOP**')
      RETURN
    5 CONTINUE
C----------
C  SET PRE-THIN TREES PER ACRE IF THINNING HAS JUST OCCURRED.
C---------
      TPA=TPROB
      IF (LTHIN) TPA=OLDTPA
C----------
C  ENTER TREE LOOP.
C----------
      DO 100 I=1,ITRN
      ISPI = MAPISP(ISP(I))
      RAD = TRECW(I)*.5
      CR = ICR(I)/100.
      CL = CR*HT(I)
C----------
C  COMPUTE DISCRIMINANT SCORE FOR EACH SHAPE, AND ASSIGN SHAPE TO
C  HIGHEST SCORE.
C----------
      SCORM1 = -999.
      DO 10 J=1,5
      SCORE(J) = CONST(J,ISPI) +BCR(J,ISPI)*CR + BHT(J,ISPI)*HT(I) +
     &        BCL(J,ISPI)*CL + BRAD(J,ISPI)*RAD +
     &        BDBH(J,ISPI)*DBH(I) + BTPA(J,ISPI)*TPA
      IF (SCORE(J) .LE. SCORM1) GO TO 10
      ISHAPE(I)=J
      SCORM1 = SCORE(J)
   10 CONTINUE
C
      IF (DEBUG) WRITE (JOSTND,7000) I,ISPI,DBH(I),HT(I),CR,
     &     CL,RAD,TPA,(SCORE(J),J=1,5),ISHAPE(I)
 7000 FORMAT (I7,I3,F6.2,5F10.2,5F12.4,I5)
  100 CONTINUE
      RETURN
      END
