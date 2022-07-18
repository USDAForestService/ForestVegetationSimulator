      SUBROUTINE DMSHAP(DMTRCW, IDMSHP)
      IMPLICIT NONE
C----------
C CANADA-NEWMIST $Id$
C----------
C **DMSHAP -- NISI Date of last revision: 08/05/94
C This module was made by modifying the COVER MODEL module CVSHAP
C Modified for Central Rockies.
C--------------------------------------------------------------------
C Purpose:
C   This routine is one of three routines taken from the COVER
C model (the other two are DMCW-- and DMSUM), which together estimate
C the radius and volume of treelist records. This routine is
C responsible for determining the geometrical shape of the tree from
C among 5 possible shapes. The information derived here is used in
C the DMSUM subroutine.
C
C SUBROUTINE DMSHAP ASSIGNS EACH TREE CROWN TO ONE OF FIVE SHAPES.
C SHAPES ARE:
C
C     ISHAPE(I)    PLANE SHAPE     SOLID FORM
C     ---------    -----------     ----------
C          1       CIRCLE          SPHERE
C          2       TRIANGLE        CONE
C          3       NEILOID         NEILOID
C          4       PARABOLA        PARABOLOID
C          5       ELLIPSE         ELLIPSOID
C
C THE CLASSIFICATION FUNCTION COEFFICIENTS WERE ESTIMATED USING
C FISHER'S LINEAR DISCRIMINANT FUNCTIONS, INDIVIDUALLY FOR SPECIES.
C
C FOR DOCUMENTATION SEE: MOEUR, M. MODELS FOR PREDICTING CROWN SHAPES
C OF NORTHERN ROCKY MOUNTAIN CONIFERS. REPORT ON FILE, NOVEMBER 1983.
C--------------------------------------------------------------------
C
C Called by:
C
C     DMMTRX 
C
C Other routines called:
C
C     DBCHK
C
C Argument list definitions:
C
C     REAL     DMTRCW (I) Estimated tree crown diameter (feet).
C     INTEGER  IDMSHP (O) Tree crown shape category.
C
C Local variable definitions (not complete):
C
C     REAL     CONST      COEFFICIENTS.
C     REAL     BCL        COEFFICIENTS.
C     REAL     BCR        COEFFICIENTS.
C     REAL     BRAD       COEFFICIENTS.
C     REAL     BDBH       COEFFICIENTS.
C     REAL     BHT        COEFFICIENTS.
C     REAL     BTPA       COEFFICIENTS.
C     REAL     CL         CROWN LENGTH.
C     REAL     CR         CROWN RATIO.
C     REAL     RAD        CROWN RADIUS.
C     REAL     TPA        TREES PER ACRE.
C     REAL     SCORE      DISCRIMINANT FUNCTION SCORE.
C
C Common block variables and parameters:
C
C     [none related to NISI; FVS commons are not documented]
C
C**********************************************************************      

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'DMCOM.F77'

C Argument list variables.

      REAL      DMTRCW
      INTEGER   IDMSHP

      DIMENSION DMTRCW(MAXTRE)
      DIMENSION IDMSHP(MAXTRE)

C Local variables.

      LOGICAL DEBUG !, LTHIN
      REAL    CONST(5,11),BCR(5,11),BHT(5,11),BRAD(5,11),
     &        BDBH(5,11),BCL(5,11),BTPA(5,11),SCORE(5)
      INTEGER MAPISP(49),MAPAK(49),MAPCA(49),MAPBM(49),MAPCR(49),
     &        MAPEM(49),MAPIE(49),MAPNI(49),MAPSO(49),
     &        MAPTT(49),MAPUT(49),MAPWC(49),MAPBC(49)
      INTEGER I,ISPI,J
      REAL    TPA,RAD,CR,CL,SCORM1

C Data assigments for coefficients of discriminant function.
C      SHAPE      1       2       3       4      5
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
C  THE FOLLOWING MAPPING SHOULD BE ADJUSTED HERE AND IN **CVCBMS**
C----------
      DATA MAPBC /
C     PW=WP,LW=WL,FD=DF,BF=GF,HW=WH,CW=RC,PL=LP,SE=ES,BL=AF,PY=PP
     &    1,    2,    3,    4,    5,    6,    7,    8,    9,   10,
C     EP=RC,AT=RC,AC=RC,OC=DF,OH=RC
     &    6,    6,    6,    3,    6, 34*0/

      DATA MAPNI /
C     WP=WP,WL=WL,DF=DF,GF=GF,WH=WH,RC=RC,LP=LP,ES=ES,AF=AF,PP=PP
     &    1,    2,    3,    4,    5,    6,    7,    8,    9,   10,
C     MH=MH
     &   11, 38*0/
C
      DATA MAPAK /
C     WS=ES,RC=RC,SF=AF,MH=OT,WH=WH,YC=RC,LP=LP,SS=ES,AF=AF,RA=RC
     &    8,    6,    9,   11,    5,    6,    7,    8,    9,    6,
C     CW=RC,OH=RC,OS=DF
     &    6,    6,    3, 36*0/
C
      DATA MAPBM /
C     WP=WP,WL=WL,DF=DF,GF=GF,MH=WH,WJ=LP,LP=LP,ES=ES,AF=AF,PP=PP
     &    1,    2,    3,    4,    5,    7,    7,    8,    9,   10,
C     WB=AF,LM=WP,PY=AF,YC=ES,AS=RC,CW=RC,OS=RC,OH=RC
     &    9,    1,    9,    8,    6,    6,    6,    6, 31*0/
C
      DATA MAPCA /
C     PC=DF,IC=DF,RC=RC,WF=GF,RF=GF,SH=OT,DF=DF,WH=WH,MH=OT,WB=AF
     &    3,    3,    6,    4,    4,   11,    3,    5,   11,    9,
C     KP=PP,LP=LP,CP=PP,LM=PP,JP=PP,SP=PP,WP=WP,PP=PP,MP=PP,GP=PP
     &   10,    7,   10,   10,   10,   10,    1,   10,   10,   10,
C     WJ=AF,BR=ES,GS=PP,PY=AF,OS=DF,LO=RC,CY=RC,BL=RC,EO=RC,WO=RC
     &    9,    8,   10,    9,    3,    6,    6,    6,    6,    6,
C     BO=RC,VO=RC,IO=RC,BM=RC,BU=RC,RA=RC,MA=RC,GC=RC,DG=RC,FL=RC
     &    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
C     WN=RC,TO=RC,SY=RC,AS=RC,CW=RC,WI=RC,CN=RC,CL=RC,OH=RC      
     &    6,    6,    6,    6,    6,    6,    6,    6,    6/
C
      DATA MAPCR /
C     AF=AF,CB=AF,DF=DF,GF=GF,WF=GF,MH=OT,RC=RC,WL=WL,BC=PP,LM=WP
     &    9,    9,    3,    4,    4,   11,    6,    2,   10,    1,
C     LP=LP,PI=LP,PP=PP,WB=AF,SW=WP,UJ=AF,BS=ES,ES=ES,WS=ES,AS=RC
     &    7,    7,   10,    9,    1,    9,    8,    8,    8,    6,
C     NC=RC,PW=RC,GO=RC,AW=RC,EM=RC,BK=RC,SO=RC,PB=RC,AJ=AF,RM=AF
     &    6,    6,    6,    6,    6,    6,    6,    6,    9,    9,
C     OJ=AF,ER=AF,PM=LP,PD=LP,AZ=LP,CI=PP,OS=DF,OH=RC
     &    9,    9,    7,    7,    7,   10,    3,    6, 11*0/
C
C
      DATA MAPEM /
C     WB=WL,WL=WL,DF=DF,LM=OT,LL=AF,RM=OT,LP=LP,ES=ES,AF=AF,PP=PP
     &    2,    2,    3,   11,    9,   11,    7,    8,    9,   10,
C     GA=OT,AS=OT,CW=OT,BA=OT,PW=OT,NC=OT,PB=OT,OS=OT,OH=OT
     &   11,   11,   11,   11,   11,   11,   11,   11,   11, 30*0/
C
C
      DATA MAPIE /
C     WP=WP,WL=WL,DF=DF,GF=GF,WH=WH,RC=RC,LP=LP,ES=ES,AF=AF,PP=PP
     &    1,    2,    3,    4,    5,    6,    7,    8,    9,   10,
C     MH=OT,WB=WL,LM=OT,LL=OT,PI=OT,RM=OT,PY=OT,AS=OT,CO=OT,MM=OT
     &   11,    2,   11,   11,   11,   11,   11,   11,   11,   11,
C     PB=OT,OH=OT,OS=OT
     &   11,   11,   11, 26*0/
C
C 
       DATA MAPSO /
C     WP=WP,SP=PP,DF=DF,WF=GF,MH=OT,IC=RC,LP=LP,ES=ES,SH=OT,PP=PP
     &    1,   10,    3,    4,   11,    6,    7,    8,   11,   10,
C     WJ=LP,GF=GF,AF=AF,SF=AF,NF=AF,WB=AF,WL=WL,RC=RC,WH=WH,PY=AF
     &    7,    4,    9,    9,    9,    9,    2,    6,    5,    9,
C     WA=RC,RA=RC,BM=RC,AS=RC,CW=RC,CH=RC,WO=RC,WI=RC,GC=RC,MC=RC
     &    6,    6,    6,    6,    6,    6,    6,    6,    6,    6,
C     MB=RC,OS=DF,OH=RC
     &    6,    3,    6, 16*0/
C
      DATA MAPTT /
C     WB=WP,LM=WL,DF=DF,PM=LP,BS=ES,AS=RC,LP=LP,ES=ES,AF=AF,PP=PP
     &    1,    2,    3,    7,    8,    6,    7,    8,    9,   10,
C     UJ=AF,RM=AF,BI=RC,MM=OT,NC=RC,MC=RC,OS=MH,OH=RC
     &    9,    9,    6,   11,    6,    6,   11,    6, 31*0/
C
C
      DATA MAPUT /
C     WB=AF,LM=WP,DF=DF,WF=GF,BS=ES,AS=RC,LP=LP,ES=ES,AF=AF,PP=PP,
     &    9,    1,    3,    4,    8,    6,    7,    8,    9,   10,
C     PI=LP,WJ=AF,GO=RC,PM=LP,RM=AF,UJ=AF,GB=PP,NC=RC,FC=RC,MC=RC,
     &    7,    9,    6,    7,    9,    9,   10,    6,    6,    6,
C    &BI=RC,BE=RC,OS=RC,OH=RC
     &    6,    6,    6,    6, 25*0/
C
      DATA MAPWC /
C     SF=AF,WF=AF,GF=GF,AF=AF,RF=GF,--=OT,NF=AF,YC=ES,IC=ES,ES=ES
     &    9,    9,    4,    9,    4,   11,    9,    8,    8,    8,
C     LP=LP,JP=PP,SP=PP,WP=WP,PP=PP,DF=DF,RW=PP,RC=RC,WH=WH,MH=OT
     &    7,   10,   10,    1,   10,    3,   10,    6,    5,   11,
C     BM=RC,RA=RC,WA=RC,PB=RC,GC=RC,AS=RC,CW=RC,WO=RC,WJ=LP,LL=WL
     &    6,    6,    6,    6,    6,    6,    6,    6,    7,    2,
C     WB=AF,KP=PP,PY=AF,DG=RC,HT=RC,CH=RC,WI=RC,--=OT,OT=RC
     &    9,   10,    9,    6,    6,    6,    6,   11,    6, 10*0/

C    
      SELECT CASE (VARACD)
      CASE('BC')
        MAPISP=MAPBC
C  ORIGINAL 11 SPECIES VARIANTS
      CASE('CI','EC','KT','NC','NI','WS')
        MAPISP=MAPNI
      CASE('AK')
        MAPISP=MAPAK
      CASE('BM')
        MAPISP=MAPBM
      CASE('CA')
        MAPISP=MAPCA
      CASE('SM','SP','BP','SF','LP')
        MAPISP=MAPCR
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
      CASE('WC','PN')
        MAPISP=MAPWC
      END SELECT

C----------
C  CHECK FOR DEBUG.
C----------

      CALL DBCHK(DEBUG,'DMSHAP',6,ICYC)
      IF (DEBUG) WRITE (JOSTND,7001) ICYC
 7001 FORMAT (/' **CALLING DMSHAP, CYCLE = ',I2/
     &'    I  ISPI  DBH        HT        CR        CL       RAD',
     &'       TPA                          SCORE(J)              ',
     &'           IDMSHP')
     
C----------
C  RETURN IF NOTREES OPTION IN EFFECT.
C----------

      IF (ITRN .GT. 0) GO TO 5
      IF (DEBUG) WRITE (JOSTND,9001) ITRN
 9001 FORMAT (' ITRN =', I5,' : NOTREES : RETURN TO **DMMTRX**')
      RETURN
    5 CONTINUE
    
C----------
C  SET PRE-THIN TREES PER ACRE IF THINNING HAS JUST OCCURRED.
C---------

c      TPA = TPROB
c      IF (LTHIN) TPA=OLDTPA
       TPA = MAX(TPROB,OLDTPA)

C----------
C  ENTER TREE LOOP.
C----------

      DO 100 I = 1,ITRN
        ISPI = MAPISP(ISP(I))
        RAD = DMTRCW(I)*.5
        CR = ICR(I)/100.
        CL = CR*HT(I)
        
C----------
C  COMPUTE DISCRIMINANT SCORE FOR EACH SHAPE, AND ASSIGN SHAPE TO
C  HIGHEST SCORE.
C----------

        SCORM1 = -999.
        DO 10 J = 1,5
          SCORE(J) = CONST(J,ISPI) +BCR(J,ISPI)*CR +
     >      BHT(J,ISPI)*HT(I) + BCL(J,ISPI)*CL + BRAD(J,ISPI)*RAD +
     >      BDBH(J,ISPI)*DBH(I) + BTPA(J,ISPI)*TPA
          IF (SCORE(J) .LE. SCORM1) GO TO 10
          IDMSHP(I) = J
          SCORM1 = SCORE(J)
   10   CONTINUE
C
        IF (DEBUG) WRITE (JOSTND,7000) I,ISPI,DBH(I),HT(I),CR,
     &     CL,RAD,TPA,(SCORE(J),J = 1,5),IDMSHP(I)
 7000   FORMAT (I7,I3,F6.2,5F10.2,5F12.4,I5)
C
  100 CONTINUE
  
      RETURN
      END
