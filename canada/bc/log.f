      SUBROUTINE LOG(IO,IZ,IM,DBH,HT,SH,TD,GOL,NL,VLOG,TDL,HLL,DBT,TVOL,
     1               GROS,BAR,HTRUNC)
C----------
C CANADA-BC $Id$
C----------
C
CC         ***** V E R S I O N    # 4.1   F E B R U A R Y    1 9 9 4 *****
CC
CC   ** A  V A R I A B L E - E X P O N E N T  T A P E R  E Q U A T I O N **
CC
CC                      ***** B Y   A.  K O Z A K *****
CC
CC                            ***** 1 9 8 9 *****
CC
CC IO=SPECIES CODE (1-16),  IZ=ZONE CODE (1-3), IM = 1=IMMATURE, 2=MATURE
CC
CC  SPECIES CODE       1=F        2=C       3=H       4=B
CC  AND ORDER          5=S        6=CY      7=PW      8=PL
CC                     9=PY      10=L      11=CT     12=D
CC                    13=MB      14=BI     15=A      16=WP
CC
CC ZONE CODE  1=A,B,C   2=D,E,F,G,H,I,J   3=K,L
CC
CC DBH=DIAMETER OUTSIDE BARK IN CM.  HT=TOTAL HEIGHT IN M.  SH=STUMP HEIGHT
CC IN M.  TD=TOP DIAMETER FOR UTILIZATION IN CM.  GOL=LOG LENGTH IN M.
CC NL=NO. OF LOGS.
CC VOLUMES AND TOP DIAMETERS CALCULATED HERE ARE METRIC
CC VLOG=VOLUMES FOR EACH LOG (40),   TDL=TOP DIAMETERS FOR EACH LOG (40)
CC HLL=LENGTH OF TOP LOG,   DBT=BUTT DIAMETER OF FIRST LOG
CC TVOL=VOLUME BETWEEN STUMP HT AND TOP DIAMETER FOR UTILIZATION
CC GROS=TOTAL VOLUME FROM GROUND TO TOP
CC BAR=BARK THICKNESS ***** NOT AVAILABLE IN THIS PROGRAM
C
      REAL VLOG(50),TDL(50),PP(16,3),A1(16,3),A2(16,3),A3(16,3)
      REAL A4(16,3),A5(16,3),A6(16,3),A7(16,3),A8(16,3),AA(16,3,8)
      REAL B(8)
      INTEGER*2 IO,IZ,IM,NL
      COMMON/TAPER/ DH,DDD,HH,PER,B,FF
      EQUIVALENCE (AA(1,1,1),A1(1,1)),(AA(1,1,2),A2(1,1)),
     1            (AA(1,1,3),A3(1,1)),(AA(1,1,4),A4(1,1)),
     2            (AA(1,1,5),A5(1,1)),(AA(1,1,6),A6(1,1)),
     3            (AA(1,1,7),A7(1,1)),(AA(1,1,8),A8(1,1))
C
CC  A1 ..... A8 ARE THE EIGHT COEFFICIENTS FOR THE TAPER EQUATION
CC  FIRST 4 LINES - ZONES A,B,C     LINES 4-7 - ZONES D-J
CC  LINES 8-B - ZONES K,L **** ORDER OF SPECIES AS ABOVE
CC  SEE DESCRIPTION OF THE EQUATION FOR DETAILS.
C
      DATA A1/  1.012675,  1.218296,  0.830874,  0.988964,
     1          0.924126,  0.928138,  0.868943,  0.774601,
     2          0.856592,  0.746827,  0.802839,  0.719188,
     3          1.097880,  0.648830,  0.855966,  1.078961,
C
     4          0.920840,  1.033575,  0.752027,  1.008741,
     5          0.897311,  0.928138,  0.984019,  0.774601,
     6          0.856592,  0.746827,  0.802839,  0.719188,
     7          1.097880,  0.648830,  0.855966,  1.078961,
C
     8          0.920840,  1.033575,  0.752027,  0.764353,
     9          0.897595,  0.928138,  0.984019,  0.793793,
     A          0.856592,  1.164819,  0.852579,  0.719188,
     B          1.097880,  0.633306,  0.891641,  1.078961/
C
      DATA A2/  0.899136,  0.855983,  1.005210,  0.951803,
     1          0.950707,  0.945293,  0.976312,  1.040320,
     2          0.936402,  1.003900,  0.993776,  1.052190,
     3          0.840504,  1.121390,  0.987014,  0.894083,
C
     4          0.923867,  0.896971,  1.028970,  0.916357,
     5          0.957090,  0.945293,  0.941322,  1.040320,
     6          0.936402,  1.003900,  0.993776,  1.052190,
     7          0.840504,  1.121390,  0.987014,  0.894083,
C
     8          0.923867,  0.896971,  1.028970,  1.053220,
     9          0.957499,  0.945293,  0.941322,  1.049320,
     A          0.936402,  0.831995,  0.952969,  1.052190,
     B          0.840504,  1.110510,  0.957835,  0.894083/
C
      DATA A3/  1.000123,  0.999921,  0.999142,  0.999789,
     1          0.999518,  0.999206,  0.999773,  0.996984,
     2          1.002104,  0.997233,  0.998974,  0.997551,
     3          1.006569,  0.992077,  0.999828,  1.001749,
C
     4          1.000568,  0.999079,  0.998660,  1.001159,
     5          0.999370,  0.999206,  0.999700,  0.996984,
     6          1.002104,  0.997233,  0.998974,  0.997551,
     7          1.006569,  0.992077,  0.999828,  1.001749,
C
     8          1.000568,  0.999079,  0.998660,  0.994711,
     9          0.998952,  0.999206,  0.999700,  0.995709,
     A          1.002104,  1.003909,  1.000477,  0.997551,
     B          1.006569,  0.994733,  1.001450,  1.001749/
C
      DATA A4/  0.968978,  2.037620,  1.770670,  2.336270,
     1          1.750510,  0.301423,  1.676930,  0.745750,
     2          0.566217,  0.747048,  0.706093,  0.599235,
     3          0.981297,  0.865974,  0.424473,  1.377540,
C
     4          1.095560,  1.598260,  1.174800,  1.415990,
     5          1.532270,  0.301423,  1.571030,  0.745750,
     6          0.566217,  0.747048,  0.706093,  0.599235,
     7          0.981297,  0.865974,  0.424473,  1.377540,
C
     8          1.095560,  1.598260,  1.174800,  1.381630,
     9          1.110150,  0.301423,  1.571030,  0.583403,
     A          0.566217,  1.880250,  0.731911,  0.599235,
     B          0.981297,  1.021680,  0.695143,  1.377540/
C
      DATA A5/ -0.190913, -0.486492, -0.329190, -0.502311,
     1         -0.408021, -0.040792, -0.372195, -0.130177,
     2         -0.087141, -0.133729, -0.096789, -0.033036,
     3         -0.222619, -0.106757, -0.037553, -0.286807,
C
     4         -0.202191, -0.411541, -0.263576, -0.325671,
     5         -0.364679, -0.040792, -0.369344, -0.130177,
     6         -0.087141, -0.133729, -0.096789, -0.033036,
     7         -0.222619, -0.106757, -0.037553, -0.286807,
C
     8         -0.202191, -0.411541, -0.263576, -0.306534,
     9         -0.281544, -0.040792, -0.369344, -0.077654,
     A         -0.087141, -0.401856, -0.084192, -0.033036,
     B         -0.222619, -0.141481, -0.039652, -0.286807/
C
      DATA A6/  0.825961,  2.632080,  2.185610,  4.154490,
     1          2.659000, -1.235630,  2.567510,  0.558818,
     2         -0.063450,  0.397110,  0.312724, -0.261339,
     3          1.428220,  0.257139, -0.517540,  1.038780,
C
     4          0.967329,  2.402420,  2.233330,  2.793270,
     5          2.741210, -1.235630,  2.703200,  0.558818,
     6         -0.063450,  0.397110,  0.312724, -0.261339,
     7          1.428220,  0.257139, -0.517540,  1.038780,
C
     8          0.967329,  2.402420,  2.233330,  2.637080,
     9          2.125100, -1.235630,  2.703200, -0.036267,
     A         -0.063450,  3.082780,  0.196339, -0.261339,
     B          1.428220,  0.641499, -0.603404,  1.038780/
C
      DATA A7/  0.048766,  0.109094,  0.105050,  0.086560,
     1          0.092651,  0.030743,  0.071928,  0.198687,
     2          0.071720,  0.078345,  0.119634,  0.215536,
     3          0.228560,  0.254574,  0.102211,  0.072537,
C
     4          0.081696,  0.094283,  0.045184,  0.108427,
     5          0.117756,  0.030743,  0.049628,  0.198687,
     6          0.071720,  0.078345,  0.119634,  0.215536,
     7          0.228560,  0.254574,  0.102211,  0.072537,
C
     8          0.081696,  0.094283,  0.045184,  0.163996,
     9          0.148340,  0.030743,  0.049628,  0.142531,
     A          0.071720,  0.276300,  0.148285,  0.215536,
     B          0.228560,  0.258921,  0.193706,  0.072537/
C
      DATA A8/ -0.426214, -1.486550, -1.192970, -2.186240,
     1         -1.396760,  0.672879, -1.340830, -0.324178,
     2          0.051415, -0.183542, -0.080057,  0.123059,
     3         -0.654456, -0.149926,  0.303931, -0.647372,
C
     4         -0.514604, -1.252170, -1.002020, -1.326790,
     5         -1.362760,  0.672879, -1.334700, -0.324178,
     6          0.051415, -0.183542, -0.080057,  0.123059,
     7         -0.654456, -0.149926,  0.303931, -0.647372,
C
     8         -0.514604, -1.252170, -1.002020, -1.292080,
     9         -1.005560,  0.672879, -1.334700, -0.022523,
     A          0.051415, -1.661550, -0.069852,  0.123059,
     B         -0.654456, -0.359508,  0.209159, -0.647372/
C
C
CC  PP CONTAINS THE PROPORTIONAL HEIGHT OF INFLECTION POINTS
CC  PPI SAME AS PP FOR IMMATURE SPECIES
C
      DATA PP/     0.25,         0.25,       0.20,         0.25,
     1             0.25,         0.30,       0.25,         0.25,
     2             0.25,         0.30,       0.25,         0.30,
     3             0.25,         0.25,       0.20,         0.25,
C
     4             0.25,         0.30,       0.25,         0.30,
     5             0.30,         0.30,       0.25,         0.25,
     6             0.25,         0.30,       0.25,         0.30,
     7             0.25,         0.25,       0.20,         0.25,
C
     8             0.25,         0.30,       0.25,         0.30,
     9             0.30,         0.30,       0.25,         0.25,
     A             0.25,         0.30,       0.25,         0.30,
     B             0.25,         0.25,       0.20,         0.25/
C
      DATA CONS/0.00007854/
C
      EX(D4,D5,D6,D7,D8,D,E)=D4*D**2+D5*ALOG(D+0.001)+D6*SQRT(D)+
     1D7*E+D8*EXP(D)
C
CC  FIND COEFFICIENTS BY SPECIES AND MATURITY CLASS
C
      IS=IO
c     IZ=IM


      ZZZ=PP(IS,IZ)
      DO 11 I=1,8
11    B(I)=AA(IS,IZ,I)
C
CC  CALCULATE DIB AT STUMP HEIGHT
C
2     PER=1.0-SQRT(ZZZ)
      DH=DBH/HT
      FF=B(1)*DBH**B(2)*B(3)**DBH
      DDD=DBH
      HH=HT
      Y=SH/HT
      X=(1.0-SQRT(Y))/PER
c>
c>    IF (X .LE. 0.0) RETURN
c>
      EXPO=EX(B(4),B(5),B(6),B(7),B(8),Y,DH)
      DBT=FF*X**EXPO
      DO 8 I=1,40
      TDL(I)=0.
8     VLOG(I)=0.
      HLL=0.
      TVOL=0.
      GROS=0.
      BAR=0.
      NL=0
      DI3=DBT
      HM=SH
C
CC    calculate the diameter at the point of top kill (if any)
C
	DIAM = 0.0
      IF (HTRUNC .LT. HT) THEN
        Y=HTRUNC/HT
        X=(1-SQRT(Y))/PER
        EXPO=EX(B(4),B(5),B(6),B(7),B(8),Y,DH)
        DIAM=FF*X**EXPO
      ENDIF
C
CC  RETURN ZERO VALUES IF THERE IS NO MERCH VOLUME
C
      IF (DBT.LE.TD) GO TO 10
C
CC  CALCULATE MERCH HEIGHT
C
      X=0.9
      NN=0
3     EXPO=EX(B(4),B(5),B(6),B(7),B(8),X,DH)
      Y=(TD/FF)**(1.0/EXPO)
      Y=(1.0-Y*PER)**2
      IF (ABS(X-Y).LT.0.0001) GO TO 4
      X=X+(Y-X)/2.0
        IF (X.GT.1.0) X=(SH/HT)*(DI3/TD)
      NN=NN+1
      IF (NN.GT.9) GO TO 4
      GO TO 3
4     HM=X*HT
      IF (HM.LT.SH) HM=SH+0.01
c>        
      IF (HTRUNC .LT. HM) HM=HTRUNC
c>      
      NL=(HM-SH)/GOL+1.0
      HLL=HM-(NL-1.0)*GOL-SH
C
CC  CALCULATE TOP DIAMETERS AND VOLUMES FOR EACH LOG
C
      X1=SH
      K1=1
       IF (X1.GE.0.3) GO TO 9
      X1=X1+GOL
      X2=0.3
      IF (X1.GT.HM) X1=HM
      K1=2
       Y=X2/HT
       X=(1.0-SQRT(Y))/PER
       EXPO=EX(B(4),B(5),B(6),B(7),B(8),Y,DH)
       DI3=FF*X**EXPO
      Y=X1/HT
       X=(1.0-SQRT(Y))/PER
       EXPO=EX(B(4),B(5),B(6),B(7),B(8),Y,DH)
       TDL(1)=FF*X**EXPO
       VLOG(1)=CONS*DI3**2*(0.3-SH)+VLM(X2,X1,DI3,TDL(1))
      TVOL=TVOL+VLOG(1)
      IF (NL.LT.2) GO TO 15
9     DO 5 I=K1,NL
      X1=X1+GOL
      X2=X1-GOL
      IF (X1.GT.HM) X1=HM
      Y=X1/HT
      X=(1.0-SQRT(Y))/PER
      EXPO=EX(B(4),B(5),B(6),B(7),B(8),Y,DH)
      TDL(I)=FF*X**EXPO
      D2=TDL(I)
      IF (I.LT.2) GO TO 13
      D1=TDL(I-1)
      GO TO 14
13    D1=DI3
       IF (SH.GE.0.3) D1=DBT
14    VLOG(I)=VLM(X2,X1,D1,D2)
5     TVOL=TVOL+VLOG(I)
15    TDL(NL)=TD
C
CC  CALCULATE VOLUMES FOR STUMP AND TOP
C
10    IF (SH.LE.0.3) GO TO 6
      HL=0.3
      HU=SH
       Y=0.3/HT
       X=(1.0-SQRT(Y))/PER
       EXPO=EX(B(4),B(5),B(6),B(7),B(8),Y,DH)
       DI3=FF*X**EXPO
      STMV=CONS*DI3**2*0.3+VLM(HL,HU,DI3,DBT)
      GO TO 7
6     STMV=CONS*DI3**2*SH
7    	DD=TD
      IF (DI3.LT.TD) DD=DI3
c>      
      TOPV = 0.0
      IF (HM .NE. HTRUNC) TOPV=VLM(HM,HTRUNC,DD,DIAM)
c>      
      
C
CC  CALCULATE TOTAL VOLUME FOR THE TREE
C
      GROS=TVOL+STMV+TOPV
      RETURN
      END
      FUNCTION VLM(HL,HU,D1,D2)
C
CC  CALCULATE VOLUME FOR LOGS BY NEWTON'S FORMULA
C
      REAL D(7),B(8)
      COMMON/TAPER/ DH,DBH,HT,PER,B,FF
      DATA CONS/0.00005236/
      EE(D4,D5,D6,D7,D8,X,E)=D4*X**2+D5*ALOG(X+0.001)+D6*SQRT(X)+
     1D7*E+D8*EXP(X)
      VLM=0.
      DIS=HU-HL
      IF (DIS.LT.0.01) RETURN
      N=4
      IF (DIS.GT.5.01) N=6
      D(1)=D1
      D(N+1)=D2
      DS=DIS/N
      X1=HL
      DO 1 I=2,N
      X1=X1+DS
      Y=X1/HT
      EXPO=EE(B(4),B(5),B(6),B(7),B(8),Y,DH)
      X=(1.0-SQRT(Y))/PER
1     D(I)=FF*X**EXPO
       M=N+1
      DO 2 I=1,M
2     D(I)=D(I)*D(I)
      IF (N.GT.4) GO TO 3
      VLM=CONS*DS*(D(1)/2.+2.*D(2)+D(3)+2.*D(4)+D(5)/2.)
      RETURN
3     VLM=CONS*DS*(D(1)/2.+2.*D(2)+D(3)+2.*D(4)+D(5)+2.*D(6)+D(7)/2.)
      RETURN
      END
