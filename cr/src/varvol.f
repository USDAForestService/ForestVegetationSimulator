        SUBROUTINE VARVOL
        IMPLICIT NONE
C----------
C CR $Id$
C----------
C
C  THIS SUBROUTINE CALLS THE APPROPRIATE VOLUME CALCULATION ROUTINE
C  FROM THE NATIONAL CRUISE SYSTEM VOLUME LIBRARY FOR METHB OR METHC
C  EQUAL TO 6.  IT ALSO CONTAINS ANY OTHER SPECIAL VOLUME CALCULATION
C  METHOD SPECIFIC TO A VARIANT (METHB OR METHC = 8)
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'VOLSTD.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'GGCOM.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
COMMONS
C
C----------
      REAL LOGLEN(20),BOLHT(21),TVOL(15)
      INTEGER UNT
      LOGICAL DEBUG,TKILL,CTKFLG,BTKFLG,LCONE
      CHARACTER CTYPE*1,FORST*2,HTTYPE,PROD*2
      CHARACTER*10 EQNC,EQNB
C
      REAL BN(10),B0(10),B1(10),B2(10),F0(10),F1(10),
     &   F2(10),F3(10),E0(10),E1(10),E2(10),E3(10),A0(10),A1(10)
C
      INTEGER IR,I1,I2,IFIASP,IFC,IREGN,I02,I03,I04,I05,ISPC,INTFOR
      INTEGER IERR,IZERO,I01,IT,ITRNC,I0,IHTLL,IEQN,ERRFLAG
      REAL HT1PRD,VU,X,VMR,RIC,VI,RSI
      REAL BBFV,VM,VN,DBT,X01,X02,X03,X04,X05,X06,X07,X08,X09,X010
      REAL X011,X012,FC,DBTBH,TVOL1,TVOL4,X0,TDIBB,TDIBC,BRATIO
      REAL VMAX,BARK,H,D,TOPDIB,DRC,TDIB
C----------
C  SPECIES ORDER:
C   1=AF,  2=CB,  3=DF,  4=GF,  5=WF,  6=MH,  7=RC,  8=WL,  9=BC, 10=LM,
C  11=LP, 12=PI, 13=PP, 14=WB, 15=SW, 16=UJ, 17=BS, 18=ES, 19=WS, 20=AS,
C  21=NC, 22=PW, 23=GO, 24=AW, 25=EM, 26=BK, 27=SO, 28=PB, 29=AJ, 30=RM,
C  31=OJ, 32=ER, 33=PM, 34=PD, 35=AZ, 36=CI, 37=OS, 38=OH
C
C  SPECIES EXPANSION:
C  UJ,AJ,RM,OJ,ER USE CR JU                              
C  NC,PW USE CR CO
C  GO,AW,EM,BK,SO USE CR OA                             
C  PB USES CR AS                              
C  PM,PD,AZ USE CR PI
C  CI USES CR PP                              
C----------
C SOUTHWESTERN TREE VOLUME EQUATIONS (HANN AND BARE, 1978, INT-209)
C GROSS T.C.F. COEFFICIENTS C.F. TABLE 2, PG.5
C
C ORDER OF COEFFICIENTS IN THESE ARRAYS:
C   1 = WHITE PINE
C   2 = ENGELMANN SPRUCE / CORKBARK FIR
C   3 = YELLOW PINE
C   4 = PONDEROSA PINE  (COCONINO, TONTO, LINCOLN)
C   5 = PONDEROSA PINE  (SANTA FE, CARSON)
C   6 = DOUGLAS-FIR     (LINCOLN, TONTO)
C   7 = DOUGLAS-FIR     (SANTA FE, CARSON)
C   8 = ASPEN
C   9 = WHITE FIR       (LINCOLN)
C  10 = WHITE FIR       (SANTA FE, CARSON)
C----------
      DATA A0/0.16089,0.22547,0.23720,0.08107,0.04831,0.43837,0.34113,
     &0.03270,0.21090,0.15778/
      DATA A1/0.0020325,0.0021697,0.0022112,0.0019835,0.0020497,
     &0.0017564,0.0019180,0.0023112,0.0018400,0.0020091/
C----------
C GROSS M.C.F. C.F. TABLE 4, PG. 7
C----------
      DATA BN/1.5,1.5,1.0,1.5,1.5,1.0,1.5,1.5,1.0,1.5/
      DATA B0/-0.21301,-0.26648,0.01855,-0.12535,-0.13397,-0.08315,
     &-0.18763,-0.23643,-0.18270,-0.18756/
      DATA B1/0.0049121,0.0061290,0.0007882,0.0036042,0.0065017,
     &0.0012190,0.0067187,0.0058021,0.0012482,0.0063265/
      DATA B2/0.0060606,0.0074311,0.0050551,0.0054063,0.0049022,
     &0.0054174,0.0053645,0.0060804,0.0062448,0.0060413/
C----------
C GROSS BD.FT. INT. C.F. TABLE 7, PG. 11
C----------
      DATA F0/6.69197,5.98736,7.10051,6.84752,7.58122,6.58735,6.59717,
     &6.68808,6.24688,5.73645/
      DATA F1/7.52011,9.84792,7.97922,7.69491,8.51941,0.89272,0.89405,
     &-1.27685,7.01994,1.72093/
      DATA F2/216.34837,-300.81281,229.55650,221.37723,245.09754,
     &243.51491,243.87797,-4.50480,201.95873,74.57379/
      DATA F3/0.0,2855.34246,0.0,0.0,0.0,0.0,0.0,1423.98524,0.0,0.0/
C----------
C GROSS BD. FT. SCR. C.F. TABLE 9, PG. 13
C----------
      DATA E0/1.00609,0.87845,0.98210,0.96579,0.99399,1.00090,0.87026,
     &0.88789,1.0,1.01725/
      DATA E1/2.38466,0.0,0.92603,0.40579,1.46349,0.0,0.0,0.0,1.88814,
     &1.87057/
      DATA E2/0.0,0.0,0.0,0.0,0.0,4.10007,0.0,0.0,0.0,0.0/
      DATA E3/0.0,15.99846,14.49444,16.93678,12.40585,0.0,19.49594,
     &17.19374,8.85145,8.51445/
C
C----------
C  NATIONAL CRUISE SYSTEM ROUTINES (METHOD = 6)
C----------
      ENTRY NATCRS (VN,VM,BBFV,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,
     1              CTKFLG,BTKFLG,IT)
C-----------
C  SEE IF WE NEED TO DO SOME DEBUG.
C-----------
      CALL DBCHK (DEBUG,'VARVOL',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,3)ICYC
    3 FORMAT(' ENTERING SUBROUTINE VARVOL CYCLE =',I5)
C
C----------
C  SET PARAMETERS
C----------
      INTFOR = KODFOR - (KODFOR/100)*100
      WRITE(FORST,'(I2)')INTFOR
      IF(INTFOR.LT.10)FORST(1:1)='0'
      HTTYPE='F'
      IERR=0
      DBT = D*(1-BARK)
      BARK=BRATIO(ISPC,D,H)
C----------
C  BRANCH TO R2 LOGIC, OR R3 LOGIC. ASSUME THAT
C  BF AND CF WILL BE R3 EQUATIONS
C----------
      IF((IFOR.LT.IGFOR).AND.(VEQNNC(ISPC)(1:1).NE.'3'))THEN
C----------
C
C
C  REGION 2 NATCRS LOGIC:
C
C  SET R2VOL PARAMETERS & CALL PROFILE TO COMPUTE R2 VOLUMES.
C----------
        IF(VEQNNC(ISPC).EQ.'          ') THEN
          VN=0.
          VMAX=0.
          VM=0.
          BBFV=0.
          GO TO 102
C
C
        ELSEIF(VEQNNC(ISPC)(8:10).EQ.'746')THEN
          DO 105 IZERO=1,15
          TVOL(IZERO)=0.
  105     CONTINUE
          DRC=0.
          IFC=0
          IF(DEBUG)WRITE(JOSTND,*)' CALLING R2OLDV VEQNNC(ISPC),UNT,H,',
     &    'D= ',VEQNNC(ISPC),UNT,H,D
          CALL R2OLDV(VEQNNC(ISPC),H,D,DRC,IFC,TVOL,IERR)
          IF(DEBUG)WRITE(JOSTND,*)' ATER R2OLDV TVOL= ',TVOL
C----------
C  SET RETURN VALUES.
C----------
          VN=TVOL(1)
          IF(VN.LT.0.)VN=0.
          VMAX=VN
          IF(D .LT. DBHMIN(ISPC))THEN
            VM = 0.
          ELSE
            VM=TVOL(4)
            IF(VM.LT.0.)VM=0.
          ENDIF
          IF(D.LT.BFMIND(ISPC))THEN
            BBFV=0.
          ELSE
            IF(METHB(ISPC).EQ.9) THEN
              BBFV=TVOL(10)
            ELSE
              BBFV=TVOL(2)
            ENDIF
            IF(BBFV.LT.0.)BBFV=0.
          ENDIF
          CTKFLG = .TRUE.
          BTKFLG = .TRUE.
          GO TO 102
C
C
        ELSE
          DO 103 IZERO=1,15
          TVOL(IZERO)=0.
  103     CONTINUE
          TOPDIB=TOPD(ISPC)*BARK
C----------
C  CALL TO VOLUME INTERFACE - PROFILE
C  CONSTANT INTEGER ZERO ARGUMENTS
C----------
          I01=0
          I02=0
          I03=0
          I04=0
          I05=0
C----------
C  CONSTANT REAL ZERO ARGUMENTS
C----------
          X01=0.
          X02=0.
          X03=0.
          X04=0.
          X05=0.
          X06=0.
          X07=0.
          X08=0.
          X09=0.
          X010=0.
          X011=0.
          X012=0.
C----------
C  CONSTANT CHARACTER ARGUMENTS
C----------
          CTYPE=' '
          PROD='  '
C----------
C  CONSTANT INTEGER ARGUMENTS
C----------
          I1= 1
          IREGN= 2
C
          IF(DEBUG)WRITE(JOSTND,*)' CALLING PROFILE CF ISPC,ARGS = ',
     &    ISPC,IREGN,FORST,VEQNNC(ISPC),TOPD(ISPC),STMP(ISPC),D,H,
     &    DBT,BARK
C
          CALL PROFILE (IREGN,FORST,VEQNNC(ISPC),TOPDIB,X01,STMP(ISPC),
     &    D,HTTYPE,H,I01,X02,X03,X04,X05,X06,X07,X08,X09,I02,DBT,
     &    BARK*100.,LOGDIA,BOLHT,LOGLEN,LOGVOL,TVOL,I03,X010,X011,
     &    I1,I1,I1,I04,I05,X012,CTYPE,I01,PROD,IERR)
C
          IF(D.GE.BFMIND(ISPC))THEN
            IF(IT.GT.0)HT2TD(IT,1)=X02
          ELSE
            IF(IT.GT.0)HT2TD(IT,1)=0.
          ENDIF
          IF(D.GE.DBHMIN(ISPC))THEN
            IF(IT.GT.0)HT2TD(IT,2)=X02
          ELSE
            IF(IT.GT.0)HT2TD(IT,2)=0.
          ENDIF        
C
          IF(DEBUG)WRITE(JOSTND,*)' AFTER PROFILE CF TVOL= ',TVOL
C----------
C  IF TOP DIAMETER IS DIFFERENT FOR BF CALCULATIONS, STORE APPROPRIATE
C  VOLUMES AND CALL PROFILE AGAIN.
C----------
          IF((BFTOPD(ISPC).NE.TOPD(ISPC)).OR.
     &       (BFSTMP(ISPC).NE.STMP(ISPC)).OR.
     &       (VEQNNB(ISPC).NE.VEQNNC(ISPC))) THEN
            TVOL1=TVOL(1)
            TVOL4=TVOL(4)
            DO 101 IZERO=1,15
            TVOL(IZERO)=0.
  101       CONTINUE
            TOPDIB=BFTOPD(ISPC)*BARK
C----------
C  CALL TO VOLUME INTERFACE - PROFILE
C  CONSTANT INTEGER ZERO ARGUMENTS
C----------
            I01=0
            I02=0
            I03=0
            I04=0
            I05=0
C--------  --
C  CONSTANT REAL ZERO ARGUMENTS
C----------
            X01=0.
            X02=0.
            X03=0.
            X04=0.
            X05=0.
            X06=0.
            X07=0.
            X08=0.
            X09=0.
            X010=0.
            X011=0.
            X012=0.
C----------
C  CONSTANT CHARACTER ARGUMENTS
C----------
            CTYPE=' '
            PROD='  '
C----------
C  CONSTANT INTEGER ARGUMENTS
C----------
            I1= 1
            IREGN= 2
C
            IF(DEBUG)WRITE(JOSTND,*)' CALLING PROFILE BF ISPC,ARGS = ',
     &      ISPC,IREGN,FORST,VEQNNB(ISPC),BFTOPD(ISPC),BFSTMP(ISPC),D,H,
     &      DBT,BARK
C
            CALL PROFILE (IREGN,FORST,VEQNNB(ISPC),TOPDIB,X01,
     &      BFSTMP(ISPC),D,HTTYPE,H,I01,X02,X03,X04,X05,X06,X07,
     &      X08,X09,I02,DBT,BARK*100.,LOGDIA,BOLHT,LOGLEN,LOGVOL,
     &      TVOL,I03,X010,X010,I1,I1,I1,I04,I05,X012,CTYPE,I01,PROD,
     &      IERR)
C
            IF(D.GE.BFMIND(ISPC))THEN
              IF(IT.GT.0)HT2TD(IT,1)=X02
            ELSE
              IF(IT.GT.0)HT2TD(IT,1)=0.
            ENDIF
C
            IF(DEBUG)WRITE(JOSTND,*)' AFTER PROFILE BF TVOL= ',TVOL
            TVOL(1)=TVOL1
            TVOL(4)=TVOL4
          ENDIF
C----------
C  SET RETURN VALUES.
C----------
          VN=TVOL(1)
          IF(VN.LT.0.)VN=0.
          VMAX=VN
          IF(D .LT. DBHMIN(ISPC))THEN
            VM = 0.
          ELSE
            VM=TVOL(4)
            IF(VM.LT.0.)VM=0.
          ENDIF
          IF(D.LT.BFMIND(ISPC))THEN
            BBFV=0.
          ELSE
            IF(METHB(ISPC).EQ.9) THEN
              BBFV=TVOL(10)
            ELSE
              BBFV=TVOL(2)
            ENDIF
            IF(BBFV.LT.0.)BBFV=0.
          ENDIF
          CTKFLG = .TRUE.
          BTKFLG = .TRUE.
        ENDIF
C
C
      ELSE
C
C
C----------
C  REGION 3 NATCRS LOGIC:
C  ASSUME TOTAL VOLUME IS EQUIVALENT TO MERCH (FIREWOOD)
C  ASSUME BOARD FOOT VOLUME IS ZERO.
C----------
        IERR=0
        UNT=3
        IF(D .GE. BFMIND(ISPC))UNT=1
        HT1PRD = 0.
        DRC=0.
        IFC=0
        IHTLL=0
        DO 109 IZERO=1,15
        TVOL(IZERO)=0.
  109   CONTINUE
C----------
C  CALL R3D2HV TO COMPUTE CUBIC VOLUME.
C----------
        IF(DEBUG)WRITE(JOSTND,*)' CALLING R3D2HV VEQNNC(ISPC)',
     & 'UNT,H,D= ',VEQNNC(ISPC),UNT,H,D
C
        CALL R3D2HV(VEQNNC(ISPC),UNT,H,HT1PRD,D,DRC,IFC,IHTLL,TVOL,IERR)
C
        IF(DEBUG)WRITE(JOSTND,*)' AFTER R3D2HV TVOL= ',TVOL
C----------
C  SET RETURN VARIABLES.
C----------
        VN=TVOL(1)
        IF(VN.LT.0.)VN=0.
        VMAX=VN
        IF(D .LT. DBHMIN(ISPC))THEN
          VM = 0.
        ELSE
          VM=TVOL(4)
          IF(VM.LT.0.)VM=0.
        ENDIF
        IF(D.LT.BFMIND(ISPC))THEN
          BBFV=0.
        ELSE
          IF(METHB(ISPC).EQ.9) THEN
            BBFV=TVOL(10)
          ELSE
            BBFV=TVOL(2)
          ENDIF
          IF(BBFV.LT.0.)BBFV=0.
        ENDIF
        CTKFLG = .TRUE.
        BTKFLG = .TRUE.
      ENDIF
C
C
C----------
  102 CONTINUE
      IF(VN.LE.0.) THEN
        VM=0.
        BBFV=0.
        CTKFLG = .FALSE.
        BTKFLG = .FALSE.
      ENDIF
      RETURN
C
C
C
C----------
C  ENTER ANY OTHER CUBIC HERE
C----------
      ENTRY OCFVOL (VN,VM,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,
     1              CTKFLG,IT)
C----------
C  USE CIBOLA COEFFICIENTS FOR R2 FORESTS
C----------
      IF(IFOR .LT. IGFOR) THEN
        IEQN=IEQMAP(     3,ISPC)
      ELSE
        IEQN=IEQMAP(IFOR-12,ISPC)
      ENDIF
C----------
C  SET INITIAL VALUES. IF DIAMETER LIMITS NOT MET THEN RETURN.
C----------
      VN=0.
      VM=0.
      IF (D .LT. 1.0 .OR. H .LT. 1.0) GO TO 400
      IF (IEQN .LT. 1 .OR. IEQN .GT. 10) GO TO 300
C----------
C COMPUTE TOTAL STEM GROSS CUBIC FOOT VOLUME - UNFORKED TREES
C----------
      VN=A0(IEQN)+(A1(IEQN)*D*D*H)
      IF(VN .LE. 0.0) VN=0.
C----------
C   COMPUTE MERCH CUBIC FOOT VOLUME. IF DIAMETER LIMITS NOT MET
C   THEN BYPASS THIS CALCULATION.
C----------
      IF (D .LT. DBHMIN(ISPC) .OR. D .LE. TOPD(ISPC)) GO TO 400
C----------
C COMPUTE MERCHANTABLE GROSS CUBIC FOOT VOLUME- UNFORKED TREES, TOP
C DIAMETER (TOPD) 3 TO 8 IN. I.B.
C----------
      VU=B0(IEQN)+(B1(IEQN)*((TOPD(ISPC)**3)*H)/(D**BN(IEQN)))
     & +(B2(IEQN)*D*D)
      IF(VU .GT. VN) GO TO 400
      VM = VN - VU
      GO TO 400
C----------
C PINYON AND JUNIPER EQUATIONS
C CHOJNACKY & OTT  RES. NOTE INT-363  NOV 1986
C OAK FROM DAVE WILSON (ALSO A CHOJNACKY EQN)
C SET MERCH CUBIC = TOTAL SINCE THESE ARE FIREWOOD SPECIES
C----------
  300 CONTINUE
      X=D*D*H/1000.
      IF(ISPC .EQ. 12) THEN
        IF(X .LE. 5) THEN
          VN = -0.07 + 2.51*X + 0.098*X*X
        ELSE
          VN = 7.29 + 2.51*X - 24.53/X
        ENDIF
      ELSEIF (ISPC .EQ. 16) THEN
        IF(X .LE. 5) THEN
          VN = -0.05 + 2.48*X + 0.057*X*X
        ELSE
          VN = 4.24 + 2.48*X - 14.29/X
        ENDIF
      ELSEIF (ISPC .EQ. 22) THEN
        IF(X .LE. 5) THEN
          VN = -0.068 + 2.4048*X + 0.1383*X*X
        ELSE
          VN = 6.571 + 2.4048*X - 17.704/X
        ENDIF
      ENDIF
      IF(VN .LT. 0.) VN=0.
      IF(D .LT. DBHMIN(ISPC))THEN
        VM = 0.
      ELSE
        VM=VN
      ENDIF
C
  400 CONTINUE
      VMAX=VN
      CTKFLG = .TRUE.
      IF(VN .LE. 0.) THEN
        VM=0.
        CTKFLG=.FALSE.
      ENDIF
      RETURN
C
C
C----------
C  ENTER ANY OTHER BOARD HERE
C----------
      ENTRY OBFVOL (BBFV,ISPC,D,H,TKILL,BARK,ITRNC,VMAX,LCONE,
     1              BTKFLG,IT)
C----------
C  USE CIBOLA COEFFICIENTS FOR R2 FORESTS
C----------
      IF(IFOR .LT. IGFOR) THEN
        IEQN=IEQMAP(     3,ISPC)
      ELSE
        IEQN=IEQMAP(IFOR-12,ISPC)
      ENDIF
      BBFV=0.
      BTKFLG=.FALSE.
C----------
C  IF NON-COMMERCIAL SPECIES, RETURN 0 BOARD FOOT VOLUME.
C----------
      IF(IEQN .LT. 1) RETURN
C----------
C   IF DIAMETER LIMITS NOT MET THEN RETURN.
C----------
      IF (D .LT. BFMIND(ISPC) .OR. D .LT. BFTOPD(ISPC)) RETURN
C----------
C CALCULATE M.C.F. FOR DTOPB INCH TOP
C----------
      VU=B0(IEQN)+(B1(IEQN)*((BFTOPD(ISPC)**3)*H)/(D**BN(IEQN)))
     & +(B2(IEQN)*D*D)
      IF(VU .GT. VMAX) GO TO 600
      VMR = VMAX - VU
C----------
C CALCULATE B.F.I. USING M.C.F. TO DTOPB INCH TOP
C----------
      RIC=F0(IEQN)-(F1(IEQN)/D)-(F2(IEQN)/(D**2))-(F3(IEQN)/(D**3))
      IF(RIC .LT. 0.0) GO TO 600
      VI = VMR * RIC
C----------
C CALCULATE B.F.S. USING B.F.I.
C----------
      RSI=E0(IEQN)-(E1(IEQN)/D)-(E2(IEQN)/(D**1.177748))-(E3(IEQN)/
     & (D**2))
      IF(RSI .LT. 0.0) GO TO 600
      BBFV = VI * RSI
C
  600 CONTINUE
      BTKFLG = .TRUE.
      IF(BBFV .LE. 0.) THEN
        BBFV=0.
        BTKFLG = .FALSE.
      ENDIF
      RETURN
C
C
C----------
C  ENTRY POINT FOR SENDING VOLUME EQN NUMBER TO THE FVS-TO-NATCRZ ROUTINE
C----------
      ENTRY GETEQN(ISPC,D,H,EQNC,EQNB,TDIBC,TDIBB)
      EQNC=VEQNNC(ISPC)
      EQNB=VEQNNB(ISPC)
      TDIBC=TOPD(ISPC)*BRATIO(ISPC,D,H)
      TDIBB=BFTOPD(ISPC)*BRATIO(ISPC,D,H)
      RETURN
C
      END
