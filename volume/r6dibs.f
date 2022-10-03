!== last modified  08-13-2003
      SUBROUTINE R6DIBS (IAPZ,DBHOB,IBTR,FCLASS,MTOPP,TLH,TH,
     >                   XLOGS,LOGDIA,SL,XL,A)

C THIS IS THE BASIC TREE MENSURATION ALGORITHM USED AS A SUBROUTINE
C BY ANY PROGRAM OF THE CRS6 SYSTEM.

      INTEGER FCLASS,I,IAPZ,IRET,ILH,ID
      REAL IBTR,LOGDIA(21,3),SL(20),XL(20),S,XLOGS,AT,A,BT,H1,HX,HR,DR
      REAL FC16,FC32,AIFC,MTOPP,TLH,TH,DBHOB,DOB16,T,B,H,LOGS,H2,BTR

      AIFC=FCLASS
C     ***** CLEAR PREVIOUS TREE *****
      DO 5 I=1,20
        SL(I) = 0.0
    5   XL(I) = 0.0

      DO 7 I=1,21
        LOGDIA(I,1) = 0.0
        LOGDIA(I,2) = 0.0
    7 CONTINUE

      FC16=0.0
      FC32=0.0
      S = 0.0
      XLOGS = 0.0

      IF (IAPZ.EQ.2) GO TO 200

C***********************************************************
C********************** EASTSIDE ***************************
C***********************************************************
      FC16 = AIFC / 100.0
c                             btr diameter logic removed 8/98
c      IF (IBTR.GT.0) GO TO 10

C     ***** DBHOB-SIGHTING - COMPUTE LOGDIA(1,2) WITH FC *****
      LOGDIA(1,2) = DBHOB * FC16
      GO TO 20

C     ***** 16-FT SIGHTING - COMPUTE LOGDIA(1,2) WITH BTR *****
   10 DOB16 = DBHOB
      LOGDIA(1,2) = DOB16 * IBTR
      DBHOB = LOGDIA(1,2) / FC16

C    ***** COMPUTE TAPER COEFFICIENT *****
   20 IRET = 1
      GO TO 1000
   25 IF (TLH.EQ.0.0) GO TO 70

C     ***************** LOG HEIGHT ******************
      SL(1) = 16.3
      IF (TLH.GT.1.0.AND.LOGDIA(1,2).GT.MTOPP) GO TO 30

C     ***** ONLY ONE LOG IN TREE *****
      XLOGS = 1.0
      LOGDIA(1,2) = MTOPP
c      IF (IBTR.GT.0) DBHOB=LOGDIA(1,2)/FC16
      GO TO 130

C     ***** MORE THAN ONE LOG IN TREE *****
   30 ILH = TLH
      S = TLH - ILH
      IF(S.EQ.0.0) GO TO 40

C     ***** TREE HAS A HALF LOG AT THE TOP *****
      S = 8.0
      SL(ILH+1) = 8.0
      LOGDIA(ILH+1,2) = MTOPP
      XLOGS = 0.5
      IF (ILH.GT.1) GO TO 40

C     ***** 1.5-LOG TREE *****
      XLOGS = 1.5
      GO TO 130

C     ***** COMPUTE DIBS OF ALL FULL LOGS *****
   40 T = MTOPP / LOGDIA(1,2)
      AT = A / (1.0 - (A*T))
      BT = (1.0 / (1.0-T)) -AT
      H1 = (ILH - 1) * 16.3 + S
      DO 50 I=2,ILH
        HX = H1 - (I-1)*16.3
        HR = HX / H1
        DR = T + (HR / (AT*HR+BT))
        LOGDIA(I,2) = LOGDIA(1,2) * DR
        SL(I) = 16.3
   50   CONTINUE
      IF (S.EQ.0.0) LOGDIA(ILH,2)=MTOPP
      XLOGS = XLOGS + ILH
      GO TO 130


C     ****************** TOTAL HEIGHT ******************
   70 B = 1.0 - A
      SL(1) = 16.3
      XL(1) = 16.3
      IF (LOGDIA(1,2).GT.MTOPP) GO TO 80

C     ***** ONLY ONE LOG IN TREE *****
      LOGDIA(1,2) = MTOPP
      XLOGS = 1.0
c      IF (IBTR.GT.0) DBHOB=LOGDIA(1,2)/FC16
      GO TO 130

C     ***** COMPUTE DIBS OF ALL FULL LOGS *****
   80 H1 = TH - 16.3
      DO 90 I=2,19
        HX = H1 - ((I-1) * 16.3)
        HR = HX / H1
        IF (HR.GT.0.0) GO TO 85
        LOGDIA(I,2) = 1.0
        GO TO 100
   85   DR = HR / (A*HR+B)
        LOGDIA(I,2) = DR * LOGDIA(1,2)
        SL(I) = 16.3
        XL(I) = 16.3
        IF (LOGDIA(I,2).LT.MTOPP) GO TO 100
        IF (LOGDIA(I,2).GT.MTOPP) GO TO 90
        XLOGS = I-1
        SL(I) = 16.3
        XL(I) = 16.3
        GO TO 130
   90   CONTINUE

C     ***** ROUND ANY SHORT LENGTH AT TOP OF TREE *****
  100 DR = MTOPP / LOGDIA(1,2)
      HX = (DR*B*H1) / (1.0 - (A*DR))
      H = (I-2) * 16.3
      S = H1 - HX - H
      IF (S.GE.4.0) GO TO 110
      XLOGS = I-1
      LOGDIA(I,2) = 0.0
      SL(I) = 0.0
      XL(I) = S
      GO TO 130
  110 IF (S.GT.12.0) GO TO 120
      XLOGS = (I-1) + 0.5
      LOGDIA(I,2) = MTOPP
      SL(I) = 8.0
      XL(I) = S
      GO TO 130
  120 XLOGS = I
      LOGDIA(I,2) = MTOPP
      SL(I) = 16.3
      XL(I) = S

C     ***** ROUND DIBS TO NEAREST INCH *****
  130 DO 140 I=1,20
        ID = LOGDIA(I,2) + 0.5
        LOGDIA(I,1) = ID
  140   continue
      RETURN



C*************************************************
C******************* WESTSIDE ********************
C*************************************************
  200 FC32 = AIFC / 100.0
      SL(1) = 16.3
      XL(1) = 16.3

C     ***** GET TAPER COEFFICIENT *****
      IRET = 2
      GO TO 1000
  210 IF ( TLH .EQ. 0.0 ) GO TO 500
      IF ( IBTR .GT. 0 ) GO TO 400
      GO TO 300
  220 IF ( IBTR .GT. 0 ) GO TO 600
      GO TO 500



C     *********** LOG HEIGHT WITH DBHOB-SIGHTING ***********
  300 LOGS = TLH*10.0 / 5.0

C     ***** ONLY ONE 16-FT SEGMENT IN TREE *****
      IF (LOGS.GT.1) GO TO 320
      XLOGS = 1.0
      LOGDIA(1,2) = MTOPP
C      ID = LOGDIA(1,2) + 0.5
C      LOGDIA(1,1) = ID
C      RETURN
      GO TO 370

C     ***** TWO 16'S *****
  320 IF (LOGS.GT.2) GO TO 330
      XLOGS = 2.0
      LOGDIA(2,2) = MTOPP
      ID = LOGDIA(2,2) + 0.5
      LOGDIA(2,1) = ID
      SL(2) = 16.3
      LOGDIA(1,2) = IFIX( (LOGDIA(2,2)+DBHOB) / 2.0 + 0.5 )
C      ID = LOGDIA(1,2) + 0.5
C      LOGDIA(1,1) = ID
C      RETURN
      GO TO 370

C     ***** REDUCE TOPDIB IF CONFLICT WITH LOGDIA(2,2) *****
  330 LOGDIA(2,2) = INT(DBHOB*FC32+0.5 )
      SL(2) = 16.3
      IF (LOGDIA(2,2).LE.MTOPP) MTOPP=LOGDIA(2,2)-1.0

C     ***** PROJECT TAPER FROM LOGDIA(2,2) TO LOGDIA(1,2) *****
      T = MTOPP/LOGDIA(2,2)
      AT = A / (1.0 -(A*T))
      BT = (1.0 / (1.0-T)) -AT
      H1 = (LOGS-1) * 16.3
      H2 = H1 - 16.3
      HR = H2/H1
      DR = T + (HR / (AT*HR+BT))
      LOGDIA(1,2) = IFIX( LOGDIA(2,2)/DR+0.5 )

C     ***** THREE 16'S *****
      IF (LOGS.GT.3) GO TO 340
      XLOGS = 3.0
      LOGDIA(3,2) = MTOPP
      SL(3) = 16.3
C      DO 331 I=1,3 
C  331  LOGDIA(I,1) = IFIX( LOGDIA(I,2)+0.5 )
C      RETURN
      GO TO 370

C     ***** FOUR OR MORE 16'S *****
  340 H1 = (LOGS-2) * 16.3
      DO 350 I=3,INT(LOGS)
        HX = H1 - (I-2)*16.3
        HR = HX/H1
        DR = T + (HR / (AT*HR+BT))
        LOGDIA(I,2) = LOGDIA(2,2)*DR
  350   SL(I) = 16.3
      XLOGS = LOGS

C     ***** ROUND DIBS *****
  370 DO 360 I=1,20
  360   LOGDIA(I,1) = IFIX( LOGDIA(I,2)+0.5 )
      RETURN



C     *********** LOG HEIGHT WITH 16-FT SIGHTING *************
  400 LOGS = TLH*10.0 / 5.0

c     ***** ONE 16-FT SEGMENT IN TREE *****
      IF (LOGS.GT.1) GO TO 410
      XLOGS = 1.0
      LOGDIA(1,2) = MTOPP
C      LOGDIA(1,1) = IFIX( LOGDIA(1,2)+0.5 )
      DBHOB = MTOPP+2.0
C      RETURN
      GO TO 370

C     ***** REDUCE TOPDIB IF CONFLICT WITH LOGDIA(1,2) *****
  410 DOB16 = DBHOB
      BTR = IBTR
      LOGDIA(1,2) = IFIX( DOB16*BTR+0.5 )
      IF (LOGDIA(1,2).LE.MTOPP) MTOPP=LOGDIA(1,2)-1.0

C     ***** TWO 16'S *****
      IF (LOGS.GT.2) GO TO 420
      XLOGS = 2.0
      LOGDIA(2,2) = MTOPP
      SL(2) = 16.3
      DBHOB = LOGDIA(2,2)/FC32
      IF (DBHOB.LE.LOGDIA(1,2)) DBHOB=LOGDIA(1,2)+2.0
C      DO 411 I=1,2 
C  411   LOGDIA(I,1) = IFIX( LOGDIA(I,2)+0.5 )
C      RETURN
      GO TO 450

C     ***** THREE OR MORE 16'S *****
  420 T = MTOPP/LOGDIA(1,2)
      AT = A / (1.0 - (A*T))
      BT = (1.0 / (1.0-T)) -AT
      H1 = (LOGS-1) * 16.3
      DO 430 I=2,INT(LOGS)
        HX = H1 - (I-1)*16.3
        HR = HX/H1
        DR = T + (HR / (AT*HR+BT))
        LOGDIA(I,2) = LOGDIA(1,2)*DR
  430   SL(I) = 16.3
      XLOGS = LOGS

C     ***** COMPUTE DBHOB *****
      DBHOB = LOGDIA(2,1) / FC32
      IF (DBHOB.LE.LOGDIA(1,1)) DBHOB= LOGDIA(1,1)+2.0

C     ***** ROUND DIBS *****
  450 DO 440 I=1,20
  440   LOGDIA(I,1) = IFIX( LOGDIA(I,2)+0.5 )
      RETURN



C     ********* TOTAL HEIGHT WITH DBHOB-SIGHTING *********
  500 B = 1.0 - A

C     ***** START WITH ROUNDED LOGDIA(2,2) *****
      LOGDIA(2,2) =  DBHOB*FC32 
      SL(2) = 16.3
      XL(2) = 16.3

C     ***** REDUCE HEIGHT IF CONFLICT WITH LOGDIA(2,2) *****
      IF (LOGDIA(2,2).GT.MTOPP) GO TO 510
      XLOGS = 2.0
      LOGDIA(2,2) = MTOPP
C      LOGDIA(2,1) = IFIX( LOGDIA(2,2)+0.5 )
      SL(2) = 16.3
      XL(2) = 16.3

      LOGDIA(1,2) = MTOPP + 1.0
C      LOGDIA(1,1) = IFIX( LOGDIA(1,2)+0.5 )
      IF (TH.GE.40.0) GO TO 560

      LOGDIA(2,2) = 0.0
      LOGDIA(2,1) = 0.0
      SL(2) = 0.0
      XL(2) = 0.0
      XLOGS = 1.0
      LOGDIA(1,2) = MTOPP
C      LOGDIA(1,1) = IFIX( LOGDIA(1,2)+0.5 )
C      RETURN
      GO TO 560

C     ***** PROJECT TAPER FROM LOGDIA(2,2) TO LOGDIA(1,2) *****
  510 H1 = TH - 16.3
      H2 = H1 - 16.3
      HR = H2/H1
      IF (HR.GT.0.0) GO TO 520
      LOGDIA(2,1) = 0.0
      SL(2) = 0.0
      XL(2) = 0.0
      XLOGS = 1.0
      LOGDIA(1,2) = MTOPP
C      LOGDIA(1,1) = IFIX( LOGDIA(1,2)+0.5 )
C      RETURN
      GO TO 560

  520 DR = HR / (A*HR+B)
      LOGDIA(1,2) = IFIX( LOGDIA(2,2)/DR + 0.5 )

C     ***** COMPUTE DIBS ABOVE LOGDIA(2,22 *****
      H1 = TH - 32.6
      DO 540 I=3,19
        HX = H1 - (I-2)*16.3
        HR = HX/H1
        IF (HR.GT.0.0) GO TO 530
        XLOGS = I-1
        GO TO 560
  530   DR = HR / (A*HR+B)
        LOGDIA(I,2) = DR*LOGDIA(2,2)
        SL(I) = 16.3
        XL(I) = 16.3
        IF (LOGDIA(I,2).LE.MTOPP) GO TO 550
  540   CONTINUE

C     ***** ROUND ANY SHORT LENGTH AT TOP OF TREE *****
  550 XLOGS = I
      
      LOGDIA(I,2) = MTOPP
      DR = MTOPP/LOGDIA(2,2)
      HX = (DR*B*H1) / (1.0 - (A*DR))
      H = (I-3)*16.3
      S = H1 - HX - H
      XL(I) = S
      IF (S.GE.8.0) GO TO 560
      XLOGS = I-1
      LOGDIA(I,2) = 0.0
      SL(I) = 0.0

C     ***** ROUND DIBS *****
  560 DO 570 I=1,20
  570   LOGDIA(I,1) = IFIX( LOGDIA(I,2)+0.5 )
      RETURN


C     *********** TOTAL HEIGHT WITH 16-FT SIGHTING ***********
  600 B = 1.0 - A

C     ***** START WITH ROUNDED LOGDIA(1,2) *****
      DOB16 = DBHOB
      BTR = IBTR
      LOGDIA(1,2) = IFIX( DOB16*BTR + 0.5 )

C     ***** REDUCE HEIGHT IF CONFLICT WITH LOGDIA(1,2) *****
      IF (LOGDIA(1,2).GT.MTOPP) GO TO 610
      XLOGS = 1.0
      LOGDIA(1,2) = MTOPP
C      LOGDIA(1,1) = IFIX( LOGDIA(1,2)+0.5 )
      DBHOB = MTOPP+2.0
C      RETURN
      GO TO 650

C     ***** COMPUTE DIBS ABOVE LOGDIA(1,2) *****
  610 H1 = TH - 16.3
      DO 630 I=2,19
        HX = H1 - (I-1)*16.3
        HR = HX/H1
        IF (HR.GT.0.0) GO TO 620
        XLOGS = I-1
        GO TO 650
  620   DR = HR / (A*HR+B)
        LOGDIA(I,2) = DR*LOGDIA(1,2)
        SL(I) = 16.3
        IF (LOGDIA(I,2).LE.MTOPP) GO TO 640
  630   CONTINUE

C     ***** ROUND ANY SHORT LENGTH AT TOP OF TREE *****
  640 XLOGS = I
      LOGDIA(I,2) = MTOPP
      DR = MTOPP/LOGDIA(1,2)
      HX = (DR*B*H1) / (1.0 - (A*DR))
      H = (I-2)*16.3
      S = H1 - HX - H
      XL(I) = S
      IF (S.GE.8.0) GO TO 650
      XLOGS = I-1
      LOGDIA(I,2) = 0.0
      SL(I) = 0.0

C     ***** COMPUTE DBHOB *****
      DBHOB = LOGDIA(2,2)/FC32
      IF (DBHOB.LE.LOGDIA(1,2)) DBHOB = LOGDIA(1,2)+2.0

C     ***** ROUND DIBS *****
  650 DO 660 I=1,20
  660   LOGDIA(I,1) = IFIX( LOGDIA(I,2)+0.5 )
      RETURN


C     *****************************************************
C     ************* COMPUTE TAPER COEFFICIENT *************
C     *****************************************************

 1000 IF (A.GE.1.0) GO TO 1010
      IF (A.EQ.0.0) A=0.62
      GO TO (25,210), IRET

 1010 IF (A.NE.1.0) GO TO 1020
      A = 0.71469 - 0.002536 * DBHOB + 0.000004719 * DBHOB**2
      GO TO (25,210), IRET

 1020 IF (A.NE.2.0) GO TO 1030
      A = 0.98104 - .0226*DBHOB + .000692*DBHOB**2 - .000007387*DBHOB**3
      GO TO (25,210), IRET

 1030 A = 0.62
      GO TO (25,210), IRET

      RETURN
      END
C USES A HYPERBOLIC CURVE TO DETERMINE DIAM OF ALL LOGS IN A TREE.
C IF TREE HEIGHT WAS MEASURED IN FEET THEN ALSO DETERMINE HOW MANY
C LOGS ARE IN THE TREE.

C ENTER WITH:

C IAPZ  = 1  (EASTSIDE) TOTAL # LOGS WILL RETURN AS 16.3-FT SGMTS
C       = 2  (WESTSIDE)                             32.6-FT
C DBHOB   ---  WILL BE XXX.0 FOR NON-3P AND XXX.X FOR 3P CRUISES
C IBTR = 0   ASSUMES SIGHTING POINT AT DBHOB
C      > 0     "        "      "    AT 16 FT & DBHOB IS DOB16
C FCLASS   ---  ALWAYS PRESENT
C MTOPP    ---  ALWAYS LESS THAN DBHOB
C TLH   ---  IF > 0 IT IS TREE HT EXPRESSED AS TOTAL # LOGS TO MTOPP
C             (EASTSIDE) MIN. TLH = 1.0, (WESTSIDE) MIN. TLH = 0.5
C TH    ---  IF > 0 IT IS TREE HT EXPRESSED IN FT FROM STUMP TO TIP
C             (EITHER TH OR TLH WILL ENTER AS ZERO.)
C A     ---  IF = 0 USE STANDARD TAPER COEFFICIENT.
C            IF > 0 IT IS A FLAG FOR COMPUTING SPECIAL COEFFICIENTS
C RETURN WITH:
C DBHOB   ---  REPLACED WITH COMPUTED DBHOB IF SIGHTING WAS AT 16 FT.
C MTOPP    ---  IF COMPUTED BASAL DIAM FOR CURVE (DBHOB*FC) OR
C            (DOB16*BTR) IS < MTOPP, MEASUREMENTS ARE ILLOGICAL AND MTOPP
C            WILL BE REDUCED

C XLOGS ---  THE TOTAL # OF 16.3-FT SGMTS IN THE TREE (1.0 - 20.0)
C             (EASTSIDE) MAY RETURN AS XX.5, (WESTSIDE) AS XX.0

C D     ---  THE DIBS OF THE LOG SEGMENTS.  NOTE --
C            IF HT IS MEASURED AS TOTAL TREE HT (TH BEING USED),
C            LAST DIB (D) COMPUTED MAY NOT EQUAL MTOPP.  I.E. CANT GET
C            A FULL # OF LOG SGMTS TO THE TOP THAT WAS REQUESTED.

C SL    ---  THE SCALING LENGTH FOR EACH LOG SEGMENT.

C XL    ---  THE LENGTH IN FT BETWEEN LAST FULL 16.3-FT SEG & MTOPP.
C            CORRESPONDING SL OF 0.0, 8.0, OR 16.3 SHOWS WHICH WAY
C            IT WAS ROUNDED.  XL ONLY APPLIES FOR TOTAL-HEIGHT
C            MEASUREMENT.

