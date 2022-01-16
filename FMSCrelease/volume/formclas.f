C----------
C VOLUME $Id$
C----------
c  last modified  02-10-2012
C     SUBROUTINES to determine the default Form Class.
c      contains the following subroutines
c           FORMCL_BM
c           FORMCL_CA
c           FORMCL_EC
c           FORMCL_NI
c           FORMCL_PN
c           FORMCL_SO
c           FORMCL_WA

      SUBROUTINE FORMCL_BM(SPEC,IFOR,D,FC)
C----------
C  **FORMCL--BM     DATE OF LAST REVISION:  08/13/96
C----------
      CHARACTER*3 FIAJSP(11),SPEC
      REAL D
      INTEGER MALHFC(11,5),OCHOFC(11,5),UMATFC(11,5)
      INTEGER WLWHFC(11,5),FC,IFCDBH,ISPC,IFOR
      INTEGER DONEFLAG,LASTFLAG,FIRST,LAST,HALF
C----------
C  FOREST ORDER: (IFOR)
C  4=MALHEUR(604)          7=OCHOCO(607)        14=UMATILLA(614)
C 16=WALLOWA-WHITMAN(616)
C
C  SPECIES ORDER: (ISPC)
C  1=WP  2=WL  3=DF  4=GF  5=MH  6=    7=LP  8=ES  9=AF 10=PP 11=J/OT
      DATA FIAJSP /'   ','017','019','073','093','108','119','122',
     &             '202','264','999'/

C----------
C  MALHEUR FORM CLASS VALUES
C----------
      DATA MALHFC/
     &   80,  76,  78,  78,  77,  80,  78,  78,  78,  75,  60,
     &   80,  78,  80,  79,  80,  83,  78,  78,  77,  79,  60,
     &   80,  77,  80,  80,  82,  83,  79,  80,  77,  79,  60,
     &   80,  76,  82,  82,  84,  80,  81,  82,  80,  79,  60,
     &   80,  76,  82,  77,  84,  80,  78,  83,  77,  78,  60/
C----------
C  OCHOCO FORM CLASS VALUES
C----------
      DATA OCHOFC/
     &   80,  76,  78,  78,  82,  70,  78,  76,  79,  75,  60,
     &   80,  78,  76,  78,  82,  75,  80,  78,  79,  78,  60,
     &   80,  77,  74,  80,  82,  75,  80,  78,  76,  79,  60,
     &   80,  74,  74,  80,  82,  75,  82,  80,  76,  79,  60,
     &   80,  74,  74,  80,  82,  75,  80,  80,  76,  78,  60/
C----------
C  UMATILLA FORM CLASS VALUES
C----------
      DATA UMATFC/
     &   80,  76,  74,  78,  77,  86,  78,  78,  77,  75,  60,
     &   80,  78,  74,  78,  77,  86,  78,  78,  77,  75,  60,
     &   80,  77,  74,  78,  75,  86,  80,  80,  77,  75,  60,
     &   80,  76,  75,  78,  75,  86,  81,  81,  77,  79,  60,
     &   80,  76,  75,  78,  75,  86,  81,  81,  77,  78,  60/
C----------
C  WALLOWA-WHITMAN FORM CLASS VALUES
C----------
      DATA WLWHFC/
     &   80,  76,  78,  78,  84,  85,  78,  78,  78,  75,  60,
     &   80,  78,  79,  82,  84,  86,  78,  78,  77,  79,  60,
     &   80,  77,  79,  77,  84,  85,  78,  80,  77,  79,  60,
     &   80,  76,  79,  75,  84,  85,  78,  82,  77,  79,  60,
     &   80,  76,  79,  75,  84,  85,  78,  83,  77,  78,  60/
C----------
C  FOR REGION 6 FORESTS, LOAD THE FORM CLASS USING TABLE VALUES.
C  IF A FORM CLASS HAS BEEN ENTERED VIA KEYWORD, USE IT INSTEAD.
C----------
C     BINARY SEARCH FOR CORRECT COEFFICIENTS
      DONEFLAG = 0
      LASTFLAG = 0
      FIRST = 1
      LAST = 11
      DO 5, WHILE (DONEFLAG.EQ.0)
         IF(FIRST.EQ.LAST) LASTFLAG = 1
C  DETERMINE WHERE TO CHECK
          HALF=((LAST-FIRST+1)/2) + FIRST
      !FOUND THE COEFFECIENTS
          IF(SPEC.EQ.FIAJSP(HALF))THEN
             ISPC = HALF
             DONEFLAG=1
C  MOVE DOWN THE LIST
          ELSEIF(SPEC.GT.FIAJSP(HALF))THEN
             FIRST = HALF
C  MOVE UP THE LIST
          ELSEIF(SPEC.LT.FIAJSP(HALF))THEN
             LAST = HALF - 1
          ENDIF
C  DID NOT FIND A MATCH
          IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0)THEN
             FC = 80
             RETURN
          ENDIF
   5  CONTINUE
C     END BINARY SEARCH

      IFCDBH = INT((D - 1.0) / 10.0 + 1.0)
      IF(IFCDBH .LT. 1) IFCDBH=1
      IF(D.GT.40.9) IFCDBH=5
      IF(IFOR.EQ.4) THEN
          FC = MALHFC(ISPC,IFCDBH)
      ELSEIF(IFOR.EQ.7) THEN
          FC = OCHOFC(ISPC,IFCDBH)
      ELSEIF(IFOR.EQ.14) THEN
          FC = UMATFC(ISPC,IFCDBH)
      ELSE
          FC = WLWHFC(ISPC,IFCDBH)
      ENDIF
C
      RETURN
      END

C*************************************************************
C*************************************************************
      SUBROUTINE FORMCL_CA(SPEC,IFOR,D,FC)
C----------
C  **FORMCL--CA     DATE OF LAST REVISION:  08/27/99
C----------
      CHARACTER*3 FIAJSP(50),SPEC
      REAL D
      INTEGER ROGRFC(50,5),SISKFC(50,5),FC,IFCDBH,ISPC,IFOR
      INTEGER DONEFLAG,LASTFLAG,FIRST,LAST,HALF
C----------
C  FOREST ORDER: (IFOR)
C  10=ROGUE RIVER(610)      11=SISKIYOU(611)
C
C
C  SPECIES ORDER:
C  1=PC  2=IC  3=RC  4=WF  5=RF  6=SH  7=DF  8=WH  9=MH 10=WB
C 11=KP 12=LP 13=CP 14=LM 15=JP 16=SP 17=WP 18=PP 19=MP 20=GP
C 21=JU 22=BR 23=GS 24=PY 25=OS 26=LO 27=CY 28=BL 29=EO 30=WO
C 31=BO 32=VO 33=IO 34=BM 35=BU 36=RA 37=MA 38=GC 39=DG 40=OA
C 41=WN 42=TO 43=SY 44=AS 45=CW 46=WI 47=CN 48=CL 49=OH 50=RW

      DATA FIAJSP /
     &  '001','004','015','020','021','041','064','081','092','101',
     &  '103','108','109','113','116','117','119','122','124','127',
     &  '202','212','231','242','251','263','264','312','330','351',
     &  '361','431','492','542','600','631','730','746','747','801',
     &  '805','807','811','815','818','821','839','920','981','211'/

C----------
C  ROGUE RIVER FORM CLASS VALUES
C----------
      DATA ROGRFC/
     & 95., 94., 95., 96., 95., 94., 94., 91., 96., 92.,
     & 95., 95., 95., 95., 93., 94., 94., 93., 95., 95.,
     & 95., 93., 95., 98., 95., 95., 94., 95., 95., 89.,
     & 98., 95., 95., 98., 95., 98., 95., 95., 94., 98.,
     & 95., 98., 95., 98., 98., 98., 95., 95., 95., 82.,
C
     & 82., 94., 82., 91., 82., 90., 87., 82., 83., 92.,
     & 79., 79., 95., 95., 93., 90., 87., 93., 82., 95.,
     & 95., 89., 82., 98., 95., 95., 94., 95., 95., 89.,
     & 88., 79., 95., 84., 95., 84., 86., 86., 94., 88.,
     & 95., 88., 95., 98., 80., 98., 95., 86., 95., 82.,
C
     & 76., 78., 76., 84., 76., 84., 82., 79., 79., 92.,
     & 78., 78., 95., 95., 86., 84., 83., 83., 79., 82.,
     & 95., 86., 76., 98., 82., 86., 85., 95., 95., 89.,
     & 84., 78., 95., 81., 95., 81., 82., 78., 85., 81.,
     & 86., 80., 95., 98., 78., 98., 95., 82., 95., 79.,
C
     & 74., 75., 75., 83., 74., 82., 81., 78., 77., 92.,
     & 78., 78., 82., 82., 83., 82., 82., 81., 78., 79.,
     & 95., 84., 74., 98., 79., 82., 80., 86., 86., 89.,
     & 81., 76., 95., 80., 95., 80., 79., 76., 80., 81.,
     & 82., 78., 95., 98., 77., 98., 95., 79., 95., 78.,
C
     & 74., 74., 74., 82., 74., 81., 81., 78., 76., 87.,
     & 78., 76., 82., 82., 81., 82., 81., 80., 78., 78.,
     & 95., 84., 74., 88., 78., 82., 80., 86., 86., 89.,
     & 81., 76., 95., 79., 95., 80., 79., 75., 80., 80.,
     & 82., 77., 95., 98., 77., 98., 95., 78., 95., 78./
C----------
C  SISKIYOU FORM CLASS VALUES
C----------
      DATA SISKFC/
     & 94., 89., 96., 93., 92., 96., 93., 93., 93., 91.,
     & 96., 96., 91., 91., 93., 96., 96., 93., 91., 91.,
     & 91., 93., 91., 88., 91., 95., 95., 95., 95., 95.,
     & 98., 95., 95., 98., 95., 91., 98., 95., 98., 98.,
     & 95., 91., 95., 95., 92., 92., 95., 95., 95., 82.,
C
     & 90., 89., 83., 93., 87., 96., 86., 93., 89., 91.,
     & 88., 88., 91., 91., 93., 91., 88., 93., 86., 91.,
     & 91., 89., 86., 88., 91., 95., 95., 95., 95., 95.,
     & 88., 79., 95., 84., 95., 86., 88., 86., 98., 88.,
     & 95., 91., 95., 95., 83., 92., 95., 86., 95., 82.,
C
     & 84., 75., 78., 86., 82., 90., 81., 90., 84., 91.,
     & 86., 86., 91., 91., 86., 85., 84., 83., 84., 86.,
     & 91., 86., 81., 88., 86., 86., 86., 95., 95., 95.,
     & 84., 78., 95., 81., 95., 84., 84., 78., 88., 81.,
     & 86., 82., 95., 95., 81., 92., 95., 82., 95., 79.,
C
     & 82., 71., 76., 85., 80., 87., 80., 89., 82., 91.,
     & 86., 86., 86., 86., 83., 84., 83., 81., 82., 84.,
     & 91., 84., 79., 88., 84., 82., 82., 86., 86., 95.,
     & 81., 76., 95., 80., 95., 82., 81., 76., 84., 81.,
     & 82., 80., 95., 95., 80., 92., 95., 79., 95., 78.,
C
     & 81., 71., 76., 85., 79., 87., 80., 88., 81., 86.,
     & 86., 85., 86., 86., 81., 83., 82., 80., 82., 82.,
     & 91., 84., 78., 80., 82., 82., 82., 86., 86., 95.,
     & 81., 76., 95., 79., 95., 82., 81., 75., 84., 80.,
     & 82., 79., 95., 95., 80., 92., 95., 78., 95., 78./
C----------
C  FOR REGION 6 FORESTS, LOAD THE FORM CLASS USING TABLE VALUES.
C  IF A FORM CLASS HAS BEEN ENTERED VIA KEYWORD, USE IT INSTEAD.
C----------
C     BINARY SEARCH FOR CORRECT COEFFICIENTS
      DONEFLAG = 0
      LASTFLAG = 0
      FIRST = 1
      LAST = 49
      DO 5, WHILE (DONEFLAG.EQ.0)
         IF(FIRST.EQ.LAST) LASTFLAG = 1
C  DETERMINE WHERE TO CHECK
          HALF=((LAST-FIRST+1)/2) + FIRST
      !FOUND THE COEFFECIENTS
          IF(SPEC.EQ.FIAJSP(HALF))THEN
             ISPC = HALF
             DONEFLAG=1
C  MOVE DOWN THE LIST
          ELSEIF(SPEC.GT.FIAJSP(HALF))THEN
             FIRST = HALF
C  MOVE UP THE LIST
          ELSEIF(SPEC.LT.FIAJSP(HALF))THEN
             LAST = HALF - 1
          ENDIF
C  DID NOT FIND A MATCH
          IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0)THEN
             FC = 80
             RETURN
          ENDIF
   5  CONTINUE
C     END BINARY SEARCH

      IFCDBH = INT((D - 1.0) / 10.0 + 1.0)
      IF(IFCDBH .LT. 1) IFCDBH=1
      IF(D.GT.40.9) IFCDBH=5

      IF(IFOR.EQ.10) THEN
          FC = ROGRFC(ISPC,IFCDBH)
      ELSEIF(IFOR.EQ.11) THEN
          FC = SISKFC(ISPC,IFCDBH)
      ELSE
          FC = 80
      ENDIF
C
      RETURN
      END

C*************************************************************
C*************************************************************
      SUBROUTINE FORMCL_EC(SPEC,IFOR,D,FC)
C----------
C  **FORMCL--EC     DATE OF LAST REVISION:  08/13/96
C----------
      CHARACTER*3 FIAJSP(11),SPEC
      REAL D
      INTEGER OKANFC(11,5),WENAFC(11,5),FC,IFCDBH,ISPC,IFOR
      INTEGER DONEFLAG,LASTFLAG,FIRST,LAST,HALF
C----------
C  FOREST ORDER: (IFOR)
C  8=OKANOGAN(608)
C  17=WENATCHEE(617)        5=OKANOGAN (TONASKET RD) (699)
C
C  SPECIES ORDER: (ISPC)
C  1=WP  2=WL  3=DF  4=SF  5=RC  6=GF  7=LP  8=ES  9=AF 10=PP 11=MH/OT
      DATA FIAJSP /'011','017','019','073','093','108','119','122',
     &             '202','242','999'/
C----------
C  OKANOGAN FORM CLASS VALUES
C----------
      DATA OKANFC/
     &  82,   76,   84,   78,   82,   85,   78,   78,   72,   75,  75,
     &  82,   78,   84,   78,   82,   85,   80,   80,   72,   75,  78,
     &  82,   77,   85,   78,   83,   85,   80,   81,   73,   72,  79,
     &  84,   76,   85,   73,   86,   85,   82,   82,   75,   68,  79,
     &  84,   76,   85,   73,   86,   85,   80,   84,   75,   61,  78/
C----------
C  WENATCHEE FORM CLASS VALUES
C----------
      DATA WENAFC/
     &  85,   78,   76,   77,   79,   82,   83,   77,   75,   69,  82,
     &  86,   79,   77,   78,   80,   82,   84,   78,   76,   70,  82,
     &  84,   79,   78,   79,   80,   82,   84,   81,   75,   70,  82,
     &  86,   79,   76,   80,   82,   82,   85,   81,   76,   68,  80,
     &  86,   80,   77,   80,   82,   82,   84,   80,   73,   70,  80/
C----------
C  FOR REGION 6 FORESTS, LOAD THE FORM CLASS USING TABLE VALUES.
C  IF A FORM CLASS HAS BEEN ENTERED VIA KEYWORD, USE IT INSTEAD.
C----------
C     BINARY SEARCH FOR CORRECT COEFFICIENTS
      DONEFLAG = 0
      LASTFLAG = 0
      FIRST = 1
      LAST = 11
      DO 5, WHILE (DONEFLAG.EQ.0)
         IF(FIRST.EQ.LAST) LASTFLAG = 1
C  DETERMINE WHERE TO CHECK
          HALF=((LAST-FIRST+1)/2) + FIRST
C  FOUND THE COEFFECIENTS
          IF(SPEC.EQ.FIAJSP(HALF))THEN
             ISPC = HALF
             DONEFLAG=1
C  MOVE DOWN THE LIST
          ELSEIF(SPEC.GT.FIAJSP(HALF))THEN
             FIRST = HALF
C  MOVE UP THE LIST
          ELSEIF(SPEC.LT.FIAJSP(HALF))THEN
             LAST = HALF - 1
          ENDIF
C  DID NOT FIND A MATCH
          IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0)THEN
             FC = 80
             RETURN
          ENDIF
   5  CONTINUE
C     END BINARY SEARCH

      IFCDBH = INT((D - 1.0) / 10.0 + 1.0)

      IF(IFCDBH .LT. 1) IFCDBH=1
      IF(D.GT.40.9) IFCDBH=5

      IF(IFOR.EQ.8) THEN
          FC = OKANFC(ISPC,IFCDBH)
      ELSE
          FC = WENAFC(ISPC,IFCDBH)
      ENDIF
C
      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE FORMCL_NI(SPEC,IFOR,D,FC)
C----------
C  **FORMCL--NI     DATE OF LAST REVISION:  08/13/96
C----------
      CHARACTER*3 FIAJSP(11),SPEC
      REAL D
      INTEGER COLVFC(11,5),FC,IFCDBH,ISPC,IFOR
      INTEGER DONEFLAG,LASTFLAG,FIRST,LAST,HALF
      INTEGER IDANUW
C----------
C  FOREST ORDER: (IFOR)
C  5=COLVILLE(621)
C
C  SPECIES ORDER: (ISPC)
C  1=WP  2=L   3=DF  4=GF  5=WH  6=C   7=LP  8=S   9=AF 10=PP 11=OT
      DATA FIAJSP /'017','019','073','093','108','119','122','202',
     >             '242','263','999'/

C----------
C  COLVILLE FORM CLASS VALUES
C----------
      DATA COLVFC/
     & 76,   78,   78,   77,   80,   78,   78,   78,   64,   76,  75,
     & 78,   76,   78,   79,   82,   80,   80,   76,   65,   78,  78,
     & 77,   74,   80,   80,   82,   80,   80,   75,   66,   80,  79,
     & 76,   74,   80,   80,   80,   82,   82,   74,   66,   80,  79,
     & 76,   74,   80,   81,   80,   80,   80,   74,   66,   82,  78/
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      IDANUW = IFOR
C----------
C  FOR REGION 6 FOREST, LOAD THE FORM CLASS USING TABLE VALUES.
C  IF A FORM CLASS HAS BEEN ENTERED VIA KEYWORD, USE IT INSTEAD.
C
C  REGION 1 VOLUME ROUTINES DON'T USE FORM CLASS.
C----------
C     BINARY SEARCH FOR CORRECT COEFFICIENTS
      DONEFLAG = 0
      LASTFLAG = 0
      FIRST = 1
      LAST = 11
      DO 5, WHILE (DONEFLAG.EQ.0)
         IF(FIRST.EQ.LAST) LASTFLAG = 1
      !DETERMINE WHERE TO CHECK
          HALF=((LAST-FIRST+1)/2) + FIRST
C  FOUND THE COEFFECIENTS
          IF(SPEC.EQ.FIAJSP(HALF))THEN
             ISPC = HALF
             DONEFLAG=1
C  MOVE DOWN THE LIST
          ELSEIF(SPEC.GT.FIAJSP(HALF))THEN
             FIRST = HALF
C  MOVE UP THE LIST
          ELSEIF(SPEC.LT.FIAJSP(HALF))THEN
             LAST = HALF - 1
          ENDIF
C  DID NOT FIND A MATCH
          IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0)THEN
             FC = 80
             RETURN
          ENDIF
   5  CONTINUE
C     END BINARY SEARCH

      IFCDBH = INT((D - 1.0) / 10.0 + 1.0)
      IF(IFCDBH .LT. 1) IFCDBH=1
      IF(D.GT.40.9) IFCDBH=5

      FC = COLVFC(ISPC,IFCDBH)

C
      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE FORMCL_PN(SPEC,IFOR,D,FC)
C----------
C  **FORMCL--PN     DATE OF LAST REVISION:  09/10/99
C----------
      CHARACTER*3 FIAJSP(39),SPEC
      REAL D
      INTEGER OLYMFC(39,5),SIUSFC(39,5),BLM708(39),BLM709(39)
      INTEGER BLM712(39),FC,IFCDBH,ISPC,IFOR
      INTEGER DONEFLAG,LASTFLAG,FIRST,LAST,HALF
C----------
C  FOREST ORDER: (IFOR)
C  1=OLYMPIC(609)  2=SIUSLAW(612)  3=QUINAULT(800)  4=BLM SALAM (708)
C  5=BLM EUGENE (709)  6=BLM COOS BAY (712)
C
C  SPECIES ORDER: (ISPC)
C  1=SF  2=WF  3=GF  4=AF  5=RF  6=    7=NF  8=YC  9=IC 10=ES
C 11=LP 12=JP 13=SP 14=WP 15=PP 16=DF 17=RW 18=RC 19=WH 20=MH
C 21=BM 22=RA 23=WA 24=PB 25=GC 26=AS 27=CW 28=WO 29=J  30=LL
C 31=WB 32=KP 33=PY 34=DG 35=HT 36=CH 37=WI 38=   39=OT
      DATA FIAJSP/'   ','011','015','017','019','020','022','042','060',
     & '072','081','093','098','101','103','108','116','117','119',
     & '122','202','211','231','242','263','264','312','351','352',
     & '376','431','492','500','746','747','764','815','920','999'/

C----------
C  OLYMPIC FORM CLASS VALUES  (ALSO USE FOR QUINAULT)
C----------
      DATA OLYMFC/
     & 97., 97., 86., 97., 83., 89., 88., 99., 81., 90., 96.,
     & 92., 79., 95., 89., 82., 82., 93., 96., 98., 86., 84.,
     & 79., 79., 87., 85., 82., 95., 81., 92., 96., 96., 76.,
     & 95., 95., 86., 92., 84., 84.,
C
     & 97., 97., 86., 97., 83., 89., 88., 99., 81., 90., 96.,
     & 92., 79., 95., 89., 82., 82., 93., 96., 98., 86., 84.,
     & 79., 79., 87., 85., 82., 95., 81., 92., 96., 96., 76.,
     & 95., 95., 86., 92., 84., 84.,
C
     & 91., 93., 83., 97., 80., 86., 84., 88., 72., 86., 96.,
     & 82., 76., 91., 82., 79., 79., 87., 93., 95., 84., 81.,
     & 76., 76., 81., 81., 80., 82., 81., 92., 96., 89., 69.,
     & 86., 95., 86., 92., 84., 80.,
C
     & 90., 91., 82., 95., 80., 84., 82., 87., 71., 85., 93.,
     & 80., 76., 90., 80., 78., 78., 86., 91., 94., 82., 80.,
     & 76., 74., 79., 80., 79., 82., 81., 92., 96., 87., 65.,
     & 82., 95., 84., 92., 84., 79.,
C
     & 90., 91., 82., 95., 79., 84., 82., 86., 70., 85., 93.,
     & 79., 75., 90., 80., 78., 78., 86., 91., 93., 82., 79.,
     & 75., 74., 79., 79., 79., 82., 74., 92., 96., 86., 65.,
     & 82., 95., 84., 92., 84., 78./
C----------
C  SIUSLAW FORM CLASS VALUES
C----------
      DATA SIUSFC/
     & 95., 97., 86., 96., 83., 89., 88., 88., 81., 90., 98.,
     & 92., 79., 93., 89., 73., 82., 68., 75., 89., 86., 84.,
     & 79., 79., 87., 85., 82., 95., 81., 92., 96., 96., 76.,
     & 95., 95., 86., 92., 84., 84.,
C
     & 95., 97., 86., 96., 83., 89., 88., 88., 81., 90., 98.,
     & 92., 79., 93., 89., 73., 82., 68., 75., 89., 86., 84.,
     & 79., 79., 87., 85., 82., 95., 81., 92., 96., 96., 76.,
     & 95., 95., 86., 92., 84., 84.,
C
     & 89., 93., 83., 93., 80., 86., 84., 79., 72., 86., 93.,
     & 82., 76., 89., 82., 71., 79., 63., 72., 83., 84., 81.,
     & 76., 76., 81., 81., 80., 82., 81., 92., 96., 89., 69.,
     & 86., 95., 86., 92., 80., 80.,
C
     & 88., 91., 82., 91., 80., 84., 82., 77., 71., 85., 90.,
     & 80., 76., 88., 80., 70., 78., 63., 71., 82., 82., 80.,
     & 76., 74., 79., 80., 79., 82., 81., 92., 96., 87., 65.,
     & 82., 95., 84., 92., 79., 79.,
C
     & 88., 91., 82., 91., 79., 84., 82., 76., 70., 85., 90.,
     & 79., 75., 88., 80., 70., 78., 62., 71., 81., 82., 79.,
     & 75., 74., 79., 79., 79., 82., 74., 92., 96., 86., 65.,
     & 82., 95., 84., 92., 78., 78./
C
      DATA  BLM708 /
     & 74, 84, 86, 84, 82, 75, 84, 73, 60, 75, 73, 77, 80, 82, 82, 68,
     & 75, 75, 76, 82, 80, 75, 60, 76, 88, 72, 84, 88, 70, 70, 75, 70,
     & 70, 75, 74, 75, 70, 75, 74/
C
      DATA BLM709/
     & 78, 82, 78, 82, 78, 78, 78, 78, 78, 78, 70, 78, 78, 78, 78, 78,
     & 78, 72, 78, 70, 78, 78, 78, 72, 80, 78, 78, 80, 78, 78, 80, 78,
     & 78, 78, 82, 78, 78, 78, 78/
C
      DATA  BLM712/
     & 74, 80, 84, 86, 80, 75, 78, 70, 60, 75, 70, 72, 80, 82, 82, 80,
     & 75, 76, 80, 80, 74, 75, 82, 70, 84, 72, 82, 82, 82, 70, 82, 70,
     & 70, 75, 74, 86, 70, 75, 74/
C----------
C  FOR REGION 6 FORESTS, LOAD THE FORM CLASS USING TABLE VALUES.
C  IF A FORM CLASS HAS BEEN ENTERED VIA KEYWORD, USE IT INSTEAD.
C----------
C     BINARY SEARCH FOR CORRECT COEFFICIENTS
      DONEFLAG = 0
      LASTFLAG = 0
      FIRST = 1
      LAST = 39
      DO 5, WHILE (DONEFLAG.EQ.0)
         IF(FIRST.EQ.LAST) LASTFLAG = 1
C  DETERMINE WHERE TO CHECK
          HALF=((LAST-FIRST+1)/2) + FIRST
C  FOUND THE COEFFECIENTS
          IF(SPEC.EQ.FIAJSP(HALF))THEN
             ISPC = HALF
             DONEFLAG=1
C  MOVE DOWN THE LIST
          ELSEIF(SPEC.GT.FIAJSP(HALF))THEN
             FIRST = HALF
C  MOVE UP THE LIST
          ELSEIF(SPEC.LT.FIAJSP(HALF))THEN
             LAST = HALF - 1
          ENDIF
C  DID NOT FIND A MATCH
          IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0)THEN
             FC = 80
             RETURN
          ENDIF
   5  CONTINUE
C     END BINARY SEARCH

      IFCDBH = INT((D - 1.0) / 10.0 + 1.0)
      IF(IFCDBH .LT. 1) IFCDBH=1
      IF(D.GT.40.9) IFCDBH=5

      IF(IFOR.EQ.9) THEN
          FC = OLYMFC(ISPC,IFCDBH)
      ELSE IF(IFOR.EQ.12)THEN
          FC = SIUSFC(ISPC,IFCDBH)
      ELSE IF(IFOR.EQ.708)THEN
          FC = BLM708(ISPC)
      ELSE IF(IFOR.EQ.709)THEN
          FC = BLM709(ISPC)
      ELSE
          FC = BLM712(ISPC)
      ENDIF
C
      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE FORMCL_SO(SPEC,IFOR,D,FC)
C----------
C  **FORMCL--SO     DATE OF LAST REVISION:  08/13/96
C----------
      CHARACTER*3 FIAJSP(11),SPEC
      REAL D
      INTEGER DESHFC(11,5),FREMFC(11,5),WINEFC(11,5),FC,IFCDBH
      INTEGER IFOR,ISPC
      INTEGER DONEFLAG,LASTFLAG,FIRST,LAST,HALF
C----------
C  FOREST ORDER: (IFOR)
C  1=DESCHUTES(601)  2=FREMONT(602)  3=WINEMA(620)
C
C  SPECIES ORDER: (ISPC)
C  1=WP  2=SP  3=DF  4=WF  5=MH  6=IC  7=LP  8=ES  9=RF 10=PP 11=J/OT
      DATA FIAJSP /'015','020','081','093','108','117','119','122',
     &             '202','264','999'/

C----------
C  DESCHUTES FORM CLASS VALUES
C----------
      DATA DESHFC/
     &  80,   80,   65,   82,   82,   78,   80,   76,   80,   78,   60,
     &  80,   80,   65,   82,   82,   78,   81,   76,   80,   78,   60,
     &  78,   78,   65,   82,   82,   78,   81,   79,   78,   78,   60,
     &  78,   78,   65,   82,   82,   80,   82,   80,   76,   76,   60,
     &  76,   76,   65,   82,   82,   80,   82,   80,   76,   74,   60/
C----------
C  FREMONT FORM CLASS VALUES
C----------
      DATA FREMFC/
     &  68,   66,   64,   77,   83,   70,   83,   70,   66,   64,   34,
     &  76,   74,   66,   79,   82,   82,   82,   82,   68,   64,   48,
     &  82,   80,   66,   80,   80,   82,   80,   82,   68,   66,   48,
     &  83,   81,   66,   80,   80,   84,   80,   84,   68,   66,   50,
     &  80,   78,   66,   81,   80,   78,   80,   80,   68,   66,   50/
C----------
C  WINEMA FORM CLASS VALUES
C----------
      DATA WINEFC/
     &  76,   81,   62,   80,   83,   72,   78,   73,   78,   75,   60,
     &  81,   81,   65,   80,   82,   76,   79,   78,   77,   78,   60,
     &  78,   81,   64,   80,   80,   76,   80,   80,   75,   79,   60,
     &  78,   78,   62,   79,   80,   77,   79,   80,   74,   79,   60,
     &  76,   74,   62,   78,   80,   77,   78,   80,   73,   78,   60/
C----------
C  FOR REGION 6 FORESTS, LOAD THE FORM CLASS USING TABLE VALUES.
C  IF A FORM CLASS HAS BEEN ENTERED VIA KEYWORD, USE IT INSTEAD.
C----------
C     BINARY SEARCH FOR CORRECT COEFFICIENTS
      DONEFLAG = 0
      LASTFLAG = 0
      FIRST = 1
      LAST = 11
      DO 5, WHILE (DONEFLAG.EQ.0)
         IF(FIRST.EQ.LAST) LASTFLAG = 1
C  DETERMINE WHERE TO CHECK
          HALF=((LAST-FIRST+1)/2) + FIRST
C  FOUND THE COEFFECIENTS
          IF(SPEC.EQ.FIAJSP(HALF))THEN
             ISPC = HALF
             DONEFLAG=1
C  MOVE DOWN THE LIST
          ELSEIF(SPEC.GT.FIAJSP(HALF))THEN
             FIRST = HALF
C  MOVE UP THE LIST
          ELSEIF(SPEC.LT.FIAJSP(HALF))THEN
             LAST = HALF - 1
          ENDIF
C  DID NOT FIND A MATCH
          IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0)THEN
             FC = 80
             RETURN
          ENDIF
   5  CONTINUE
C     END BINARY SEARCH

      IFCDBH = INT((D - 1.0) / 10.0 + 1.0)
      IF(IFCDBH .LT. 1) IFCDBH=1
      IF(D.GT.40.9) IFCDBH=5

      IF(IFOR.EQ.1) THEN
          FC = DESHFC(ISPC,IFCDBH)
      ELSEIF(IFOR.EQ.2) THEN
          FC = FREMFC(ISPC,IFCDBH)
      ELSE
          FC = WINEFC(ISPC,IFCDBH)
      ENDIF
C
      RETURN
      END


C*************************************************************
C*************************************************************
      SUBROUTINE FORMCL_WC(SPEC,IFOR,D,FC)
C----------
C  **FORMCL--WC     DATE OF LAST REVISION:  09/10/99
C----------
      CHARACTER*3 FIAJSP(39),SPEC
      REAL D
      INTEGER GIFPFC(39,5),MBSNFC(39,5),MTHDFC(39,5)
      INTEGER UMPQFC(39,5),WILLFC(39,5),BLM710(39),BLM711(39)
      INTEGER FC,IFCDBH,ISPC,IFOR
      INTEGER DONEFLAG,LASTFLAG,FIRST,LAST,HALF
C----------
C  FOREST ORDER: (IFOR)
C  1=GIFFORD PINCHOT(603)  2=MT BAKER/SNOQ(605)  3=MT HOOD(606)
C  5=UMPQUA(615)         6=WILLAMETTE(618)
C  9=BLM ROSEBURG(710)
C 10=BLM MEDFORD/LAKEVIEW(711)
C
C  FOR VALUES NOT SUPPLIED BY BLM, USE THE FOLLOWING FOREST
C  FORM CLASS VALUES FOR THE MIDDLE (21.0"-30.9") DBH RANGE:
C  SALEM --- MT HOOD     EUGENE --- WILLAMETTE
C  ROSEBURG --- UMPQUA   MEDFORD/LAKEVIEW --- ROGUE RIVER
C
C  SPECIES ORDER: (ISPC)
C  1=SF  2=WF  3=GF  4=AF  5=RF  6=    7=NF  8=YC  9=IC 10=ES
C 11=LP 12=JP 13=SP 14=WP 15=PP 16=DF 17=RW 18=RC 19=WH 20=MH
C 21=BM 22=RA 23=WA 24=PB 25=GC 26=AS 27=CW 28=WO 29=J  30=LL
C 31=WB 32=KP 33=PY 34=DG 35=HT 36=CH 37=WI 38=   39=OT
      DATA FIAJSP/'   ','   ','011','015','017','019','020','022','042',
     &'060','072','081','093','101','103','108','116','117','119','122',
     &'202','211','231','242','263','264','312','351','352','376','431',
     &'492','500','746','747','764','815','920','999'/

C----------
C  GIFFORD PINCHOT FORM CLASS VALUES
C----------
      DATA GIFPFC/
     & 97., 95., 93., 98., 87., 74., 95., 93., 81., 90., 96.,
     & 92., 79., 93., 89., 97., 82., 79., 95., 96., 84., 84.,
     & 82., 79., 87., 85., 82., 95., 81., 92., 96., 96., 81.,
     & 91., 95., 86., 92., 74., 84.,
C
     & 97., 95., 93., 98., 87., 74., 95., 93., 81., 90., 96.,
     & 92., 79., 93., 89., 90., 82., 79., 95., 96., 84., 84.,
     & 82., 79., 87., 85., 82., 95., 81., 92., 96., 96., 81.,
     & 91., 95., 86., 92., 74., 84.,
C
     & 93., 91., 89., 90., 82., 74., 90., 86., 72., 86., 91.,
     & 82., 76., 89., 82., 87., 79., 76., 91., 90., 82., 80.,
     & 78., 76., 81., 81., 80., 95., 81., 92., 96., 89., 74.,
     & 84., 95., 86., 92., 74., 80.,
C
     & 92., 90., 88., 89., 80., 74., 88., 86., 71., 85., 91.,
     & 80., 76., 88., 80., 86., 78., 74., 91., 89., 81., 78.,
     & 76., 74., 79., 80., 79., 95., 81., 92., 96., 87., 70.,
     & 82., 95., 84., 92., 74., 79.,
C
     & 92., 90., 88., 89., 79., 74., 88., 84., 70., 85., 90.,
     & 79., 75., 88., 80., 86., 78., 74., 90., 88., 80., 78.,
     & 76., 74., 79., 79., 79., 95., 74., 92., 96., 86., 70.,
     & 82., 95., 84., 92., 74., 78./
C----------
C  MOUNT BAKER / SNOQUALMIE FORM CLASS VALUES
C----------
      DATA MBSNFC/
     & 97., 95., 86., 97., 87., 74., 88., 92., 81., 90., 96.,
     & 92., 79., 95., 89., 82., 82., 85., 96., 98., 84., 84.,
     & 82., 79., 87., 85., 82., 95., 81., 92., 96., 96., 76.,
     & 86., 95., 86., 92., 74., 84.,
C
     & 97., 95., 86., 97., 87., 74., 88., 92., 81., 90., 96.,
     & 92., 79., 95., 89., 82., 82., 85., 96., 98., 84., 84.,
     & 82., 79., 87., 85., 82., 95., 81., 92., 96., 96., 76.,
     & 86., 95., 86., 92., 74., 84.,
C
     & 92., 91., 83., 97., 82., 74., 84., 85., 72., 86., 91.,
     & 82., 76., 91., 82., 80., 79., 81., 93., 95., 82., 80.,
     & 78., 76., 81., 81., 80., 95., 81., 92., 96., 89., 69.,
     & 79., 95., 86., 92., 74., 80.,
C
     & 91., 90., 82., 95., 80., 74., 82., 85., 71., 85., 91.,
     & 80., 76., 90., 80., 79., 78., 79., 92., 94., 81., 78.,
     & 76., 74., 79., 80., 79., 95., 81., 92., 96., 87., 65.,
     & 78., 95., 84., 92., 74., 79.,
C
     & 91., 90., 82., 95., 79., 74., 82., 83., 70., 85., 90.,
     & 79., 75., 90., 80., 78., 78., 79., 91., 93., 80., 78.,
     & 76., 74., 79., 79., 79., 95., 74., 92., 96., 86., 65.,
     & 78., 95., 84., 92., 74., 78./
C----------
C  MOUNT HOOD FORM CLASS VALUES
C----------
      DATA MTHDFC/
     & 98., 97., 84., 98., 87., 74., 88., 92., 92., 87., 89.,
     & 92., 79., 93., 92., 83., 82., 85., 86., 84., 84., 84.,
     & 82., 79., 87., 85., 82., 95., 81., 92., 96., 96., 81.,
     & 86., 95., 86., 92., 74., 84.,
C
     & 98., 97., 84., 98., 87., 74., 88., 92., 92., 87., 89.,
     & 92., 79., 93., 92., 83., 82., 85., 86., 84., 84., 84.,
     & 82., 79., 87., 85., 82., 95., 81., 92., 96., 96., 81.,
     & 86., 95., 86., 92., 74., 84.,
C
     & 93., 94., 81., 95., 82., 74., 84., 85., 82., 83., 84.,
     & 82., 76., 89., 85., 81., 79., 81., 83., 79., 82., 80.,
     & 78., 76., 81., 81., 80., 95., 81., 92., 96., 89., 74.,
     & 79., 95., 86., 92., 74., 80.,
C
     & 92., 92., 80., 93., 80., 74., 82., 85., 80., 82., 84.,
     & 80., 76., 88., 84., 80., 78., 79., 82., 78., 81., 78.,
     & 76., 74., 79., 80., 79., 95., 81., 92., 96., 87., 70.,
     & 78., 95., 84., 92., 74., 79.,
C
     & 92., 92., 79., 93., 79., 74., 82., 83., 79., 81., 83.,
     & 79., 75., 88., 83., 79., 78., 79., 82., 77., 80., 78.,
     & 76., 74., 79., 79., 79., 95., 74., 92., 96., 86., 70.,
     & 78., 95., 84., 92., 74., 78./
C----------
C  UMPQUA FORM CLASS VALUES
C----------
      DATA UMPQFC/
     & 94., 92., 92., 97., 97., 74., 94., 76., 76., 97., 96.,
     & 94., 82., 86., 90., 84., 82., 79., 96., 90., 85., 85.,
     & 82., 79., 87., 85., 83., 95., 92., 92., 96., 96., 95.,
     & 92., 95., 87., 92., 74., 79.,

     & 94., 92., 92., 97., 97., 74., 94., 76., 76., 97., 96.,
     & 94., 82., 86., 90., 84., 82., 79., 96., 90., 85., 85.,
     & 82., 79., 87., 85., 83., 95., 92., 92., 96., 96., 95.,
     & 92., 95., 87., 92., 74., 79.,

     & 89., 88., 88., 97., 91., 74., 89., 70., 68., 93., 91.,
     & 84., 79., 83., 83., 82., 79., 76., 93., 84., 83., 81.,
     & 78., 76., 81., 81., 81., 95., 92., 92., 96., 89., 86.,
     & 85., 95., 87., 92., 74., 76.,

     & 88., 87., 87., 95., 89., 74., 87., 70., 66., 91., 91.,
     & 82., 79., 82., 81., 81., 78., 74., 92., 83., 82., 79.,
     & 76., 74., 79., 80., 80., 95., 92., 92., 96., 87., 82.,
     & 83., 95., 85., 92., 74., 74.,

     & 87., 87., 87., 95., 88., 74., 87., 69., 66., 91., 90.,
     & 81., 78., 82., 81., 80., 78., 74., 91., 82., 81., 79.,
     & 76., 74., 79., 79., 80., 95., 92., 92., 96., 86., 82.,
     & 83., 95., 85., 92., 74., 74./
C----------
C  WILLAMETTE FORM CLASS VALUES
C----------
      DATA WILLFC/
     & 78., 95., 76., 81., 87., 74., 78., 69., 69., 79., 82.,
     & 92., 76., 78., 82., 71., 82., 63., 75., 77., 75., 77.,
     & 82., 79., 87., 85., 75., 95., 81., 92., 96., 96., 76.,
     & 86., 95., 86., 92., 74., 84.,
C
     & 78., 95., 76., 81., 87., 74., 78., 69., 69., 79., 82.,
     & 92., 76., 78., 82., 71., 82., 63., 75., 77., 75., 77.,
     & 82., 79., 87., 85., 75., 95., 81., 92., 96., 96., 76.,
     & 86., 95., 86., 92., 74., 84.,
C
     & 74., 91., 73., 75., 82., 74., 74., 63., 61., 76., 78.,
     & 82., 74., 74., 76., 69., 79., 60., 72., 72., 73., 73.,
     & 78., 76., 81., 81., 73., 95., 81., 92., 96., 89., 69.,
     & 79., 95., 86., 92., 74., 80.,
C
     & 73., 90., 72., 73., 80., 74., 73., 63., 60., 74., 78.,
     & 80., 73., 73., 74., 68., 78., 59., 72., 71., 72., 72.,
     & 76., 74., 79., 80., 73., 95., 81., 92., 96., 87., 65.,
     & 78., 95., 84., 92., 74., 79.,
C
     & 73., 90., 72., 73., 79., 74., 72., 62., 59., 74., 76.,
     & 79., 73., 73., 74., 68., 78., 59., 71., 71., 71., 71.,
     & 76., 74., 79., 79., 72., 95., 74., 92., 96., 86., 65.,
     & 78., 95., 84., 92., 74., 78./
C
      DATA BLM710/
     &  76, 76, 82, 76, 80, 82, 76, 76, 70, 76, 76,
     &  66, 76, 80, 80, 80, 80, 80, 80, 80, 72, 76,
     &  76, 72, 82, 76, 82, 82, 76, 76, 76, 76, 76,
     &  76, 76, 76, 76, 76, 76/
C
      DATA BLM711/
     &  70, 70, 74, 78, 77, 75, 78, 75, 67, 70, 70,
     &  66, 74, 73, 68, 68, 70, 76, 76, 80, 76, 70,
     &  72, 70, 78, 70, 72, 72, 72, 70, 72, 69, 70,
     &  72, 72, 68, 66, 72, 70/
C----------
C  FOR REGION 6 FORESTS, LOAD THE FORM CLASS USING TABLE VALUES.
C  IF A FORM CLASS HAS BEEN ENTERED VIA KEYWORD, USE IT INSTEAD.
C----------
C     BINARY SEARCH FOR CORRECT COEFFICIENTS
      DONEFLAG = 0
      LASTFLAG = 0
      FIRST = 1
      LAST = 39
      DO 5, WHILE (DONEFLAG.EQ.0)
         IF(FIRST.EQ.LAST) LASTFLAG = 1
C  DETERMINE WHERE TO CHECK
          HALF=((LAST-FIRST+1)/2) + FIRST
C  FOUND THE COEFFECIENTS
          IF(SPEC.EQ.FIAJSP(HALF))THEN
             ISPC = HALF
             DONEFLAG=1
C  MOVE DOWN THE LIST
          ELSEIF(SPEC.GT.FIAJSP(HALF))THEN
             FIRST = HALF
C  MOVE UP THE LIST
          ELSEIF(SPEC.LT.FIAJSP(HALF))THEN
             LAST = HALF - 1
          ENDIF
C  DID NOT FIND A MATCH
          IF(LASTFLAG.EQ.1 .AND. DONEFLAG.EQ.0)THEN
             FC = 80
             RETURN
          ENDIF
   5  CONTINUE
C     END BINARY SEARCH

      IFCDBH = INT((D - 1.0) / 10.0 + 1.0)
      IF(IFCDBH .LT. 1) IFCDBH=1
      IF(D.GT.40.9) IFCDBH=5

      IF(IFOR.EQ.3) THEN
          FC = GIFPFC(ISPC,IFCDBH)
      ELSEIF(IFOR.EQ.5) THEN
          FC = MBSNFC(ISPC,IFCDBH)
      ELSEIF(IFOR.EQ.6) THEN
          FC = MTHDFC(ISPC,IFCDBH)
      ELSEIF(IFOR.EQ.15) THEN
          FC = UMPQFC(ISPC,IFCDBH)
      ELSEIF(IFOR.EQ.18) THEN
          FC = WILLFC(ISPC,IFCDBH)
      ELSEIF(IFOR.EQ.710) THEN
          FC = BLM710(ISPC)
      ELSE
          FC = BLM711(ISPC)
      ENDIF
C
      RETURN
      END
