      SUBROUTINE HABTYP (KARD2,ARRAY2)
      IMPLICIT NONE
C----------
C  **HABTYP--CR   DATE OF LAST REVISION:  01/24/11
C----------
C
C     TRANSLATES HABITAT TYPE  CODE INTO A SUBSCRIPT, ITYPE, AND IF
C     KODTYP IS ZERO, THE ROUTINE RETURNS 0.
C     IF A REFERENCE CODE IS PRESENT THEN A PV CODE/REFERENCE CODE
C     COMBINATION WAS READ FROM THE STDINFO KEYEWORD, AND THESE
C     MUST BE MAPPED, IN A CALL TO **PVREF**, TO A FVS HABITAT CODE
C     FOR FURTHER PROCESSING
C
C     THIS ROUTINE IS PROGRAMMED SO WHEN A HABITAT TYPE IS ENTERED ON
C     THE STDINFO OR FROM SETSITE WITH NO PARMS STATEMENT, EITHER THE
C     HABITAT TYPE CAN BE ENTERED OR THE FVS SEQUENCE NUMBER FOR THE
C     HABITAT TYPE CAN BE ENTERED. WHEN USING SETSITE WITH THE PARMS
C     OPTION, USERS MUST USE THE FVS SEQUENCE NUMBER FOR THE HABITAT
C     TYPE. IF A HABITAT TYPE IS ENTERED DIRECTLY IN A PARMS STATEMENT
C     IT MAY GET DIGESTED INCORRECTLY (E.G. R2 FOREST HT=00101 WHICH
C     THIS ROUTINE THINKS IS INDEX 101 = HT 00502).
C     UPON EXITING THIS ROUTINE, KODTYP WILL CARRY THE FVS SEQUENCE
C     NUMBER FOR THE HT; ITYPE WILL CARRY THE INDEX OF THAT HT IN
C     THE R2 OR R3 HT ARRAY.
C----------
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'VARCOM.F77'
C
C
COMMONS
C----------
      INTEGER NR2,NR3,I,IR2,IR3,LIMIT,IHB
      PARAMETER (NR2=376)
      PARAMETER (NR3=242)
      CHARACTER*8 R2HABT(NR2),R3HABT(NR3)
C
      REAL ARRAY2
      CHARACTER*10 KARD2,TEMP
      REAL REFTMP
      LOGICAL DEBUG,LNE1ST
      LOGICAL LPVCOD,LPVREF,LPVXXX
C----------
C  LOAD REGION 2 HABITAT TYPE ARRAY
C----------
      DATA (R2HABT(I),I=1,50)/
C 1-25
     &'00101   ', '001010  ', '001011  ', '001012  ', '001013  ',
     &'00102   ', '00103   ', '00104   ', '001040  ', '001041  ',
     &'00105   ', '001050  ', '001052  ', '001053  ', '001054  ',
     &'00109   ', '00110   ', '00111   ', '00112   ', '00113   ',
     &'00114   ', '00115   ', '00116   ', '00117   ', '00118   ',
C 26-50
     &'00201   ', '00203   ', '00301   ', '00302   ', '003020  ',
     &'003021  ', '003022  ', '003023  ', '00303   ', '00304   ',
     &'00305   ', '00306   ', '00307   ', '00308   ', '00309   ',
     &'00310   ', '003100  ', '003101  ', '00311   ', '00313   ',
     &'003130  ', '003131  ', '003132  ', '00315   ', '00316   '/
      DATA (R2HABT(I),I=51,100)/
C 51-75
     &'00318   ', '00319   ', '003190  ', '003191  ', '00320   ',
     &'003200  ', '003201  ', '00321   ', '003210  ', '003211  ',
     &'003212  ', '003213  ', '003216  ', '003217  ', '00322   ',
     &'003220  ', '003221  ', '00323   ', '00324   ', '003240  ',
     &'003241  ', '00325   ', '00326   ', '00327   ', '00328   ',
C 76-100
     &'003280  ', '003281  ', '00329   ', '00330   ', '00401   ',
     &'00402   ', '00403   ', '00405   ', '00406   ', '00407   ',
     &'00408   ', '00409   ', '00413   ', '00414   ', '004140  ',
     &'004141  ', '004142  ', '004145  ', '00415   ', '004150  ',
     &'004151  ', '00416   ', '00417   ', '00418   ', '00501   '/
      DATA (R2HABT(I),I=101,150)/
C 101-125
     &'00502   ', '00503   ', '005030  ', '005031  ', '005032  ',
     &'00601   ', '006010  ', '006011  ', '00602   ', '00603   ',
     &'00604   ', '006040  ', '006043  ', '00605   ', '00606   ',
     &'00607   ', '00608   ', '00609   ', '006090  ', '006091  ',
     &'00610   ', '00611   ', '00612   ', '006120  ', '006121  ',
C 126-150
     &'00701   ', '007010  ', '007011  ', '00702   ', '007020  ',
     &'007021  ', '00703   ', '00704   ', '00705   ', '007050  ',
     &'007051  ', '00706   ', '00801   ', '00802   ', '00803   ',
     &'00804   ', '00805   ', '00901   ', '00903   ', '009030  ',
     &'009031  ', '00905   ', '00907   ', '00908   ', '009080  '/
      DATA (R2HABT(I),I=151,200)/
C 151-175
     &'009081  ', '00909   ', '00910   ', '00911   ', '00912   ',
     &'01001   ', '01002   ', '01003   ', '01004   ', '010040  ',
     &'010041  ', '010042  ', '01005   ', '01006   ', '01007   ',
     &'01008   ', '01009   ', '01010   ', '01101   ', '01102   ',
     &'01103   ', '01104   ', '01105   ', '011050  ', '011051  ',
C 176-200
     &'011052  ', '01106   ', '01107   ', '01108   ', '01109   ',
     &'011090  ', '011091  ', '011092  ', '01110   ', '011100  ',
     &'011101  ', '01111   ', '01112   ', '01113   ', '01115   ',
     &'01117   ', '011170  ', '011171  ', '011172  ', '01118   ',
     &'01119   ', '01120   ', '01121   ', '011210  ', '011211  '/
      DATA (R2HABT(I),I=201,250)/
C 201-225
     &'011214  ', '011215  ', '01122   ', '011220  ', '011221  ',
     &'011222  ', '011224  ', '011225  ', '011226  ', '01123   ',
     &'01124   ', '011240  ', '011242  ', '011243  ', '01125   ',
     &'011250  ', '011251  ', '01126   ', '01127   ', '011270  ',
     &'011272  ', '011273  ', '011274  ', '01128   ', '011280  ',
C 226-250
     &'011281  ', '01129   ', '011290  ', '011291  ', '01131   ',
     &'01132   ', '01140   ', '01150   ', '01151   ', '01201   ',
     &'012010  ', '012011  ', '012012  ', '01202   ', '012020  ',
     &'012021  ', '01203   ', '012030  ', '012032  ', '01204   ',
     &'01205   ', '012050  ', '012052  ', '012053  ', '01206   '/
      DATA (R2HABT(I),I=251,300)/
C 251-275
     &'01207   ', '01208   ', '01209   ', '01210   ', '01211   ',
     &'01212   ', '01213   ', '01214   ', '012140  ', '012141  ',
     &'01215   ', '012150  ', '012151  ', '012153  ', '01216   ',
     &'01217   ', '012170  ', '012171  ', '012172  ', '012173  ',
     &'012174  ', '01218   ', '01219   ', '01220   ', '01221   ',
C 276-300
     &'01222   ', '01223   ', '01233   ', '01241   ', '01702   ',
     &'10202   ', '10203   ', '10301   ', '103010  ', '103011  ',
     &'10302   ', '10303   ', '10304   ', '10305   ', '10306   ',
     &'10401   ', '104010  ', '104011  ', '10402   ', '10403   ',
     &'10404   ', '10405   ', '10501   ', '10502   ', '105020  '/
      DATA (R2HABT(I),I=301,350)/
C 301-325
     &'105021  ', '105022  ', '10503   ', '10504   ', '10505   ',
     &'10507   ', '10508   ', '10510   ', '10511   ', '10512   ',
     &'105120  ', '105121  ', '105123  ', '10513   ', '10514   ',
     &'10515   ', '105150  ', '105151  ', '105152  ', '105153  ',
     &'105154  ', '10516   ', '10517   ', '10518   ', '10519   ',
C 326-350
     &'105190  ', '105191  ', '10520   ', '10521   ', '10522   ',
     &'10523   ', '10601   ', '106010  ', '106011  ', '10603   ',
     &'10701   ', '107010  ', '107011  ', '107020  ', '107021  ',
     &'10801   ', '10901   ', '12051   ', '20101   ', '20102   ',
     &'20103   ', '20201   ', '20202   ', '20203   ', '202030  '/
      DATA (R2HABT(I),I=351,NR2)/
C 351-375
     &'202031  ', '20204   ', '20205   ', '20206   ', '20301   ',
     &'20302   ', '20303   ', '20304   ', '20306   ', '20307   ',
     &'20401   ', '204010  ', '204011  ', '20402   ', '20403   ',
     &'204030  ', '204031  ', '20404   ', '20405   ', '20406   ',
     &'20407   ', '20408   ', '20409   ', '204090  ', '204091  ',
C 376-NR2
     &'20410   '/
C----------
C  LOAD REGION 3 HABITAT TYPE ARRAY
C----------
      DATA (R3HABT(I),I=1,50)/
C 1-25
     &'001010  ', '001011  ', '001012  ', '001013  ', '001020  ',
     &'001021  ', '001022  ', '001030  ', '001040  ', '001041  ',
     &'001042  ', '001050  ', '001051  ', '001052  ', '001053  ',
     &'001054  ', '001060  ', '001070  ', '001080  ', '001081  ',
     &'001090  ', '001100  ', '001110  ', '001111  ', '001120  ',
C 26-50
     &'001130  ', '001140  ', '001141  ', '001150  ', '001160  ',
     &'003     ', '003060  ', '003080  ', '003090  ', '003110  ',
     &'003111  ', '003112  ', '003200  ', '003201  ', '003202  ',
     &'003203  ', '003231  ', '003240  ', '003300  ', '003301  ',
     &'003310  ', '003320  ', '003350  ', '003370  ', '004060  '/
      DATA (R3HABT(I),I=51,100)/
C 51-75
     &'004061  ', '004062  ', '00415   ', '004151  ', '004152  ',
     &'004300  ', '004310  ', '004320  ', '004330  ', '004340  ',
     &'00435   ', '004350  ', '004351  ', '004360  ', '006010  ',
     &'00604   ', '006060  ', '006070  ', '006071  ', '006080  ',
     &'006090  ', '006130  ', '011     ', '011030  ', '011031  ',
C 76-100
     &'011032  ', '011033  ', '011034  ', '011035  ', '011090  ',
     &'011091  ', '011092  ', '011093  ', '011130  ', '011210  ',
     &'011211  ', '011212  ', '011213  ', '011214  ', '011215  ',
     &'011216  ', '011220  ', '011320  ', '011330  ', '011340  ',
     &'011341  ', '011350  ', '011360  ', '011361  ', '011370  '/
      DATA (R3HABT(I),I=101,150)/
C 101-125
     &'011380  ', '011390  ', '011391  ', '011392  ', '011400  ',
     &'011410  ', '011411  ', '011420  ', '011430  ', '011440  ',
     &'011460  ', '011470  ', '011500  ', '01203   ', '01213   ',
     &'012140  ', '012141  ', '012142  ', '012143  ', '01231   ',
     &'012320  ', '012330  ', '012331  ', '012332  ', '012333  ',
C 126-150
     &'012340  ', '012341  ', '012350  ', '012360  ', '012361  ',
     &'012362  ', '012380  ', '01239   ', '01241   ', '01242   ',
     &'012430  ', '03101   ', '03102   ', '032010  ', '032030  ',
     &'033010  ', '033020  ', '033030  ', '103     ', '104     ',
     &'123     ', '130     ', '201010  ', '201011  ', '201020  '/
      DATA (R3HABT(I),I=151,200)/
C 151-175
     &'201040  ', '201331  ', '201332  ', '201333  ', '201340  ',
     &'201350  ', '20140   ', '201400  ', '201410  ', '201420  ',
     &'201430  ', '202020  ', '202320  ', '202321  ', '202330  ',
     &'202331  ', '202500  ', '204010  ', '204011  ', '204012  ',
     &'204021  ', '204022  ', '204023  ', '204024  ', '204031  ',
C 176-200
     &'204032  ', '204033  ', '20404   ', '204050  ', '20406   ',
     &'20410   ', '20411   ', '204300  ', '20431   ', '204320  ',
     &'204321  ', '204330  ', '204350  ', '204360  ', '204370  ',
     &'204400  ', '20441   ', '204500  ', '230030  ', '230040  ',
     &'230041  ', '230042  ', '231010  ', '231020  ', '231021  '/
      DATA (R3HABT(I),I=201,242)/
C 201-225
     &'231030  ', '231040  ', '231050  ', '232020  ', '232030  ',
     &'23204   ', '232050  ', '232060  ', '232070  ', '232330  ',
     &'233010  ', '233020  ', '233021  ', '233022  ', '233030  ',
     &'233040  ', '233041  ', '233042  ', '233050  ', '233330  ',
     &'238040  ', '238300  ', '238310  ', '240300  ', '25000   ',
C 226-NR3
     &'335     ', '610010  ', '610020  ', '620010  ', '620020  ',
     &'620021  ', '620030  ', '620040  ', '630010  ', '630020  ',
     &'630030  ', '630040  ', '630041  ', '630042  ', '630043  ',
     &'630050  ', '650010 '/
C
      LPVREF=.FALSE.
      LPVCOD=.FALSE.
      LPVXXX=.FALSE.
C-----------
C  CHECK FOR DEBUG.
C-----------
      CALL DBCHK (DEBUG,'HABTYP',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,*)
     &' ENTERING HABTYP CYCLE,KODTYP,KODFOR,KARD2,ARRAY2,CPVREF= ',
     &ICYC,KODTYP,KODFOR,KARD2,ARRAY2,CPVREF
C----------
C  CHECK FOR OLD HABITAT TYPE CODE AND REMOVE PREPENDED 2 (R2)
C  OR PREPENDED 3 (R3)
C----------
      IR2=0
      IR3=0
      IF(KODFOR.LT.300)THEN
        IR2=1
      ELSE
        IR3=1
      ENDIF
      LNE1ST=.FALSE.
      KARD2=ADJUSTL(KARD2)
      IF(IR2 .EQ. 1) THEN
        IF((KARD2(1:1).EQ.'2').AND.
     &    ((KARD2.NE. '201       ').AND.(KARD2.NE.'2010      ').AND.
     &     (KARD2.NE. '20101     ').AND.(KARD2.NE.'20102     ').AND.
     &     (KARD2.NE. '202       ').AND.(KARD2.NE.'2020      ').AND.
     &     (KARD2.NE. '20202     ').AND.(KARD2.NE.'20203     ').AND.
     &     (KARD2.NE. '20204     ').AND.(KARD2.NE.'202040    ').AND.
     &     (KARD2.NE. '20210     ').AND.(KARD2.NE.'203       ').AND.
     &     (KARD2.NE. '2030      ').AND.(KARD2.NE.'20301     ').AND.
     &     (KARD2.NE. '20303     ').AND.(KARD2.NE.'20304     ').AND.
     &     (KARD2.NE. '20306     ').AND.(KARD2.NE.'20307     ').AND.
     &     (KARD2.NE. '204010    ').AND.(KARD2.NE.'204011    ').AND.
     &     (KARD2.NE. '204030    ').AND.(KARD2.NE.'204031    ').AND.
     &     (KARD2.NE. '20405     ').AND.(KARD2.NE.'20406     ').AND.
     &     (KARD2.NE. '20409     ').AND.(KARD2.NE.'204090    ').AND.
     &     (KARD2.NE. '2110      ').AND.(KARD2.NE.'20103     ')))THEN
            LNE1ST=.TRUE.
        ENDIF
       IF((KARD2(1:1).EQ.'2').AND.LNE1ST.AND.
     &    ((KARD2.NE. '20201     ').AND.(KARD2.NE.'20304     ').AND.
     &     (KARD2.NE. '202030    ').AND.(KARD2.NE.'204       ').AND.
     &     (KARD2.NE. '20205     ').AND.(KARD2.NE.'20402     ').AND.
     &     (KARD2.NE. '203       ').AND.(KARD2.NE.'20404     ').AND.
     &     (KARD2.NE. '20302     ').AND.(KARD2.NE.'20407     ').AND.
     &     (KARD2.NE. '204091    ').AND.(KARD2.NE.'20306     ').AND.
     &     (KARD2.NE. '202       ').AND.(KARD2.NE.'20401     ').AND.
     &     (KARD2.NE. '202010    ').AND.(KARD2.NE.'20403     ').AND.
     &     (KARD2.NE. '202031    ').AND.(KARD2.NE.'204040    ').AND.
     &     (KARD2.NE. '20206     ').AND.(KARD2.NE.'20408     ').AND.
     &     (KARD2.NE. '203       ').AND.(KARD2.NE.'20410     ').AND.
     &     (KARD2.NE. '20302     ')))THEN
            KARD2=KARD2(2:10)
        ENDIF
      ELSEIF(IR3 .EQ. 1)THEN
        IF((KARD2(1:1).EQ.'3').AND.(KARD2.NE. '335       '))
     &      KARD2=KARD2(2:10)
      ENDIF
  100 CONTINUE
C----------
C  IF REFERENCE CODE IS NON-ZERO THEN MAP PV CODE/REF. CODE TO
C  FVS HABITAT TYPE/ECOCLASS CODE. THEN PROCESS FVS CODE
C----------
      IF(CPVREF.NE.'          ') THEN
        IF(IR2 .EQ. 1) THEN
          ICL5=0
          CALL PVREF2(KARD2,ARRAY2,LPVCOD,LPVREF)
        ELSE
          ICL5=0
          CALL PVREF3(KARD2,ARRAY2,LPVCOD,LPVREF)
        ENDIF
        IF((LPVCOD.AND.LPVREF).AND.
     &  (KARD2.EQ.'          ').AND.(PCOMX.NE.'2NDPASS '))THEN
          CALL ERRGRO(.TRUE.,34)
          LPVXXX=.TRUE.
        ELSEIF((.NOT.LPVCOD).AND.(.NOT.LPVREF).AND.
     &    (PCOMX.NE.'2NDPASS '))THEN
          CALL ERRGRO(.TRUE.,33)
          CALL ERRGRO(.TRUE.,32)
          LPVXXX=.TRUE.
        ELSEIF((.NOT.LPVREF).AND.LPVCOD.AND.
     &    (PCOMX.NE.'2NDPASS '))THEN
          CALL ERRGRO(.TRUE.,32)
          LPVXXX=.TRUE.
        ELSEIF((.NOT.LPVCOD).AND.LPVREF.AND.
     &    (PCOMX.NE.'2NDPASS '))THEN
          CALL ERRGRO(.TRUE.,33)
          LPVXXX=.TRUE.
        ENDIF
      ENDIF
C----------
C  DIGEST HABITAT TYPE CODE
C  IF KODFOR IS 0 (COULD HAPPEN IF PROCESSING A SETSITE KEYWORD BEFORE
C  A STDINFO KEYWORD) THEN SEARCH THROUGH THE ARRAYS FOR A VALID CODE,
C  IF NONE FOUND, THEN ASSUME IT IS A SEQUENCE NUMBER IF IT IS IN THE
C  CORRECT RANGE TO BE A SEQUENCE NUMBER. IF NEITHER OF THESE IS TRUE
C  THEN RETURN 0.
C----------
      IF(DEBUG)WRITE(JOSTND,*)' DIGESTING HABITAT CODE: KODFOR,KODTYP= '
     &,KODFOR,KODTYP
      IF(IR2 .EQ. 1) THEN
        CALL CRDECD(KODTYP,R2HABT(1),NR2,ARRAY2,KARD2)
        IF(DEBUG)WRITE(JOSTND,*)' AFTER R2 DECODE,KODTYP= ',KODTYP
        IF(KODTYP .LT. 0) KODTYP=0
        ITYPE=KODTYP
      ELSEIF(IR3 .EQ. 1)THEN
        CALL CRDECD(KODTYP,R3HABT(1),NR3,ARRAY2,KARD2)
        IF(DEBUG)WRITE(JOSTND,*)' AFTER R3 DECODE,KODTYP= ',KODTYP
        IF(KODTYP .LT. 0) KODTYP=0
        ITYPE=KODTYP
        IF(KODTYP .GT. 0)KODTYP=KODTYP+NR2
      ENDIF
      IF(KODFOR.EQ.0 .OR. KODTYP.EQ.0)THEN
        IF(DEBUG)WRITE(JOSTND,*)' DIGESTING WITH NO FOREST CODE OR ',
     &   ' KODTYP OF ZERO, KODFOR,KODTYP,KARD2= ',KODFOR,KODTYP,KARD2
        LIMIT = NR2 + NR3
        DO 200 I=1,LIMIT
        IF(I.LE.NR2 .AND. (KODFOR.EQ.0 .OR. IR2.EQ.1))THEN
          IF(KARD2 .EQ. R2HABT(I))THEN
            KODTYP=I
            ITYPE=I
            GO TO 300
          ENDIF
        ELSEIF(I.GT.NR2)THEN
          IF(KARD2 .EQ. R3HABT(I-NR2))THEN
            KODTYP=I
            ITYPE=I-NR2
            GO TO 300
          ENDIF
        ENDIF
  200   CONTINUE
C
C  KODFOR IS ZERO AND NO MATCH WAS FOUND, TREAT IT AS A SEQUENCE NUMBER.
C  
        IF(DEBUG)WRITE(JOSTND,*)' EXAMINING FOR INDEX, ARRAY2= ',ARRAY2
        IHB = IFIX(ARRAY2)
        IF(IHB.LE.NR2 .AND. IR2.EQ.1)THEN
          KODTYP=IHB
          ITYPE=IHB
        ELSEIF((IHB.GT.NR2 .AND. IHB.LE.LIMIT) .AND. IR3.EQ.1)THEN
          KODTYP = IHB
          ITYPE=IHB-NR2
        ELSE
          KODTYP=0
        ENDIF
      ENDIF
C
  300 CONTINUE
      IF(KODTYP .NE. 0)THEN
        ICL5=KODTYP
        IF((KODFOR.LT.300).AND.(KODFOR.GE.200)) THEN
          KARD2=R2HABT(ITYPE)
        ELSEIF(KODFOR.GE.300)THEN
          KARD2=R3HABT(ITYPE)
        ENDIF 
      ENDIF
      IF (KODTYP.EQ.0)THEN
        IF((LSTART).AND.(.NOT.LPVXXX).AND.(PCOMX.NE.'2NDPASS '))
     &  CALL ERRGRO (.TRUE.,14)
        KARD2='UNKNOWN   '
        PCOM='UNKNOWN '
        PCOMX='2NDPASS '
        ICL5=0
      ENDIF
      IF(DEBUG)WRITE(JOSTND,*)' LEAVING HABTYP KODTYP,ITYPE,ICL5,',
     &'KARD2,PCOM= ',KODTYP,ITYPE,ICL5,KARD2,PCOM
C
      RETURN
      END
