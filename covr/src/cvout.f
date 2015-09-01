      SUBROUTINE CVOUT
      IMPLICIT NONE
C----------
C  **CVOUT--COVR   DATE OF LAST REVISION:  03/26/15
C----------
C  PRINTS (PRE- AND POST-THIN) **COVER** STATISTICS BY CYCLE.
C  CALLED FROM **MAIN** ONCE PER STAND, AT THE END OF PROJECTION.
C---------
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
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'OUTCOM.F77'
C
C
      INCLUDE 'CVCOM.F77'
C
C
COMMONS
C
      INTEGER NCYCP1,I,ITHN,J,K,KODE
      INTEGER*4 IO1(16),IO2(16),IO3(16),IO4(16),IO5(16)
      INTEGER*4 IT1,IT2,IT3,IT4,IT5,ISUM1,ISUM2,ISUM3,ISUM4,ISUM31,
     >          ISUM32,ISUM33,ISUM7,ISUM8,ISUM9,ISUM10,ISUM11,ISUM12
      CHARACTER*4 SNAME(32)
      DATA SNAME /
     & 'ARUV','BERB','LIBO','PAMY','SPBE','VASC','CARX','LONI',
     & 'MEFE','PHMA','RIBE','ROSA','RUPA','SHCA','SYMP','VAME',
     & 'XETE','FERN','COMB','ACGL','ALSI','AMAL','CESA','CEVE',
     & 'COST','HODI','PREM','PRVI','SALX','SAMB','SORB','OTHR'/
C
C     CALL CVNOHD TO WRITE THE COVER OUTPUT WITHOUT HEADINGS.
C
      CALL CVNOHD
      NCYCP1 = NCYC + 1
C==================== CROWN COVER STATISTICS TABLE =====================
C SKIP TO STATEMENT 150 IF OUTPUT IS NOT DESIRED.
C----------
      IF ((.NOT.LCOVER).OR.(.NOT.LCNOP)) GO TO 150
C----------
C HEADINGS
C----------
      CALL GROHED (JOSHRB)
      WRITE (JOSHRB,9016) NPLT,MGMID,ITITLE
 9016 FORMAT (/'STAND ID: ',A26,4X,'MGMT CODE: ',A4,2X,A72/)
      CALL PPLABS (JOSHRB)
C----------
C WRITE DEFINITIONS OF ATTRIBUTES.
C----------
      WRITE (JOSHRB,9017)
 9017 FORMAT (//,51('-'), '  CANOPY COVER STATISTICS  ',52('-')
     &       / 52X,'(BASED ON STOCKABLE AREA)' // T49,31('-') /
     &       T50,'ATTRIBUTE BY 10'' HEIGHT CLASS' / T49,31('-') //
     &       T26,'  TREES -- TREES PER ACRE' /
     &       T26,'  COVER -- PERCENTAGE OF CANOPY COVER CONTRIBUTED',
     &           ' BY TREES IN HEIGHT CLASS' /
     &       T26,'   AREA -- CROWN PROFILE AREA (SQ.FT. PER ACRE)'/
     &       T26,' VOLUME -- CROWN VOLUME (CU.FT. PER ACRE X 100)'/
     &       T26,'BIOMASS -- FOLIAGE BIOMASS (LBS. PER ACRE)' ///)
C----------
C WRITE COLUMN HEADINGS.
C----------
      WRITE (JOSHRB,9019)
 9019 FORMAT(130('-')/T54,'STAND HEIGHT CLASS'/
     &  '            0.0-  10.1-  20.1-  30.1-  40.1-  50.1-  60.1-  ',
     &  '70.1-  80.1-  90.1- 100.1- 110.1- 120.1- 130.1- ',
     &  '140.1- 150.1+' /
     &  'YEAR       10.0''  20.0''  30.0''  40.0''  50.0''  ',
     &              '60.0''  70.0''  80.0''  90.0'' 100.0'' ',
     &             '110.0'' 120.0'' 130.0'' 140.0'' 150.0'' ',
     &       T125,'TOTAL'/,130('-'))
C----------
C OUTPUT STATISTICS, CONVERTING TO FIXED VALUES.
C----------
      DO 140 I = 1,NCYCP1
      IF (I .LT. ICVBGN) GO TO 140
      ITHN = 1
  130 IF (ITHN .EQ. 1) WRITE (JOSHRB,9071) IOSUM(1,I)
      IF (ITHN .EQ. 2) WRITE (JOSHRB,9070) IOSUM(1,I)
 9071 FORMAT (I4)
 9070 FORMAT (I4, ':POST-THIN')
      DO 142 J = 1,16
      IO1(J) = IFIX(.5+TXHT(I,ITHN,J))
      IO2(J) = IFIX(.5+PCXHT(I,ITHN,J))
      IO3(J) = IFIX(.5+PROXHT(I,ITHN,J))
      IO5(J) = IFIX(.5+VOLXHT(I,ITHN,J)/100.)
      IO4(J) = IFIX(.5+CFBXHT(I,ITHN,J))
  142 CONTINUE
      IT1 = IFIX(.5+TRETOT(I,ITHN))
      IT2 = IFIX(.5+TPCTCV(I,ITHN))
      IT3 = IFIX(.5+TPROAR(I,ITHN))
      IT5 = IFIX(.5+TCVOL(I,ITHN)/100.)
      IT4 = IFIX(.5+TOTBMS(I,ITHN))
      WRITE (JOSHRB,9018)
     & (IO1(J),J=1,16),IT1,
     & (IO2(J),J=1,16),IT2,
     & (IO3(J),J=1,16),IT3,
     & (IO5(J),J=1,16),IT5,
     & (IO4(J),J=1,16),IT4
 9018 FORMAT ( '    TREES',16I7,I8 /
     &         '    COVER',16I7,I8 /
     &         '     AREA',16I7,I8 /
     &         '   VOLUME',16I7,I8 /
     &         '  BIOMASS',16I7,I8/)
C----------
C  GO TO NEXT CYCLE IF POST-THIN STATS WERE JUST WRITTEN.
C----------
      IF (ITHN .EQ. 2) GO TO 140
      IF (LTHIND(I)) ITHN = 2
C----------
C OUTPUT POST-THIN STATISTICS IF APPLICABLE.
C----------
      IF (ITHN .EQ. 2) GO TO 130
  140 CONTINUE
  150 CONTINUE
C======================== SHRUB OUTPUT OPTIONS =========================
C
      IF ((.NOT.LSHRUB).OR.(.NOT. LBROW)) GO TO 175
C
C==================== SHRUB LAYER CALIBRATION TABLE ====================
      IF ( .NOT. LCAL1 ) GO TO 15
      CALL GROHED(JOSHRB)
      WRITE (JOSHRB,9016) NPLT,MGMID,ITITLE
      CALL PPLABS (JOSHRB)
      WRITE(JOSHRB,1000)
 1000 FORMAT(//46('-'),
     &       '  SHRUB MODEL CALIBRATION STATISTICS  ',47('-'), / )
      WRITE(JOSHRB,1010)
 1010 FORMAT( // ,'CALIBRATION BY SHRUB LAYER (SHRBLAYR KEYWORD',
     &       ' CARD):' //
     & T5,'      AVERAGE HEIGHT (FEET)        ',
     &T45,'       AVERAGE PERCENT COVER       '/
     & T5,'-----------------------------------',
     &T45,'-----------------------------------'/
     & T5,'SHRUB  OBSERVED  PREDICTED  SCALING',
     &T45,'SHRUB  OBSERVED  PREDICTED  SCALING'/
     & T5,'LAYER  VALUES    VALUES     FACTORS',
     &T45,'LAYER  VALUES    VALUES     FACTORS'/
     & T5,'-----  --------  ---------  -------',
     &T45,'-----  --------  ---------  -------'/)
      DO 5 I=1,NKLASS
      WRITE(JOSHRB,1020) I,AVGBHT(I),HTAVG(I),HTFRAC(I),
     &                   I,AVGBPC(I),CVAVG(I),CVFRAC(I)
 1020 FORMAT(T5,I4,F10.1,F11.1,F9.2,T46,I4,F10.1,F11.1,F9.2)
    5 CONTINUE
      WRITE(JOSHRB,1030)
 1030 FORMAT( //,T32,'HEIGHT',T47,'% COVER', / ,
     &       T7,'SHRUB',5X,'ASSIGNED',7X,'SCALING',8X,
     &       'SCALING', / ,T6,'SPECIES',5X,'LAYER',9X,
     &       'FACTOR',9X,'FACTOR', / ,T6,'-------',4X,
     &       '--------',6X,'---------',6X,'---------', / )
      DO 10 I=1,31
         WRITE(JOSHRB,1040) SNAME(I),ILAYR(I),BHTCF(I),BPCCF(I)
 1040    FORMAT(T8,A4,T20,I1,T33,F5.2,T48,F5.2)
   10 CONTINUE
      GO TO 19
   15 CONTINUE
C============== SHRUB HEIGHT/PERCENT COVER CALIBRATION TABLE ===========
      IF ( .NOT. LCAL2 ) GO TO 19
      CALL GROHED(JOSHRB)
      WRITE (JOSHRB,9016) NPLT,MGMID,ITITLE
      CALL PPLABS (JOSHRB)
      WRITE(JOSHRB,1000)
      WRITE(JOSHRB,1050)
 1050 FORMAT( //,'CALIBRATION BY INDIVIDUAL SPECIES (SHRUBHT ',
     &       'AND/OR SHRUBPC KEYWORD CARDS):', // ,
     &       T25,'SHRUB HEIGHT (FEET)  ',T68,'PERCENT COVER' /
     &       T19,34('-'),5X,34('-') /
     &       ' ',T6,'SHRUB',T19,' OBSERVED     PREDICTED',4X,'SCALING',
     &       5X, ' OBSERVED     PREDICTED',4X,'SCALING' /
     &       ' ',T5,'SPECIES',T22,'VALUE ',7X,'VALUE',6X,'FACTOR',
     &       9X, 'VALUE ',7X,'VALUE',6X,'FACTOR' /
     &       ' ',T5,7('-'),T19,11('-'),3X,9('-'),3X,8('-'),5X,
     &          11('-'),3X,9('-'),3X,8('-') )
C----------
C  OUTPUT THE ACTUAL FIGURES. IF NO DATA WAS INPUT FOR A SHRUB SPECIES
C  (SHRBHT(I) AND/OR SHRBPC(I) = -99999.0), BYPASS OUTPUT OF THAT
C  MISSING VALUE.
C----------
      DO 17 I=1,31
         IF ((SHRBHT(I) .EQ. -99999.0) .AND. (SHRBPC(I) .NE. -99999.0))
     &        WRITE(JOSHRB,1060) SNAME(I),XSH(I),BHTCF(I),
     &                           SHRBPC(I),XCV(I),BPCCF(I)
 1060    FORMAT(T7,A4,T33,F7.1,T45,F7.2,T59,F7.1,T72,F7.1,T84,F7.2)
         IF ((SHRBHT(I) .NE. -99999.0) .AND. (SHRBPC(I) .EQ. -99999.0))
     &        WRITE(JOSHRB,1061) SNAME(I),SHRBHT(I),XSH(I),
     &                           BHTCF(I),XCV(I),BPCCF(I)
 1061    FORMAT(T7,A4,T20,F7.1,T33,F7.1,T45,F7.2,T72,F7.1,T84,F7.2)
         IF ((SHRBHT(I) .EQ. -99999.0) .AND. (SHRBPC(I) .EQ. -99999.0))
     &        WRITE(JOSHRB,1062) SNAME(I),XSH(I),BHTCF(I),
     &                           XCV(I),BPCCF(I)
 1062    FORMAT(T7,A4,T33,F7.1,T45,F7.2,T72,F7.1,T84,F7.2)
         IF ((SHRBHT(I) .NE. -99999.0) .AND. (SHRBPC(I) .NE. -99999.0))
     &        WRITE(JOSHRB,1063) SNAME(I),SHRBHT(I),XSH(I),
     &                           BHTCF(I),SHRBPC(I),XCV(I),BPCCF(I)
 1063    FORMAT(T7,A4,T20,F7.1,T33,F7.1,T45,F7.2,T59,F7.1,
     &          T72,F7.1,T84,F7.2)
   17 CONTINUE
   19 CONTINUE
C======================== SHRUB STATISTICS TABLE =======================
C WRITE HEADINGS.
C----------
      CALL GROHED (JOSHRB)
      WRITE (JOSHRB,9016) NPLT,MGMID,ITITLE
      CALL PPLABS (JOSHRB)
      WRITE (JOSHRB,9023)
 9023 FORMAT (//,54('-'),'  SHRUB STATISTICS  ',55('-') /
     &       52X,'(BASED ON STOCKABLE AREA)' /)
C----------
C WRITE DEFINITIONS OF SHRUB SPECIES ABBREVIATIONS.
C----------
      WRITE (JOSHRB,9026)
 9026 FORMAT(
     &T17,'   LOW SPECIES (0-1.7 FT)',
     &T49,' MEDIUM SPECIES (1.7-7 FT)',
     &T81,'   TALL SPECIES (7+ FT)' /
     &T17,'-----------------------------',
     &T49,'---------------------------',
     &T81,'--------------------------' /
     &T17,'ARUV:ARCTOSTAPHYLOS UVA-URSI',T49,'LONI:LONICERA SPP.     ',
     &'    ',T81,'ACGL:ACER GLABRUM         '/
     &T17,'BERB:BERBERIS SPP.          ',T49,'MEFE:MENZIESIA FERRUGIN',
     &'EA  ',T81,'ALSI:ALNUS SINUATA        '/
     &T17,'LIBO:LINNAEA BOREALIS       ',T49,'PHMA:PHYSOCARPUS MALVAC',
     &'EUS ',T81,'AMAL:AMELANCHIER ALNIFOLIA'/
     &T17,'PAMY:PACHISTIMA MYRSINITES  ',T49,'RIBE:RIBES SPP.        ',
     &'    ',T81,'CESA:CEANOTHUS SANGUINEUS '/
     &T17,'SPBE:SPIRAEA BETULIFOLIA    ',T49,'ROSA:ROSA SPP.         ',
     &'    ',T81,'CEVE:CEANOTHUS VELUTINUS  ')
      WRITE (JOSHRB,9027)
 9027 FORMAT (
     &T17,'VASC:VACCINIUM SCOPARIUM    ',T49,'RUPA:RUBUS PARVIFLORUS ',
     &'    ',T81,'COST:CORNUS STOLONIFERA   '/
     &T17,'CARX:CAREX SPP.             ',T49,'SHCA:SHEPHERDIA CANADEN',
     &'SIS ',T81,'HODI:HOLODISCUS DISCOLOR  '/
     &                                   T49,'SYMP:SYMPHORICARPOS SPP',
     &'.   ',T81,'PREM:PRUNUS EMARGINATA    '/
     &                                   T49,'VAME:VACCINIUM MEMBRANA',
     &'CEUM',T81,'PRVI:PRUNUS VIRGINIANA    '/
     &                                   T49,'XETE:XEROPHYLLUM TENAX ',
     &'    ',T81,'SALX:SALIX SPP.           '/
     &                                   T49,'FERN:FERNS             ',
     &'    ',T81,'SAMB:SAMBUCUS SPP.        '/
     &                                   T49,'COMB:OTHER SHRUBS COMBI',
     &'NED ',T81,'SORB:SORBUS SPP.          '/)
C----------
C WRITE DEFINITIONS OF ATTRIBUTES.
C----------
      WRITE (JOSHRB,9028)
 9028 FORMAT(T25,80('-')/T25,' ATTRIBUTES OF THE FIRST THREE SPECIES ',
     &'WITH GREATEST COVER IN EACH HEIGHT GROUP'/T36,' (ALL OTHERS ',
     &'WITHIN GROUP COMBINED INTO CATEGORY "OTHR")' /T25,80('-')/
     &       T32,'  COVER -- SPECIES COVER '/
     &       T32,' HEIGHT -- AVERAGE SPECIES HEIGHT (FEET)  ' /
     &       T32,'   PROB -- SPECIES PROBABILITY OF OCCURRENCE '//
     &'YEAR           ',T20,'      LOW SPECIES    ',
     &T46,'    MEDIUM SPECIES   ',
     &T72,'     TALL SPECIES    ',
     &T96,'    USER-SELECTED INDICATOR SPECIES'/
     &'----           ',T20,'----------------------',
     &T46,'----------------------',
     &T72,'----------------------',
     &T96,'  ----------------------------------'/)
C----------
C WRITE SHRUB PROBABILITY, HEIGHT, AND PERCENT COVER STATISTICS.
C----------
      ITHN = 1
      DO 170 I=1,NCYCP1
      IF (I .LT. ICVBGN) GO TO 170
      ITHN = 1
  157 CONTINUE
C----------
C SHRUB STATISTICS WILL ONLY BE OUTPUT UP TO THE POINT WHERE TIME
C SINCE DISTURBANCE EXCEEDS RANGE OF DATA (40 YEARS).
C----------
      IF ( TIMESD(I,ITHN) .LE. 40. ) GO TO 159
      IF (ITHN .EQ. 1) WRITE (JOSHRB,9024) IOSUM(1,I),TIMESD(I,ITHN)
 9024 FORMAT (I4,'           : TIME SINCE DISTURBANCE=',F5.0,
     & ' EXCEEDS 40 YEARS.  SHRUB STATISTICS NOT COMPUTED.'/)
      IF (ITHN .EQ. 2) WRITE (JOSHRB,9025) IOSUM(1,I),TIMESD(I,ITHN)
 9025 FORMAT (I4,' POST-THIN : TIME SINCE DISTURBANCE=',F5.0,
     & ' EXCEEDS 40 YEARS.  SHRUB STATISTICS NOT COMPUTED.'/)
      GO TO 160
  159 CONTINUE
      IF (ITHN .EQ. 1) WRITE (JOSHRB,9071) IOSUM(1,I)
      IF (ITHN .EQ. 2) WRITE (JOSHRB,9070) IOSUM(1,I)
      IF (NSHOW.GT.0) GO TO 158
      WRITE (JOSHRB,9032)
     &          (SNAME(ISSP(I,ITHN,K)),K=1,12),
     &          (SCV(I,ITHN,K),K=1,12),
     &          (SHT(I,ITHN,K),K=1,12),
     &          (SPB(I,ITHN,K),K=1,12)
 9032 FORMAT('SPECIES',10X,3(4(2X,A4),2X) /
     &        '  COVER',10X,3(4F6.1,2X)   /
     &        ' HEIGHT',10X,3(4F6.1,2X)   /
     &        '   PROB',10X,3(4F6.1,2X)   /)
      GO TO 160
  158 CONTINUE
      WRITE (JOSHRB,9151) (SNAME(ISSP(I,ITHN,K)),K=1,12),
     &               (SNAME(INDSP(I,ITHN,K)),K=1,NSHOW)
      WRITE (JOSHRB,9152) (SCV(I,ITHN,K),K=1,12),
     &      (CIND(I,ITHN,K),K=1,NSHOW)
      WRITE (JOSHRB,9153) (SHT(I,ITHN,K),K=1,12),
     &      (HIND(I,ITHN,K),K=1,NSHOW)
      WRITE (JOSHRB,9154) (SPB(I,ITHN,K),K=1,12),
     &      (PIND(I,ITHN,K),K=1,NSHOW)
 9151 FORMAT( 'SPECIES',10X,3(4(2X,A4),2X),T96,6(2X,A4) )
 9152 FORMAT( '  COVER',10X,3(4F6.1,2X)   ,T96,6F6.1    )
 9153 FORMAT( ' HEIGHT',10X,3(4F6.1,2X)   ,T96,6F6.1    )
 9154 FORMAT( '   PROB',10X,3(4F6.1,2X)   ,T96,6F6.1   /)
  160 CONTINUE
C----------
C  GO TO NEXT CYCLE IF POST-THIN STATS WERE JUST WRITTEN.
C----------
      IF (ITHN .EQ. 2) GO TO 170
      IF (LTHIND(I)) ITHN = 2
C----------
C OUTPUT POST-THIN STATISTICS IF APPLICABLE.
C----------
      IF (ITHN .EQ. 2) GO TO 157
  170 CONTINUE
  175 CONTINUE
C==================== CROWN AND SHRUB SUMMARY TABLE ====================
      IF ((.NOT.LCVSUM).OR.((.NOT.LCNOP).AND.(.NOT.LBROW))) GO TO 400
C----------
C HEADINGS.
C----------
      CALL GROHED (JOSHRB)
      WRITE (JOSHRB,9016) NPLT,MGMID,ITITLE
      CALL PPLABS (JOSHRB)
      WRITE (JOSHRB,9033)
 9033 FORMAT (//48('-'),'  CANOPY AND SHRUBS SUMMARY  ',49('-') /
     &       50X,'(BASED ON STOCKABLE AREA)' /)
C----------
C  COMPUTE SUMMARY TABLE OUTPUT CODE.
C----------
C  CASE 1:  OUTPUT SHRUB AND CANOPY STATS.
C----------
      KODE = 1
C----------
C  CASE 2:  OUTPUT SHRUB STATS ONLY.
C----------
      IF ((LBROW) .AND. (.NOT. LCNOP)) KODE=2
C----------
C  CASE 3:  OUTPUT CANOPY STATS ONLY.
C----------
      IF ((.NOT. LBROW) .AND. (LCNOP)) KODE=3
C----------
C WRITE DEFINITIONS OF SUCCESSIONAL STAGE CODES USED IN
C SUMMARY OUTPUT TABLE.
C----------
      IF (LSHRUB) WRITE (JOSHRB,9050)
 9050 FORMAT( // T26,'-------- DEFINITIONS OF SUCCESSIONAL ',
     &       'STAGE CODES USED IN OUTPUT --------' / T25,
     &       ' 1: RECENT DISTURBANCE',T63,
     &       ' 6: TALL SHRUB WITH MOSTLY CONIFERS' / T25,
     &       ' 2: LOW SHRUB',T63,' 7: SAPLING TIMBER' / T25,
     &       ' 3: MEDIUM SHRUB',T63,' 8: POLE TIMBER' / T25,
     &       ' 4: TALL SHRUB WITH NO CONIFERS',T63,
     &       ' 9: MATURE TIMBER' / T25,
     &       ' 5: TALL SHRUB WITH FEW CONIFERS',T63,
     &       '10: OLD-GROWTH TIMBER' //)
C----------
C WRITE COLUMN HEADINGS.
C----------
      WRITE (JOSHRB,9044)
 9044 FORMAT(
     &9X,'------------------- UNDERSTORY ATTRIBUTES -----',
     &'--------------         ------------ OVERSTORY A',
     &'TTRIBUTES -------------'/
     &9X,'TIME     PROB.   -----SHRUB COVER----  AVG.    ',
     &'DORMANT                                        ',
     &'         SUM OF        '/
     &9X,'SINCE    (SHRUB                        SHRUB   ',
     &'SHRUB    TWIGS  SUCC.  STAND  TOP     CANOPY   ',
     &'FOLIAGE  STEM    NUMBER'/
     &9X,'DISTURB. COV>0)  LOW  MED TALL TOTAL   HEIGHT  ',
     &'BIOMASS  (NO./  STAGE  AGE    HEIGHT   COVER   ',
     &'BIOMASS  DIAMS.   OF   '/
     &'DATE',5X,'(YEARS)   (%)    (%)  (%)  (%)  (%)    (FEET)  ',
     &'(LB/AC)  SQFT)  CODE   (YRS)  (FEET)    (%)    ',
     &'(LB/AC)  (FEET)  STEMS '/
     &126('-'))
      DO 340 I=1,NCYCP1
      IF (I .LT. ICVBGN) GO TO 340
      ITHN = 1
  220 CONTINUE
      ISUM1 = IFIX (.5+TIMESD(I,ITHN))
      ISUM2 = IFIX (.5+PGT0(I,ITHN)*100.)
      ISUM31 = IFIX (.5+CLOW(I,ITHN))
      ISUM32 = IFIX (.5+CMED(I,ITHN))
      ISUM33 = IFIX (.5+CTALL(I,ITHN))
      ISUM3 = IFIX (.5+TOTLCV(I,ITHN))
      ISUM4 = IFIX (.5+SBMASS(I,ITHN))
      ISUM7 = ICVAGE(I)
      ISUM8 = IFIX (.5+STDHT(I,ITHN))
      ISUM9 = IFIX (.5+TPCTCV(I,ITHN))
      ISUM10 = IFIX (.5+TOTBMS(I,ITHN))
      ISUM11 = IFIX (.5+SDIAM(I,ITHN))
      ISUM12 = IFIX (.5+TRETOT(I,ITHN))
      GO TO (200,250,300),KODE
C----------
C  CASE 1: BOTH
C----------
  200 CONTINUE
      IF (TIMESD(I,ITHN) .GT. 40.) GO TO 215
      IF (ITHN .EQ. 1) WRITE (JOSHRB,9046) IOSUM(1,I),ISUM1,
     &  ISUM2,ISUM31,ISUM32,ISUM33,ISUM3,
     &  ASHT(I,ITHN),ISUM4,TWIGS(I,ITHN),ISTAGE(I,ITHN),
     &  ISUM7,ISUM8,ISUM9,ISUM10,ISUM11,ISUM12
 9046 FORMAT(I4,T10,I6,
     &      T16,I7,2X,4I5,2X,F7.1,I9,F8.1,I6,
     &      T78,I6,I7,I8,I11,2I8)
      IF (ITHN .EQ. 2) WRITE (JOSHRB,9047) ISUM1,
     &  ISUM2,ISUM31,ISUM32,ISUM33,ISUM3,
     &  ASHT(I,ITHN),ISUM4,TWIGS(I,ITHN),ISTAGE(I,ITHN),
     &  ISUM7,ISUM8,ISUM9,ISUM10,ISUM11,ISUM12
 9047 FORMAT('POST-THIN',T10,I6,
     &           T16,I7,2X,4I5,2X,F7.1,I9,F8.1,I6,
     &           T78,I6,I7,I8,I11,2I8)
      GO TO 222
  215 CONTINUE
      IF (ITHN .EQ. 1) WRITE (JOSHRB,9048) IOSUM(1,I),ISUM1,
     &  ISUM7,ISUM8,ISUM9,ISUM10,ISUM11,ISUM12
 9048 FORMAT(I4,T10,I6,
     &           T78,I6,I7,I8,I11,2I8)
      IF (ITHN .EQ. 2) WRITE (JOSHRB,9049) ISUM1,
     &  ISUM7,ISUM8,ISUM9,ISUM10,ISUM11,ISUM12
 9049 FORMAT('POST-THIN',T10,I6,
     &           T78,I6,I7,I8,I11,2I8)
  222 CONTINUE
C----------
C WRITE POST-THIN STATISTICS IF APPLICABLE.
C----------
      IF (ITHN .EQ. 2) GO TO 340
      IF (LTHIND(I)) ITHN = 2
      IF (ITHN .EQ. 2) GO TO 220
      GO TO 340
C----------
C  CASE 2:  SHRUBS ONLY
C----------
  250 CONTINUE
      IF (TIMESD(I,ITHN) .GT. 40.) GO TO 265
      IF (ITHN .EQ. 1) WRITE (JOSHRB,9056) IOSUM(1,I),ISUM1,
     &  ISUM2,ISUM31,ISUM32,ISUM33,ISUM3,
     &  ASHT(I,ITHN),ISUM4,TWIGS(I,ITHN),ISTAGE(I,ITHN)
 9056 FORMAT(I4,T10,I6,
     &           T16,I7,2X,4I5,2X,F7.1,I9,F8.1,I6)
      IF (ITHN .EQ. 2) WRITE (JOSHRB,9057) ISUM1,
     &  ISUM2,ISUM31,ISUM32,ISUM33,ISUM3,
     &  ASHT(I,ITHN),ISUM4,TWIGS(I,ITHN),ISTAGE(I,ITHN)
 9057 FORMAT('POST-THIN',T10,I6,
     &           T16,I7,2X,4I5,2X,F7.1,I9,F8.1,I6)
      GO TO 270
  265 CONTINUE
      IF (ITHN .EQ. 1) WRITE (JOSHRB,9058) IOSUM(1,I),ISUM1
 9058 FORMAT(I4,T10,I6)
      IF (ITHN .EQ. 2) WRITE (JOSHRB,9059) ISUM1
 9059 FORMAT('POST-THIN',T10,I6)
  270 CONTINUE
C----------
C WRITE POST-THIN STATISTICS IF APPLICABLE.
C----------
      IF (ITHN .EQ. 2) GO TO 340
      IF (LTHIND(I)) ITHN = 2
      IF (ITHN .EQ. 2) GO TO 220
      GO TO 340
C----------
C  CASE 3: CROWN COVER ONLY
C----------
  300 CONTINUE
      IF (ITHN .EQ. 1) WRITE (JOSHRB,9066) IOSUM(1,I),
     &  ISUM7,ISUM8,ISUM9,ISUM10,ISUM11,ISUM12
 9066 FORMAT(I4,
     &           T78,I6,I7,I8,I11,2I8)
      IF (ITHN .EQ. 2) WRITE (JOSHRB,9067)
     &  ISUM7,ISUM8,ISUM9,ISUM10,ISUM11,ISUM12
 9067 FORMAT('POST-THIN',
     &           T78,I6,I7,I8,I11,2I8)
C----------
C WRITE POST-THIN STATISTICS IF APPLICABLE.
C----------
      IF (ITHN .EQ. 2) GO TO 340
      IF (LTHIND(I)) ITHN = 2
      IF (ITHN .EQ. 2) GO TO 220
      GO TO 340
  340 CONTINUE
      WRITE (JOSHRB,9099)
C----------
C  WRITE SHRUB-SMALL CONIFER COMPETITION DISPLAY.
C----------
      IF (KODE .EQ. 3) GO TO 400
      WRITE (JOSHRB,9090)
 9090 FORMAT (//,45('-'),'  SHRUB-SMALL CONIFER COMPETITION  ',
     & 46('-') // T31,'SHRUB COVER -- TOTAL COVER OF SHRUBS GREATER ',
     & 'THAN HEIGHT' / T31,'TREES/ACRE  -- TOTAL NUMBER OF TREES ',
     & 'PER ACRE GREATER THAN HEIGHT'/)
      WRITE (JOSHRB,9092)
 9092 FORMAT(126('-') / T57,'  HEIGHT (FEET)' /
     &'YEAR',24X,'0.0    0.5    1.0    2.0    3.0    4.0    5.0    ',
     &'7.5   10.0   15.0   20.0' / 126('-'))
C----------
C OUTPUT STATISTICS, CONVERTING TO FIXED VALUES.
C----------
      DO 390 I = 1,NCYCP1
      IF (I .LT. ICVBGN) GO TO 390
      ITHN = 1
  380 CONTINUE
      DO 385 J = 1,11
      IO1(J) = IFIX(.5+SCOV(I,ITHN,J))
      IO2(J) = IFIX(.5+TRSH(I,ITHN,J))
  385 CONTINUE
      IF ((TIMESD(I,ITHN).LE. 40.).AND.(ITHN .EQ. 1))
     &   WRITE (JOSHRB,9094) IOSUM(1,I),
     & (IO1(J),J=1,11),(IO2(J),J=1,11)
 9094 FORMAT (I4,'        SHRUB COVER ',11I7/
     &            '            TREES/ACRE  ',11I7 /)
      IF ((TIMESD(I,ITHN).LE. 40.).AND.(ITHN .EQ. 2))
     &   WRITE (JOSHRB,9096)
     & (IO1(J),J=1,11),(IO2(J),J=1,11)
 9096 FORMAT ('POST-THIN : SHRUB COVER ',11I7/
     &          '            TREES/ACRE  ',11I7 /)
C----------
C  GO TO NEXT CYCLE IF POST-THIN STATS WERE JUST WRITTEN.
C----------
      IF (ITHN .EQ. 2) GO TO 390
      IF (LTHIND(I)) ITHN = 2
C----------
C OUTPUT POST-THIN STATISTICS IF APPLICABLE.
C----------
      IF (ITHN .EQ. 2) GO TO 380
  390 CONTINUE
      WRITE (JOSHRB,9099)
 9099 FORMAT(126('-'))
  400 CONTINUE
      RETURN
      END
