      SUBROUTINE SUMOUT(IOSUM,I20,ICALLF,JOPRT,JSTND2,JSUM2,
     >                  LENG,MGMID,NPLT,SAMWT,ITITLE,IPTINV)
      IMPLICIT NONE
C----------
C METRIC-VBASE $Id: sumout.f 2494 2018-09-12 20:34:17Z lancedavid $
C----------
C
C     WRITES SUMMARY OUTPUT.
C
C     IOSUM = THE SUMMARY OUTPUT ARRAY FROM THE PROGNOSIS MODEL.
C              1: YEAR
C              2: AGE
C              3: TREES/HA                       START OF PERIOD
C              4: TOTAL CU M                     START OF PERIOD
C     *        4: MERCH CU M  (PULP AND SAWLOG)  START OF PERIOD
C              5: MERCH CU M                     START OF PERIOD 
C     *        5: MERCH CU M  (SAWLOG)           START OF PERIOD
C              6: MERCH CU M                     START OF PERIOD  NET MERCH with cull in ON
C     *        6: MERCH CU M  (SAWLOG)           START OF PERIOD  NET MERCH with cull in ON
C              7: REMOVED TREES/HA  
C              8: REMOVED TOTAL CU M 
C     *        8: REMOVED MERCH CU M  (PULP AND SAWLOG)
C              9: REMOVED MERCH CU M 
C     *        9: REMOVED MERCH CU M  (SAWLOG)
C             10: REMOVED MERCH CU M                              NET MERCH with cull in ON
C     *       10: REMOVED MERCH CU M  (SAWLOG)                    NET MERCH with cull in ON
C             11: BASAL AREA/HA                  AFTER TREATMENT
C             12: CCF                            AFTER TREATMENT
C             13: AVERAGE DOMINANT HEIGHT        AFTER TREATMENT (M)
C             14: PERIOD LENGTH (YEARS)
C             15: ACCRETION (ANNUAL IN CU M/HA)
C             16: MORTALITY (ANNUAL IN CU M/HA)
C             17: SAMPLE WEIGHT
C             18: FOREST COVER TYPE CODE
C             19: SIZE CLASS
C             20: STOCKING CLASS
C
C     ICALLF= CALL FLAG, 0=NORMAL
C     JSTND2= DATA SET REFERENCE NUMBER FOR 'PRINTED' COPY (WITH
C             HEADINGS AND CARRAGE CONTROL BYTE).  IF JSTND2=0, NO
C             DATA WILL BE WRITTEN.
C     JOPRT = PRINTER OUTPUT FOR MESSAGES.
C     JSUM2 = DATA SET REFERENCE NUMBER FOR 'NON-PRINTED' COPY (WITH
C             OUT HEADINGS, NO CARRAGE CONTROL BYTE). IF JSUM2=0,
C             NO DATA WILL BE WRITTEN.
C     LENG   = NUMBER OF ROWS (ENTRIES) IN IOSUM.
C     MGMID = MANAGEMENT IDENTIFICATION FIELD. ASSUMED ALPHANUMERIC.
C     NPLT  = PLOT IDENTIFICATION FIELD. ASSUMED ALPHANUMERIC.
C NOTE: * Indicates R8 and R9 specific (CS, LS, NE, OZ, SE, SN)
C----------
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'SUMTAB.F77'
C
C
      INCLUDE 'METRIC.F77'
C      
C
COMMONS
C
C----------
C  VARIABLE DECLARATIONS:
C----------
C
      LOGICAL LCONN,LDSK,LPRT
C
      CHARACTER CISN*11,DAT*10,ITITLE*72,MGMID*4,NPLT*26,RECORD*250
      CHARACTER REV*10,TIM*8
C
      INTEGER I,I12,I20,ICALLF,II,ISTLNB,JOPRT,JSTND2,JSUM2,K,LENG
C
      INTEGER*4 IPTINV
C
      INTEGER*4 IOSUM(I20,LENG)
C
      REAL SAMWT
C----------
C     **************************************************************
C
C     STEP1: SET SWITCHES.
C
      LPRT= JSTND2 .GT. 0
      LDSK= JSUM2 .GT. 0
      IF (.NOT. (LPRT.OR.LDSK)) RETURN
C----------
C  SET OLD PPE VARIABLE TO BLANK, BUT KEEP TO PRESERVE HEADER FORMAT
C----------
      CISN = '           '
C      
      CALL REVISE (VARACD,REV)
      CALL GRDTIM (DAT,TIM)
      
      IF(LDSK) THEN
        INQUIRE(UNIT=JSUM2,opened=LCONN)
        IF (.NOT.LCONN) THEN
          CALL fvsGetKeywordFileName(RECORD,len(RECORD),II)
          IF (RECORD.NE.' ') THEN
            II=index(RECORD,".k")
            IF (II == 0) II=index(RECORD,".K")
            IF (II == 0) II=len_trim(RECORD)
            RECORD=TRIM(RECORD(:II-1))//".sum"
            OPEN(UNIT=JSUM2,FILE=TRIM(RECORD),STATUS='replace')
          ENDIF
        ENDIF
        WRITE (JSUM2,2) LENG,NPLT,MGMID,SAMWT,VARACD,DAT,TIM,
     &                  REV,CISN,IPTINV
    2   FORMAT ('-999',I5,1X,A26,1X,A4,E15.7,5(1X,A),I3)
      ENDIF
C
C     STEP2: WRITE VARIANT SPECIFIC HEADING.
C            SKIP A FEW LINES, DO NOT START A NEW PAGE.
C
      IF (LPRT) THEN
      WRITE (JSTND2,5) NPLT,MGMID,ITITLE(1:ISTLNB(ITITLE))
    5 FORMAT(/'STAND ID: ',A26,4X,'MGMT ID: ',A4,4X,A/)
C
      SELECT CASE (VARACD)
C----------
C  WRITE HEADER FOR CS, LS, NE, SN
C----------
      CASE ('CS','LS','NE','SN')
      WRITE (JSTND2,12)
  12  FORMAT(//32X,'SUMMARY STATISTICS (PER HA OR STAND BASED ON TOTAL
     & STAND AREA)',/,
     &  134(1H-),/15X,'START OF SIMULATION PERIOD',21X,'REMOVALS',13X,
     &  'AFTER TREATMENT',4X,'GROWTH THIS PERIOD',/9X,45(1H-),1X,
     &  23(1H-),1X,21(1H-),2X,18(1H-),3X,'MAI  ------',/9X,
     &  'NO OF',14X,'TOP',
     &  6X,'MERCH SAWLG SAWLG NO OF MERCH SAWLG SAWLG',14X,'TOP  RES  ',
     &  'PERIOD ACCRE MORT   MERCH FOR SS',/,
     &  'YEAR AGE TREES  BA  SDI CCF ',
     &  'HT  QMD  CU M  CU M  BD FT TREES CU M  CU M  BD FT  BA  SDI ',
     &  'CCF HT   QMD  YEARS   PER  YEAR   CU M  TYP ZT',
     &  /'---- --- ----- ',
     &  '--- ---- --- --- ---- ',7('----- '),'--- ---- --- --- ----  ',
     &  '------ ---- -----   ----- ------')
C----------
C  WRITE HEADER FOR BC, ON
C----------
      CASE ('ON')
      WRITE (JSTND2,15)
  15  FORMAT(//32X,'SUMMARY STATISTICS (PER HA OR STAND BASED ON TOTAL
     & STAND AREA)',/,
     &  134(1H-),/15X,'START OF SIMULATION PERIOD',21X,'REMOVALS',13X,
     &  'AFTER TREATMENT',4X,'GROWTH THIS PERIOD',/9X,45(1H-),1X,
     &  23(1H-),1X,21(1H-),2X,18(1H-),3X,' MAI ------',/9X,
     &  'NO OF',14X,'TOP',
     &  6X,'  GTV   GMV   NMV NO OF   GTV   GMV   NMV',14X,'TOP  RES ',
     &  'PERIOD  ACCRE MORT     GTV FOR SS',/,
     &  'YEAR AGE TREES  BA  SDI CCF  ',
     &  'HT  QMD  CU M  CU M  CU M TREES  CU M  CU M  CU M  BA  SDI ',
     &  'CCF  HT  QMD  YEARS   PER  YEAR    CU M TYP ZT',
     &  /'---- --- ----- ',
     &  '--- ---- --- --- ---- ',7('----- '),'--- ---- --- --- ----  ',
     &  '------ ---- -----   ----- ------')

      CASE ('BC')
      WRITE (JSTND2,13)
  13  FORMAT(//32X,'SUMMARY STATISTICS (PER HA OR STAND BASED ON TOTAL
     & STAND AREA)',/,
     &  134(1H-),/15X,'START OF SIMULATION PERIOD',21X,'REMOVALS',12X,
     &  'AFTER TREATMENT',5X,'GROWTH THIS PERIOD',/9X,45(1H-),1X,
     &  23(1H-),1X,21(1H-),2X,18(1H-),2X,'  MAI ------',/9X,
     &  'NO OF',14X,'TOP',
     &  6X,'TOTAL MERCH   N/A NO OF TOTAL MERCH   N/A',14X,
     &  'TOP  RES ',' PERIOD ACCRE MORT   TOTAL FOR SS',/,
     &  'YEAR AGE TREES  BA  SDI CCF ',
     &  ' HT  QMD  CU M  CU M       TREES  CU M  CU M        BA  SDI ',
     &  'CCF  HT  QMD   YEARS  PER  YEAR    CU M TYP ZT',
     &  /'---- --- ----- ',
     &  '--- ---- --- --- ---- ',7('----- '),'--- ---- --- --- ----  ',
     &  '------ ---- -----   ----- ------')

C----------
C  WRITE HEADER FOR ALL OTHER VARIANTS
C----------
      CASE DEFAULT
      WRITE (JSTND2,14)
   14 FORMAT(//32X,'SUMMARY STATISTICS (PER HA OR STAND BASED ON TOTAL
     & STAND AREA)',/,
     &  134(1H-),/15X,'START OF SIMULATION PERIOD',21X,'REMOVALS',13X,
     &  'AFTER TREATMENT',4X,'GROWTH THIS PERIOD',/9X,45(1H-),1X,
     &  23(1H-),1X,21(1H-),2X,18(1H-),3X,'MAI  ------',/9X,
     &  'NO OF',14X,'TOP',
     &  6X,'TOTAL MERCH MERCH NO OF TOTAL MERCH MERCH',14X,'TOP  RES  ',
     &  'PERIOD ACCRE MORT   MERCH FOR SS',/,
     &  'YEAR AGE TREES  BA  SDI CCF ',
     &  'HT  QMD  CU M  CU M  BD FT TREES CU M  CU M  BD FT  BA  SDI ',
     &  'CCF HT   QMD  YEARS   PER  YEAR   CU M  TYP ZT',
     &  /'---- --- ----- ',
     &  '--- ---- --- --- ---- ',7('----- '),'--- ---- --- --- ----  ',
     &  '------ ---- -----   ----- ------')
      END SELECT
      ENDIF
C----------
C     STEP3: LOOP THRU ALL ROWS IN IOSUM...WRITE OUTPUT.
C
C  THIS STEP TAKES JUST THE FIRST 12 ITEMS IN THE IOSUM ARRAY
C----------
      I12=I20-8
      DO 50 I=1,LENG
C
      SELECT CASE (VARACD)
C
      CASE ('CS','LS','NE','SN','ON')
C
        IF(LPRT)
     &    WRITE(JSTND2,20) 
     &      IOSUM(1,I),
     &      IOSUM(2,I),
     &      INT(IOSUM(3,I)/ACRtoHA),
     &      INT(IOLDBA(I)*FT2pACRtoM2pHA),
     &      INT(ISDI(I)/ACRtoHA),
     &      IBTCCF(I),
     &      INT(IBTAVH(I)*FTtoM),
     &      QSDBT(I)*INtoCM,
     &      INT(IOSUM(4,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(5,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(6,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(7,I)/ACRtoHA),
     &      INT(IOSUM(8,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(9,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(10,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(11,I)*FT2pACRtoM2pHA),
     &      INT(ISDIAT(I)/ACRtoHA),
     &      IOSUM(12,I),
     &      INT(IOSUM(13,I)*FTtoM),
     &      QDBHAT(I)*INtoCM,
     &      IOSUM(14,I),
     &      INT(IOSUM(15,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(16,I)*FT3pACRtoM3pHA),
     &      BCYMAI(I)*FT3pACRtoM3pHA,
     &      (IOSUM(K,I),K=18,20)
C
        IF(LDSK)
     &    WRITE(JSUM2,9014) IOSUM(1,I),IOSUM(2,I),
     &      INT(IOSUM(3,I)/ACRtoHA),
     &      INT(IOLDBA(I)*FT2pACRtoM2pHA),
     &      INT(ISDI(I)/ACRtoHA),
     &      IBTCCF(I),
     &      INT(IBTAVH(I)*FTtoM),
     &      QSDBT(I)*INtoCM,
     &      INT(IOSUM(4,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(5,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(6,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(7,I)/ACRtoHA),
     &      INT(IOSUM(8,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(9,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(10,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(11,I)*FT2pACRtoM2pHA),
     &      INT(ISDIAT(I)/ACRtoHA),
     &      IOSUM(12,I),
     &      INT(IOSUM(13,I)*FTtoM),
     &      QDBHAT(I)*INtoCM,
     &      IOSUM(14,I),
     &      INT(IOSUM(15,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(16,I)*FT3pACRtoM3pHA),
     &      BCYMAI(I)*FT3pACRtoM3pHA,
     &      (IOSUM(K,I),K=18,20)
C
      CASE DEFAULT
C
        IF(LPRT)
     &    WRITE(JSTND2,20) IOSUM(1,I),IOSUM(2,I),
     &      INT(IOSUM(3,I)/ACRtoHA),
     &      INT(IOLDBA(I)*FT2pACRtoM2pHA),
     &      INT(ISDI(I)/ACRtoHA),
     &      IBTCCF(I),
     &      INT(IBTAVH(I)*FTtoM),
     &      QSDBT(I)*INtoCM,
     &      INT(IOSUM(4,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(5,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(6,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(7,I)/ACRtoHA),
     &      INT(IOSUM(8,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(9,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(10,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(11,I)*FT2pACRtoM2pHA),
     &      INT(ISDIAT(I)/ACRtoHA),
     &      IOSUM(12,I),
     &      INT(IOSUM(13,I)*FTtoM),
     &      QDBHAT(I)*INtoCM,
     &      IOSUM(14,I),
     &      INT(IOSUM(15,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(16,I)*FT3pACRtoM3pHA),
     &      BCYMAI(I)*FT3pACRtoM3pHA,
     &      (IOSUM(K,I),K=18,20)
C
        IF(LDSK)
     &    WRITE(JSUM2,9014) IOSUM(1,I),IOSUM(2,I),
     &      INT(IOSUM(3,I)/ACRtoHA),
     &      INT(IOLDBA(I)*FT2pACRtoM2pHA),
     &      INT(ISDI(I)/ACRtoHA),
     &      IBTCCF(I),
     &      INT(IBTAVH(I)*FTtoM),
     &      QSDBT(I)*INtoCM,
     &      INT(IOSUM(4,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(5,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(6,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(7,I)/ACRtoHA),
     &      INT(IOSUM(8,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(9,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(10,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(11,I)*FT2pACRtoM2pHA),
     &      INT(ISDIAT(I)/ACRtoHA),
     &      IOSUM(12,I),
     &      INT(IOSUM(13,I)*FTtoM),
     &      QDBHAT(I)*INtoCM,
     &      IOSUM(14,I),
     &      INT(IOSUM(15,I)*FT3pACRtoM3pHA),
     &      INT(IOSUM(16,I)*FT3pACRtoM3pHA),
     &      BCYMAI(I)*FT3pACRtoM3pHA,
     &      (IOSUM(K,I),K=18,20)

      END SELECT
C
C      CALL THE DBSSUMRY FOR POPULATING THE DATABASE WITH
C      THE SUMMARY INFORMATION
C
        IF (ICALLF.EQ.0) CALL DBSSUMRY(IOSUM(1,I),IOSUM(2,I),
     &     NPLT,
     &     INT(IOSUM(3,I)/ACRtoHA),
     &     INT(IOLDBA(I)*FT2pACRtoM2pHA),
     &     INT(ISDI(I)/ACRtoHA),
     &     IBTCCF(I),
     &     INT(IBTAVH(I)*FTtoM),
     &     QSDBT(I)*INtoCM,
     &     INT(IOSUM(4,I)*FT3pACRtoM3pHA),
     &     INT(IOSUM(5,I)*FT3pACRtoM3pHA),
     &     INT(IOSUM(6,I)*FT3pACRtoM3pHA),
     &     INT(IOSUM(7,I)/ACRtoHA),
     &     INT(IOSUM(8,I)*FT3pACRtoM3pHA),
     &     INT(IOSUM(9,I)*FT3pACRtoM3pHA),
     &     INT(IOSUM(10,I)*FT3pACRtoM3pHA),
     &     INT(IOSUM(11,I)*FT2pACRtoM2pHA),
     &     INT(ISDIAT(I)/ACRtoHA),
     &     IOSUM(12,I),
     &     INT(IOSUM(13,I)*FTtoM),
     &     QDBHAT(I)*INtoCM,
     &     IOSUM(14,I),
     &     INT(IOSUM(15,I)*FT3pACRtoM3pHA),
     &     INT(IOSUM(16,I)*FT3pACRtoM3pHA),
     &     BCYMAI(I)*FT3pACRtoM3pHA,
     &     IOSUM(18,I),IOSUM(19,I),IOSUM(20,I))
C
   20 FORMAT(2I4,I6,I4,I5,2I4,F5.1,7I6,I4,I5,2I4,F5.1,2X,I6,
     &       I5,I6,2X,F6.1,1X,I3,1X,2I1)
C 
 9014 FORMAT(2I4,I6,I4,I5,2I4,F5.1,7I6,I4,I5,2I4,F5.1,2X,I6,
     &       I5,I6,2X,F6.1,1X,I3,1X,2I1)
   50 CONTINUE
C
      IF (.NOT.LDSK) RETURN

      WRITE (JOPRT,60) LENG,JSUM2
   60 FORMAT(/'NOTE:',I3,' LINES OF SUMMARY DATA HAVE BEEN WRITTEN',
     >       ' TO THE FILE REFERENCED BY LOGICAL UNIT',I3)
C
      RETURN
      END
