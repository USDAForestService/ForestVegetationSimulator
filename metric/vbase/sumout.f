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
C              6: MERCH BD M                     START OF PERIOD  
C     *        6: MERCH BD M  (SAWLOG)           START OF PERIOD 
C              7: REMOVED TREES/HA  
C              8: REMOVED TOTAL CU M 
C     *        8: REMOVED MERCH CU M  (PULP AND SAWLOG)
C              9: REMOVED MERCH CU M 
C     *        9: REMOVED MERCH CU M  (SAWLOG)
C             10: REMOVED MERCH BD M 
C     *       10: REMOVED MERCH BD M  (SAWLOG)
C             11: BASAL AREA/HA                  AFTER TREATMENT (M**2/HA)
C             12: CCF                            AFTER TREATMENT
C             13: AVERAGE DOMINANT HEIGHT        AFTER TREATMENT
C             14: PERIOD LENGTH (YEARS)
C             15: ACCRETION (ANNUAL IN CU M /ACRE)
C             16: MORTALITY (ANNUAL IN CU M /ACRE)
C             17: SAMPLE WEIGHT
C             18: FOREST COVER TYPE CODE
C             19: SIZE CLASS
C             20: STOCKING CLASS
C
C     ICALLF= CALL FLAG, 0=NORMAL, 1=PPMAIN CALL FOR INITIAL VALUES.
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
      REAL SAMWT,X3,X4,X5,X6,X8,X9,X10,X11,X14,X15,X015,X16,X18
C----------
C     **************************************************************
C
C     STEP1: SET SWITCHES.
C
      LPRT= JSTND2 .GT. 0
      LDSK= JSUM2 .GT. 0
      IF (.NOT. (LPRT.OR.LDSK)) RETURN
C
      CALL PPISN (CISN)
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
  12  FORMAT(//32X,'SUMMARY STATISTICS (PER HA   OR STAND BASED ON TOTAL
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
      CASE ('BC','ON')
      WRITE (JSTND2,13)
  13  FORMAT(//32X,'SUMMARY STATISTICS (PER HA   OR STAND BASED ON TOTAL
     & STAND AREA)',/,
     &  134(1H-),/15X,'START OF SIMULATION PERIOD',21X,'REMOVALS',13X,
     &  'AFTER TREATMENT',4X,'GROWTH THIS PERIOD',/9X,45(1H-),1X,
     &  23(1H-),1X,21(1H-),2X,18(1H-),3X,'MAI  ------',/9X,
     &  'NO OF',14X,'TOP',
     &  6X,' GTV   GMV   NMV  NO OF  GTV   GMV   NMV ',14X,'TOP  RES  ',
     &  'PERIOD ACCRE MORT    GTV  FOR SS',/,
     &  'YEAR AGE TREES  BA  SDI CCF ',
     &  'HT  QMD  CU M  CU M    M   TREES CU M  CU M  BD FT  BA  SDI ',
     &  'CCF HT   QMD  YEARS   PER  YEAR   CU M  TYP ZT',
     &  /'---- --- ----- ',
     &  '--- ---- --- --- ---- ',7('----- '),'--- ---- --- --- ----  ',
     &  '------ ---- -----   ----- ------')
C----------
C  WRITE HEADER FOR ALL OTHER VARIANTS
C----------
      CASE DEFAULT
      WRITE (JSTND2,14)
   14 FORMAT(//32X,'SUMMARY STATISTICS (PER HA   OR STAND BASED ON TOTAL
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
C       PREVENT UGLY ERROR MSGS WHEN ENORMOUS VALUES APPEAR IN THE MAIN
C       OUTPUT FILE AND SUMMARY OUTPUT FILE: SUBSTITUE -1 INSTEAD
C
        X3 = IOSUM(3,I)/ACRtoHA
        IF (X3 .GE. 9999.0)  X3 = -1.0      ! f6.0
C
        X4 = IOLDBA(I)*FT2pACRtoM2pHA
        IF (X4 .GE. 99.0)    X4 = -1.0      ! f5.1
C
        X5 = ISDI(I)/ACRtoHA
        IF (X5 .GE. 9999.0)  X5 = -1.0      ! f6.0
C
        X6 = IBTCCF(I)
        IF (X6 .GE. 9999.0)  X6 = -1.0      ! f5.0
C
        X8 = QSDBT(I)*INtoCM
        IF (X8 .GE. 9999.0)  X6 = -1.0      ! f6.1
C
        X9 = IOSUM(4,I)*FT3pACRtoM3pHA
        IF (X9 .GE. 9999.0)  X9 = -1.0      ! f6.1
C
        X10 = IOSUM(5,I)*FT3pACRtoM3pHA
        IF (X10 .GE. 9999.0) X10 = -1.0     ! f6.1
C
C       X11 = IOSUM(6,I)*FT3pACRtoM3pHA
C       IF (X10 .GE. 9999.0) X10 = -1.0     ! f6.1
C  LEAVE MERCH BF IN IMPERIAL UNITS
         X11 = IOSUM(6,I)/ACRtoHA
         IF (X11 .GE. 9999.0) X11 = -1.0    ! f6.1
C
        X14 = IOSUM(11,I)*FT2pACRtoM2pHA
        IF (X14 .GE. 999.0)  X14 = -1.0     ! f5.1
C
        X15 = ISDIAT(I)/ACRtoHA
        IF (X15 .GE. 9999.0) X15 = -1.0     ! f6.0
C
        X015=IOSUM(10,I)/ACRtoHA            ! F6.1
        IF (X015 .GE. 9999.0) X015= -1.0    ! f6.1
C
        X16 = IOSUM(12,I)*1.0
        IF (X16 .GE. 999.0)  X16 = -1.0     ! f5.1
C
        X18 = QDBHAT(I)*INtoCM
        IF (X18 .GE. 9999.0) X18 = -1.0     ! f6.1
C
C
      SELECT CASE (VARACD)
C
      CASE ('CS','LS','NE','SN')
C
        IF(LPRT)
     &    WRITE(JSTND2,20) IOSUM(1,I),IOSUM(2,I),
     &              NINT(X3),                          ! IOSUM(3,I)
     &                   X4,                           ! IOLDBA(I)
     &                   X5,                           ! ISDI(I)
     &                   X6,                           ! IBTCCF(I)
     &                   IBTAVH(I)*FTtoM,              ! IBTAVH(I)
     &                   X8,                           ! QSDBT(I)
     &                   X9,                           ! IOSUM(4,I)
     &                   X10,                          ! IOSUM(5,I)
     &                   X11,                          ! IOSUM(6,I)
     &                   NINT(IOSUM(7,I)/ACRtoHA),     ! IOSUM(7,I)
     &                   IOSUM(8,I)*FT3pACRtoM3pHA,    ! IOSUM(8,I)
     &                   IOSUM(9,I)*FT3pACRtoM3pHA,    ! IOSUM(9,I)
C     &                   IOSUM(10,I)*FT3pACRtoM3pHA,
     &                   X015,                         ! IOSUM(10,I)
     &                   X14,                          ! IOSUM(11,I)
     &                   X15,                          ! ISDIAT(I)
     &                   X16,                          ! ISOUM(12,I)
     &                   IOSUM(13,I)*FTtoM,            ! IOSUM(13,I)
     &                   X18,                          ! QDBHAT(I)
     &                   IOSUM(14,I),                  ! IOSUM(14,I)
     &                   IOSUM(15,I)*FT3pACRtoM3pHA,   ! IOSUM(15,I)
     &                   IOSUM(16,I)*FT3pACRtoM3pHA,   ! IOSUM(16,I)
     &                   BCYMAI(I)*FT3pACRtoM3pHA,     ! BCYMAI(I)
     &                   (IOSUM(K,I),K=18,20)          ! IOSUM(18-20,I)
C
        IF(LDSK)
     &    WRITE(JSUM2,9014) IOSUM(1,I),IOSUM(2,I),
     &              NINT(X3),                          ! IOSUM(3,I)
     &                   X4,                           ! IOLDBA(I)
     &                   X5,                           ! ISDI(I)
     &                   X6,                           ! IBTCCF(I)
     &                   IBTAVH(I)*FTtoM,              ! IBTAVH(I)
     &                   X8,                           ! QSDBT(I)
     &                   X9,                           ! IOSUM(4,I)
     &                   X10,                          ! IOSUM(5,I)
     &                   X11,                          ! IOSUM(6,I)
     &                   NINT(IOSUM(7,I)/ACRtoHA),     ! IOSUM(7,I)
     &                   IOSUM(8,I)*FT3pACRtoM3pHA,    ! IOSUM(8,I)
     &                   IOSUM(9,I)*FT3pACRtoM3pHA,    ! IOSUM(9,I)
C     &                   IOSUM(10,I)*FT3pACRtoM3pHA,
     &                   X015,                         ! IOSUM(10,I)
     &                   X14,                          ! IOSUM(11,I)
     &                   X15,                          ! ISDIAT(I)
     &                   X16,                          ! ISOUM(12,I)
     &                   IOSUM(13,I)*FTtoM,            ! IOSUM(13,I)
     &                   X18,                          ! QDBHAT(I)
     &                   IOSUM(14,I),                  ! IOSUM(14,I)
     &                   IOSUM(15,I)*FT3pACRtoM3pHA,   ! IOSUM(15,I)
     &                   IOSUM(16,I)*FT3pACRtoM3pHA,   ! IOSUM(16,I)
     &                   BCYMAI(I)*FT3pACRtoM3pHA,     ! BCYMAI(I)
     &                   (IOSUM(K,I),K=18,20)          ! IOSUM(18-20,I)
C
      CASE DEFAULT
C
        IF(LPRT)
     &    WRITE(JSTND2,20) IOSUM(1,I),IOSUM(2,I),
     &              NINT(X3),                          ! IOSUM(3,I)
     &                   X4,                           ! IOLDBA(I)
     &                   X5,                           ! ISDI(I)
     &                   X6,                           ! IBTCCF(I)
     &                   IBTAVH(I)*FTtoM,              ! IBTAVH(I)
     &                   X8,                           ! QSDBT(I)
     &                   X9,                           ! IOSUM(4,I)
     &                   X10,                          ! IOSUM(5,I)
     &                   X11,                          ! IOSUM(6,I)
     &                   NINT(IOSUM(7,I)/ACRtoHA),     ! IOSUM(7,I)
     &                   IOSUM(8,I)*FT3pACRtoM3pHA,    ! IOSUM(8,I)
     &                   IOSUM(9,I)*FT3pACRtoM3pHA,    ! IOSUM(9,I)
C     &                   IOSUM(10,I)*FT3pACRtoM3pHA,
     &                   X015,                         ! IOSUM(10,I)
     &                   X14,                          ! IOSUM(11,I)
     &                   X15,                          ! ISDIAT(I)
     &                   X16,                          ! ISOUM(12,I)
     &                   IOSUM(13,I)*FTtoM,            ! IOSUM(13,I)
     &                   X18,                          ! QDBHAT(I)
     &                   IOSUM(14,I),                  ! IOSUM(14,I)
     &                   IOSUM(15,I)*FT3pACRtoM3pHA,   ! IOSUM(15,I)
     &                   IOSUM(16,I)*FT3pACRtoM3pHA,   ! IOSUM(16,I)
     &                   BCYMAI(I)*FT3pACRtoM3pHA,     ! BCYMAI(I)
     &                   (IOSUM(K,I),K=18,20)          ! IOSUM(18-20,I)
C
        IF(LDSK)
     &    WRITE(JSUM2,9014) IOSUM(1,I),IOSUM(2,I),
     &              NINT(X3),                          ! IOSUM(3,I)
     &                   X4,                           ! IOLDBA(I)
     &                   X5,                           ! ISDI(I)
     &                   X6,                           ! IBTCCF(I)
     &                   IBTAVH(I)*FTtoM,              ! IBTAVH(I)
     &                   X8,                           ! QSDBT(I)
     &                   X9,                           ! IOSUM(4,I)
     &                   X10,                          ! IOSUM(5,I)
     &                   X11,                          ! IOSUM(6,I)
     &                   NINT(IOSUM(7,I)/ACRtoHA),     ! IOSUM(7,I)
     &                   IOSUM(8,I)*FT3pACRtoM3pHA,    ! IOSUM(8,I)
     &                   IOSUM(9,I)*FT3pACRtoM3pHA,    ! IOSUM(9,I)
C     &                   IOSUM(10,I)*FT3pACRtoM3pHA,
     &                   X015,                         ! IOSUM(10,I)
     &                   X14,                          ! IOSUM(11,I)
     &                   X15,                          ! ISDIAT(I)
     &                   X16,                          ! ISOUM(12,I)
     &                   IOSUM(13,I)*FTtoM,            ! IOSUM(13,I)
     &                   X18,                          ! QDBHAT(I)
     &                   IOSUM(14,I),                  ! IOSUM(14,I)
     &                   IOSUM(15,I)*FT3pACRtoM3pHA,   ! IOSUM(15,I)
     &                   IOSUM(16,I)*FT3pACRtoM3pHA,   ! IOSUM(16,I)
     &                   BCYMAI(I)*FT3pACRtoM3pHA,     ! BCYMAI(I)
     &                   (IOSUM(K,I),K=18,20)          ! IOSUM(18-20,I)

      END SELECT

C
C      CALL THE DBSSUMRY FOR POPULATING THE DATABASE WITH
C      THE SUMMARY INFORMATION
C
        IF (ICALLF.EQ.0) CALL DBSSUMRY(
     &                   IOSUM(1,I),IOSUM(2,I),NPLT,
     &                   NINT(IOSUM(3,I)/ACRtoHA),
     &                   NINT(IOLDBA(I)*FT2pACRtoM2pHA),
     &                   NINT(ISDI(I)/ACRtoHA),
     &                   IBTCCF(I),
     &                   IBTAVH(I)*FTtoM,
     &                   QSDBT(I)*INtoCM,
     &                   NINT(IOSUM(4,I)*FT3pACRtoM3pHA),
     &                   NINT(IOSUM(5,I)*FT3pACRtoM3pHA),
     &                   NINT(IOSUM(6,I)*FT3pACRtoM3pHA),
     &                   NINT(IOSUM(7,I)/ACRtoHA),
     &                   NINT(IOSUM(8,I)*FT3pACRtoM3pHA),
     &                   NINT(IOSUM(9,I)*FT3pACRtoM3pHA),
     &                   NINT(IOSUM(10,I)*FT3pACRtoM3pHA),
     &                   NINT(IOSUM(11,I)*FT2pACRtoM2pHA),
     &                   NINT(ISDIAT(I)/ACRtoHA),
     &                   IOSUM(12,I),
     &                   IOSUM(13,I)*FTtoM,
     &                   QDBHAT(I)*INtoCM,
     &                   IOSUM(14,I),
     &                   NINT(IOSUM(15,I)*FT3pACRtoM3pHA),
     &                   NINT(IOSUM(16,I)*FT3pACRtoM3pHA),
     &                   BCYMAI(I)*FT3pACRtoM3pHA,
     &                   IOSUM(18,I),IOSUM(19,I),IOSUM(20,I))
C
   20 FORMAT(2I4,I6 ,F5.1,F6.0,F5.0,F5.1,F6.1,3F6.1,
     &       I6,F6.1,F6.1,  F6.0, F4.1, F6.0, F5.0,F5.1
     &       F6.1,I6, F5.1,F6.1,F8.1,1X,I3,1X,2I1)
 9014 FORMAT(2I4,F6.0,F5.1,2F6.0,F5.0,F5.1,2F6.1,F6.1,I5,F6.1,F5.1,F6.1,
     &       F5.1,F6.0,F5.0,F5.1,F6.1,I8,F7.1,F6.1,F7.1,1X,I3,1X,2I1)
C
   50 CONTINUE
C
      IF (.NOT.LDSK) RETURN

      WRITE (JOPRT,60) LENG,JSUM2
   60 FORMAT(/'NOTE:',I3,' LINES OF SUMMARY DATA HAVE BEEN WRITTEN',
     >       ' TO THE FILE REFERENCED BY LOGICAL UNIT',I3)
C
      RETURN
      END
