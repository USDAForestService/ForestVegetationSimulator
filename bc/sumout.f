      SUBROUTINE SUMOUT(IOSUM,I20,ICFLAG,JOPRT,JOSTND,JOSUM,
     >                  LEN,MGMID,NPLT,SAMWT,ITITLE,LCVOLS,IPTINV)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     WRITES SUMMARY OUTPUT.
C
C     IOSUM = THE SUMMARY OUTPUT ARRAY FROM THE PROGNOSIS MODEL.
C              1: YEAR
C              2: AGE
C              3: TREES/ACRE                     START OF PERIOD
C              4: TOTAL CU FT                    START OF PERIOD
C     *        4: MERCH CU FT (PULP AND SAWLOG)  START OF PERIOD
C              5: MERCH CU FT                    START OF PERIOD 
C     *        5: MERCH CU FT (SAWLOG)           START OF PERIOD    
C              6: MERCH BD FT                    START OF PERIOD  
C     *        6: MERCH BD FT (SAWLOG)           START OF PERIOD 
C              7: REMOVED TREES/ACRE
C              8: REMOVED TOTAL CU FT
C     *        8: REMOVED MERCH CU FT (PULP AND SAWLOG)
C              9: REMOVED MERCH CU FT
C     *        9: REMOVED MERCH CU FT (SAWLOG)
C             10: REMOVED MERCH BD FT
C     *       10: REMOVED MERCH BD FT (SAWLOG)
C             11: BASAL AREA/ACRE                AFTER TREATMENT
C             12: CCF                            AFTER TREATMENT
C             13: AVERAGE DOMINANT HEIGHT        AFTER TREATMENT
C             14: PERIOD LENGTH (YEARS)
C             15: ACCRETION (ANNUAL IN CU FT/ACRE)
C             16: MORTALITY  (ANNUAL IN CU FT/ACRE)
C             17: SAMPLE WEIGHT
C             18: FOREST COVER TYPE CODE
C             19: SIZE CLASS
C             20: STOCKING CLASS
C
C     ICFLAG= CALL FLAG, 0=NORMAL, 1=PPMAIN CALL FOR INITIAL VALUES.
C     JOSTND= DATA SET REFERENCE NUMBER FOR 'PRINTED' COPY (WITH
C             HEADINGS AND CARRAGE CONTROL BYTE).  IF JOSTND=0, NO
C             DATA WILL BE WRITTEN.
C     JOPRT = PRINTER OUTPUT FOR MESSAGES.
C     JOSUM = DATA SET REFERENCE NUMBER FOR 'NON-PRINTED' COPY (WITH
C             OUT HEADINGS, NO CARRAGE CONTROL BYTE). IF JOSUM=0,
C             NO DATA WILL BE WRITTEN.
C     LEN   = NUMBER OF ROWS (ENTRIES) IN IOSUM.
C     MGMID = MANAGEMENT IDENTIFICATION FIELD. ASSUMED ALPHANUMERIC.
C     NPLT  = PLOT IDENTIFICATION FIELD. ASSUMED ALPHANUMERIC.
C NOTE: * Indicates R8 and R9 specific (CS, LS, NE, OZ, SE, SN)
C
COMMONS
C
      INCLUDE 'PRGPRM.F77' 
      INCLUDE 'SUMTAB.F77'
      INCLUDE 'METRIC.F77'
C
COMMONS
C
      CHARACTER CISN*11,NPLT*26,TIM*8,DAT*10,MGMID*4,VVER*7,REV*10
      CHARACTER ITITLE*72
      INTEGER*4 IOSUM(I20,LEN),IPTINV
	REAL      SAMWT,X3,X4,X5,X6,X9,X10,X14,X15,X16
      INTEGER   JOSUM,JOSTND,JOPRT,ICFLAG,I20,LEN,ISTLNB,I12,I,K
      LOGICAL   LPRT,LDSK,LCVOLS
C
C     **************************************************************
C
C     STEP1: SET SWITCHES.
C
      LPRT= JOSTND .GT. 0
      LDSK= JOSUM .GT. 0
      IF (.NOT. (LPRT.OR.LDSK)) RETURN
C
      CALL PPISN (CISN)
      CALL VARVER (VVER)
      CALL REVISE (VVER,REV)
      CALL GRDTIM (DAT,TIM)
      IF(LDSK) WRITE (JOSUM,2) LEN,NPLT,MGMID,SAMWT,VVER,DAT,TIM,
     &                         REV,CISN,IPTINV
    2 FORMAT ('-999',I5,1X,A26,1X,A4,E15.7,5(1X,A),I3)
C
C     STEP2: WRITE VARIANT SPECIFIC HEADING.
C            SKIP A FEW LINES, DO NOT START A NEW PAGE.
C
      IF (LPRT) THEN
        WRITE (JOSTND,5) NPLT,MGMID,ITITLE(1:ISTLNB(ITITLE))
    5   FORMAT(/' STAND ID: ',A26,4X,'MGMT ID: ',A4,4X,A/)
C
        IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR. 
     &    (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     &    (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN')) THEN
C----------
C  WRITE HEADER FOR CS, LS, NE, OZ, SE, SN
C----------
        WRITE (JOSTND,12)
   12     FORMAT(//31X,'SUMMARY STATISTICS (PER HA OR STAND BASED ON ',
     &           'TOTAL STAND AREA)',
     &    /1X,134(1H-),
     &    /16X,'START OF SIMULATION PERIOD',21X,'REMOVALS',13X,
     &    'AFTER TREATMENT',4X,'GROWTH THIS PERIOD',
     &    /10X,45(1H-),1X,23(1H-),1X,21(1H-),2X,18(1H-),3X,
     &    'MAI  ------',
     &    /10X,'NO OF',14X,'TOP',6X,'MERCH SAWLG NO OF MERCH SAWLG',
     &    14X,'TOP  RES   PERIOD ACCRE MORT   MERCH FOR SS',
     &    /1X,'YEAR AGE TREES  BA  SDI CCF HT  QMD  CU  M CU  M ',
     &    'TREES CU  M CU  M  BA  SDI CCF HT   QMD  YEARS   ',
     &    'PER  YEAR  CU  M  TYP ZT',
     &    /1X,'---- --- ----- ',
     &    '--- ---- --- --- ---- ',7('----- '),
     &    '--- ---- --- --- ----  ',
     &    '------ ---- -----   ----- ------')
        ELSE
C----------
C  WRITE HEADER FOR ALL OTHER VARIANTS
C----------
          WRITE (JOSTND,14)
   14     FORMAT(//31X,'SUMMARY STATISTICS (PER HA OR STAND BASED ON ',
     &           'TOTAL STAND AREA)',
     &    /1X,134(1H-),
     &    /17X,'START OF SIMULATION PERIOD',17X,'REMOVALS',11X,
     &    'AFTER TREATMENT',8X,'GROWTH THIS PERIOD',
     &    /10X,44(1H-),1X,17(1H-),1X,26(1H-),2X,20(1H-),3X,
     &    'MAI  ------',
     &    /10X,'NO OF',17X,'TOP',8X,'TOTAL MERCH NO OF TOTAL MERCH',
     &    17X,'TOP   RES    PERIOD  ACCRE  MORT  MERCH FOR SS',
     &    /1X,'YEAR AGE TREES  BA   SDI   CCF  HT   QMD  CU  M CU  M ',
     &    'TREES CU  M CU  M  BA   SDI  CCF   HT   QMD    YEARS    ',
     &    'PER   YEAR  CU  M TYP ZT',
     &    /1X,'---- --- ----- ---- ----- ---- ---- ----- ',
     &    5('----- '),'---- ----- ---- ---- -----   ',
     &    '------  ----- -----  ----- ------')
        ENDIF
      ENDIF
C----------
C     STEP3: LOOP THRU ALL ROWS IN IOSUM...WRITE OUTPUT.
C
C  THIS STEP TAKES JUST THE FIRST 12 ITEMS IN THE IOSUM ARRAY
C----------
      I12=I20-8
      DO 50 I=1,LEN
      IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR.
     1    (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     2    (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN') .OR.
     3    (VVER(:2) .EQ. 'ON')) THEN
C
        IF(LPRT)
     &  WRITE(JOSTND,20) IOSUM(1,I),IOSUM(2,I),
     &                   IOSUM(3,I)/ACRtoHA,
     &                   IOLDBA(I)*FT2pACRtoM2pHA,
     &                   ISDI(I)/ACRtoHA,
     &                   IBTCCF(I)*1.0,
     &                   IBTAVH(I)*FTtoM,
     &                   QSDBT(I)*INtoCM,
     &                   IOSUM(4,I)*FT3pACRtoM3pHA,
     &                   IOSUM(5,I)*FT3pACRtoM3pHA,
     &                   NINT(IOSUM(7,I)/ACRtoHA),
     &                   IOSUM(8,I)*FT3pACRtoM3pHA,
     &                   IOSUM(9,I)*FT3pACRtoM3pHA,
     &                   IOSUM(11,I)*FT2pACRtoM2pHA,
     &                   ISDIAT(I)/ACRtoHA,
     &                   IOSUM(12,I)*1.0,
     &                   IOSUM(13,I)*FTtoM,
     &                   QDBHAT(I)*INtoCM,
     &                   IOSUM(14,I),
     &                   IOSUM(15,I)*FT3pACRtoM3pHA,
     &                   IOSUM(16,I)*FT3pACRtoM3pHA,
     &                   BCYMAI(I)*FT3pACRtoM3pHA,
     &                   (IOSUM(K,I),K=18,20)
        IF(LDSK)
     &  WRITE(JOSUM,9014) IOSUM(1,I),IOSUM(2,I),
     &                    IOSUM(3,I)/ACRtoHA,
     &                    IOLDBA(I)*FT2pACRtoM2pHA,
     &                    ISDI(I)/ACRtoHA,
     &                    IBTCCF(I)*1.0,
     &                    IBTAVH(I)*FTtoM,
     &                    QSDBT(I)*INtoCM,
     &                    IOSUM(4,I)*FT3pACRtoM3pHA,
     &                    IOSUM(5,I)*FT3pACRtoM3pHA,
     &                    NINT(IOSUM(7,I)/ACRtoHA),
     &                    IOSUM(8,I)*FT3pACRtoM3pHA,
     &                    IOSUM(9,I)*FT3pACRtoM3pHA,
     &                    IOSUM(11,I)*FT2pACRtoM2pHA,
     &                    ISDIAT(I)/ACRtoHA,
     &                    IOSUM(12,I)*1.0,
     &                    IOSUM(13,I)*FTtoM,
     &                    QDBHAT(I)*INtoCM,
     &                    IOSUM(14,I),
     &                    IOSUM(15,I)*FT3pACRtoM3pHA,
     &                    IOSUM(16,I)*FT3pACRtoM3pHA,
     &                    BCYMAI(I)*FT3pACRtoM3pHA,
     &                    (IOSUM(K,I),K=18,20)
      ELSE

C
C       PREVENT UGLY ERROR MSGS WHEN ENORMOUS VALUES APPEAR IN THE MAIN
C       OUTPUT FILE AND SUMMARY OUTPUT FILE: SUBSTITUE -1 INSTEAD
C     
	  X3 = IOSUM(3,I)/ACRtoHA 
	  IF (X3 .GT. 99999)   X3 = -1.0   ! i5
	  X4 = IOLDBA(I)*FT2pACRtoM2pHA
	  IF (X4 .GT. 9999)    X4 = -1.0   ! i4
	  X5 = ISDI(I)/ACRtoHA
	  IF (X5 .GT. 99999)   X5 = -1.0   ! i5
	  X6 = IBTCCF(I)*1.0
	  IF (X6 .GT. 9999)    X6 = -1.0   ! i4
        X9 = IOSUM(4,I)*FT3pACRtoM3pHA
	  IF (X9 .GT. 999)     X9 = -1.0   ! f5.1
        X10 = IOSUM(5,I)*FT3pACRtoM3pHA
	  IF (X10 .GT. 9999)  X10 = -1.0   ! f6.1
        X14 = IOSUM(11,I)*FT2pACRtoM2pHA
	  IF (X14 .GT. 9999)  X14 = -1.0   ! i4
        X15 = ISDIAT(I)/ACRtoHA
	  IF (X15 .GT. 99999) X15 = -1.0   ! i5
        X16 = IOSUM(12,I)*1.0
	  IF (X16 .GT. 9999)  X16 = -1.0   ! i4

        IF(LPRT)
     &  WRITE(JOSTND,20) IOSUM(1,I),IOSUM(2,I),
     &                   NINT(X3),
     &                   NINT(X4),
     &                   NINT(X5),
     &                   NINT(X6),
     &                   IBTAVH(I)*FTtoM,
     &                   QSDBT(I)*INtoCM,
     &                   X9,
     &                   X10,
     &                   NINT(IOSUM(7,I)/ACRtoHA),
     &                   IOSUM(8,I)*FT3pACRtoM3pHA,
     &                   IOSUM(9,I)*FT3pACRtoM3pHA,
     &                   NINT(X14),
     &                   NINT(X15),
     &                   NINT(X16),
     &                   IOSUM(13,I)*FTtoM,
     &                   QDBHAT(I)*INtoCM,
     &                   IOSUM(14,I),
     &                   IOSUM(15,I)*FT3pACRtoM3pHA,
     &                   IOSUM(16,I)*FT3pACRtoM3pHA,
     &                   BCYMAI(I)*FT3pACRtoM3pHA,
     &                   (IOSUM(K,I),K=18,20)
         IF(LDSK)
     &   WRITE(JOSUM,9014) IOSUM(1,I),IOSUM(2,I),
     &                   NINT(X3),
     &                   NINT(X4),
     &                   NINT(X5),
     &                   NINT(X6),
     &                   IBTAVH(I)*FTtoM,
     &                   QSDBT(I)*INtoCM,
     &                   X9,
     &                   X10,
     &                   NINT(IOSUM(7,I)/ACRtoHA),
     &                   IOSUM(8,I)*FT3pACRtoM3pHA,
     &                   IOSUM(9,I)*FT3pACRtoM3pHA, 
     &                   NINT(X14),
     &                   NINT(X15),
     &                   NINT(X16),
     &                   IOSUM(13,I)*FTtoM,
     &                   QDBHAT(I)*INtoCM,
     &                   IOSUM(14,I),
     &                   IOSUM(15,I)*FT3pACRtoM3pHA,
     &                   IOSUM(16,I)*FT3pACRtoM3pHA,
     &                   BCYMAI(I)*FT3pACRtoM3pHA,
     &                   (IOSUM(K,I),K=18,20)
      ENDIF
C
C      CALL THE DBSSUMRY FOR POPULATING THE DATABASE WITH
C      THE SUMMARY INFORMATION
C
      IF (ICFLAG.EQ.0) CALL DBSSUMRY(
     &                   IOSUM(1,I),IOSUM(2,I),NPLT,
     &                   NINT(IOSUM(3,I)/ACRtoHA),
     &                   NINT(IOLDBA(I)*FT2pACRtoM2pHA),
     &                   NINT(ISDI(I)/ACRtoHA),
     &                   NINT(IBTCCF(I)*1.0),
     &                   IBTAVH(I)*FTtoM,
     &                   QSDBT(I)*INtoCM,
     &                   NINT(IOSUM(4,I)*FT3pACRtoM3pHA),
     &                   NINT(IOSUM(5,I)*FT3pACRtoM3pHA),
     &                   IOSUM(6,I),
     &                   NINT(IOSUM(7,I)/ACRtoHA),
     &                   NINT(IOSUM(8,I)*FT3pACRtoM3pHA),
     &                   NINT(IOSUM(9,I)*FT3pACRtoM3pHA),
     &                   IOSUM(10,I),
     &                   NINT(IOSUM(11,I)*FT2pACRtoM2pHA),
     &                   NINT(ISDIAT(I)/ACRtoHA),
     &                   NINT(IOSUM(12,I)*1.0),
     &                   IOSUM(13,I)*FTtoM,
     &                   QDBHAT(I)*INtoCM,
     &                   IOSUM(14,I),
     &                   NINT(IOSUM(15,I)*FT3pACRtoM3pHA),
     &                   NINT(IOSUM(16,I)*FT3pACRtoM3pHA),
     &                   BCYMAI(I)*FT3pACRtoM3pHA,
     &                   IOSUM(18,I),IOSUM(19,I),IOSUM(20,I))
C
   20 FORMAT(1X,2I4,1X,I5,1X,I4,1X,I5,1X,I4,F5.1,3F6.1,1X,
     &       I5,2F6.1,1X,I4,1X,I5,1X,I4,F5.1,F6.1,I7,
     &       F9.1,F6.1,F7.1,1X,I3,1X,2I1)
 9014 FORMAT(   2I4,1X,I5,1X,I4,1X,I5,1X,I4,F5.1,3F6.1,1X,
     &       I5,2F6.1,I5,I6,I5,
     &       F5.1,F6.1,I7,F9.1,F6.1,F7.1,1X,I3,1X,2I1)
   50 CONTINUE
C
C     SPECIAL REPORTING FOR BC VARIANTS
C
      IF (VVER(:2) .EQ. 'BC') THEN
        WRITE(JOPRT,56)
        IF (LCVOLS) THEN
          WRITE(JOPRT,58)
        ELSE
          WRITE(JOPRT,57)
        ENDIF
      ENDIF

   56 FORMAT(/' NOTE: TOTAL VOLUME DEFINED AS ALL LIVE STEMS,',
     >        ' FROM GROUND TO TIP.')
   57 FORMAT( T8,'DEFAULT MERCHANTABLE VOLUME ASSUMPTIONS WERE USED:',
     >       /T8,'  MINIMUM DBH:  PL 12.5 CM, ALL OTHERS 17.5 CM ' 
     >       /T8,'  STUMP HEIGHT: 30 CM ',
     >       /T8,'  TOP DIAMETER: 10 CM ')
   58 FORMAT( T8,'DEFAULT VOLUME ASSUMPTIONS: ',
     >       /T8,'  MINIMUM DBH:  PL 12.5 CM, ALL OTHERS 17.5 CM ' 
     >       /T8,'  STUMP HEIGHT: 30 CM',
     >       /T8,'  TOP DIAMETER: 10 CM',
     >       /T8,'** VOLUME KEYWORDS WERE USED; DEFAULTS MAY HAVE',
     >           ' BEEN CHANGED **',/T8,'** SEE VOLUME KEYWORDS',
     >           ' FOR DETAILS **')
C
      IF (.NOT.LDSK) RETURN
      WRITE (JOPRT,60) LEN,JOSUM
   60 FORMAT(/' NOTE:',I3,' LINES OF SUMMARY DATA HAVE BEEN WRITTEN',
     >       ' TO THE FILE REFERENCED BY LOGICAL UNIT',I3)
      RETURN
      END
