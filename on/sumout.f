      SUBROUTINE SUMOUT(IOSUM,I20,ICFLAG,JOPRT,JOSTND,JOSUM,
     >                  LEN,MGMID,NPLT,SAMWT,ITITLE,IPTINV)
      IMPLICIT NONE
C----------
C  **SUMOUT--BASE/M        DATE OF LAST REVISION:  07/23/08
C----------
C
C     WRITES SUMMARY OUTPUT.
C
C     IOSUM = THE SUMMARY OUTPUT ARRAY FROM THE PROGNOSIS MODEL.
C              1: YEAR
C              2: AGE
C              3: TREES/HA                       START OF PERIOD
C              4: TOTAL CU FT
C     *        4: MERCH CU FT (PULP AND SAWLOG)  START OF PERIOD (FVS-On: GTV, M**3/HA)
C              5: MERCH CU FT
C     *        5: MERCH CU FT (SAWLOG)           START OF PERIOD (FVS-On: GMV, M**3/HA)
C              6: MERCH BD FT
C     *        6: MERCH BD FT (SAWLOG)           START OF PERIOD (FVS-On: NMV, M**3/HA)
C              7: REMOVED TREES/ACRE
C              8: REMOVED TOTAL CU FT
C     *        8: REMOVED MERCH CU FT (PULP AND SAWLOG)          (FVS-On: GTV, M**3/HA)
C              9: REMOVED MERCH CU FT
C     *        9: REMOVED MERCH CU FT (SAWLOG)                   (FVS-On: GMV, M**3/HA)
C             10: REMOVED MERCH BD FT
C     *       10: REMOVED MERCH BD FT (SAWLOG)                   (FVS-On: NMV, M**3/HA)
C             11: BASAL AREA (M**2/HA)           AFTER TREATMENT
C             12: CCF                            AFTER TREATMENT
C             13: AVERAGE DOMINANT HEIGHT (M)    AFTER TREATMENT
C             14: PERIOD LENGTH (YEARS)
C             15: ACCRETION (CU M/ACRE/YR)
C             16: MORTALITY (CU M/ACRE/YR)
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
      INTEGER*4 IOSUM(I20,LEN),IZERO,IPTINV
      INTEGER   JOSUM,JOSTND,JOPRT,ICFLAG,I20,LEN,ISTLNB,I12,I,K
      REAL      SAMWT,X3,X4,X5,X6,X8,X9,X10,X11,X14,X15,X015,X16,X18
      LOGICAL   LPRT,LDSK

      DATA IZERO /0/
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

C       WRITE HEADER FOR CS, LS, NE, OZ, SE, SN

        IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR.
     &    (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     &    (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN') .OR.
     &    (VVER(:2) .EQ. 'ON')) THEN
          WRITE (JOSTND,12)
   12     FORMAT(//31X,'SUMMARY STATISTICS (PER HA OR STAND BASED ON ',
     &           'TOTAL STAND AREA)',
     &    /1X,127(1H-),
     &    /22X,'START OF SIMULATION PERIOD',20X,'REMOVALS',14X,
     &    'AFTER TREATMENT',9X,'GROWTH THIS PERIOD',
     &    /10X,50(1H-),1X,23(1H-),1X,26(1H-),2X,20(1H-),2X,
     &    'MAI  ------',
     &    /10X,'NO OF',17X,'TOP',8X,' GTV   GMV   NMV  NO OF  GTV   ',
     &    'GMV   NMV'18X,'TOP   RES   PERIOD  ACCRE  MORT   GTV  FOR ',
     &    'SS'/1X,'YEAR AGE TREES   BA   SDI  CCF  HT   QMD  CU  M ',
     &    'CU  M CU  M TREES CU  M CU  M CU  M   BA   SDI  CCF  HT ',
     &    '  QMD    YEARS    PER  YEAR  CU  M TYP ZT',
     &    /1X,'---- --- ----- ---- ----- ---- ---- ----- ',
     &    7('----- '),'---- ----- ---- ---- -----  ',
     &    '------  ----- -----  ----- ------')
        ELSE

C       WRITE HEADER FOR ALL OTHER VARIANTS

          WRITE (JOSTND,14)
C   14     FORMAT(//31X,'SUMMARY STATISTICS (PER HA OR STAND BASED ON ',
C     &           'TOTAL STAND AREA)',
C     &    /1X,138(1H-),
C     &    /17X,'START OF SIMULATION PERIOD',17X,'REMOVALS',11X,
C     &    'AFTER TREATMENT',8X,'GROWTH THIS PERIOD',
C     &    /10X,44(1H-),1X,17(1H-),1X,26(1H-),2X,20(1H-),2X,
C     &    'MAI  ------',
C     &    /10X,'NO OF',17X,'TOP',8X,'TOTAL MERCH NO OF TOTAL MERCH',
C     &    ' MERCH',14X,'TOP   RES   PERIOD  ACCRE  MORT  MERCH FOR SS',
C     &    /1X,'YEAR AGE TREES  BA   SDI   CCF  HT   QMD  CU  M CU  M ',
C     &    'TREES CU  M CU  M BD FT   BA   SDI  CCF   HT   QMD   YEARS',
C     &    '    PER   YEAR  CU  M TYP ZT',
C     &    /1X,'---- --- ----- ---- ----- ---- ---- ----- ',
C     &    5('----- '),'---- ----- ---- ---- -----  ',
C     &    '------  ----- -----  ----- ------')
   14 FORMAT(//31X,'SUMMARY STATISTICS (PER HA OR STAND BASED ON TOTAL',
     & ' STAND AREA)',/,1X,
     &  134(1H-),/22X,'START OF SIMULATION PERIOD',21X,'REMOVALS',11X,
     &  'AFTER TREATMENT',6X,'GROWTH THIS PERIOD',/10X,50(1H-),1X,
     &  23(1H-),1X,23(1H-),1X,18(1H-),3X,'MAI  ------',/10X,
     &  'NO OF',15X,'   TOP',
     &  6X,' TOTAL MERCH MERCH NO OF TOTAL MERCH MERCH',15X,' TOP',
     &  '  RES PERIOD ACCRE MORT    MERCH FOR SS',/1X,
     &  'YEAR AGE TREES  BA   SDI  CCF ',
     &  '  HT   QMD  CU  M CU  M BD FT TREES CU  M CU  M BD FT BA',
     &  '   SDI  CCF  HT   QMD  YEARS  PER  YEAR    CU  M TYP ZT',
     &  /1X,'---- --- ----- ',
     &  '---- ----- ---- ---- ----- ',7('----- '),'--- ----- ---- ',
     &  '---- ----- ----- ---- -----   ----- ------')




        ENDIF
      ENDIF
C
C
C     STEP3: LOOP THRU ALL ROWS IN IOSUM...WRITE OUTPUT.
C
C  THIS STEP TAKES JUST THE FIRST 12 ITEMS IN THE IOSUM ARRAY
C----------
      I12=I20-8
      DO 50 I=1,LEN
C
C       PREVENT UGLY ERROR MSGS WHEN ENORMOUS VALUES APPEAR IN THE MAIN
C       OUTPUT FILE AND SUMMARY OUTPUT FILE: SUBSTITUE -1 INSTEAD
C
	  X3 = IOSUM(3,I)/ACRtoHA
	  IF (X3 .GE. 9999.0)  X3 = -1.0   ! f6.0

	  X4 = IOLDBA(I)*FT2pACRtoM2pHA
	  IF (X4 .GE. 99.0)    X4 = -1.0   ! f5.1

	  X5 = ISDI(I)/ACRtoHA
	  IF (X5 .GE. 9999.0)  X5 = -1.0   ! f6.0

	  X6 = IBTCCF(I)
	  IF (X6 .GE. 9999.0)  X6 = -1.0   ! f5.0

	  X8 = QSDBT(I)*INtoCM
	  IF (X8 .GE. 9999.0)  X6 = -1.0   ! f6.1

        X9 = IOSUM(4,I)*FT3pACRtoM3pHA
	  IF (X9 .GE. 9999.0)  X9 = -1.0   ! f6.1

        X10 = IOSUM(5,I)*FT3pACRtoM3pHA
	  IF (X10 .GE. 9999.0) X10 = -1.0  ! f6.1

C        X11 = IOSUM(6,I)*FT3pACRtoM3pHA
C	  IF (X10 .GE. 9999.0) X10 = -1.0  ! f6.1
C  LEAVE MERCH BF IN IMPERIAL UNITS
         X11 = IOSUM(6,I)/ACRtoHA
	  IF (X11 .GE. 9999.0) X11 = -1.0  ! f6.1

        X14 = IOSUM(11,I)*FT2pACRtoM2pHA
        IF (X14 .GE. 999.0)  X14 = -1.0  ! f5.1

        X15 = ISDIAT(I)/ACRtoHA
	  IF (X15 .GE. 9999.0) X15 = -1.0  ! f6.0
	 
	 X015=IOSUM(10,I)/ACRtoHA          ! F6.1
        IF (X015 .GE. 9999.0) X015= -1.0  ! f6.1

        X16 = IOSUM(12,I)*1.0
	  IF (X16 .GE. 999.0)  X16 = -1.0  ! f5.1

        X18 = QDBHAT(I)*INtoCM
	  IF (X18 .GE. 9999.0) X18 = -1.0  ! f6.1


      IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR.
     1    (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     2    (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN') .OR.
     3    (VVER(:2) .EQ. 'ON')) THEN

        IF(LPRT)
     &  WRITE(JOSTND,20) IOSUM(1,I),IOSUM(2,I),
     &              NINT(X3),
     &                   X4,
     &                   X5,
     &                   X6,
     &                   IBTAVH(I)*FTtoM,
     &                   X8,
     &                   X9,
     &                   X10,
     &                   X11,
     &                   NINT(IOSUM(7,I)/ACRtoHA),
     &                   IOSUM(8,I)*FT3pACRtoM3pHA,
     &                   IOSUM(9,I)*FT3pACRtoM3pHA,
C     &                   IOSUM(10,I)*FT3pACRtoM3pHA,
     &                   X015,                       ! LEAVE BF IN IMPERIAL UNITS PER HA
     &                   X14,
     &                   X15,
     &                   X16,
     &                   IOSUM(13,I)*FTtoM,
     &                   X18,
     &                   IOSUM(14,I),
     &                   IOSUM(15,I)*FT3pACRtoM3pHA,
     &                   IOSUM(16,I)*FT3pACRtoM3pHA,
     &                   BCYMAI(I)*FT3pACRtoM3pHA,
     &                   (IOSUM(K,I),K=18,20)
        IF(LDSK)
     &   WRITE(JOSUM,9014) IOSUM(1,I),IOSUM(2,I),
     &                   X3,
     &                   X4,
     &                   X5,
     &                   X6,
     &                   IBTAVH(I)*FTtoM,
     &                   X8,
     &                   X9,
     &                   X10,
     &                   X11,
     &                   NINT(IOSUM(7,I)/ACRtoHA),
     &                   IOSUM(8,I)*FT3pACRtoM3pHA,
     &                   IOSUM(9,I)*FT3pACRtoM3pHA,
C     &                   IOSUM(10,I)*FT3pACRtoM3pHA,
     &                   X015,            ! LEAVE BF IN IMPERIAL UNITS PER HA
     &                   X14,
     &                   X15,
     &                   X16,
     &                   IOSUM(13,I)*FTtoM,
     &                   X18,
     &                   IOSUM(14,I),
     &                   IOSUM(15,I)*FT3pACRtoM3pHA,
     &                   IOSUM(16,I)*FT3pACRtoM3pHA,
     &                   BCYMAI(I)*FT3pACRtoM3pHA,
     &                   (IOSUM(K,I),K=18,20)
      ELSE
        IF(LPRT)
     &  WRITE(JOSTND,20) IOSUM(1,I),IOSUM(2,I),
     &              NINT(X3),
     &                   X4,
     &                   X5,
     &                   X6,
     &                   IBTAVH(I)*FTtoM,
     &                   X8,
     &                   X9,
     &                   X10,
     &                   X11,
     &                   NINT(IOSUM(7,I)/ACRtoHA),
     &                   IOSUM(8,I)*FT3pACRtoM3pHA,
     &                   IOSUM(9,I)*FT3pACRtoM3pHA,
C     &                   IOSUM(10,I)/ACRtoHA,
     &                   X015,                     ! LEAVE BF IN IMPERIAL UNITS PER HA
     &                   X14,
     &                   X15,
     &                   X16,
     &                   IOSUM(13,I)*FTtoM,
     &                   X18,
     &                   IOSUM(14,I),
     &                   (IOSUM(15,I)*FT3pACRtoM3pHA),
     &                   (IOSUM(16,I)*FT3pACRtoM3pHA),
     &                   BCYMAI(I)*FT3pACRtoM3pHA,
     &                   (IOSUM(K,I),K=18,20)
         IF(LDSK)
     &   WRITE(JOSUM,9014) IOSUM(1,I),IOSUM(2,I),
     &                   X3,
     &                   X4,
     &                   X5,
     &                   X6,
     &                   IBTAVH(I)*FTtoM,
     &                   X8,
     &                   X9,
     &                   X10,
     &                   X11,
     &                   NINT(IOSUM(7,I)/ACRtoHA),
     &                   IOSUM(8,I)*FT3pACRtoM3pHA,
     &                   IOSUM(9,I)*FT3pACRtoM3pHA,
C     &                   IOSUM(10,I)*FT3pACRtoM3pHA,
     &                   X015,                       ! LEAVE BF IN IMPERIAL UNITS PER HA
     &                   X14,
     &                   X15,
     &                   X16,
     &                   IOSUM(13,I)*FTtoM,
     &                   X18,
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
   20 FORMAT(1X,2I4,I6 ,F5.1,F6.0,F5.0,F5.1,F6.1,3F6.1,
     &       I6,F6.1,F6.1,  F6.0, F4.1, F6.0, F5.0,F5.1
     &       F6.1,I6, F5.1,F6.1,F8.1,1X,I3,1X,2I1)
 9014 FORMAT(2I4,F6.0,F5.1,2F6.0,F5.0,F5.1,2F6.1,F6.1,I5,F6.1,F5.1,F6.1,
     &       F5.1,F6.0,F5.0,F5.1,F6.1,I8,F7.1,F6.1,F7.1,1X,I3,1X,2I1)
C
   50 CONTINUE

      IF (.NOT.LDSK) RETURN
      WRITE (JOPRT,60) LEN,JOSUM
   60 FORMAT(/' NOTE:',I3,' LINES OF SUMMARY DATA HAVE BEEN WRITTEN',
     >       ' TO THE FILE REFERENCED BY LOGICAL UNIT',I3)
      RETURN
      END

