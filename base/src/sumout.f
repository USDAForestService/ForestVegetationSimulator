      SUBROUTINE SUMOUT(IOSUM,I20,ICFLAG,JOPRT,JOSTND,JOSUM,
     >                  LENG,MGMID,NPLT,SAMWT,ITITLE,IPTINV)
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
C             16: MORTALITY (ANNUAL IN CU FT/ACRE)
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
C     LENG   = NUMBER OF ROWS (ENTRIES) IN IOSUM.
C     MGMID = MANAGEMENT IDENTIFICATION FIELD. ASSUMED ALPHANUMERIC.
C     NPLT  = PLOT IDENTIFICATION FIELD. ASSUMED ALPHANUMERIC.
C NOTE: * Indicates R8 and R9 specific (CS, LS, NE, OZ, SE, SN)
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'SUMTAB.F77'
C
COMMONS
C
      CHARACTER CISN*11,NPLT*26,TIM*8,DAT*10,MGMID*4,VVER*7,REV*10
      CHARACTER ITITLE*72,RECORD*250
      INTEGER*4 IOSUM(I20,LENG),IPTINV
      REAL SAMWT
      INTEGER JOSUM,JOSTND,JOPRT,ICFLAG,I20,LENG,ISTLNB,I12,I,K,II
      LOGICAL LNOR,LPRT,LDSK,LCONN
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
      
      IF(LDSK) THEN
        INQUIRE(UNIT=JOSUM,opened=LCONN)
        IF (.NOT.LCONN) THEN
          CALL fvsGetKeywordFileName(RECORD,len(RECORD),II)
          IF (RECORD.NE.' ') THEN
            II=index(RECORD,".k")
            IF (II == 0) II=index(RECORD,".K")
            IF (II == 0) II=len_trim(RECORD)
            RECORD=TRIM(RECORD(:II-1))//".sum"
            OPEN(UNIT=JOSUM,FILE=TRIM(RECORD),STATUS='replace')
          ENDIF
        ENDIF
        WRITE (JOSUM,2) LENG,NPLT,MGMID,SAMWT,VVER,DAT,TIM,
     &                  REV,CISN,IPTINV
    2   FORMAT ('-999',I5,1X,A26,1X,A4,E15.7,5(1X,A),I3)
      ENDIF
C
C     STEP2: WRITE VARIANT SPECIFIC HEADING.
C            SKIP A FEW LINES, DO NOT START A NEW PAGE.
C
      IF (LPRT) THEN
      WRITE (JOSTND,5) NPLT,MGMID,ITITLE(1:ISTLNB(ITITLE))
    5 FORMAT(/'STAND ID: ',A26,4X,'MGMT ID: ',A4,4X,A/)
C
      IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR.
     1    (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     2    (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN')) THEN
C----------
C  WRITE HEADER FOR CS, LS, NE, OZ, SE, SN
C----------
      WRITE (JOSTND,12)
  12  FORMAT(//32X,'SUMMARY STATISTICS (PER ACRE OR STAND BASED ON TOTAL
     & STAND AREA)',/,
     &  134(1H-),/15X,'START OF SIMULATION PERIOD',21X,'REMOVALS',13X,
     &  'AFTER TREATMENT',4X,'GROWTH THIS PERIOD',/9X,45(1H-),1X,
     &  23(1H-),1X,21(1H-),2X,18(1H-),3X,'MAI  ------',/9X,
     &  'NO OF',14X,'TOP',
     &  6X,'MERCH SAWLG SAWLG NO OF MERCH SAWLG SAWLG',14X,'TOP  RES  ',
     &  'PERIOD ACCRE MORT   MERCH FOR SS',/,
     &  'YEAR AGE TREES  BA  SDI CCF ',
     &  'HT  QMD  CU FT CU FT BD FT TREES CU FT CU FT BD FT  BA  SDI ',
     &  'CCF HT   QMD  YEARS   PER  YEAR   CU FT TYP ZT',
     &  /'---- --- ----- ',
     &  '--- ---- --- --- ---- ',7('----- '),'--- ---- --- --- ----  ',
     &  '------ ---- -----   ----- ------')
C
      ELSE
C----------
C  WRITE HEADER FOR ALL OTHER VARIANTS
C----------
      WRITE (JOSTND,14)
   14 FORMAT(//32X,'SUMMARY STATISTICS (PER ACRE OR STAND BASED ON TOTAL
     & STAND AREA)',/,
     &  134(1H-),/15X,'START OF SIMULATION PERIOD',21X,'REMOVALS',13X,
     &  'AFTER TREATMENT',4X,'GROWTH THIS PERIOD',/9X,45(1H-),1X,
     &  23(1H-),1X,21(1H-),2X,18(1H-),3X,'MAI  ------',/9X,
     &  'NO OF',14X,'TOP',
     &  6X,'TOTAL MERCH MERCH NO OF TOTAL MERCH MERCH',14X,'TOP  RES  ',
     &  'PERIOD ACCRE MORT   MERCH FOR SS',/,
     &  'YEAR AGE TREES  BA  SDI CCF ',
     &  'HT  QMD  CU FT CU FT BD FT TREES CU FT CU FT BD FT  BA  SDI ',
     &  'CCF HT   QMD  YEARS   PER  YEAR   CU FT TYP ZT',
     &  /'---- --- ----- ',
     &  '--- ---- --- --- ---- ',7('----- '),'--- ---- --- --- ----  ',
     &  '------ ---- -----   ----- ------')
      ENDIF
      ENDIF
C----------
C     STEP3: LOOP THRU ALL ROWS IN IOSUM...WRITE OUTPUT.
C
C  THIS STEP TAKES JUST THE FIRST 12 ITEMS IN THE IOSUM ARRAY
C----------
      I12=I20-8
      DO 50 I=1,LENG
      IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR.
     &    (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     &    (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN')) THEN
C
        IF(LPRT)
     &    WRITE(JOSTND,20) (IOSUM(K,I),K=1,3),IOLDBA(I),ISDI(I),
     &      IBTCCF(I),IBTAVH(I),QSDBT(I),(IOSUM(K,I),K=4,11),
     &      ISDIAT(I),IOSUM(12,I),IOSUM(13,I),QDBHAT(I),
     &      (IOSUM(K,I),K=14,16),BCYMAI(I),(IOSUM(K,I),K=18,20)
C
        IF(LDSK)
     &    WRITE(JOSUM,9014) (IOSUM(K,I),K=1,3),IOLDBA(I),ISDI(I),
     &      IBTCCF(I),IBTAVH(I),QSDBT(I),(IOSUM(K,I),K=4,11),
     &      ISDIAT(I),IOSUM(12,I),IOSUM(13,I),QDBHAT(I),
     &      (IOSUM(K,I),K=14,16),BCYMAI(I),(IOSUM(K,I),K=18,20)
C
      ELSE
C
        IF(LPRT)
     &    WRITE(JOSTND,20) (IOSUM(K,I),K=1,3),IOLDBA(I),ISDI(I),
     &      IBTCCF(I),IBTAVH(I),QSDBT(I),(IOSUM(K,I),K=4,11),
     &      ISDIAT(I),IOSUM(12,I),IOSUM(13,I),QDBHAT(I),
     &      (IOSUM(K,I),K=14,16),BCYMAI(I),(IOSUM(K,I),K=18,20)
C
        IF(LDSK)
     &    WRITE(JOSUM,9014) (IOSUM(K,I),K=1,3),IOLDBA(I),ISDI(I),
     &      IBTCCF(I),IBTAVH(I),QSDBT(I),(IOSUM(K,I),K=4,11),
     &      ISDIAT(I),IOSUM(12,I),IOSUM(13,I),QDBHAT(I),
     &      (IOSUM(K,I),K=14,16),BCYMAI(I),(IOSUM(K,I),K=18,20)

      ENDIF

C
C      CALL THE DBSSUMRY FOR POPULATING THE DATABASE WITH
C      THE SUMMARY INFORMATION
C
        IF (ICFLAG.EQ.0) CALL DBSSUMRY(IOSUM(1,I),
     &     IOSUM(2,I),NPLT,IOSUM(3,I),IOLDBA(I),
     &     ISDI(I),IBTCCF(I),IBTAVH(I),QSDBT(I),IOSUM(4,I),IOSUM(5,I),
     &     IOSUM(6,I),IOSUM(7,I),IOSUM(8,I),IOSUM(9,I),IOSUM(10,I),
     &     IOSUM(11,I),ISDIAT(I),IOSUM(12,I),IOSUM(13,I),QDBHAT(I),
     &     IOSUM(14,I),IOSUM(15,I),IOSUM(16,I),BCYMAI(I),IOSUM(18,I),
     &     IOSUM(19,I),IOSUM(20,I))
C
   20     FORMAT(2I4,I6,I4,I5,2I4,F5.1,7I6,I4,I5,2I4,F5.1,2X,I6,
     &           I5,I6,2X,F6.1,1X,I3,1X,2I1)
C
 9014     FORMAT(2I4,I6,I4,I5,2I4,F5.1,7I6,I4,I5,2I4,F5.1,2X,I6,
     &           I5,I6,2X,F6.1,1X,I3,1X,2I1)
   50 CONTINUE
C
      IF (.NOT.LDSK) RETURN

      WRITE (JOPRT,60) LENG,JOSUM
   60 FORMAT(/'NOTE:',I3,' LINES OF SUMMARY DATA HAVE BEEN WRITTEN',
     >       ' TO THE FILE REFERENCED BY LOGICAL UNIT',I3)
      RETURN
      END
