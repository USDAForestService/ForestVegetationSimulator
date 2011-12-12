      SUBROUTINE DISPLY
      IMPLICIT NONE
C----------
C  **DISPLY--BASE/M    DATE OF LAST REVISION:  07/02/10
C----------
C  **DISPLY** PRINTS STAND AND TREE STATISTICS AT THE BEGINNING
C  OF THE PROJECTION, AND AT THE END OF EACH CYCLE.  ESTIMATES
C  STAND AGE IF MISSING FROM THE STNDINFO KEYWORD
C----------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'OUTCOM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'ECON.F77'
      INCLUDE 'SCREEN.F77'
      INCLUDE 'SUMTAB.F77'
      INCLUDE 'VARCOM.F77'
      INCLUDE 'METRIC.F77'
C
COMMONS
C
C----------
C  DIMENSIONS FOR INTERNAL VARIABLES (USED AS OUTPUT LABELS):
C----------
      INTEGER IRT,JYR,I,IOAGE,IKNT,IXF,IFIA,ISWT,J,JSDI,ISNOFT
      REAL DUM1,SUMAGE,AGEKNT,TEM,SDIBCTMP,SDIACTMP
      REAL X3,X4,X5,X9,X14,X15
      CHARACTER*7 VVER
      CHARACTER*9 AT1,AT2,AT3,AT4
      CHARACTER*10 STD(3)
C----------
C  DATA STATEMENTS:
C----------
      DATA AT1/'REMOVAL  '/,AT2/'VOLUME:  '/,AT3/
     & 'RESIDUAL '/,AT4/'ACCRETION'/
C----------
C  IDENTIFY VARIANT
C----------
      CALL VARVER (VVER)
C----------
C  IF ICL6 IS NEGATIVE,  PROJECTION IS COMPLETE AND WE NEED ONLY TO
C  PRINT THE FINAL LINE FOR  THE SAMPLE TREE OUTPUT  (DISPLAYING
C  STAND ATTRIBUTES FOLLOWING THINNING), AND THE FINAL LINE OF THE
C  SUMMARY OUTPUT.   BRANCH TO STATEMENT 34.
C----------
      IF (.NOT.LSTART.OR.ITABLE(2).EQ.1) GO TO 10
      IRT = 0
      WRITE (JOTREE) IRT,NPLT,MGMID
   10 CONTINUE
      IF(ICL6.LT.0) GO TO 34
C----------
C  ASSIGN JYR.
C----------
      JYR=IY(ICYC+1)
C----------
C   IF USER HAS SPECIFIED KEYWORDS THAT ALTER VOLUME EQUATION PARAMETERS
C   (IE VOLUMME BFVOLUME,ETC) THEN PRINT LABEL THAT INDICATES USER
C   SPECIFIED STANDARDS. THESE LABELS ARE VARIANT SPECIFIC
C----------
      IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR. 
     1    (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     2    (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN') .OR.
     3    (VVER(:2) .EQ. 'ON')) THEN
C
         IF (LCVOLS) THEN
            STD(1)='USER MERCH'
            STD(2)='SAWLG'
         ELSE
            STD(1)='MERCH'
            STD(2)='SAWLG'
         ENDIF
C
         IF (LBVOLS) THEN
            STD(3)='USER SAWLG'
         ELSE
            STD(3)='SAWLG'
         ENDIF
C
      ELSE
C
        IF (LCVOLS) THEN
           STD(1)='USER TOTAL'
           STD(2)='USER MERCH'
        ELSE
           STD(1)='TOTAL'
           STD(2)='MERCH'
        ENDIF
C
        IF (LBVOLS) THEN
           STD(3)='USER MERCH'
        ELSE
           STD(3)='MERCH'
        ENDIF
      ENDIF
C----------
C  WRITE REMOVAL STATISTICS.  BYPASS IF LSTART IS TRUE OR NUMBER
C  OF TREES REMOVED WAS LESS THAN OR EQUAL TO ZERO.
C  SKIP PRINTING OF STAND COMPOSITION TABLE IF NOT WANTED.
C----------
      IF (ITABLE(1) .EQ. 1) GO TO 32
      IF(LSTART) GO TO 30
      IF(ONTREM(7).LE.0.0) GO TO 20
      WRITE(JOSTND,9003)
 9003 FORMAT(/)
      WRITE(JOSTND,9004) AT1,
     &                   (ONTREM(I)*INtoCM,I=1,6),
     &                   ONTREM(7)/ACRtoHA,
     &                   (OSPTT(I),IOSPTT(I),I=1,4)
 9004 FORMAT(7X,A9,3X,5F7.1,F8.1,F9.0,' TREES   ',
     >       3(F5.0,'% ',A3,','),F5.0,'% ',A3)
C
C
      WRITE(JOSTND,9015) AT2,
     &                   STD(1),
     &                   (OCVREM(I)*INtoCM,I=1,6),
     &                   OCVREM(7)*FT3pACRtoM3pHA,
     &                   (OSPTV(I),IOSPTV(I),I=1,4),
     &                   STD(2),
     &                   (OMCREM(I)*INtoCM,I=1,6),
     &                   OMCREM(7)*FT3pACRtoM3pHA
 9015 FORMAT( 7X,A9,
     &      /9X,A10,5F7.1,F8.1,F9.0,' CU M    ',
     &       3(F5.0,'% ',A3,','),F5.0,'% ',A3,
     &      /9X,A10,5F7.1,F8.1,F9.0,' CU M    ',
     &       3(F5.0,'% ',A3,','),F5.0,'% ',A3)
C
      WRITE(JOSTND,9003)
      WRITE(JOSTND,9004) AT3,
     &                   (ONTRES(I)*INtoCM,I=1,6),
     &                   ONTRES(7)/ACRtoHA,
     &                   (OSPRT(I),IOSPRT(I),I=1,4)
C----------
C  WRITE ACCRETION AND MORTALITY STATISTICS.  BYPASSED IF LSTART
C  IS TRUE. IF MORTALITY FUNCTIONS HAVE BEEN MODIFIED BY CERTAIN
C  KEYWORDS THEN CHANGE LABEL FROM 'MORTALITY' TO 'USER MORT'.
C----------
   20 CONTINUE
      WRITE(JOSTND,9003)
      WRITE(JOSTND,9006) AT4,
     &                   (OACC(I)*INtoCM,I=1,6),
     &                    OACC(7)*FT3pACRtoM3pHA,
     &                   (OSPAC(I),IOSPAC(I),I=1,4)
C
      IF (LMORT) THEN
        WRITE(JOSTND,9006) 'USER MORT',
     &                     (OMORT(I)*INtoCM,I=1,6),
     &                     OMORT(7)*FT3pACRtoM3pHA,
     &                     (OSPMO(I),IOSPMO(I),I=1,4)
      ELSE
        WRITE(JOSTND,9006) 'MORTALITY',
     &                     (OMORT(I)*INtoCM,I=1,6),
     &                     OMORT(7)*FT3pACRtoM3pHA,
     &                     (OSPMO(I),IOSPMO(I),I=1,4)
      ENDIF
 9006 FORMAT(7X,A9,3X,5F7.1,F8.1,F9.1,' CU M/YR ',
     >       3(F5.0,'% ',A3,','),F5.0,'% ',A3)
C----------
C  WRITE CURRENT STAND STATISTICS.
C----------
   30 CONTINUE
      WRITE(JOSTND,9003)
      WRITE(JOSTND,9007) JYR,
     &                   (ONTCUR(I)*INtoCM,I=1,6),
     &                   ONTCUR(7)/ACRtoHA,
     &                   (OSPCT(I),IOSPCT(I),I=1,4)
 9007 FORMAT(/,1X,I4,'  TREES',T20,5F7.1,F8.1,F9.0,' TREES   ',
     >       3(F5.0,'% ',A3,','),F5.0,'% ',A3)
C
      WRITE(JOSTND,9015) AT2,
     &                   STD(1),
     &                   (OCVCUR(I)*INtoCM,I=1,6),
     &                   OCVCUR(7)*FT3pACRtoM3pHA,
     &                   (OSPCV(I),IOSPCV(I),I=1,4),
     &                   STD(2),
     &                   (OMCCUR(I)*INtoCM,I=1,6),
     &                   OMCCUR(7)*FT3pACRtoM3pHA,
     &                   (OSPMC(I),IOSPMC(I),I=1,4)
   32 CONTINUE
C----------
C  WRITE OUTPUT FOR SAMPLE TREES.  FIRST, OUTPUT STAND CONDITIONS
C  FOR PREVIOUS CYCLE.  BYPASS IF LSTART IS TRUE. ALSO SKIP TREE
C  DATA IF TABLE IS NOT DESIRED.
C----------
      IF(LSTART) GO TO 39
   34 CONTINUE
      IOAGE=IAGE+IY(ICYC)-IY(1)
      IF (ITABLE(2) .EQ. 1) GO TO 41
C----------
C  GO TO STATEMENT 35 AND WRITE BOTH PRE AND POST THINNING STATISTICS
C  IF THINNING OCCURRED IN PREVIOUS CYCLE.
C----------
      IF(LZEIDE)THEN
        JSDI=SDIBC2 + 0.5
      ELSE
        JSDI=SDIBC + 0.5
      ENDIF
      IF(ONTREM(7).GT.0.0) GO TO 35
      IRT=1
      WRITE(JOTREE) IRT,
     &              IOAGE,
     &              ORMSQD*INtoCM,
     &              OLDTPA/ACRtoHA,
     &              OLDBA*FT2pACRtoM2pHA,
     &              OLDAVH*FTtoM,
     &              RELDM1,
     &              JSDI/ACRtoHA ! dr - 17 nov 00
      GO TO 36
   35 CONTINUE
      IRT=2
      WRITE(JOTREE) IRT,
     &              IOAGE,
     &              ORMSQD*INtoCM,
     &              OLDTPA/ACRtoHA,
     &              OLDBA*FT2pACRtoM2pHA,
     &              OLDAVH*FTtoM,
     &              RELDM1,
     &              JSDI/ACRtoHA !

      IRT=3
      JSDI=SDIAC + 0.5
      IF(LZEIDE)THEN
        JSDI=SDIAC2 + 0.5
      ELSE
        JSDI=SDIAC + 0.5
      ENDIF
      WRITE(JOTREE) IRT,
     &              ATAVD*INtoCM,
     &              ATTPA/ACRtoHA,
     &              ATBA*FT2pACRtoM2pHA,
     &              ATAVH*FTtoM,
     &              ATCCF,
     &              JSDI/ACRtoHA !
   36 CONTINUE
C----------
C  IF ICL6.LT.0 BRANCH TO STMT. 90 TO LOAD LAST LINE OF SUMMARY OUTPUT
C----------
      IF(ICL6.LT.0) GO TO 90
C----------
C  WRITE YEAR  AND PERIOD LENGTH.
C----------
   39 CONTINUE
      IF(ITABLE(2).EQ.1)GO TO 41
C-------
C  IF SAMPLE TREES WERE RESELECTED THIS CYCLE (IFST=99), THEN SET
C  SIGN OF YEAR NEGETIVE.
C-------
      I=JYR
      IF (IFST.EQ.0) GOTO 40
      IFST=0
      IF (LSTART) GOTO 40
      I=-I
   40 CONTINUE
      IRT=4
      WRITE(JOTREE) IRT,I,IFINT
C----------
C  WRITE EXAMPLE TREE DATA.
C----------
      IRT=5
      WRITE(JOTREE) IRT,
     &              IONSP,
     &              DBHIO*INtoCM,
     &              HTIO*FTtoM,
     &              IOICR,
     &              DGIO*INtoCM,
     &              PCTIO,
     &              PRBIO/ACRtoHA ! dr 17 nov 00
  41  CONTINUE
C----------
C  LOAD THE ARRAY CONTAINING SUMMARY DATA.
C----------
      IKNT=ICYC+1
C----------
C  CALCULATE FOREST TYPE (IFORTP), SIZE CLASS (ISZCL),
C  AND STOCKING CLASS (ISTCL)
C----------
      DUM1=0.
      IXF=2
      ISNOFT=IFORTP
      CALL FORTYP(IXF,DUM1)
C-------
C  IF THIS IS THE SN VARIANT, AND THE FOREST TYPE JUST CHANGED, THEN
C  WE MAY ALSO NEED TO UPDATE THE SDIDEF ARRAY AND SDIMAX AND BAMAX
C  SCALERS.
C-------
      IF(VVER(:2).EQ.'SN' .AND. IFORTP.NE.ISNOFT)THEN
        IF (.NOT. LFIXSD) THEN
C----------
C  IF USER SPECIFIED SDIMAX OR BAMAX, THEN MODIFY SDIDEF
C----------
          TEM=PMSDIU
          IF(TEM .GT. 1.0) TEM=TEM/100.
          DO 4 I=1,MAXSP
          IF(MAXSDI(I) .GE. 1) GO TO 4
          IF (LBAMAX)THEN
            SDIDEF(I)=BAMAX/(0.5454154*TEM)
          ELSE
            SDIDEF(I)=DUM1
          ENDIF
    4     CONTINUE
        ENDIF
      CALL SDICAL(SDIMAX)
      BAMAX = SDIMAX*PMSDIU*0.5454154
      ENDIF
C----------
C  ESTIMATE STAND AGE FROM SIZE CLASS AND ABIRTH
C  IF IT WAS MISSING FROM THE INPUT DATA, SET IT FROM THIS ESTIMATE..
C----------
      IF(LSTART)THEN
        SUMAGE=0.
        AGEKNT=0.
C*        WRITE(JOSTND,*)' IAGE,ISZCL,ITRN= ',IAGE,ISZCL,ITRN
        IF(ISZCL.EQ.0 .OR. ISZCL.GT.3)THEN
          ICAGE=0
          GO TO 42
        ELSEIF(ISZCL .EQ. 3) THEN
          DO I=1,ITRN
          IF(DBH(I).LT.5.)THEN
            SUMAGE=SUMAGE+ABIRTH(I)*PROB(I)
            AGEKNT=AGEKNT+PROB(I)
C*            WRITE(JOSTND,*)' I,D,AGE,PROB,SUMAGE,AGEKNT= ',
C*     &      I,DBH(I),ABIRTH(I),PROB(I),SUMAGE,AGEKNT
          ENDIF
          ENDDO
        ELSEIF(ISZCL .EQ. 2) THEN
          DO I=1,ITRN
          IF(FIAJSP(ISP(I))(1:1).EQ.'-')THEN
            IFIA=998
          ELSE
            READ(FIAJSP(ISP(I)),'(I4)') IFIA
          ENDIF
          IF((IFIA.LT.300. .AND. DBH(I).GE.5. .AND. DBH(I).LT.9.) .OR.
     &       (IFIA.GE.300. .AND. DBH(I).GE.5. .AND. DBH(I).LT.11.))THEN
            SUMAGE=SUMAGE+ABIRTH(I)*PROB(I)
            AGEKNT=AGEKNT+PROB(I)
C*            WRITE(JOSTND,*)' I,IFIA,D,AGE,PROB,SUMAGE,AGEKNT= ',
C*     &      I,IFIA,DBH(I),ABIRTH(I),PROB(I),SUMAGE,AGEKNT
          ENDIF
          ENDDO
        ELSE
          DO I=1,ITRN
          IF(FIAJSP(ISP(I))(1:1).EQ.'-')THEN
            IFIA=998
          ELSE
            READ(FIAJSP(ISP(I)),'(I4)') IFIA
          ENDIF
          IF((IFIA.LT.300. .AND. DBH(I).GE.9.) .OR.
     &       (IFIA.GE.300. .AND. DBH(I).GE.11.))THEN
            SUMAGE=SUMAGE+ABIRTH(I)*PROB(I)
            AGEKNT=AGEKNT+PROB(I)
C*            WRITE(JOSTND,*)' I,IFIA,D,AGE,PROB,SUMAGE,AGEKNT= ',
C*     &      I,IFIA,DBH(I),ABIRTH(I),PROB(I),SUMAGE,AGEKNT
          ENDIF
          ENDDO
        ENDIF
        IF(AGEKNT.GT.0.)THEN
C*        WRITE(JOSTND,*)' AGEKNT= ',AGEKNT
          ICAGE=IFIX(SUMAGE/AGEKNT)
        ELSE
          ICAGE=0
        ENDIF
C*        WRITE(JOSTND,*)' SUMAGE,AGEKNT,ICAGE= ',SUMAGE,AGEKNT,ICAGE
C----------
C  REMOVE THE COMMENT CHARACTERS (C**) IN THE FOLLOWING STATEMENT
C  TO ENABLE REPORTING THE INITIAL STAND AGE, AGECMP, (IF 0 IN STDINFO
C  KEYWORD) IN THE SUMMARY STATISTICS TABLE.  DO **NOT** USE FOR THE
C  FOLLOWING VARIANTS, AK, CI, CS, IE, KT, LS, NE, NI, SE, TT, UT, WS,
C  WHICH DO NOT CONSISTENTLY USE SITE CURVES FOR HEIGHT GROWTH.
C----------
C**        IF(IAGE .EQ. 0)IAGE=ICAGE

      ENDIF
   42 CONTINUE
C
      IOSUM(01,IKNT)=IY(IKNT)
      IOSUM(03,IKNT)=ONTCUR(7)/GROSPC+0.5
      IOSUM(04,IKNT)=OCVCUR(7)/GROSPC+0.5
      IOSUM(05,IKNT)=OMCCUR(7)/GROSPC+0.5
      IOSUM(06,IKNT)=OBFCUR(7)/GROSPC+0.5
C
      IOSUM(18,IKNT)=IFORTP
      IOSUM(19,IKNT)=ISZCL
      IOSUM(20,IKNT)=ISTCL
C----------
C  IF LSTART IS TRUE,  DONT LOAD REMOVAL AND GROWTH STATISTICS.
C----------
      IF(LSTART) GOTO 100
C----------
C  ENTER HERE TO LOAD FINAL LINE OF SUMMARY OUTPUT.
C----------
   90 CONTINUE
      IKNT=ICYC
      IOSUM(02,IKNT)=IOAGE
      IF (ONTREM(7).LE.0.0) GOTO 91
      IOSUM(03,IKNT)=OLDTPA/GROSPC+.5
      IOSUM(11,IKNT)=ATBA/GROSPC+0.5
      IOSUM(12,IKNT)=ATCCF/GROSPC+0.5
      IOSUM(13,IKNT)=ATAVH+0.5
      QDBHAT(IKNT)=ATAVD
      SDIACTMP=SDIAC
      IF(LZEIDE)SDIACTMP=SDIAC2
      ISDIAT(IKNT)=SDIACTMP/GROSPC + 0.5
      GOTO 92
   91 CONTINUE
      IOSUM(11,IKNT)=OLDBA/GROSPC+0.5
      IOSUM(12,IKNT)=RELDM1/GROSPC+0.5
      IOSUM(13,IKNT)=OLDAVH+0.5
      QDBHAT(IKNT)=ORMSQD
      SDIBCTMP=SDIBC
      IF(LZEIDE)SDIBCTMP=SDIBC2
      ISDIAT(IKNT)=SDIBCTMP/GROSPC + 0.5
   92 CONTINUE
      SDIBCTMP=SDIBC
      IF(LZEIDE)SDIBCTMP=SDIBC2
      ISDI(IKNT)=SDIBCTMP/GROSPC + 0.5
      IOSUM(07,IKNT)=ONTREM(7)/GROSPC+0.5
      IOSUM(08,IKNT)=OCVREM(7)/GROSPC+0.5
      IOSUM(09,IKNT)=OMCREM(7)/GROSPC+0.5
      IOSUM(10,IKNT)=OBFREM(7)/GROSPC+0.5
      IOSUM(14,IKNT)=IFINT
      IOSUM(15,IKNT)=OACC(7)/GROSPC+0.5
      IOSUM(16,IKNT)=OMORT(7)/GROSPC+0.5
      QSDBT(IKNT)=ORMSQD
      IOLDBA(IKNT)=OLDBA/GROSPC + 0.5
      IBTCCF(IKNT)=RELDM1/GROSPC+0.5
      IBTAVH(IKNT)=OLDAVH+0.5
      IF (SAMWT .GE. .99999) ISWT = IFIX(SAMWT+.0001)
      IF (SAMWT .LT. .99999) ISWT = IFIX(SAMWT*100000.+.5)
      IOSUM(17,IKNT)=ISWT
  100 CONTINUE
      IF (ICL6 .GT. 0) GO TO 150
C----------
C  PRINT THE EXAMPLE TREE RECORD AND STAND ATTRIBUTE TABLE.
C  IF NOT DESIRED THEN SKIP.
C----------
      IF (ITABLE(2) .EQ. 1) GO TO 125
C----------
C  WRITE AN END OF FILE MARK ON SUMMARY TREE SCRATCH FILE.
C----------
      ENDFILE JOTREE
      CALL PRTEXM (JOTREE,JOSTND,ITITLE)
  125 CONTINUE
C----------
C  PRINT THE SUMMARY OUTPUT
C----------
      DO 130 I=7,10
      IOSUM(I,IKNT)=0
  130 CONTINUE
      DO 140 I=14,16
      IOSUM(I,IKNT)=0
  140 CONTINUE
C----------
C  LOAD FINAL MAI VALUE
C----------
      IF(MAIFLG .EQ. 0)THEN
        IF(IOSUM(2,IKNT).GT.0.)THEN
          IF ((VVER(:2) .EQ. 'CS') .OR. (VVER(:2) .EQ. 'LS') .OR.
     1      (VVER(:2) .EQ. 'NE') .OR. (VVER(:2) .EQ. 'OZ') .OR.
     2      (VVER(:2) .EQ. 'SE') .OR. (VVER(:2) .EQ. 'SN') .OR.
     3      (VVER(:2) .EQ. 'ON')) THEN
            BCYMAI(IKNT)=(IOSUM(4,IKNT)+TOTREM)/IOSUM(2,IKNT)
          ELSE
            BCYMAI(IKNT)=(IOSUM(5,IKNT)+TOTREM)/IOSUM(2,IKNT)
          ENDIF
        ELSE
          BCYMAI(IKNT)=0.
        ENDIF
      ELSE
        BCYMAI(IKNT)=0.
      ENDIF
C----------
C     CALL **ECEND** TO OUTPUT LAST LINES OF ECONOMIC OUTPUT
C----------
      IF (LECON) CALL ECEND
      I=0
      IF (LSUMRY) I=JOSUM
C---------
C     SKIP PRINTING OF SUMMARY OUTPUT IF NOT DESIRED.
C---------
      J=JOSTND
      IF (ITABLE(3).EQ.1) J=0
      CALL GROHED (JOSTND)
      CALL LBSPLW (JOSTND)
      CALL SUMOUT (IOSUM,20,0,JOSTND,J,I,IKNT,MGMID,NPLT,SAMWT,
     &ITITLE,IPTINV)
      CALL OPLIST (.FALSE.,NPLT,MGMID,ITITLE)
C----------
C     CALL ECLBL TO OUTPUT ECONOMIC LABEL INFORMATION.
C----------
      IF (LECON) CALL ECLBL
  150 CONTINUE
      IF ((LSTART).AND.(LSCRN)) CALL SUMHED
      IF ((.NOT.LSTART).AND.(LSCRN)) THEN
C
C       PREVENT UGLY ERROR MSGS WHEN ENORMOUS VALUES APPEAR IN THE MAIN
C       OUTPUT FILE AND SUMMARY OUTPUT FILE: SUBSTITUE -1 INSTEAD
C     
	  X3 = IOSUM(3,IKNT)/ACRtoHA 
	  IF (X3 .GT. 9999)     X3 = -1.0  ! i4
	  X4 = IOLDBA(IKNT)*FT2pACRtoM2pHA
	  IF (X4 .GT. 99999)    X4 = -1.0  ! i5
	  X5 = ISDI(IKNT)/ACRtoHA
	  IF (X5 .GT. 9999)     X5 = -1.0  ! i4
        X9 = IOSUM(4,IKNT)*FT3pACRtoM3pHA
	  IF (X9 .GT. 9999)     X9 = -1.0  ! i5 
        X14 = IOSUM(11,IKNT)*FT2pACRtoM2pHA
	  IF (X14 .GT. 9999)   X14 = -1.0  ! i4
        X15 = ISDIAT(IKNT)/ACRtoHA
	  IF (X15 .GT. 9999)   X15 = -1.0  ! i4

        WRITE (JOSCRN,170) IOSUM(1,IKNT),
     >                    NINT(X3),   !i5
     >                    NINT(X4),   !i5
     >                    NINT(X5),   !i4
     >                    NINT(IBTAVH(IKNT)*FTtoM), !i4
     >                    QSDBT(IKNT)*INtoCM,       !f5.1
     >                    NINT(X9),                 !i5 
     >                    NINT(IOSUM(7,IKNT)/ACRtoHA),  !i5
     >                    NINT(IOSUM(8,IKNT)*FT3pACRtoM3pHA ), !i5
     >                    NINT(IOSUM(9,IKNT)*FT3pACRtoM3pHA),  !i5
     >                    NINT(X14),   !i4
     >                    NINT(X15),   !i4
     >                    NINT(IOSUM(13,IKNT)*FTtoM), !i4
     >                    QDBHAT(IKNT)*INtoCM,        !f5.1
     >                    NINT(IOSUM(15,IKNT)*FT3pACRtoM3pHA), !i4
     >                    NINT(IOSUM(16,IKNT)*FT3pACRtoM3pHA)  !i4
      ENDIF
  170 FORMAT (1X,I4,1X,I5,3I4,F5.1,4(1X,I5),   3I4,F5.1,I4,I4)
      RETURN
      END
