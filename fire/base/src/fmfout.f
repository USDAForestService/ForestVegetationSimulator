      SUBROUTINE FMFOUT(IYR, FLAME, FMD, IFIRE, CFTMP)
      IMPLICIT NONE
C----------
C  $Id$
C  $Id$
C----------
*     SINGLE-STAND VERSION
*     CALLED FROM: FMBURN
*  PURPOSE:
*     TO PRINT THE THREE OUTPUT FILES WHICH MAY GET PRINTED WHEN A FIRE
*     OCCURS.  THESE FILES ARE: BURN CONDITIONS, FUEL CONSUMPTION, AND
*     TREE MORTALITY.
*----------------------------------------------------------------------
*
*  CALL LIST DEFINITIONS:
*     IFIRE:   FIRE 'TYPE': 0=FUEL TREATMENT FIRE ONLY,
*                 1=STAND FIRE ONLY, 2=STAND FIRE & TREATMENT FIRE.
*                -1=NONE (JUST CALLED TO CHECK OPTION PROCESSOR)
*     IYR:     CURRENT YEAR
*     FLAME:   FLAME LENGTH (FT)
*     FMD:     CURRENT FUEL MODEL
*     CFTMP:   LABEL FOR FIRE TYPE
*
*  LOCAL VARIABLE DEFINITIONS:
*     CLSKIL:  FIRE TREE MORTALITY, BY SPECIES AND 2" DBH SIZE CLASS
*     LFIRST:  IS THIS THE FIRST LINE OF THE BLOCK TO BE PRINTED?
*                 (note that we will need to remove this if want totally
*                  machine readable output)
*     TOTCLS:  TOTAL NUMBER OF TREES IN THE CLASS, BEFORE MORTALITY
*
*  COMMON BLOCK VARIABLES AND PARAMETERS:
*
***********************************************************************

C.... PARAMETER STATEMENTS.

C.... PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... COMMON INCLUDE FILES.
C
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'FMCOM.F77'
      INCLUDE 'FMFCOM.F77'

C.... VARIABLE DECLARATIONS.
C        note: if change MAXCL,must also change the formats 335,345!
C
      INTEGER MIDBH,MAXCL,MAXCL1,MXSP1
C
      PARAMETER (MIDBH = 20)
C      PARAMETER (MAXCL = 14)
      PARAMETER (MAXCL = 7)
      PARAMETER (MAXCL1 = MAXCL + 1)
      PARAMETER (MXSP1 = MAXSP + 1)
C
      INTEGER  FMD, IFIRE
      LOGICAL  LFIRST, DEBUG
      REAL     CLSKIL(MXSP1,MAXCL1), TOTCLS(MXSP1,MAXCL1),
     &         TOTBAK(MXSP1),TOTVOLK(MXSP1), BurnG12

      CHARACTER VVER*7
      CHARACTER*(*) CFTMP
      CHARACTER*90 ALLINE
      INTEGER  WPOS1, WPOS2
C
      INTEGER IYR,JROUT,K,I,J,L,II,IJ,IK,ICRB,KSP,ICLS
      INTEGER DBSKODEM, DBSKODEB, DBSKODEF
      REAL    FLAME,SUML3,SUMG3,BLIVE,TOTG3,PDUFF,PGR3,BDTOT

      CALL DBCHK (DEBUG,'FMFOUT',6,ICYC)
      IF (DEBUG) WRITE(JOSTND,7) ICYC,IYR,FLAME,FMD,IFIRE,CFTMP
    7 FORMAT(' ENTERING FMFOUT CYCLE=',I3,' IYR=',I4,' FLAME=',F14.4,
     >       ' FMD=',I2,' IFIRE=',I4,' CFTMP=',A)

C     ROUTINE BEGINS

C     BURN CONDITIONS REPORT: IFMBRB, IFMBRE ARE THE BEGINNING AND 
C     ENDING YEARS FOR THE REPORT.  
C     RETREIVE THE UNIT NUMBER FOR ANY REPORTS WHICH MAY BE WRITTEN.
C

      CALL GETLUN (JROUT)
C
      IF (DEBUG) WRITE(JOSTND,777) IFMBRB,IFMBRE,IDBRN,JROUT
  777 FORMAT(' IN FMFOUT, IFMBRB IFMBRE IDBRN JROUT: ',4I5)
C
C     WRITE THE BURN CONDITIONS REPORT IF CURRENT CYCLE IS WITHIN THE
C     TIME WINDOW FOR PRODUCING THE REPORT. WRITE REPORT HEADER
C     IF REQUESTED.
C
      IF (IYR .GE. IFMBRB .AND. IYR .LE. IFMBRE .AND. IFIRE .GE. 0) THEN
C
C     CALL THE DBS MODULE TO OUTPUT BURN CONDITIONS DATA TO A DATABASE
C
        IF (IFIRE .NE. 0) THEN
          DBSKODEB = 1
          CALL DBSFMBURN(IYR,NPLT,MOIS(1,1)*100,MOIS(1,2)*100,
     &      MOIS(1,3)*100,MOIS(1,4)*100,MOIS(1,5)*100,MOIS(2,1)*100,
     &      MOIS(2,2)*100,FWIND,INT(FMSLOP*100),FLAME,SCH,CFTMP,FMOD,
     &      FWT,DBSKODEB)
          IF(DBSKODEB.EQ.0) GOTO 105
        ENDIF

        IBRPAS = IBRPAS + 1
        IF (IBRPAS .EQ. 1) THEN
          WRITE (JROUT,39) IDBRN,IDBRN
          WRITE (JROUT,40) IDBRN
          WRITE (JROUT,41) IDBRN
          WRITE (JROUT,43) IDBRN
          WRITE (JROUT,44) IDBRN,NPLT,MGMID
          WRITE (JROUT,40) IDBRN
          WRITE (JROUT,45) IDBRN
          WRITE (JROUT,46) IDBRN
          WRITE (JROUT,47) IDBRN
          WRITE (JROUT,40) IDBRN
   39     FORMAT(2(/1X,I5))
   40     FORMAT (1X,I5,1X,113('-'))
   41     FORMAT (1X,I5,37X,'******  FIRE MODEL VERSION 1.0 ******')
   43     FORMAT (1X,I5,26X,'BURN CONDITIONS REPORT --',
     &         ' CONDITIONS AT THE TIME OF THE FIRE')
   44     FORMAT (1X,I5,' STAND ID: ',A26,4X,'MGMT ID: ',A4)
   45     FORMAT (1X,I5,43X,'MIDFLAME       FLAME  SCORCH',21X,
     &         'FUEL MODELS')
   46     FORMAT (1X,I5,7X,'------------ % MOISTURE ------------- WIND',
     &         '  SLOPE  LENGTH HEIGHT',12X,31('-'))
   47     FORMAT (1X,I5,1X,'YEAR  1HR 10HR 100HR  3+ DUFF LIVE W LIVE',
     &        ' H (MPH)  (%)    (FT)  (FT)  FIRE TYPE   ',4('MOD %WT '))
        ENDIF

        IF (IFIRE .NE. 0) THEN
          IF (FMD.EQ.0) THEN
            WRITE (JROUT,100) IDBRN,IYR,
     &           (MOIS(1,1)*100),(MOIS(1,2)*100),
     &           (MOIS(1,3)*100),(MOIS(1,4)*100),
     &           (MOIS(1,5)*100),(MOIS(2,1)*100),(MOIS(2,2)*100),
     &           FWIND, INT(FMSLOP*100), FLAME, SCH, CFTMP,
     &           0,100
          ELSE
            WRITE (JROUT,100) IDBRN,IYR,
     &           (MOIS(1,1)*100),(MOIS(1,2)*100),
     &           (MOIS(1,3)*100),(MOIS(1,4)*100),
     &           (MOIS(1,5)*100),(MOIS(2,1)*100),(MOIS(2,2)*100),
     &           FWIND, INT(FMSLOP*100.), FLAME, SCH, CFTMP,
     &           (FMOD(I),INT((FWT(I)*100.)+0.5),
     &            I=1,NFMODS)
  100        FORMAT(1X,2I5,5F5.0,F6.0,1X,F6.0,1X,F5.1,I6,2F7.1,2X,A,T89,
     &       8(1X,I3))
           ENDIF
        ENDIF
      ENDIF
  105 CONTINUE
C*************************************
C     FUEL CONSUMPTION REPORT: IFMFLB, IFMFLE ARE THE BEGINNING AND 
C     ENDING YEARS FOR THE REPORT.  
C
      IF (DEBUG) WRITE(JOSTND,778) IFMFLB,IFMFLE,IDFUL,JROUT
  778 FORMAT(' IFMFLB IFMFLE IDFUL JROUT: ',4I5)
C
C     WRITE THE FUEL CONSUMPTION REPORT IF CURRENT CYCLE IS WITHIN THE
C     TIME WINDOW FOR PRODUCING THE REPORT. WRITE REPORT HEADER
C     IF REQUESTED.
C
      IF (IYR.GE.IFMFLB.AND.IYR.LE.IFMFLE.AND.IFIRE.GE.0) THEN

        SUML3 = 0.0
        SUMG3 = 0.0
        BLIVE = 0.0
        TOTG3 = 0.0
        PDUFF = 0.0
C
C       First, sum up the CWD info:
C       Zero the summation columns (index 1=3 and 4=5)

        DO I = 1, MXFLCL
          DO J = 1, 2
            CWD(3,I,J,5) = 0.0
          ENDDO
        ENDDO

C       Calculate summation in Pile categories

        DO I = 1, 2
          DO J = 1, MXFLCL
            DO K = 1, 2
              DO L = 1, 4
                CWD(3,J,K,5) = CWD(3,J,K,5) + CWD(I,J,K,L)
              ENDDO
            ENDDO
          ENDDO
        ENDDO

        DO II=1,3
          IJ = II + 3
          IK = II + 6
          SUML3 = SUML3 + BURNED(3,II)
          SUMG3 = SUMG3 + BURNED(3,IJ) + BURNED(3,IK)
          TOTG3 = TOTG3 + CWD(3,IJ,1,5) + CWD(3,IJ,2,5) + 
     &                    CWD(3,IK,1,5) + CWD(3,IK,2,5)
        ENDDO
        BurnG12 = BURNED(3,6)+ BURNED(3,7)+ BURNED(3,8)+ BURNED(3,9)
        IF (BURNED(3,11)+CWD(3,11,1,5)+CWD(3,11,2,5) .GT. 0.0)
     &    PDUFF = 100. * (BURNED(3,11) /
     &    (BURNED(3,11) + CWD(3,11,1,5) + CWD(3,11,2,5)))
        IF (SUMG3 + TOTG3 .GT. 0.0)
     &    PGR3 = 100.0 * (SUMG3 / (SUMG3 + TOTG3))
        SMOKE(1) = SMOKE(1) * P2T
        SMOKE(2) = SMOKE(2) * P2T

        BLIVE = BURNLV(1) + BURNLV(2)

        BDTOT = SUML3 + SUMG3 + BURNED(3,11) + BURNED(3,10)
     &          + BLIVE + BURNCR

        ICRB=INT(CRBURN*100.0+.5)
        IF (IFIRE.EQ.0) ICRB=0
        IF (ICRB.LT.0) ICRB=-1
C
C     CALL THE DBS MODULE TO OUTPUT FUEL CONSUMPTION INFO TO A DATABASE
C
        DBSKODEF = 1
        CALL DBSFMFUEL(IYR,NPLT,EXPOSR,BURNED(3,10),BURNED(3,11),
     &   SUML3,SUMG3,BURNED(3,4),BURNED(3,5),BurnG12,BLIVE,BURNCR,
     &   BDTOT,PDUFF,PGR3,ICRB,SMOKE(1),SMOKE(2),DBSKODEF)
        IF(DBSKODEF.EQ.0) GOTO 252

        IFLPAS = IFLPAS + 1
C
C       WRITE THE HEADER IF REQUESTED.
C
        IF (IFLPAS .EQ. 1) THEN
          WRITE (JROUT,200) IDFUL,IDFUL
          WRITE (JROUT,201) IDFUL
          WRITE (JROUT,202) IDFUL
          WRITE (JROUT,203) IDFUL
          WRITE (JROUT,44) IDFUL,NPLT,MGMID
          WRITE (JROUT,201) IDFUL
          WRITE (JROUT,205) IDFUL
          WRITE (JROUT,206) IDFUL
          WRITE (JROUT,207) IDFUL
          WRITE (JROUT,208) IDFUL
          WRITE (JROUT,201) IDFUL
  200     FORMAT(2(/1X,I5))
  201     FORMAT(1X,I5,1X,104('-'))
  202     FORMAT(1X,I5,30X,'******  FIRE MODEL VERSION 1.0 ******')
  203     FORMAT(1X,I5,28X,'FUEL CONSUMPTION & PHYSICAL EFFECTS'
     &         ' REPORT (BASED ON STOCKABLE AREA)')
  205     FORMAT(1X,I5,6X,'PERCENT',11X,'FUEL CONSUMED ',
     &         '(TONS/ACRE)',12X,12X,9X,6X,'%',2X,'     SMOKE')
  206     FORMAT(1X,I5,6X,'MINERAL',61('-'),9X,3X,'TREES',
     &         1X,'  PRODUCTION')
  207     FORMAT(1X,I5,7X,'SOIL',1X,45X,'HERB&',6X,
     &         ' TOTAL',2X,'%CONSUME',1X,' WITH ',
     &         2X,' (TONS/ACRE)')
  208     FORMAT(1X,I5,1X,'YEAR EXPOSR  LITR ',
     &         ' DUFF   0-3"    3"+  3-6" 6-12" ',
     &         ' 12"+ SHRUB CRWNS CONS.  DUFF 3"+',
     &         '  CRWNG   <2.5   < 10')
        ENDIF
C
        WRITE (JROUT,250) IDFUL,IYR,INT(EXPOSR),
     &    BURNED(3,10), BURNED(3,11), SUML3, SUMG3,
     &    BURNED(3,4), BURNED(3,5), BURNG12, BLIVE, BURNCR,
     &    BDTOT, INT(PDUFF), INT(PGR3), ICRB,
     &    SMOKE(1), SMOKE(2)
  250   FORMAT(1X,I5,1X,I4,1X,I4,3X,2(F5.1,1X),2(F6.1,1X),
     &    5(F5.1,1X),F6.1,2X,I3,1X,I3,2X,I4,2X,F6.2,1X,F6.2)

      ENDIF
  252 CONTINUE
C*************************************
C     MORTALITY REPORT: IFMMRB, IFMMRE ARE THE BEGINNING AND 
C     ENDING YEARS FOR THE REPORT.  
C
      IF (DEBUG) WRITE(JOSTND,779) IFMMRB,IFMMRE,IDMRT,JROUT
  779   FORMAT(' IFMMRB IFMMRE IDMRT JROUT: ',4I5)
C
C     WRITE THE MORTALITY REPORT IF CURRENT CYCLE IS WITHIN THE
C     TIME WINDOW FOR PRODUCING THE REPORT.
C
      IF (IYR .GE. IFMMRB .AND. IYR .LE. IFMMRE .AND.
     &                                 IFIRE .GE. 0) THEN
C
C       FIRST, ACCUMULATE MORTALITY INTO SPECIES & SIZE CLASSES
C
        DO KSP=1,MXSP1
          TOTBAK(KSP)=0.
          TOTVOLK(KSP)=0.
          DO ICLS=1,MAXCL1
            CLSKIL(KSP,ICLS) = 0.
            TOTCLS(KSP,ICLS) = 0.
          ENDDO
        ENDDO

        DO 310 I= 1, ITRN
          KSP = ISP(I)
          IF (KSP .GT. 0) THEN

C           ACCUMULATED THE BA AND VOL KILLED.

            TOTBAK(KSP)=TOTBAK(KSP)+(CURKIL(I)*DBH(I)*DBH(I)*.005454154)
            TOTVOLK(KSP)=TOTVOLK(KSP)+(CURKIL(I)*CFV(I))

C           Find the first lower bound that this dbh is less than
C           This means that the dbh is in the class just before
C           If the dbh is less than the first class (ICLS=0) then
C           this tree will not be included in the output.

            DO 301 ICLS=1, 7
              IF (DBH(I) .LT. LOWDBH(ICLS)) GOTO 302
  301       CONTINUE
  302       CONTINUE
            ICLS = ICLS - 1
            IF (ICLS .EQ. 0) GOTO 310

            CLSKIL(KSP,ICLS) = CLSKIL(KSP,ICLS) + CURKIL(I)
            CLSKIL(KSP,MAXCL1) = CLSKIL(KSP,MAXCL1) + CURKIL(I)
            CLSKIL(MXSP1,ICLS) = CLSKIL(MXSP1,ICLS) + CURKIL(I)
            TOTCLS(KSP,ICLS) = TOTCLS(KSP,ICLS) + CURKIL(I)
     &        + FMPROB(I)
            TOTCLS(KSP,MAXCL1) = TOTCLS(KSP,MAXCL1) + CURKIL(I)
     &        + FMPROB(I)
            TOTCLS(MXSP1,ICLS) = TOTCLS(MXSP1,ICLS) + CURKIL(I)
     &        + FMPROB(I)
            TOTCLS(MXSP1,MAXCL1) = TOTCLS(MXSP1,MAXCL1) +
     &        CURKIL(I) + FMPROB(I)
          ENDIF
  310   CONTINUE

        DO KSP=1,MAXSP
          TOTBAK(MXSP1)=TOTBAK(MXSP1)+TOTBAK(KSP)
          TOTVOLK(MXSP1)=TOTVOLK(MXSP1)+TOTVOLK(KSP)
        ENDDO

C
C     CALL THE DBS MODULE TO OUTPUT THE MORTALITY REPORT TO A DATABASE
C
        DBSKODEM = 1
        CALL DBSFMMORT(IYR,CLSKIL,TOTCLS,TOTBAK,TOTVOLK,DBSKODEM)
        IF(DBSKODEM.EQ.0) RETURN

        IMRPAS = IMRPAS + 1
C
C       WRITE HEADER IF REQUESTED.
C
        CALL VARVER(VVER)
        IF (IMRPAS .EQ. 1) THEN
          WRITE (JROUT,315) IDMRT, IDMRT
          WRITE (JROUT,316) IDMRT
          WRITE (JROUT,317) IDMRT
          WRITE (JROUT,318) IDMRT
          WRITE (JROUT,44) IDMRT,NPLT,MGMID
          WRITE (JROUT,316) IDMRT
          IF ((VVER(1:2) .EQ. 'SN') .OR. (VVER(1:2) .EQ. 'LS') .OR.
     &        (VVER(1:2) .EQ. 'NE') .OR. (VVER(1:2) .EQ. 'CS')) THEN
            WRITE (JROUT,420) IDMRT
          ELSE
            WRITE (JROUT,320) IDMRT
          ENDIF
          WRITE (JROUT,321) IDMRT,
     &      (LOWDBH(I),LOWDBH(I+1),I=1,MAXCL-1),
     &      LOWDBH(MAXCL)
          WRITE (JROUT,316) IDMRT
C
  315     FORMAT (2(/1X,I5))
  316     FORMAT (1X,I5,1X,117('-'))
  317     FORMAT(1X,I5,33X,'******  FIRE MODEL VERSION 1.0 ******')
  318     FORMAT (1X,I5,43X,'MORTALITY REPORT '
     &                      '(BASED ON STOCKABLE AREA)')
  319     FORMAT (1X,I5)
  320     FORMAT (1X,I5,21X,
     &      'NUMBER KILLED / NUMBER BEFORE ',
     &      '(BY DIAMETER CLASS IN INCHES)',T111,'BASAL    TOTAL')
  420     FORMAT (1X,I5,21X,
     &      'NUMBER KILLED / NUMBER BEFORE ',
     &      '(BY DIAMETER CLASS IN INCHES)',T111,'BASAL    MERCH')
  321     FORMAT (1X,I5,1X,'YEAR  SP  ',
     &      6(2X,F4.1,'-',F4.1,2X),4X,'>=',F4.1,T111,'AREA     CU FT')
        ENDIF

        LFIRST = .TRUE.
        DO KSP=1,MXSP1
          ALLINE = ' '
          WPOS1 = 2
          WPOS2 = 6
          DO 335 ICLS=1,MAXCL
            IF (TOTCLS(KSP,ICLS) .GT. 0.0) THEN
              WRITE (ALLINE(WPOS1:WPOS2),'(I5)')
     &          INT(CLSKIL(KSP,ICLS))
              WPOS1 = WPOS2 + 1
              WPOS2 = WPOS1
              WRITE (ALLINE(WPOS1:WPOS2),'("/")')
              WPOS1 = WPOS1 + 1
              WPOS2 = WPOS2 + 5
              WRITE (ALLINE(WPOS1:WPOS2),'(I5)')
     &          INT(TOTCLS(KSP,ICLS))
              WPOS1 = WPOS1 + 7
              WPOS2 = WPOS2 + 7
            ELSE
              CLSKIL(KSP,ICLS) = -1.0
              WPOS1 = WPOS1 + 13
              WPOS2 = WPOS2 + 13
            ENDIF
  335     CONTINUE

C
C         PRINT ONLY THOSE SPECIES WHICH ARE PRESENT IN THE STAND
C         (EVEN IF THEY HAD NO MORTALITY).
C
          IF (TOTCLS(KSP,MAXCL1) .GT. 0.0 .AND. KSP .LT. MXSP1) THEN

            IF (LFIRST) THEN
              WRITE (JROUT,340) IDMRT,IYR,JSP(KSP),
     &          ALLINE,TOTBAK(KSP),INT(TOTVOLK(KSP))

  340         FORMAT(1X,I5,1X,I4,2X,A3,1X,A90,T108,F8.2,I9)
              LFIRST = .FALSE.
            ELSE
              WRITE (JROUT,345) IDMRT,JSP(KSP),ALLINE,
     &                          TOTBAK(KSP),INT(TOTVOLK(KSP))
  345         FORMAT(1X,I5,7X,A3,1X,A90,T108,F8.2,I9)
            ENDIF
          ELSEIF (TOTCLS(KSP,MAXCL1).GT.0.0 .AND. KSP.EQ.MXSP1) THEN
            IF (LFIRST) THEN
              WRITE (JROUT,340) IDMRT,IYR,'ALL',ALLINE,
     &                          TOTBAK(KSP),INT(TOTVOLK(KSP))
              LFIRST = .FALSE.
            ELSE
              WRITE (JROUT,345) IDMRT,'ALL',ALLINE,
     &                          TOTBAK(KSP),INT(TOTVOLK(KSP))
            ENDIF
          ENDIF
        ENDDO
      ENDIF

      RETURN
      END
