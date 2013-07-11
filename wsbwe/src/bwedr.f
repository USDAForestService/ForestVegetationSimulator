      SUBROUTINE BWEDR
      IMPLICIT NONE
C----------
C  **BWEDR                  DATE OF LAST REVISION:  06/17/13
C----------
C
C     PROCESSES ONE STAND IN ONE YEAR OF A BUDWORM MODEL OUTBREAK.
C
C     PART OF THE WESTERN SPRUCE BUDWORM MODEL.
C     FEB. 1984. - FORESTRY SCIENCES LAB - MOSCOW, ID.
C
C     DESCRIPTION :
C
C     BWEDR DRIVES THE BUDWORM MODEL IN ONE STAND FOR ONE
C     YEAR.   NO STAND SWAPS OR YEAR LOOPS OCCUR.
c
c     major modification by K.Sheehan to strip out BW Pop.Dyn.stuff
c        July 1996+
C
C     CALLED FROM :
C
C       BWECUP - SINGLE STAND BUDWORM MODEL LINK TO PROGNOSIS.
C
C     SUBROUTINES CALLED :
C
C     BWEDEF - COMPUTE MANUAL DEFOLIATION.
C     BWELIT - BUD-LITE MODEL.
C     BWEAGE - AGES FOLIAGE ONE YEAR, OLD NEEDLES FALL OFF.
C     BWEDAM - COMPUTES ANNUAL DAMAGE TO THE STAND.
C     OPNEW
C
C   PARAMETERS:
C
C   LCALBW = TRUE IF BUDLITE MODEL IS CALLED
C   LDEFOL = TRUE IF USER SUPPLIES ANNUAL DEFOLIATION RATES
C   LREGO  - TRUE IF A REGIONAL OUTBREAK IS ACTIVE, FALSE IF NOT
C   IYRCUR = CURRENT YEAR
C   IYRST - YEAR THAT NEXT REGIONAL OUTBREAK STARTS  [BWECM2]
C   IYREND - YEAR THAT NEXT REGIONAL OUTBREAK ENDS [BWECM2]
C   IYRECV - YEAR THAT TREES WILL BE COMPLETELY RECOVERED [BWECM2]
C                 FROM NEXT OUTBREAK
C
C Revision History:
C   18-MAY-00 Lance David (FHTET)
C      .Added debug handling.
C      .Added variable IACTC for activity codes due to problems 
C       experienced with passing literals using LF95 compiler.
C   30-AUG-2006 Lance R. David (FHTET)
C      Changed array orientation of IEVENT from (4,250) to (250,4) for
C      efficiency purposes of the PPE processes in bwepppt and bweppgt.
C   06-OCT-2006 Lance R. David
C      Corrected incompatible literal parameter in OPNEW call to PRMS.
C   02-JUN-2009 Lance R. David (FMSC)
C      Added Stand ID and comma delimiter to output tables, some header
C      and column labels modified.
C   14-JUL-2010 Lance R. David (FMSC)
C      Added IMPLICIT NONE and declared variables as needed.
C
C----------------------------------------------------------------------
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'OPCOM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'BWESTD.F77'
      INCLUDE 'BWECOM.F77'
      INCLUDE 'BWECM2.F77'
      INCLUDE 'BWEBOX.F77'

      INTEGER I, IACTC, IC, ICROWN, IFAGE, IHOST, IR, ISZI, IT,
     &        KEND, KODE

      REAL TOTP(6,3),TOTR(6,3),AVPRBO(6,3,2),PRMS(1)

      LOGICAL LBWEGO
      LOGICAL DEBUG

C
C.... Check for DEBUG
C
      CALL DBCHK(DEBUG,'BWEDR',5,ICYC)

      IF (DEBUG) WRITE (JOSTND,*) 'ENTER BWEDR: NPLT,ICYC=',NPLT(1:8),
     &   ICYC
      IF (DEBUG) WRITE (JOSTND,*)
     & ' LCALBW=',LCALBW,' LDEFOL =',LDEFOL,' LREGO  =',LREGO,
     & ' IYRCUR =',IYRCUR,' IYRST =',IYRST,' IYREND =',IYREND,
     & ' IYRECV =',IYRECV

C
C
C     ************************ EXECUTION BEGINS ************************
C
C     WRITE A MSG INDICATING WHICH STAND IS BEING PROCESSED.  DON'T
C     COUNT LBWDAM (DAMAGE) AS NEEDING A HEADING.
C
c      WRITE (JOWSBW,117) IYRCUR,ISTAND,MGMID
c  117 FORMAT (/,'PROCESSING:  YEAR=',I5,'; STAND NO=',I3,
c     >         '; MANAGEMENT ID= ',A4)
C
C     ZERO OUT THE TOTAL DEFOLIATION ARRAYS.
C
      DO 130 IR=1,6
        DO 130 IC=1,3
          TOTP(IR,IC)=0.0
          TOTR(IR,IC)=0.0
          CDEF(IR,IC)=0.0
        DO 130 IT=1,2
          AVPRBO(IR,IC,IT)=0.0
  130 CONTINUE
C
C     SUM THE TOTAL FOLIAGE BY TREE SIZE.
C
C.    IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: IFHOST= ',IFHOST
C.    IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: FOLPOT= ',FOLPOT
C.    IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: FOLADJ= ',FOLADJ
C.    IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: FREM = ',FREM
      DO 160 IHOST=1,NHOSTS
        IF (IFHOST(IHOST).EQ.0) GO TO 160
        DO 150 ICROWN=1,NCROWN
          ISZI=(ICROWN+2)/3
          DO 140 IFAGE=1,3
            TOTP(IHOST,ISZI)=TOTP(IHOST,ISZI)
     >                      +(FOLPOT(IHOST,ICROWN,IFAGE)*.6)
     >                      +(FOLADJ(IHOST,ICROWN,IFAGE)*.4)
  140     CONTINUE
          TOTP(IHOST,ISZI)=TOTP(IHOST,ISZI)+FREM(ICROWN,IHOST)
  150   CONTINUE
  160 CONTINUE

C.    IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: TOTP= ',TOTP
C
C     IF CALLING THE BUDWORM MODEL, BRANCH TO CALL.
C
      IF (LDEFOL) THEN
C
C        MANUAL DEFOLIATION IS BEING APPLIED.
C
         IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: CALL BWEDEF'
         CALL BWEDEF
C
      ELSEIF (LREGO) THEN
C
C        CHECK TO SEE IF THE OUTBREAK SCHEDULED FOR THIS CYCLE
C        HAS STARTED YET
C
         KEND=0
         IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: KRECVR= ',KRECVR,
     &              ' IYRCUR, IYRST, IYREND= ',IYRCUR,IYRST,IYREND

C        IF (KRECVR.EQ.0) KEND=0
C
c        IF (IYRCUR.LT.IYRST) THEN
c	      GO TO 100
C
C           CHECK TO SEE IF THE CURRENT OUTBREAK HAS ENDED.
C
         IF (IYRCUR.GT.IYREND .AND. IYREND.GT.0) THEN
            KEND=1
            LCALBW=.FALSE.
C
C           CHECK TO SEE IF POPULATIONS HAVE CRASHED (NON-SCHEDULED ENDINGS)
C
         ELSEIF (IYREND.EQ.-1 .AND. LOWYRS.GE.3 .AND. IOBDUR.GT.5
     &      .AND. IYRCUR.GE.IYRST) THEN
            KEND=1
            IYREND=IYRCUR-1
            LCALBW=.FALSE.
C
C           CALL THE BUDWORM DEFOLIATION (BUD-LITE) MODEL.
C
         ELSEIF (IYRCUR.GE.IYRST) THEN 
             IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: CALL BWELIT'
             CALL BWELIT
             IOBDUR=IOBDUR+1
         ENDIF
C
C        IF THE OUTBREAK HAS ENDED, SCHEDULE THE NEXT ONE
C
         IF (KEND.EQ.1) THEN
            IF (LP5) WRITE (JOBWP5,180)
  180       FORMAT (4X,6('----------'))
            NEVENT=NEVENT+1
            IF (NEVENT.GT.250) THEN
              WRITE (JOBWP5,8250)
 8250         FORMAT ('********   ERROR - WSBW: MORE THAN 250 ENTRIES!')
              LP5 = .FALSE.
            ELSE
               IEVENT(NEVENT,1)=IYREND
               IEVENT(NEVENT,2)=7
               IEVENT(NEVENT,3)=0
               IEVENT(NEVENT,4)=2
            ENDIF
            IF (LP6) WRITE (JOBWP6,8600) NPLT,IYRCUR,IYRCUR
 8600       FORMAT (A26,', ',I5,',',7X,'0,',7X,'0,',3(5X,'   ,'),
     &              1X,I5,',',6(7X,'0,'))

            IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: CALL BWEOB'
            CALL BWEOB

            IF (IYRST.LT.(IY(ICYC)+BWFINT)) THEN
               LBWEGO=.TRUE.
               LREGO=.TRUE.
               LCALBW=.TRUE.
            ENDIF
            KEND=2
         ENDIF
C
C        IF THE RECOVERY PERIOD IS OVER, RESET THE REGIONAL OB FLAG
C        AND THE COUNTER FOR RECOVERY YEARS (KRECVR)
C
         IF (KEND.EQ.2) THEN
            KRECVR=KRECVR+1
            IF (KRECVR.GT.IYRSRC) THEN
               LREGO=.FALSE.
               IF (IYRCUR.GE.IYRST) LREGO=.TRUE.
               IF (IYRST.LT.(IY(ICYC)+BWFINT)) LREGO=.TRUE.
               KRECVR=0
               KEND=0
            ENDIF
         ENDIF
C
      ENDIF
C
C     AGE THE FOLIAGE ONE YEAR.
C
  100 CONTINUE
      IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: CALL BWEAGE'
      CALL BWEAGE (TOTR)
C
C     COMPUTE ONE YEARS WORTH OF DAMAGE.
C
      IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: CALL BWEDAM'
      CALL BWEDAM (TOTP,TOTR,AVPRBO)
C
C IF MANUAL DEFOLIATION, THEN RETURN. IF BUDLITE IS ACTIVE,
C TELL THE ACTIVITY SUMMARY WHAT'S HAPPENED
C
	IF (LDEFOL) GO TO 9000                                        ! RETURN
C
      IF (LCALBW.AND.IYRCUR.GE.IYRST) THEN
         IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: OPNEW 2150 YR=',IYRCUR
         I=IMGL
C                                           Changed activity codes (RNH jan99)
         IACTC = 2150
         CALL OPNEW (KODE,IYRCUR,IACTC,0,PRMS)
         IACT(I,4)=IYRCUR
      ELSEIF (KRECVR.NE.0) THEN
         IF (DEBUG) WRITE (JOSTND,*) 'IN BWEDR: OPNEW 2154 YR=',IYRCUR
         I=IMGL
C        IACTC = 2154
C        CALL OPNEW (KODE,IYRCUR,IACTC,0,0)
         IACT(I,4)=IYRCUR
      ENDIF
C
 9000 CONTINUE
      
      IF (DEBUG) WRITE (JOSTND,*) 'EXIT BWEDR: ICYC= ',ICYC
      RETURN
      END
