      SUBROUTINE BWEGO(LBWEGO)
      IMPLICIT NONE
C----------
C  **BWEGO                  DATE OF LAST REVISION:  06/17/13
C----------
C
C     DETERMINE IF BUDWORM MODEL IS TO BE CALLED THIS CYCLE.
C
C     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
C     N.L. CROOKSTON--FORESTRY SCIENCES LAB., MOSCOW, ID.--MAY 1983.
c
c     minor changes by K.Sheehan 7/96 to remove LBWDEB
c     major changes by K.Sheehan 8/96+ to add Defoliation Model
C
C     CALLED FROM :
C
C       GRINCR - 
C
C     SUBROUTINES CALLED :
C
C       OPFIND - FIND OPTION IN OPTION LIST.
C       EVUST4 - UNSET EVENT MONITOR VARIABLES.
C       BWEOB  - SCHEDULE THE NEXT BUDWORM OUTBREAK
C
C     PARAMETERS :
C
C   BWFINT = NUMBER OF YEARS IN THE BUDWORM PORTION OF A CYCLE
C   ICYC -- INDEX TO CURRENT CYCLE.  VALUE SET IN **MAIN**.
C   IY -- IY(1)=INVENTORY DATE, IY(2)=ENDPOINT OF FIRST CYCLE,
C          IY(3)=ENDPOINT OF SECOND CYCLE,...,IY(MAXCY1)=ENDPOINT
C           OF FORTIETH CYCLE.  KEYWORD CONTROLLED.
C   IYREND - YEAR THAT NEXT REGIONAL OUTBREAK ENDS [BWECM2]
C   IYRECV - YEAR THAT TREES WILL BE COMPLETELY RECOVERED [BWECM2]
C                 FROM NEXT OUTBREAK
C   IYRSRC - NUMBER OF YEARS AFTER END OF OUTBREAK THAT TREES [BWECM2]
C                 WILL BE COMPLETELY RECOVERED
C   IYRST - YEAR THAT NEXT REGIONAL OUTBREAK STARTS  [BWECM2]
C   LBUDL  - FLAG SET TO TRUE IF A BW OUTBREAK IS (WILL BE) ACTIVE [BWECM2]
C   LREGO  - TRUE IF A REGIONAL OUTBREAK IS ACTIVE, FALSE IF NOT
C   LBWEGO  - RETURNED AS TRUE IF THE BUDWORM MODEL IS TO BE CALLED
C   LCALBW = TRUE IF BUDLITE MODEL IS CALLED
C   LDEFOL = TRUE IF USER SUPPLIES ANNUAL DEFOLIATION RATES
C              IN THIS CYCLE...FALSE IF NOT.
C
C  REVISION HISTORY
C    27-APR-2000 Lance David (FHTET)
C       Minor restructuring of IF blocks and removed extra RETURN
C       statements. Removed code that had been commented-out by
C       someone else, probably Bob Havis or Kathy.
C       Added FVS PLOT.F77 common file for FINT (cycle length)
C       variable that I removed from the subroutine argument list
C       because it was not in the call statement in FVS GRINCR.F
C       routine.
C   30-AUG-2006 Lance R. David (FHTET)
C      Changed array orientation of IEVENT from (4,250) to (250,4) for
C      efficiency purposes of the PPE processes in bwepppt and bweppgt.
C   02-JUN-2009 Lance R. David (FMSC)
C      Added Stand ID and comma delimiter to output tables, some header
C      and column labels modified.
C   14-JUL-2010 Lance R. David (FMSC)
C      Added IMPLICIT NONE and declared variables as needed.
C
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'BWESTD.F77'
      INCLUDE 'BWECOM.F77'
      INCLUDE 'BWECM2.F77'
      INCLUDE 'BWEBOX.F77'
C
COMMONS
C
      INTEGER I, MYACT(4)
      LOGICAL LBWEGO

      DATA MYACT/2150,2151,2152,2153/

C     CHECK FOR MANUAL DEFOL.  ACTIVITIES
C
      LBWEGO=.FALSE.
      CALL OPFIND (1,MYACT(2),I)
      LDEFOL=I.GT.0

C     THIS SECTION DEALS WITH USER-SUPPLIED DEFOLIATION (DEFOL KEYWORD)
C
      IF (LDEFOL) THEN
        LCALBW=.FALSE.
        LBWEGO=.TRUE.
        CALL EVUST4 (5)

C       THIS SECTION DEALS WITH THE DEFOLIATION MODEL
C
      ELSEIF (LCALBW.OR.LBUDL) THEN

C       CHECK THE REGIONAL OUTBREAK FLAG (LREGO).  IF TRUE, THEN
C       CONTINUE THE ONGOING OUTBREAK (WILL CHECK FOR END OF OUTBREAK
C       AT START OF EACH YEAR IN BWEDR).
C       OTHERWISE, CHECK TO SEE IF A NEW REG. OUTBREAK WILL START DURING
C       THIS CYCLE.  IYRST (YR THAT NEXT OUTBREAK STARTS) & IYREND (YR 
C       THAT NEXT OUTBREAK ENDS) WERE SET WHEN THE SIMULATION WAS 
C       INITIALIZED OR WHEN THE MOST RECENT OUTBREAK ENDED.                            
C       IF IYREND=-1, THEN BUDLITE ENDS THE OUTBREAK WHEN DEFOLIATION
C       FALLS BELOW A THRESHOLD.
C       IF IYREND=-2, THEN ONCE AN OUTBREAK STARTS, BUDLITE CONTINUES 
C       SIMULATING BW POPULATIONS UNTIL THE END OF THE SIMULATION.

        LCALBW=.FALSE.
        IF (LREGO) THEN 
           LBWEGO=.TRUE.

           IF (IYREND.NE.-1) THEN
             IF (IY(ICYC).LE.IYREND) THEN
               LCALBW=.TRUE.
             ELSE 
               LCALBW=.FALSE.
             ENDIF
           ELSEIF (IYREND.EQ.-2) THEN
             LCALBW=.TRUE.
           ELSE
             IF (LOWYRS.LT.3.OR.IOBDUR.LT.5) THEN
               LCALBW=.TRUE.
             ELSE
               LCALBW=.FALSE.
               IYREND=IY(ICYC)-1
               NEVENT=NEVENT+1

               IF (NEVENT.GT.250) THEN
                 WRITE (JOBWP4,8250)
 8250            FORMAT ('********   ERROR - WSBW: ',
     &                   'MORE THAN 250 ENTRIES!')
                 LP4 = .FALSE.
               ELSE
                 IEVENT(NEVENT,1)=IYREND
                 IEVENT(NEVENT,2)=7
                 IEVENT(NEVENT,3)=0
                 IEVENT(NEVENT,4)=2
                 IF (LP6) WRITE (JOBWP6,8600) NPLT,IY(ICYC),IY(ICYC)
 8600            FORMAT (A26,', ',I5,',',7X,'0,',7X,'0,',3(5X,'   ,'),
     &                   1X,I5,',',6(7X,'0,'))
               ENDIF
             ENDIF
           ENDIF

C       THIS OUTBREAK HAS ENDED, CALC. TIMING OF NEXT OUTBREAK
C
        ELSEIF (IYRST.LT.(IY(ICYC)+FINT).AND.IYRST.NE.0) THEN
          LBWEGO=.TRUE.
          LREGO=.TRUE.
          LCALBW=.TRUE.
        ELSE
          LBWEGO=.TRUE.
        ENDIF
      ENDIF                  
C
C IF NEITHER LDEFOL OR LCALBW IS TRUE, THEN SOMETHING'S AMISS...
C

      RETURN
      END
