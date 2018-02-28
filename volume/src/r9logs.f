C----------
C VOLUME $Id: r9logs.f 0000 2018-02-14 00:00:00Z gary.dixon24@gmail.com $
C----------
C  subroutine for calculating region log volumes
C
C  Created TDH 11/20/09 
C
C  Revised TDH 07/26/10
C  Added a check to makes sure pulp height > 0 and > sawht
C  was causing problems in the loglen variable and subsequent
C  boardfoot calcs
C
C  Revised TDH 03/03/2011 
C  Made changes to how R9LOGLEN was calculating top piece(LEFTOV)
C  Made it so you get volume for a leftover piece of secondary
C  change made on line 115
C
C  Revised YW 12/08/2011
C  Added check number of logs not greater than 20 in r9logs and write round
C  DBH to logdia(1,1).
C
C  Revised YW 08/21/2012
C  Added ERRFLG to R9LOGS, R9LOGDIB and R9LOGLEN subroutines.
C_______________________________________________________________________
C
      SUBROUTINE R9LOGS(SAWHT, PLPHT, STUMP, MINLEN, MAXLEN, TRIM,
     &           LOGLEN, LOGDIA, NOLOGP, NOLOGS, TLOGS, COEFFS, ERRFLG)
C_______________________________________________________________________
C

      USE DEBUG_MOD
      USE CLKCOEF_MOD
      
      IMPLICIT NONE

!**********************************************************************
!...  Parameters
      REAL    SAWHT, PLPHT, STUMP, MINLEN, MAXLEN, TRIM 
      REAL    LOGLEN(20), LOGDIA(21,3)
      INTEGER NOLOGP, NOLOGS, TLOGS, NUMSEG, ERRFLG
      TYPE(CLKCOEF):: COEFFS
      
!...  Local Variables
      INTEGER ILOG, JLOG, I
      REAL    LMERCH,LEFTOV
      ERRFLG = 0
      
!======================================================================
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 100) ' -->Enter R9LOGS'
100      FORMAT (A)
         WRITE  (LUDBG, 125)' SAWHT  PLPHT MINLEN MAXLEN TRIM'
125      FORMAT (A)
         WRITE  (LUDBG, 150)SAWHT, PLPHT, MINLEN, MAXLEN, TRIM
150      FORMAT (5F6.1)      
      END IF
C     Check number of logs
      IF (SAWHT .GT. 0) THEN
        LMERCH = SAWHT-STUMP
        NOLOGP = INT(LMERCH/(MAXLEN+TRIM))
        NUMSEG = NOLOGP
      ELSE
        LMERCH = PLPHT-STUMP
        NOLOGS = INT(LMERCH/(MAXLEN+TRIM))
        NUMSEG = NOLOGS
      ENDIF
      IF (NUMSEG .GT. 20) THEN
        ERRFLG = 12
        RETURN
      ENDIF
!------ Sawtimber segmentation----------------------------------------
      IF (SAWHT .GT. 0) THEN

!...saw portion

        LMERCH = SAWHT-STUMP     

        NOLOGP = INT(LMERCH/(MAXLEN+TRIM))

        LEFTOV=LMERCH-((MAXLEN+TRIM)*FLOAT(NOLOGP))-TRIM

!check for saw logs
        IF(.NOT.(LMERCH.LT. (MINLEN+TRIM) .OR. NOLOGP.LE.0 
     &    .OR. (NOLOGP .EQ.0 .AND. LEFTOV.LT.(MINLEN+TRIM)))) THEN 
          
            ILOG = 1
            JLOG = NOLOGP
            CALL R9LOGLEN(ILOG, JLOG, NOLOGP, MINLEN, MAXLEN, TRIM,
     &                LOGLEN, LEFTOV, ERRFLG)
            IF (ERRFLG .NE. 0) RETURN
            IF (DEBUG%MODEL) THEN
               WRITE  (LUDBG, 200)'LMERCH  NOLOGP LLEN(1) LLEN(TOP)'
     &                             //' LEFTOV'
200            FORMAT (A)
               WRITE  (LUDBG, 220)LMERCH, NOLOGP, LOGLEN(1), 
     &                 LOGLEN(NOLOGP), LEFTOV
220            FORMAT (F6.1,4X I2, 3F7.1)      
            END IF
        
!...TOP WOOD PORTION
!first check to see if there's top wood
        !make sure there's a plpht value and it > sawht
        ELSE IF (PLPHT > 0 .AND. PLPHT .GT. SAWHT) THEN
          LMERCH = PLPHT - SAWHT
          NOLOGS = INT(LMERCH/(MAXLEN+TRIM))
         
         !do we need a check for nologs >0???  
         IF(LMERCH.LT. (MINLEN+TRIM) .OR. NOLOGS.LE.0 
     &    .OR. (NOLOGS.EQ.0 .AND. LEFTOV.LT.(MINLEN+TRIM)) ) THEN
          RETURN
         ENDIF   
           
          LEFTOV=LMERCH-((MAXLEN+TRIM)*FLOAT(NOLOGS))-TRIM
          ILOG = NOLOGP + 1
          JLOG = ILOG + NOLOGS - 1
          IF (JLOG .GT. 20) THEN
            ERRFLG = 12
            RETURN
          ENDIF
          CALL R9LOGLEN(ILOG, JLOG, NOLOGS, MINLEN, MAXLEN, TRIM,
     &                 LOGLEN, LEFTOV, ERRFLG)
          IF(ERRFLG .NE. 0) RETURN
          IF (DEBUG%MODEL) THEN
           WRITE  (LUDBG, 300)'LMERCH  NOLOGS ilog  jlog'
     &                    //' LEFTOV'
300        FORMAT (A)
           WRITE  (LUDBG, 320)LMERCH, NOLOGS, ilog, 
     &                      jlog,LEFTOV
320        FORMAT (F6.1, 4X, I2, 2I4, 2X, F7.1)      
          END IF
        END IF
     
!---------------------------------------------------------------------    
!------ non-saw timber segmentation----------------------------------

      ELSE !...sawht <= 0
 
        LMERCH = PLPHT-STUMP
        NOLOGP = 0
        NOLOGS = INT(LMERCH/(MAXLEN+TRIM))
 
        LEFTOV=LMERCH-((MAXLEN+TRIM)*FLOAT(NOLOGS))-TRIM
        
        !IF(LEFTOV .GT. (MINLEN+TRIM) .AND. NOLOGS .EQ. 0) NOLOGS = 1
        
        IF(LMERCH.LT. (MINLEN+TRIM) !.OR. NOLOGS.LE.0 
     &    .OR. (NOLOGS.EQ.0 .AND. LEFTOV.LT.(MINLEN+TRIM)) ) THEN
          RETURN
        ENDIF
      
        ILOG = 1
        JLOG = NOLOGS     
        CALL R9LOGLEN (ILOG, JLOG, NOLOGS, MINLEN, MAXLEN, TRIM,
     &                 LOGLEN, LEFTOV, ERRFLG)
        IF(ERRFLG .NE. 0) RETURN
      ENDIF
      
      TLOGS = INT(NOLOGP + NOLOGS)
      
!---------------------------------------------------------------------      

      CALL R9LOGDIB(TLOGS, TRIM, STUMP, LOGLEN, LOGDIA, COEFFS)

!...      WRITE OUT ALL LOGLEN/LOGDIA TO DEBUG FILE
      IF (DEBUG%MODEL) THEN
         DO 650 I=1,TLOGS
         WRITE  (LUDBG, 600)'LOGDIA ', I, LOGDIA(I,1),'LOGLEN ', I,
     &     LOGLEN(I)
  600    FORMAT (A, I2, F6.1, 2X, A,I2, F6.1)
  650    CONTINUE
      END IF

      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 1000) ' <--EXIT R9LOGS'
1000     FORMAT (A)
      ENDIF
      
      END SUBROUTINE R9LOGS
!********************************************************************

!********************************************************************
C  subroutine for calculating region 9 log lenghts

C Created TDH 11/20/09 

C  Revised TDH 12/21/09
C  Fixed bug that was incorrectly calculating num segs.
C
C_______________________________________________________________________
C
      SUBROUTINE R9LOGLEN(ILOG, JLOG, NUMSEG, MINLEN, MAXLEN, TRIM, 
     &           LOGLEN, LEFTOV,ERRFLG)
C_______________________________________________________________________
C
      USE DEBUG_MOD
      
      IMPLICIT NONE

!**********************************************************************
!...  Parameters
      INTEGER ILOG, JLOG, NUMSEG, ERRFLG
      REAL    MINLEN, MAXLEN, TRIM 
      REAL    LOGLEN(20), LEFTOV
      
!...  Local variables
      INTEGER I
      REAL    HT
      
!======================================================================    

      HT = 0.
        
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 100) ' -->Enter R9LOGLEN'
100      FORMAT (A)
      ENDIF
      
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 120)'ILOG  JLOG numseg MAXLEN TRIM  LEFTOV'
120      FORMAT (A)
         WRITE  (LUDBG, 130)ILOG, JLOG, numseg, MAXLEN,TRIM,LEFTOV
130      FORMAT (3I5,F6.1, F7.1, 2X, F7.1)      
      END IF

      IF(JLOG .GT. 0) THEN
        IF(JLOG .GT. 20) THEN
          ERRFLG = 12
          RETURN
        ENDIF
        DO 200 I=ILOG,JLOG        
          LOGLEN(I)=MAXLEN
          ht=ht+trim+logLen(i)
200     CONTINUE
      ENDIF
      IF (DEBUG%MODEL) THEN
        WRITE (LUDBG, 600)'HT ', HT
600     FORMAT (A,F7.1)
      ENDIF
      
      IF(LEFTOV.GE.(MINLEN+TRIM)) THEN
        NUMSEG=NUMSEG+1
        JLOG = JLOG+1    
        IF(JLOG .GT. 20) THEN
          ERRFLG = 12
          RETURN
        ENDIF
        LOGLEN(JLOG)=LEFTOV !OR IS IT MINLEN????
      ENDIF

!..   SAWLOG TREES MAY GET TOP TWO LOGS RESEGMENTED (WHOLE FEET)
      IF(NUMSEG.EQ.1) THEN
        LOGLEN(ILOG)=INT(LOGLEN(ILOG))
      ELSEIF(LEFTOV.LT.MINLEN) THEN
        LOGLEN(JLOG)=INT(LOGLEN(JLOG))
      ELSE
        LOGLEN(JLOG)=INT((MAXLEN+LEFTOV)/2)
        LOGLEN(JLOG-1)=INT(MAXLEN+LEFTOV-LOGLEN(JLOG))
      ENDIF
      
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 1000) ' <--Exit R9LOGLEN'
1000     FORMAT (A)
      ENDIF
      
      END SUBROUTINE R9LOGLEN
      
!********************************************************************

!********************************************************************
C  subroutine for calculating r9 log dib's
C
C  Created TDH 11/20/09 
C
C  Revised ... ../../..
C
C_______________________________________________________________________
C
      SUBROUTINE R9LOGDIB(NUMSEG, TRIM, STUMP, LOGLEN, LOGDIA, 
     &                    COEFFS)
C_______________________________________________________________________
C
      USE CLKCOEF_MOD
      
!**********************************************************************
      IMPLICIT NONE
      
!...  Parameters
      INTEGER NUMSEG, I
      REAL    TRIM, STUMP, LOGLEN(20), LOGDIA(21,3)
      REAL    DIB
      TYPE(CLKCOEF):: COEFFS
      
      
!...  Local variables
      REAL    HT
      
!======================================================================           
C-----Get diameters at log ends
      IF(NUMSEG.GT.0) THEN
c--     Get DIB at 4.5'
        HT=4.5
        CALL R9DIB(DIB,HT,COEFFS)
        LOGDIA(1,2)= DIB
        LOGDIA(1,1)=INT(DIB+0.499)
!        LOGDIA(1,3)= 0.0
        
c--     Get DIB at all log ends
        HT=STUMP
        DO 850 I=1,NUMSEG
          HT=HT+TRIM+LOGLEN(I)
          
!------------------------------------------------------      
!..       don't need below for logs methinks
c--       Never let height get above sawtimber height
!...         if(ht.gt.sawHt) then
!...         logLen(i)=logLen(i)-(ht-sawHt)
!...         ht=sawHt
!...          endif
!------------------------------------------------------         

          CALL R9DIB(DIB,HT,COEFFS)
          LOGDIA(I+1,2)= DIB    
          LOGDIA(I+1,1)=INT(DIB+0.499)
!...          LOGDIA(I+1,3)= 0.0
          
850     CONTINUE
      ENDIF
      
      END SUBROUTINE R9LOGDIB
     
!********************************************************************


!********************************************************************
C  subroutine for calculating weighted r9 log volumes
C
C  Created TDH 11/24/09 
C
C  Revised ... ../../..
C
C_______________________________________________________________________
C
      SUBROUTINE R9LGCFT(TLOGS, LOGLEN, LOGDIA, LOGVOL,TLOGVOL,TCFVOL)
C_______________________________________________________________________
C

      USE DEBUG_MOD

      IMPLICIT NONE
!**********************************************************************
!..   Parameters
      INTEGER TLOGS
      REAL    LOGLEN(20), LOGDIA(21,3), LOGVOL(7,20), TLOGVOL, TCFVOL

!...  Local variables      
      INTEGER I
!======================================================================      
       
      TLOGVOL = 0.0
     
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 100) ' -->Enter R9LOGCUFT'
100      FORMAT (A)
         WRITE  (LUDBG, 125)' TLOGS TLOGVOL TCFVOL'
125      FORMAT (A)
         WRITE  (LUDBG, 150)TLOGS, TLOGVOL, TCFVOL
150      FORMAT (I4, 2F8.1)      
      END IF
   
      DO 200 I=1,TLOGS
        LOGVOL(4,I) = 0.00272708*(LOGDIA(I,2)**2 + LOGDIA(I+1,2)**2)
     &                * LOGLEN(I)

      IF (DEBUG%MODEL) THEN        
        WRITE  (LUDBG, 175)'LOGDIA I', LOGDIA(I,2),' LOGDIA I+1',
     &     LOGDIA(I+1,2)
175     FORMAT (A,F6.1,A,F6.1)  
      ENDIF
        
        TLOGVOL = TLOGVOL + LOGVOL(4,I)
200   CONTINUE

!..   Calculate percentage of total volume in each log and scale back
!..   to total integrated cubic volume so they match
      DO 250 I=1,TLOGS
        LOGVOL(4,I) = (LOGVOL(4,I)/TLOGVOL) * TCFVOL
250   CONTINUE

      TLOGVOL = 0.0
!..   Now sum the "weighted" log volumes to get total log volume
      DO 350 I=1,TLOGS
        TLOGVOL = TLOGVOL + LOGVOL(4,I)
350   CONTINUE
     
      IF (DEBUG%MODEL) THEN         
         WRITE  (LUDBG, 425)' TLOGS TLOGVOL TCFVOL'
425      FORMAT (A)
         WRITE  (LUDBG, 450)TLOGS, TLOGVOL, TCFVOL
450      FORMAT (I4, 2F8.1)
         WRITE  (LUDBG, 500) ' -->Exit R9LOGCUFT'
500      FORMAT (A)      
      END IF


      END SUBROUTINE R9LGCFT