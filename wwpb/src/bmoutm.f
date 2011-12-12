      SUBROUTINE BMOUTM (I,IYEAR)
C----------
C  **BMOUTM  DATE OF LAST REVISION:  JUNE 28, 2005, AJM
C                                    Aug 1999, AJM
C                                    AUG 13, 1994
C----------                          July 1998
C                                    August 1998
C     Two output files were created (units 28 and 29) to output
C     annual stand and landscape data (RN Havis July 1998)
C
C     Westwide Pine Beetle model; summary of stand state *after*
C     beetles have dispersed and *after* beetle induced
C     mortality has occurred and *before* trees and basal area have
C     been removed from the tree lists.
c
c     This routine also calculates and prints the averages needed for
c     the detailed beetle model output
C     A TABLE OF RESULTS IS OUTPUT AT THE CONCLUSION OF EACH CYCLE.
C
C     CALLED FROM -- BMDRV
C
C     VREM(A,B,C) - a temporary storage array to hold removal volumes
C                   for Sanitation and Salvage output to BMSPY.OUT
C                   A - stand, B- Sanitation(1) or Salvage(2)
C                   C - stand removal has (1) or has not (0) been
C                       printed for the simulation
C     If a particular stand is Sanitized or Salvaged
C     more than once in a cycle, the second action will not be output
C     to the BMSPY output file
C
C     PBKILL - number of trees killed by beetles by size class
C
C     SSBATK changed 8/99.  It is now dimensioned and added to common block.
C     It is dimensioned (MXSTAND) (i.e. now an array containing one value
C     per stand).  These values will be used in a new calculation of the
C     numerator of the attractiveness equation (subroutine BMCNUM).
C     The BA killed this year will be added back in next year.  AJM
C
C     CORRECTED THE SETTING OF LSBAK1, (JUST BELOW  310 CONTINUE) AJM 6/05
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77' 
      INCLUDE 'PPCNTL.F77'
      
      INCLUDE 'BMPRM.F77'
      INCLUDE 'BMCOM.F77'
      INCLUDE 'BMPCOM.F77'
C
C     Added real array LBAKL1, LBAH1, LVREM1 to store annual landscape
C     summaries (RNH July98)
C
      REAL LBAKL1(NSCL+1), LBAH1(NSCL+1), LVREM1(2), 
     1     RBAKT(MXSTND,NSCL+1), VTEMP(MXSTND,2,2)
C
      REAL LBKP1, LRVSTD1, LBAT1
      REAL LPROPH1, LDDWP1, LSDWP1
      REAL LTKILL1, LVKILL1
      REAL LSREM1 ,LSBAK1, LTBAK1
      REAL LBKPO1, LBKPI1, LBKPS1
C
      INTEGER RYEAR, RYEAR2, RYEARP
      
C      IF (.NOT. LBMDET .AND. .NOT. LBMVOL) RETURN
C
C     Test for print flags for annual stand or annual landscape output
C     (RNH July98)
C
      IF (.NOT. LBMDET .AND. .NOT. LBMVOL 
     1    .AND. .NOT. LBMSPY .AND. .NOT. LBMLPY) RETURN
      
c     RYEAR triggers the reporting. It is the last year of a cycle.
C     The average values are reported for the first year of the 
C     cycle just finished. e.g: for and outbreak from 1993-1994
C     during the 1990-1994 period, the reporting would be for 1990
C     and would be the average of 1993 and 1994.

      RYEAR = MIY(MICYC) - 1
      RYEAR2= MIY(MICYC - 1)
C
C     New local variable RYEARP to identify output table values as
C     end of cycle values (RNH June98)
C
      RYEARP= MIY(MICYC)
C
C     +++++++++
C      
C     Set the Volume removed print flags to false(0) 
C     for the first year of cycle, and inititalize VTEMP
C     If first year of cycle and inititialization of VTEMP
C     has not been carried out then dothe following
C
      IF((IYEAR .EQ. MIY(MICYC-1)) .AND. (INITV .GE. 1)) THEN
      GO TO 5
      ELSE
      INITV= 0
      ENDIF
C
      IF ((IYEAR .EQ. MIY(MICYC-1)) .AND. (INITV .LE. 0)) THEN
      DO 4 IV1= 1, MXSTND
      VTEMP(IV1,1,2)= 0.
      VTEMP(IV1,2,2)= 0.
      VTEMP(IV1,1,1)= 0.
      VTEMP(IV1,2,1)= 0.
    4 CONTINUE
      INITV= 1
      ENDIF
C
    5 CONTINUE
C
C     Sanitation and Salvage removals are reported to output BMSPY file
C     RNH (Aug98). Load the VREMOV array each year in the following
C
C     Copy VOLREM array into a temporary array for current year
C
      IF(IYEAR .NE. IYEARV) THEN
      DO 6 IV= 1, MXSTND
      VTEMP(IV,1,1)= VOLREM(IV,1)
      VTEMP(IV,2,1)= VOLREM(IV,2)
    6 CONTINUE
      IYEARV= IYEAR
      ENDIF
    
C     +++++++++++
C
c     Accumulate sums for each variable

      SRVSTD(I) = SRVSTD(I) + GRFSTD(I)
      SBAT(I) = SBAT(I) + (BANH(I,NSCL+1) + BAH(I,NSCL+1))
      SBAH(I,NSCL+1) = SBAH(I,NSCL+1) + BAH(I,NSCL+1) 
      
      SBKP(I) = SBKP(I) + BKPA(I) 
      SBKPO(I)= SBKPO(I) + BKPOUT(1,I)
      SBKPI(I)= SBKPI(I) + BKPIN(1,I)
      SBKPS(I)= SBKPS(I) + BKPS(I)

      DO 10 ISIZ= 1,NSCL 
        TOTKLL = PBKILL(I,ISIZ) + ALLKLL(I,ISIZ)
        STKILL(I)= STKILL(I) + TOTKLL
        SVKILL(I)= SVKILL(I) + TOTKLL * TVOL(I,ISIZ,1)
C
C     For annual output (RNH July98), calculate total volume killed
C
        SVKILL1= SVKILL1 + TOTKLL*TVOL(I,ISIZ,1)
C
        IF (TREE(I,ISIZ,1) .GT. 1E-6) THEN
           PBAREM = TOTKLL / TREE(I,ISIZ,1)
        ELSE
           PBAREM = 0.0
        ENDIF
        SBAKL(I,ISIZ)= SBAKL(I,ISIZ) + BAH(I,ISIZ) * PBAREM
        SBAKL(I,NSCL+1)= SBAKL(I,NSCL+1) + BAH(I,ISIZ) * PBAREM
        SBAH(I,ISIZ) = SBAH(I,ISIZ) + BAH(I,ISIZ) 
   10 CONTINUE
   
      DO 300 J=1,(MXDWHZ+1)
         DO 320 K=1,MXDWAG
            DO 322 IPC=1,MXDWPC
               SSDWP(I) = SSDWP(I) + SDWP(I,IPC,J,K)
  322       CONTINUE      
            IF (J .LE. MXDWSZ) THEN
              SDDWP(I) = SDDWP(I) + DDWP(I,J,K)
            ENDIF
  320    CONTINUE
  300 CONTINUE
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     Calculate RBAKT = ratio of killed to total stand BA for each size class
C               TVOLT = total volume of trees in stand in size class
C               TVOLK = total volume of killed trees in size class
C               SBATKT= Fraction of total stand basal area killed
C               SSBAT = sum of total stand basal area
C               SSBATK(I)= sum of basal area killed, by stand
C
C Note that PBKILL was convertied to number of trees at end 
C     of BMISTD.F
C               
C     (RNH July98, Aug98)
C
      SSBAT= 0.0
      DO 312 IIKK= 1,NSCL
      SSBAT= SSBAT + BAH(I, IIKK) + BANH(I,IIKK)
  312 CONTINUE
C
      SSBATK(I) = 0.0
      STVOLT= 0.0
      STVOLK= 0.0
      DO 310 JSCL1 = 1, NSCL
C
C     The following added 10/7/99 to correct output reporting problem.
C     Currently, RBATK(I,JSCL1) gets reported as previous year's value
C     if the size class BA gets reduced to zero.
C
      RBAKT(I,JSCL1) = 0.0
C      
      IF ((BAH(I, JSCL1) + BANH(I, JSCL1)) .LE. 0.0) GO TO 310
C
C
C     Use the PBAREM parameter that ESSA uses.  Because PBKILL was
C     transformed to number of trees per acre in BMISTD
C
      TOTKLL= (PBKILL(I,JSCL1)+ALLKLL(I,JSCL1))
C
        IF (TREE(I,JSCL1,1) .GT. 1E-6) THEN
           PBAREM = TOTKLL / TREE(I,JSCL1,1)
        ELSE
           PBAREM = 0.0
        ENDIF
C      
C     Calculate percent (RNH 18 Nov98) basal area killed by size class
C
      RBAKT(I,JSCL1)= BAH(I,JSCL1)*PBAREM/SSBAT*100.
C
C     Sum basal area killed
C     SSBATK changed to a MXSTAND-dimensioned array 8/99 (AJM)
C
      SSBATK(I)= SSBATK(I) + BAH(I,JSCL1)*PBAREM
C
      TVOLT= (TVOL(I,JSCL1,1)*TREE(I,JSCL1,1)
     1       + TVOL(I,JSCL1,2)*TREE(I,JSCL1,2))
C
      STVOLT = STVOLT + TVOLT
C
      TVOLK= TVOL(I,JSCL1,1)*TOTKLL
C
      STVOLK= STVOLK + TVOLK
C
      IF (TVOLT .LE. 1.0E-6) TVOLT = 1.0E-6
C
      IF (STVOLT .GE. 1.0E-6) THEN
      TOTKT= STVOLK/STVOLT
      ELSE
      TOTKT= 0.0
      ENDIF
C     
C
  310 CONTINUE
C
C     SET landscape BA killed
C     modified 6/28/05 ajm.  this needs to happen after we calculate SBATKT!
C     comment out next line, add it to below next IF...ENDIF block.
C
C      LSBAK1= SBATKT
C
C     Change  SBATKT to percentfor SPY output (RNH 18Nov98)
C
C     Added loop to account for the rare occasion when no BA remains in 
C     stand.
      IF (SSBAT .GT. 0) THEN
      SBATKT= SSBATK(I)/SSBAT*100.
      ELSE
      SBATKT= 0
      ENDIF
C
      LSBAK1= SBATKT
C
C     Write annual individual stand output to unit number JBMSPY
C     (RNH July98)
C     WRite header for file
C
      IF (LBMSPY .AND. IPRHEDS .LE. 0) THEN
      WRITE (JBMSPY, 313)
      WRITE (JBMSPY, 314)
      WRITE (JBMSPY, 315)
C
C     BASAL AREA OF HOST BEETLE-KILLED AND BAH REMAINING ADDED (BELOW)
C     TO THE *.SPY (ANNUAL) OUTPUT.  ajm 10/6/99
C
  313 FORMAT(T25,'INDIVIDUAL STAND ANNUAL OUTPUT VARIABLES')
  314 FORMAT(T63,'% BASAL AREA OF SIZE CLASS KILLED / TOTAL STAND BASAL 
     1AREA')
C     , 23X, 'BASAL AREA OF BEETLE-KILLED HOST TREES BY SIZE CLASS',
C     2 20X, 'BASAL AREA OF HOST TREES BY SIZE CLASS')
C
  315 FORMAT(1X,'YEAR', ' STAND ID','    BKP','      RV','    BA   ',
     1'  VOLSAN ','VOLSAL','  TVOLT ','  TVOLK ','   B1/TOT ','B2/TOT ',
     1'B3/TOT ','B4/TOT ','B5/TOT ','B6/TOT ','B7/TOT ','B8/TOT ',
     2'B9/TOT',' B10/TOT',' TOT/TOT')
C     , '  0-3  ', '  3-6  ', '  6-9  ',
C     3 '  9-12 ', ' 12-15 ', ' 15-18 ', ' 18-21 ', ' 21-25 ', ' 25-30 ',
C     4 '  >30  ', 1X, '  0-3  ', '  3-6  ', '  6-9  ',
C     5 '  9-12 ', ' 12-15 ', ' 15-18 ', ' 18-21 ', ' 21-25 ', ' 25-30 ',
C     6 '  >30  ')
C
      IPRHEDS= 1
C
      ENDIF
C
C     Output volumes for current year only through checking print flags
C
      IF ((VTEMP(I,1,2) .LE. 0.) .AND. (VTEMP(I,1,1) .GT. 0.)) THEN
      VTEMP1 = VTEMP(I,1,1)
      VTEMP(I,1,2)= 1.
      ELSE
      VTEMP1= 0.0
      ENDIF
C
      IF ((VTEMP(I,2,2) .LE. 0.) .AND. (VTEMP(I,2,1) .GT. 0.)) THEN
      VTEMP2 = VTEMP(I,2,1)
      VTEMP(I,2,2)= 1.
      ELSE
      VTEMP2= 0.0
      ENDIF
C        
      IF (LBMSPY) THEN
                 WRITE (JBMSPY, 316) IYEAR, BMSTDS(I), BKPA(I),
     2           GRFSTD(I), SSBAT, VTEMP1,VTEMP2,STVOLT,STVOLK,
     3           (RBAKT(I,JSCL1), JSCL1= 1, NSCL),SBATKT
c     ,
c     4           (SBAKL(I,JNSCL), JNSCL=1, NSCL),
c     5           (BAH(I,JSCL1), JSCL1=1,NSCL)
      ENDIF
C
C
  316 FORMAT (1X, I4, 1X, A8, 1X, F7.2, 1X, F7.4, 1X, F7.2,
     1       1X, F7.2,1X,F7.2,1X,F7.2,1X,F7.2,1X,11F7.2)
c     , 1X,
c     2       10F7.2, 1X, 10F7.2)
C
C
C     Write annual landscape means to unit number JBMLPY
C
C     Count stands and accumulate summary information using method
C     shamelessly adapted from ESSA code here
C
      IF (ICNT1 .GE. BMEND) ICNT1 = 0
      ICNT1 = ICNT1 + 1
      IF (ICNT1 .EQ. 1) SACRES1 = 0.0
      CALL SPLAAR (I, ACRES1, IRC)

      IF (ACRES1 .LE. 0.) ACRES1 = 1.0

      SACRES1 = SACRES1 + ACRES1
C
C     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     Accumulate stand data to calculate landscape means
C     using adapted code from ESSA with variable names changed here
C
        LBKP1 = LBKP1 + BKPA(I) * ACRES1
        LRVSTD1 = LRVSTD1 + GRFSTD(I) * ACRES1
        LBAT1 = LBAT1 + SSBAT*ACRES1
        LPROPH1 = LPROPH1 + PROPH(I) * ACRES1
C                
        LVKILL1= LVKILL1 + SVKILL1 * ACRES1
C
C     Reinitialize SVKILL1
      SVKILL1= 0.0
C
C     Temporary storage for volumes to be zeroed each year (RNH Jul98)
C     VOLREM(I,J) is only zeroed on a cycle basis
C
        VREM1= VOLREM(I,1)
        VREM2= VOLREM(I,2)

        LTBAK1= LTBAK1 + LSBAK1*ACRES1
        LSREM1 = LSREM1 + SREMOV(I) * ACRES1
        LVREM1(1) = LVREM1(1) + VREM1 * ACRES1
        LVREM1(2) = LVREM1(2) + VREM2 * ACRES1

        LBKPO1= LBKPO1 + BKPOUT(1,I) * ACRES1
        LBKPI1= LBKPI1 + BKPIN(1,I) * ACRES1
        LBKPS1= LBKPS1 + BKPS(I) * ACRES1
C      
C     Generate output when all stands have been counted for year
C
        IF (ICNT1 .GE. BMEND) THEN
                       
          Y1 = 1.0 / SACRES1
           
          LTBAK1= LTBAK1*Y1
          LBKP1 = LBKP1 * Y1
          LRVSTD1 = LRVSTD1 * Y1
          LBAT1 = LBAT1 * Y1
          LPROPH1 = LPROPH1 * Y1
C          
          LDDWP1 = LDDWP1 * Y1
          LSDWP1 = LSDWP1 * Y1
C          
          LVKILL1= LVKILL1 * Y1
C          
          LSREM1 = LSREM1 * Y1
          LVREM1(1) = LVREM1(1) * Y1
          LVREM1(2) = LVREM1(2) * Y1
C
          LBKPO1= LBKPO1 * Y1
          LBKPI1= LBKPI1 * Y1
          LBKPS1= LBKPS1 * Y1                  
C
          DO 73 ISIZ= 1,(NSCL+1)
            LBAKL1(ISIZ)= LBAKL1(ISIZ) * Y1
            LBAH1(ISIZ)= LBAH1(ISIZ) * Y1
   73     CONTINUE
C
C     Write annual Landscape output to unit number JBMLPY
C     (RNH July98)
C
C     The variable TTVOL is the total volume of the stand from OUTCOM
C     common and calculated from OCVCUR(7) array (rnh July98)
C
      IF (LBMLPY .AND. IPRHEDL .LE. 0) THEN
C
      WRITE (JBMLPY, 301)
      WRITE (JBMLPY, 302)
  301 FORMAT(T15,'LANDSCAPE ANNUAL STAND-AREA-WEIGHTED OUTPUT VARIABLES'
     1/)
  302 FORMAT(' YEAR','      L-BKP ','    L-RV  ','  TOT-BA  ',
     1 'T-VOL-KILL',    
     1       ' T-BA-KILL','  VOL-SANI ',' VOL-SALV ')
C
      IPRHEDL= 1
      ENDIF
C*************************************************************
C   WRITE OUTPUT FILE FOR TEST MONTECARLO SIMULATIONS AJM 7/00
c      OPEN (92,FILE='VRRAN.OUT',STATUS='OLD',POSITION='APPEND')
c      WRITE (92,*) DREWTEST,IYEAR,LBKP1,LBAT1
C*************************************************************
      IF (LBMLPY) THEN
             WRITE (JBMLPY, 303) IYEAR, LBKP1, LRVSTD1, LBAT1,
     >       LVKILL1,LTBAK1,
     >       LVREM1(1), LVREM1(2)
      ENDIF
C
C     Reinitialize
C
      TTVOL= 0.0
      LBKP1 = 0.0
      LRVSTD1 = 0.0
      LBAT1 = 0.0
      LVKILL1 = 0.0
      LTBAK1= 0.0
          LBKPS1 = 0.0
          LBKPO1 = 0.0
          LBKPI1 = 0.0
          LPROPH1 = 0.0
          LDDWP1 = 0.0
          LSDWP1 = 0.0
          LSREM1 = 0.0
          LVREM1(1) = 0.0
          LVREM1(2) = 0.0
          VREM1= 0.0
          VREM2= 0.0
          IF (.NOT. LBMDVO) LRVSTD1 = 0.0
C
      ENDIF
C
C     ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
  303 FORMAT (1X, I4, 1X, 8F10.3)
     
C     2           SRVSTD(I), SBAT(I), 
C     3           (SBAKL(I,JNSCL), JNSCL= 1, NSCL+1)
C
C   
C            WRITE (JBMVOL,90) RYEARP,'LANDSCAPE',LBKP,LRVSTD,LBAT, 
C     >       LVKILL,LBAKL(NSCL+1),
C     >       LVREM(1), LVREM(2), TVOL
C   90       FORMAT (1X, I4, 1X, A9, F8.2, F6.2, F8.2, I4, 1X, 11F10.2)
C   
C
C     End of annual landscape calculations 
C     +++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C
c       Print state information averaged within the master cycle.
       
      IF (IYEAR .EQ. RYEAR) THEN
        IF (IBMMRT .LE. 0) GOTO 85
      
        SSREM(I) = SREMOV(I)
        SVREM(I,1) = VOLREM(I,1)
        SVREM(I,2) = VOLREM(I,2)

        X = 1.0 / FLOAT(IBMMRT)     
        SBKP(I) =  SBKP(I) * X
        SRVSTD(I) = SRVSTD(I) * X
        SBAT(I) = SBAT(I) * X
        IF (SBAT(I) .GT. 1E-6) THEN 
           PROPH(I) = (SBAH(I,NSCL+1) / SBAT(I)) * X
        ELSE
           PROPH(I) = 0.0
        ENDIF
                
        SDDWP(I) = SDDWP(I) * X
        SSDWP(I) = SSDWP(I) * X
                
        STKILL(I)= STKILL(I) * X
        SVKILL(I)= SVKILL(I) * X
        SBAKL(I,NSCL+1)= SBAKL(I,NSCL+1) * X
                
        SSREM(I) = SSREM(I) * X
        SVREM(I,1) = SVREM(I,1) * X
        SVREM(I,2) = SVREM(I,2) * X
            
        DO 31 ISIZ= 1,NSCL
          SBAKL(I,ISIZ)= SBAKL(I,ISIZ) * X
          SBAH(I,ISIZ)= SBAH(I,ISIZ) * X
   31   CONTINUE

        SBKPO(I)= SBKPO(I) * X
        SBKPI(I)= SBKPI(I) * X
        SBKPS(I)= SBKPS(I) * X

c       Write the stand averages to the main beetle output file
C
C     Changed RYEAR2 to RYEARP to specify end rather than beginning
C     of cycle in output table (RNH June98), 
C
        IF (LBMVOL) THEN
C          WRITE(JBMVOL,60) RYEAR2, BMSTDS(I), SBKP(I),SRVSTD(I),SBAT(I),
          WRITE(JBMVOL,60) RYEARP, BMSTDS(I), SBKP(I),SRVSTD(I),SBAT(I),
     >      INT(PROPH(I)*100),SDDWP(I),SSDWP(I),STKILL(I),SVKILL(I), 
     >      SBAKL(I,NSCL+1), SSREM(I), SVREM(I,1), SVREM(I,2)
   60     FORMAT (1X, I4, 1X, A8, 1X, F8.2, F6.2, F8.2, I4, 8(F9.2)) 
        ENDIF
        
c       Write the stand averages to the detailed beetle output file (if requested)
   
        IF (LBMDET) THEN
C         WRITE(JBMDBH,65) RYEAR2, BMSTDS(I),INT(SBKPS(I)*100),SBKP(I),
         WRITE(JBMDBH,65) RYEARP, BMSTDS(I),INT(SBKPS(I)*100),SBKP(I),
     >       SBKPI(I),SBKPO(I), (SBAKL(I,IZ),IZ=1,NSCL),
     >       (SBAH(I,IZ),IZ=1,NSCL)
   65    FORMAT (1X,I4,1X,A8,1X,I5,3(F7.2),10(F7.2),10(F7.1)) 
        ENDIF
       


C     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.   
   
        IF (ICNT .GE. BMEND) ICNT = 0      
        ICNT = ICNT + 1
      
c       Print landscape averages.

        IF (ICNT .EQ. 1) SACRES = 0.0
        
        CALL SPLAAR (I, ACRES, IRC)
        IF (ACRES .LE. 0.0) ACRES = 1.0
                
        SACRES = SACRES + ACRES
                          
        LBKP = LBKP + SBKP(I) * ACRES
        LRVSTD = LRVSTD + SRVSTD(I) * ACRES
        LBAT = LBAT + SBAT(I) * ACRES
        LPROPH = LPROPH + PROPH(I) * ACRES
                
        LDDWP = LDDWP + SDDWP(I) * ACRES
        LSDWP = LSDWP + SSDWP(I) * ACRES
                
        LTKILL= LTKILL + STKILL(I) * ACRES
        LVKILL= LVKILL + SVKILL(I) * ACRES
        LBAKL(NSCL+1)= LBAKL(NSCL+1) + SBAKL(I,NSCL+1) * ACRES
                
        LSREM = LSREM + SSREM(I) * ACRES
        LVREM(1) = LVREM(1) + SVREM(I,1) * ACRES
        LVREM(2) = LVREM(2) + SVREM(I,2) * ACRES

        LBKPO= LBKPO + SBKPO(I) * ACRES
        LBKPI= LBKPI + SBKPI(I) * ACRES
        LBKPS= LBKPS + SBKPS(I) * ACRES
                            
        DO 81 ISIZ= 1,NSCL
          LBAKL(ISIZ)= LBAKL(ISIZ) + SBAKL(I,ISIZ) * ACRES
          LBAH(ISIZ)= LBAH(ISIZ) + SBAH(I,ISIZ) * ACRES
   81   CONTINUE
      

        IF (ICNT .GE. BMEND) THEN
                       
          y = 1.0 / SACRES
           
          LBKP = LBKP * Y
          LRVSTD = LRVSTD * Y
          LBAT = LBAT * Y
          LPROPH = LPROPH * Y
          
          LDDWP = LDDWP * Y
          LSDWP = LSDWP * Y
          
          LTKILL= LTKILL * Y
          LVKILL= LVKILL * Y
          LBAKL(NSCL+1)= LBAKL(NSCL+1) * Y
          
          LSREM = LSREM * Y
          LVREM(1) = LVREM(1) * Y
          LVREM(2) = LVREM(2) * Y
          
          LBKPO= LBKPO * Y
          LBKPI= LBKPI * Y
          LBKPS= LBKPS * Y                  
                            
          DO 82 ISIZ= 1,NSCL
            LBAKL(ISIZ)= LBAKL(ISIZ) * Y
            LBAH(ISIZ)= LBAH(ISIZ) * y
   82     CONTINUE
   
          IF (LBMVOL) THEN
C
C     Changed RYEAR2 to RYEARP to specify end rather than beginning
C     of cycle in output table (RNH June98), next 2 write statements
C
C            WRITE (JBMVOL,90) RYEAR2,'LANDSCAPE',LBKP,LRVSTD,LBAT, 
C
             WRITE (JBMVOL,90) RYEARP,'LANDSCAPE',LBKP,LRVSTD,LBAT, 
     >       INT(LPROPH*100),LDDWP,LSDWP,LTKILL,LVKILL,LBAKL(NSCL+1),
     >       LSREM, LVREM(1), LVREM(2)
   90       FORMAT (1X, I4, 1X, A9, F8.2, F6.2, F8.2, I4, 8(F9.2))
          ENDIF

          IF (LBMDET) THEN
C            WRITE(JBMDBH,95) RYEAR2,'LANDSCAPE',INT(LBKPS*100),LBKP,
            WRITE(JBMDBH,95) RYEARP,'LANDSCAPE',INT(LBKPS*100),LBKP,
     >         LBKPI,LBKPO,(LBAKL(ISIZ),ISIZ=1,NSCL),
     >         (LBAH(ISIZ),ISIZ=1,NSCL)
   95       FORMAT (1X,I4,1X,A9,I5,3(F7.2),10(F7.2),10(F7.1)) 
          ENDIF

c     Zero out all summary variables after printing. 

          LBKP = 0.0
          LBKPS = 0.0
          LBKPO = 0.0
          LBKPI = 0.0
          LBAT = 0.0 
          LPROPH = 0.0
          LDDWP = 0.0
          LSDWP = 0.0
          LTKILL = 0.0
          LVKILL = 0.0
          LSREM = 0.0
          LVREM(1) = 0.0
          LVREM(2) = 0.0
          IF (.NOT. LBMDVO) LRVSTD = 0.0

          DO 71 ISIZ= 1,NSCL+1
            LBAKL(ISIZ) = 0.0
            LBAH(ISIZ)= 0.0
   71     CONTINUE         
        ENDIF

        SBKP(I) = 0.0
        SBKPS(I) = 0.0
        SBKPI(I) = 0.0
        SBKPO(I) = 0.0
        SBAT(I) = 0.0
        PROPH(I) = 0.0
        SDDWP(I) = 0.0
        SSDWP(I) = 0.0
        STKILL(I) = 0.0
        SVKILL(I) = 0.0
        SSREM(I) = 0.0
        SVREM(I,1) = 0.0 
        SVREM(I,2) = 0.0
        IF (.NOT. LBMDVO) SRVSTD(I) = 0.0

        DO 75 ISIZ= 1,NSCL+1
          SBAH(I,ISIZ) = 0.0
          SBAKL(I,ISIZ) = 0.0
   75   CONTINUE
      END IF
      
   85 CONTINUE

      RETURN
      END
