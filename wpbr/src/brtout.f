      SUBROUTINE BRTOUT
C**********************************************************************
C  **BRTOUT       DATE OF LAST REVISION:  06/21/2013
C----------------------------------------------------------------------
C  Purpose:
C  BRTOUT writes the treelist summary to output file.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  14-MAY-1999 Lance David
C     Added Truncated Tree Height and Crown Ratio columns and
C     reformatted the output.
C     Added note on FVS crown ratio to header.
C  03-JUN-1999 Lance David
C     Implemented logical variable to control main header printing
C     instead using cycle 0 as key, which did not work for bare ground
C     runs.
C  14-SEP-2000 Lance David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Modified to allow blister rust on other species using ISPBR array.
C     Species loop (label 70) and species temp index variable (I3)
C     are new.
C  22-MAR-2001 Lance R. David (FHTET)
C     Added reserve/escape tree status code 9 handling.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM.
C  15-MAY-2001 Lance R. David (FHTET)
C     Added species code (BRSPC) to output record.
C  21-MAY-2001 Lance R. David (FHTET)
C     Added stand id line to heading and FVS commons OUTCOM and PLOT.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'OUTCOM.F77'
      INCLUDE 'PLOT.F77'

C.... Local variable declarations.
C....    CNOCNK - char. rep. of tree canker status: no cankers
C....    CNONLE - char. rep. of tree canker status: non-lethal
C....    CPRUNE - char. rep. of tree canker status: prunable
C....    CEXCIS - char. rep. of tree canker status: excisable
C....    CNONSA - char. rep. of tree canker status: non-salvable
C....    CTPKIL - char. rep. of tree canker status: top kill
C....    CTDEAD - char. rep. of tree canker status: tree kill
C....    CHSTAT - current tree status in character form

      INTEGER I1,I2,I3,I4,J,K
      REAL GGI,HTBCR,TRUNC
      CHARACTER*8 CNOCNK,CNONLE,CPRUNE,CEXCIS,CNONSA,CTPKIL,CTDEAD,
     &   CHSTAT,CESCAP

C.... Initializations.

      CNOCNK='NO      '
      CNONLE=' NL     '
      CPRUNE='  PR    '
      CEXCIS='   EX   '
      CNONSA='    NS  '
      CTPKIL='     TK '
      CTDEAD='      DD'
      CESCAP='ESCAPE  '

C.... Write treelist data for white pines. If no trees or no host
C.... trees then return.

      IF(ITRN .EQ. 0) GO TO 100

      IF(BRTHDR) THEN
C....    Write tree status key first time only.

         BRTHDR = .FALSE.

         WRITE(IDTOUT,20)
   20    FORMAT('Pine Blister Rust Treelist Summary',
     &      //,'Key to tree status codes: ',
     &      ' NL = non-lethal,   PR = prunable,  EX = excisable,',
     &      /,26X,' NS = non-salvable, TK = top kill,  DD = tree died,',
     &      /,26X,' NO = no cankers',
     &      //,'Key to stock type codes:   ',
     &      '1 = wild stock,   2 = F1 stock,   3 = F3 stock,   4 ='
     &      ' GCOP stock',
     &      //,'Notes: (N/T/A*1000) = Thousands of needles per tree',
     &      ' per acre.',/,
     &      '       Crown Ratio is from FVS and may not correlate',
     &      ' with Crown Base height if pruning has occurred.',/)
         WRITE (IDTOUT,25) NPLT,MGMID,ITITLE(1:ISTLNB(ITITLE))
   25    FORMAT(' STAND ID: ',A26,4X,'MGMT ID: ',A4,4X,A,/)
      ENDIF

C.... Write heading information.

      WRITE(IDTOUT,30) ICYC,IY(ICYC+1)
   30 FORMAT('Cycle: ',I3,4X,'Year: ',I4 /)

      WRITE(IDTOUT,40)
   40 FORMAT(38X,'DIAM',3X,'TREE',2X,'TRUNC',2X,'CROWN',1X,'CROWN',
     &   5X,'SUM OF',13X,'GROWTH',2X,'EXPECTED',2X,'TOTAL',/,
     & 2X,'TREE',7X,'STK',2X,
     &   'AGE',3X,'DBH',3X,'TREES/',1X,'GROWTH',1X,'HEIGHT',1X,
     &   'HEIGHT',1X,'RATIO',2X,
     &   'BASE',2X,'TARGET AREA',4X,'RUST',4X,'INDEX',3X,'NO. OF',
     &   3X,'NO. OF',3X,'TREE',/,
     & 1X,'NUMBER',2X,'SPC TYP',1X,'(YRS)',
     &   1X,'(INCH)',2X,'ACRE',2X,'(INCH)',1X,'(FEET)',1X,'(FEET)',2X,
     &   '(%)',2X,'(FEET)',1X,'(N/T/A*1000)',2X,'INDEX',3X,'(FEET)',2X,
     &   'CANKERS',2X,'CANKERS',2X,'STATUS')

      WRITE(IDTOUT,45)
   45 FORMAT(8('-'),1X,'---',1X,'---',1X,5('-'),1X,6('-'),1X,6('-'),
     &   1X,6('-'),1X,6('-'),1X,6('-'),1X,5('-'),1X,6('-'),1X,12('-'),
     &   1X,7('-'),1X,8('-'),1X,8('-'),1X,7('-'),1X,8('-'))

C.... Start species loop
      DO 70 I3 = 1, MAXSP

      IF (BRSPM(I3) .EQ. 0) GO TO 70
      I1=ISCT(I3,1)
      IF(I1 .EQ. 0) GO TO 70
      I2=ISCT(I3,2)

C.... Loop through for each tree.

      DO 60 J=I1,I2
         K=IND1(J)

C....    Convert growth index from meters to feet.
C....    Convert height to base of crown from cm to ft.

         GGI=GI(K)/.3048
         HTBCR=BRHTBC(K)/30.48
         TRUNC=(FLOAT(ITRUNC(K)))/100.0

C....    Convert current tree canker status to character code.

         IF(IBRSTAT(K).EQ.1) THEN
            CHSTAT=CNONLE
         ELSE IF(IBRSTAT(K).EQ.2) THEN
            CHSTAT=CPRUNE
         ELSE IF(IBRSTAT(K).EQ.3) THEN
            CHSTAT=CEXCIS
         ELSE IF(IBRSTAT(K).EQ.4) THEN
            CHSTAT=CNONSA
         ELSE IF(IBRSTAT(K).EQ.5) THEN
            CHSTAT=CTPKIL
         ELSE IF(IBRSTAT(K).EQ.7) THEN
            CHSTAT=CTDEAD
         ELSE IF(IBRSTAT(K).EQ.0) THEN
            CHSTAT=CNOCNK
         ELSE IF(IBRSTAT(K).EQ.9) THEN
            CHSTAT=CESCAP
         ENDIF

C....    Set blister rust species code index
         I4=BRSPM(ISP(K))

C....    Write summary statistics.

C....    for debug
C         WRITE(JOSTND,*) ' IDTREE=',IDTREE(K),' BRAGE=',BRAGE(K),
C     &   ' HT=',HT(K),' GGI=',GGI

         WRITE(IDTOUT,50) IDTREE(K),BRSPC(I4),ISTOTY(K),BRAGE(K),
     &      DBH(K),PROB(K),DG(K),HT(K),TRUNC,ICR(K),HTBCR,
     &      TSTARG(K),RI(K),GGI,ESTCAN(K),ITCAN(K),CHSTAT
   50    FORMAT(I8,1X,A3,2X,I1,3X,F4.0,2X,F5.1,1X,F6.1,1X,F6.2,
     &      1X,F6.1,1X,F6.1,4X,I2,1X,F6.1,4X,F9.1,1X,F7.5,1X,F8.2,
     &      3X,F6.1,4X,I4,1X,A8)
   60 CONTINUE

C.... End species loop
   70 CONTINUE

      WRITE(IDTOUT,80)
   80 FORMAT(//)

C.... Common return.

  100 CONTINUE
      RETURN
      END
