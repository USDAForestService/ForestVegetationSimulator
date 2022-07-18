      SUBROUTINE BRCOUT
      IMPLICIT NONE
C**********************************************************************
C  **BRCOUT       DATE OF LAST REVISION:  06/05/2014
C----------------------------------------------------------------------
C  Purpose:
C  BRCOUT write a detailed summary of cankers to output file.
C  The canker and tree information is printed in  FEET and INCHES.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  17-MAY-1999 Lance David
C     Reformatted the output for narrower and closer columns.
C     Added note on FVS crown ratio to header.
C  03-JUN-1999 Lance David
C     Implemented logical variable to control main header printing
C     instead using cycle 0 as key, which did not work for bare ground
C     runs.
C  13-SEP-2000 Lance David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Modified to allow blister rust on other species using ISPBR array.
C     Species loop (label 450) and species temp index variable (I3)
C     are new.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM. Instead of just being and indicator of a
C     species being a host, BRSPM holds the array index value for that
C     species and is used to access all species-specific BR arrays.
C  21-MAY-2001 Lance R. David (FHTET)
C     Added stand id line to heading and FVS common OUTCOM.F77
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'OUTCOM.F77'

C.... Local variable declarations.
C....    LPRALL - logical variable used to control when all columns
C....             are printed.
C....    HTBCR  - height to base of crown in feet.
C....    CANUP  - canker's distance above ground in feet.
C....    CANOUT - canker's distance out on branch from bole in inches.
C....    GIRD   - percent of tree's bole girdled by canker.
C....    CINACT - char. representation of canker status inactive
C....    CNONLE - char. representation of canker status non-lethal
C....    CPRUNE - char. representation of canker status prunable
C....    CEXCIS - char. representation of canker status excisable
C....    CNONSA - char. representation of canker status non-salvable
C....    CTPKIL - char. represantation of canker status top kill
C....    CTDEAD - char. represenataion of canker status tree kill
C....    CHSTAT - current tree status in character form

      LOGICAL LPRALL
      INTEGER I1, I2, I3, ISTLNB, J, JCSTAT, K, NCAN, NLCAN
      REAL    HTBCR,CANUP,CANOUT,GIRD
      CHARACTER*8 CINACT,CNONLE,CPRUNE,CEXCIS,CNONSA,CTPKIL,CTDEAD,
     &   CHSTAT

C.... Initializations.

      CINACT='IN      '
      CNONLE=' NL     '
      CPRUNE='  PR    '
      CEXCIS='   EX   '
      CNONSA='    NS  '
      CTPKIL='     TK '
      CTDEAD='      DD'

C.... If no trees or no host species in the treelist, then return.

      IF(ITRN .EQ. 0) GO TO 500

C.... Write header.

      IF(BRCHDR) THEN

C....    Write canker status key first time only.

         BRCHDR = .FALSE.

         WRITE(IDCOUT,20)
   20    FORMAT('Pine Blister Rust Detailed Canker Summary',
     &   //,'Key to canker status codes: ',
     &         ' NL = non-lethal,   PR = prunable,  EX = excisable,',
     &   /,T29,' NS = non-salvable, TK = top kill,  DD = tree died,',
     &   /,T29,' IN = canker inactivated',
     &      //,'Note: Crown Ratio is from FVS and may not correlate',
     &      ' with Crown Base height',/,
     &         '      if pruning has occurred.',/)
         WRITE (IDCOUT,25) NPLT,MGMID,ITITLE(1:ISTLNB(ITITLE))
   25    FORMAT('STAND ID: ',A26,4X,'MGMT ID: ',A4,4X,A,/)
      ENDIF

      WRITE(IDCOUT,30) ICYC,IY(ICYC+1)
   30 FORMAT('Cycle: ',I2,4X,'Year: ',I4 /)

      WRITE(IDCOUT,100)
  100 FORMAT(19X,'CANK   CANK',19X,'TREE',13X,'CROWN',/,
     &       '  TREE   TREE CANK DIST.  DIST.   PCNT  CANKER   HGT ',
     &       '  DBH  CROWN  BASE',/,
     &       ' NUMBER  INDX INDX UP(FT) OUT(IN) GIRD  STATUS   (FT)',
     &       '  (IN) RATIO  (FT)',/,
     &       '-------- ---- ---- ------ ------- ---- -------- -----',
     &       ' ----- ----- -----')

C.... Start species loop

      DO 450 I3 = 1, MAXSP

      IF (BRSPM(I3) .EQ. 0) GO TO 450

      I1=ISCT(I3,1)
      IF(I1 .EQ. 0) GO TO 450
      I2=ISCT(I3,2)

      DO 320 J=I1,I2
         K=IND1(J)
         NLCAN=ILCAN(K)
         IF(NLCAN.EQ.0) THEN

C....       Tree has no cankers. skip it.

            CONTINUE
         ELSE
            LPRALL=.TRUE.
            DO 300 NCAN=1,NLCAN

C....          Convert current canker status to character code.

               JCSTAT=ISTCAN(NCAN,K)
               IF(JCSTAT.EQ.1) THEN
                  CHSTAT=CNONLE
               ELSE IF(JCSTAT.EQ.2) THEN
                  CHSTAT=CPRUNE
               ELSE IF(JCSTAT.EQ.3) THEN
                  CHSTAT=CEXCIS
               ELSE IF(JCSTAT.EQ.4) THEN
                  CHSTAT=CNONSA
               ELSE IF(JCSTAT.EQ.5) THEN
                  CHSTAT=CTPKIL
               ELSE IF(JCSTAT.EQ.7) THEN
                  CHSTAT=CTDEAD
               ELSE IF(JCSTAT.EQ.-1) THEN
                  CHSTAT=CINACT
               ELSE
                  GO TO 300
               ENDIF

C....          Get other canker information.

               GIRD=GIRDL(NCAN,K)
               CANUP=DUP(NCAN,K)/30.48
               CANOUT= DOUT(NCAN,K)/2.54

               IF(LPRALL) THEN

C....             Print all columns of information.
C....             When a tree has more than one canker, the last
C....             four columns are printed for the first canker only.

                  LPRALL=.FALSE.
                  HTBCR=BRHTBC(K)/30.48
                  WRITE(IDCOUT,200) IDTREE(K),K,NCAN,CANUP,CANOUT,
     &               GIRD,CHSTAT,HT(K),DBH(K),ICR(K),HTBCR
  200             FORMAT(I8,1X,I4,3X,I2,2X,F5.1,3X,F5.1,1X,
     &               F4.0,1X,A8,1X,F5.1,1X,F5.1,3X,I3,1X,F5.1)
               ELSE

                  WRITE(IDCOUT,210) IDTREE(K),K,NCAN,CANUP,CANOUT,
     &               GIRD,CHSTAT
  210             FORMAT(I8,1X,I4,3X,I2,2X,F5.1,3X,F5.1,1X,F4.0,
     &               1X,A8)
               ENDIF
  300       CONTINUE
         ENDIF
  320 CONTINUE

C.... End of species loop
  450 CONTINUE

      WRITE(IDCOUT,460)
  460 FORMAT(//)

C.... Common return.

  500 CONTINUE
      RETURN
      END
