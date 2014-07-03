      SUBROUTINE BRDOUT
      IMPLICIT NONE
C**********************************************************************
C  **BRDOUT       DATE OF LAST REVISION:  06/05/2014
C----------------------------------------------------------------------
C  Purpose:
C  Writes a Blister Rust Model All-Host DBH Class Statistics table at the
C  end of the FVS listing.  This is the same information that is in the
C  WPBR Summary table but it is broken down by 2-inch DBH classes.
C  This summary includes all host species, they are not separated.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  14-SEP-2000 Lance David (FHTET)
C     Modified table heading. Changed "White Pine" to "Pine", because
C     the model now recognizes other pine host species and all are
C     represented in this table.
C     Changed version number from 1.0 to 1.1
C  04-MAY-2001 Lance R. David (FHTET)
C     Added species dimension to BROUT (only used for year).
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM.
C  21-MAY-2001 Lance R. David (FHTET)
C     Added stand id line to heading and FVS commons OUTCOM and PLOT.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'OUTCOM.F77'
      INCLUDE 'PLOT.F77'

      CHARACTER*3 BRVER
C.... Local variable declarations.

      INTEGER I3, I4, J, K, ISTLNB
      LOGICAL BRGO

C.... See if Blister Rust Model is active. If not or no trees then return.

      CALL BRATV(BRGO)
      IF(.NOT. BRGO .OR. ITRN .EQ. 0) GO TO 1000
      BRVER='1.2'

C.... Write heading information.

      WRITE(JOSTND,100)
      WRITE(JOSTND,102) BRVER
      WRITE(JOSTND,103)
      WRITE (JOSTND,15) NPLT,MGMID,ITITLE(1:ISTLNB(ITITLE))
   15 FORMAT('STAND ID: ',A26,4X,'MGMT ID: ',A4,4X,A)
      WRITE(JOSTND,104)

  100 FORMAT(//)
  101 FORMAT(' ')
  102 FORMAT(T36,'*  *  *  WHITE PINE BLISTER RUST MODEL:'
     &   ' Version ',A3,'  *  *  *')
  103 FORMAT(T42,'Stand DBH Class Statistics for Blister Rust Hosts')
  104 FORMAT(131('-'))

  105 FORMAT('YEAR',60X,'2-INCH DBH CLASS')
  106 FORMAT(5('-'),8X,118('-'))
  107 FORMAT(F5.0,9X,'0 - 2.9"',4X,'3 - 4.9"',4X,'5 - 6.9"',4X,
     &   '7 - 8.9"',4X,'9 - 10.9"',2X,'11 - 12.9"',2X,'13 - 14.9"',2X,
     &   '15 - 16.9"',2X,'17 - 18.9"',4X,'19"  +')
  108 FORMAT(11X,10(2X,10('-')))
  110 FORMAT('A.C/T All',2X,10(6X,F6.1))
  111 FORMAT('A.C/T Leth',1X,10(6X,F6.1))
  112 FORMAT('A.C/T Exp',2X,10(6X,F6.1))
  114 FORMAT('P.Inf All',2X,10(8X,F4.2))
  115 FORMAT('P.Inf Leth',1X,10(8X,F4.2))
  117 FORMAT('T/A Clean',2X,10(6X,F6.1))
  118 FORMAT('T/A NonLeth',10(6X,F6.1))
  119 FORMAT('T/A Prune',2X,10(6X,F6.1))
  120 FORMAT('T/A Excise',1X,10(6X,F6.1))
  121 FORMAT('T/A NonSalv',10(6X,F6.1))
  122 FORMAT('T/A TopKill',10(6X,F6.1))
  123 FORMAT('T/A Dead',3X,10(6X,F6.1))
  124 FORMAT('T/A Total',2X,10(6X,F6.1))

C.... Write 2" DBH class summary data; repeat DBH class headings for
C.... each year because it contains the year in the heading.
C....
C.... Just to access the year from the summary array, set species
C.... index to the first blister rust host species.
      I3 = 0
  300 I3 = I3 + 1
      IF (BRSPM(I3) .EQ. 0) GO TO 300
      I4 = BRSPM(I3)

      DO 800 K=1,NCYC+1
         WRITE(JOSTND,105)
         WRITE(JOSTND,106)
         WRITE(JOSTND,107) BROUT(I4,01,K)
         WRITE(JOSTND,108)
         WRITE(JOSTND,110) (BRDBHO(J,01,K),J=1,10)
         WRITE(JOSTND,111) (BRDBHO(J,02,K),J=1,10)
         WRITE(JOSTND,112) (BRDBHO(J,03,K),J=1,10)
         WRITE(JOSTND,101)
         WRITE(JOSTND,114) (BRDBHO(J,04,K),J=1,10)
         WRITE(JOSTND,115) (BRDBHO(J,05,K),J=1,10)
         WRITE(JOSTND,101)
         WRITE(JOSTND,117) (BRDBHO(J,06,K),J=1,10)
         WRITE(JOSTND,118) (BRDBHO(J,07,K),J=1,10)
         WRITE(JOSTND,119) (BRDBHO(J,08,K),J=1,10)
         WRITE(JOSTND,120) (BRDBHO(J,09,K),J=1,10)
         WRITE(JOSTND,121) (BRDBHO(J,10,K),J=1,10)
         WRITE(JOSTND,122) (BRDBHO(J,11,K),J=1,10)
         WRITE(JOSTND,123) (BRDBHO(J,12,K),J=1,10)
         WRITE(JOSTND,124) (BRDBHO(J,13,K),J=1,10)
         WRITE(JOSTND,101)
  800 CONTINUE

C.... Common return.

 1000 CONTINUE
      RETURN
      END
