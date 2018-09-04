      SUBROUTINE RDROUT
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C  ****** NOTICE ********* NOTICE ******* NOTICE ****** NOTICE *******
C  ***                                                             ***
C  *** 3/24/2015 Implementation of General Report Writer facility  ***
C  *** has made this subroutined obsolete. It is no longer needed. ***
C  *******************************************************************
C
C  WRITES FINAL ROOT DISEASE SUMMARY OUTPUT.  ALSO WRITES DETAILED REPORT IF
C  THE RRDOUT KEYWORD IS USED.
C
C  CALLED BY :
C     MAIN    [PROGNOSIS]
C
C  CALLS     :
C     NONE
C
C  Revision History :
C   11/06/89 - Last revision date.
C   09/02/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'RDPARM.F77'

      INCLUDE 'CONTRL.F77'

      INCLUDE 'RDCOM.F77'
C
COMMONS
C
      CHARACTER*132 DSTRNG

      IF (IROOT .EQ. 0 .OR. IRUNIT .EQ. JOSTND) RETURN

C
C     WRITE END OF FILE MARKER THEN REWIND IRUNIT
C
      ENDFILE IRUNIT
      REWIND IRUNIT

  125 CONTINUE
      READ (IRUNIT,130,END=200) DSTRNG
  130 FORMAT(A132)
      WRITE (JOSTND,130) DSTRNG
      GOTO 125

  200 CONTINUE
      REWIND IRUNIT
C
C     WRITE DETAILED OUPUT IF ASKED FOR.
C
      IF (IRDOUT .EQ. 1) THEN
C
C        WRITE END OF FILE MARKER THEN REWIND IOUNIT.
C
         ENDFILE IOUNIT
         REWIND  IOUNIT
C
C        WRITE IOUNIT (DETAILED REPORT) TO END OF JOSTND.
C
  300    CONTINUE
         READ (IOUNIT, 130, END=400) DSTRNG
         WRITE (JOSTND, 130) DSTRNG
         GOTO 300

  400    CONTINUE
         REWIND IOUNIT
      ENDIF

      RETURN
      END
