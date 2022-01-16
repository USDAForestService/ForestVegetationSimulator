      SUBROUTINE BWEOUT
      IMPLICIT NONE
C----------
C WSBWE $Id$
C----------
C
C     COPIES PRINT DATA FROM FILE JOWSBW TO JOSTND.
C
C     PART OF THE WESTERN SPRUCE BUDWORM MODEL/PROGNOSIS LINKAGE CODE.
C     N.L. CROOKSTON--FORESTRY SCIENCES LAB, MOSCOW, ID--JUNE 1983
c
c     minor changes by K.Sheehan 7/96 to remove LBWDEB
C
C     CALLED FROM :
C
C       MAIN   - MAIN PROGRAM FOR THE SINGLE STAND PROGNOSIS MODEL.
C
C  Revision History:
C    07-JUN-00 Lance R. David (FHTET)
C      .Changed final REWIND of JOWSBW to CLOSE so that output from 
C       from a previous stand is not mistaken as output for the current
C       FVS stand in a multiple stand serial run. This routine interprets
C       an open JOWSBW as the defoliation model being active.
C    16-OCT-2006 Lance R. David (FHTET)
C       Changed local variable name from RECORD to RECRD.
C    14-JUL-2010 Lance R. David (FMSC)
C       Added IMPLICIT NONE and declared variables as needed.
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'BWESTD.F77'
      INCLUDE 'BWECOM.F77'
      INCLUDE 'BWEBOX.F77'
C
COMMONS
C
      LOGICAL LOPEN
      CHARACTER*133 RECRD
      INTEGER ISTLNB
C
      IF (JOWSBW.EQ.JOSTND) GOTO 1000
C
C     FIND OUT IF THE FILE IS OPENED...IF NOT BYPASS.
C
      INQUIRE (UNIT=JOWSBW,OPENED=LOPEN)
      IF (.NOT.LOPEN) GOTO 1000
C
C     REWIND THE TEMPORARY OUTPUT FILE.
C
      !REWIND JOWSBW
      REWIND (JOWSBW, ERR=1000) ! temporary patch to allow stop/start
C
C     COPY THE FILE TO THE PRINTER.
C
   10 CONTINUE
      READ (JOWSBW,'(A)',END=40) RECRD
      WRITE (JOSTND,'(A)') RECRD(1:MAX0(1,ISTLNB(RECRD)))
      GOTO 10
   40 CONTINUE
C
C     PREPARE THE FILE FOR THE NEXT STAND.
C

C     REWIND JOWSBW
      CLOSE (JOWSBW)
 1000 CONTINUE
C
C  PRINT THE SPECIAL EVENTS TABLE
C
      IF (LP4) CALL BWEP4(2)
C
      RETURN
      END
