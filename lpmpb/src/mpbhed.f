      SUBROUTINE MPBHED
      IMPLICIT NONE
C----------
C  **MPBHED        DATE OF LAST REVISION:  06/14/13
C----------
C
C     WRITES HEADING FOR MPB MODEL OUTPUT
C
C     Writes a warning for the cix, ttx or bmx multi-pest model 
C     (RNH June 98),  if the logical variable LXNOTE is set in
C     mpbin.f
C
C Revision History
C   12/23/99 Lance R. David (FHTET-FC)
C     Updated for expansion of FVS stand id (variable NPLT)
C     from 8 to 26 characters.
C   11/1100 Lance R. David (FHTET-FC)
C     Removed use of Hollerith constants from FORMAT.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C
C**********************************************************************
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'PLOT.F77'

      INCLUDE 'MPBCOM.F77'
C
COMMONS
C
      WRITE (JOMPB,10)  NPLT,MGMID

      IF (LXNOTE) THEN
      WRITE(JOMPB, 9991)
      LXNOTE = .FALSE.
      ENDIF

   10 FORMAT (/,T34,'MOUNTAIN PINE BEETLE IN LODGE',
     >        'POLE PINE          VERSION 2.0',//,
     >        'STAND ID: ',A26,'; MANAGEMENT ID: ',A4,/)

 9991 FORMAT (' *=================================================',
     > '===========================*',/,
     > ' *---> Note:  The combined insect and pathogen models (in ',
     > 'one executable) <---*',/,
     > ' *---> should NOT be used without close consultation with ',
     > 'the forest''s    <---*',/,
     > ' *---> pathologist and entomologist.  Because of the ',
     > 'potential for more   <---*',/,
     > ' *---> than one insect and/or pathogen acting on the same ',
     > 'tree species,   <---*',/,
     > ' *---> the interpretation of the results of the combined ',
     > 'model can be     <---*',/,
     > ' *---> inaccurate without appropriate model knowledge ',
     > 'and/or experience.  <---*',/,
     > ' *==========================================================',
     > '==================*',/)

      RETURN
      END
