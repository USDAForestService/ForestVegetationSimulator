      SUBROUTINE DFBHED
      IMPLICIT NONE
C----------
C  **DFBHED  DATE OF LAST REVISION:  06/17/13
C----------
C
C  WRITES HEADING FOR DFB MODEL OUTPUT
C
C     Writes a warning for the cix, ttx or bmx multi-pest model 
C     (RNH June 98),  if the logical variable LXNOTE is set in
C     dfbin.f
C
C  CALLED BY :
C     DFBOUT  [DFB]
C
C  CALLS :
C     NONE
C
C  COMMON BLOCK VARIABLES USED :
C     JODFB  - (DFBCOM)  INPUT
C     MGMID  - (PLOT)    INPUT
C     NPLT   - (PLOT)    INPUT
C
C
C Revision History:
C     23-DEC-99; Lance R. David (FHTET-FC)
C        Updated for expansion of FVS stand id (variable NPLT)
C        from 8 to 26 characters.
C
C**********************************************************************
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'PLOT.F77'
C
C
      INCLUDE 'DFBCOM.F77'
C
C
COMMONS
C
      WRITE (JODFB,100) NPLT, MGMID

      IF (LXNOTE) THEN
      WRITE(JODFB, 9991)
      LXNOTE = .FALSE.
      ENDIF

  100 FORMAT (//,T16,'DOUGLAS-FIR BEETLE IN DOUGLAS-FIR',
     >        '             VERSION 1.0',//,
     >        ' STAND ID:  ',A26,'; MANAGEMENT ID:  ',A4,/)

 9991 FORMAT (' *=================================================',
     & '===========================*',/,
     & ' *---> Note:  The combined insect and pathogen models (in ',
     & 'one executable) <---*',/,
     & ' *---> should NOT be used without close consultation with ',
     & 'the forest''s    <---*',/,
     & ' *---> pathologist and entomologist.  Because of the ',
     & 'potential for more   <---*',/,
     & ' *---> than one insect and/or pathogen acting on the same ',
     & 'tree species,   <---*',/,
     & ' *---> the interpretation of the results of the combined ',
     & 'model can be     <---*',/,
     & ' *---> inaccurate without appropriate model knowledge ',
     & 'and/or experience.  <---*',/,
     & ' *========================================================',
     & '====================*',/)

      RETURN
      END
