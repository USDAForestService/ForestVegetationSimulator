      SUBROUTINE TMHED (IOUT, NPLT, MGMID)
      IMPLICIT NONE
C---------- 
C  **TMHED  DATE OF LAST REVISION:  06/30/10 
C---------- 
C
C Revision History:
C     23-DEC-99; Lance R. David (FHTET-FC)
C        Updated for expansion of FVS stand id (variable NPLT)
C        from 8 to 26 characters.
C
C**********************************************************************

      CHARACTER*26 NPLT  
      CHARACTER*4 MGMID 

      INTEGER IOUT
C     
C     WRITES HEADING(S) FOR INSECT MODEL OUTPUT 
C     
      WRITE (IOUT,10) NPLT, MGMID   
   10 FORMAT ('1',' * PRE-RELEASE *  DOUGLAS-FIR TUSSOCK MOTH IN ',     
     >       'DOUGLAS-FIR AND GRAND FIR:  DFTM VERSION 3.1;  ',   
     >       'PROGNOSIS (INLAND EMPIRE) 4.0'//  
     >        T20,' STAND ID= ',A26,'; MANAGEMENT ID= ',A4/) 

      RETURN
      END   
