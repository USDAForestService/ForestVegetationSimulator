      PROGRAM MAIN
      IMPLICIT NONE
C----------
C  **MAIN--BASE DATE OF LAST REVISION:  11/30/2011
C----------
      INTEGER fvsRtnCode,lenCL
C
C     PROCSS THE COMMAND LINE. Passing an empty string signals that the 
C     real command line arguments will be fetched.
C
      lenCl = 0
      CALL cmdline(' ',lenCL)
      CALL getfvsRtnCode(fvsRtnCode)
      IF (fvsRtnCode.NE.0) GOTO 10
C
C     CALL A FILE OPEN ROUTINE (FILCLS IS IN ERRGRO BELOW ENTRY GRSTOP)
C
      CALL FILOPN
      CALL getfvsRtnCode(fvsRtnCode)
      IF (fvsRtnCode.NE.0) GOTO 10
C
C     INITIALIZE THE MULTIPLE REPORT ROUTINE (THIS DOES NOT OPEN A FILE)
C
      CALL GENRPT
C
C     RUN ALL THE STANDS

      CALL FVS(fvsRtnCode)

   10 CONTINUE     

C     This call does not return.

      CALL GRSTOP
      STOP
      END      

