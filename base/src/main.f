      PROGRAM MAIN
      IMPLICIT NONE
C----------
C  **MAIN--BASE DATE OF LAST REVISION:  10/13/2012
C----------
      include "GLBLCNTL.F77"
      INTEGER rtnCode,lenCL
C
C     PROCSS THE COMMAND LINE. Passing an empty string signals that the 
C     real command line arguments will be fetched.
C
      lenCl = 0
      CALL cmdline(' ',lenCL)
      
      IF (fvsRtnCode.NE.0) GOTO 10
C
C     INITIALIZE THE MULTIPLE REPORT ROUTINE (THIS DOES NOT OPEN A FILE)
C
      CALL GENRPT

C     RUN ALL THE CYCLES and STANDS--unless there is a stop point!

      DO
        CALL FVS(rtnCode)
        IF (rtnCode .NE. 0) exit
      ENDDO

   10 CONTINUE     

C     This call does not return.

      CALL GRSTOP
      STOP
      END      

