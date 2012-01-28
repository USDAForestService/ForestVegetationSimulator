      PROGRAM MAIN
      IMPLICIT NONE
C----------
C  **MAIN--BASE DATE OF LAST REVISION:  11/30/2011
C----------
      include "GLBLCNTL.F77"
      INTEGER rtnCode,lenCL
C
C     PROCSS THE COMMAND LINE. Passing an empty string signals that the 
C     real command line arguments will be fetched.
C
      lenCl = 0
      CALL cmdline(' ',lenCL)
      
C     When running as a program, having a stop point without a stop point 
C     is nonsense. Kill the run. 

      if (majorstopptcode /= 0 .and. jstash == -1) then
      	print *,"Setting a stop point requires a stop point file",
     >    " when running as a program rather than a shared library."
        goto 10
      endif

      IF (fvsRtnCode.NE.0) GOTO 10
C
C     CALL A FILE OPEN ROUTINE (FILCLS IS AT THE BOTTOM OF FILOPN)
C
      CALL FILOPN
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

