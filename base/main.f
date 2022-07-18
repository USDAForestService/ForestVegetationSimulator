      PROGRAM MAIN
      IMPLICIT NONE
C----------
C BASE $Id$
C----------
      INTEGER rtnCode,lenCL,i
C
C     PROCSS THE COMMAND LINE. Passing an empty string signals that the 
C     real command line arguments will be fetched.
C
      lenCl = 0
      CALL fvsSetCmdLine(' ',lenCL,rtnCode)
      IF (rtnCode.NE.0) GOTO 10

C     RUN ALL THE CYCLES and STANDS--unless there is a stop point!

      DO
        CALL FVS(rtnCode)
        IF (rtnCode .NE. 0) exit
      ENDDO

   10 CONTINUE 
   
      call fvsGetICCode(i)

      IF (i .EQ. 0) STOP

      GO TO (11,12,13,14,15), i 
   11 CONTINUE
      STOP 10
   12 CONTINUE
      STOP 20
   13 CONTINUE
      STOP 30
   14 CONTINUE
      STOP 40
   15 CONTINUE
      STOP 50
      END
      
