      PROGRAM MAIN
      IMPLICIT NONE
C----------
C  $Id$
C----------
      INTEGER rtnCode,lenCL,i

#ifdef PROFILING
      ! In profiling mode multiple iterations of the same keyword
      ! file can be run to increase the sample size for gprof
      integer :: iters,j
      character(len=32) :: arg

      iters = 1
      do j=0, command_argument_count()
        call get_command_argument(j,arg)
        if (len_trim(arg)==0) exit
        if ((arg=='--iters') .or. (arg=='-i')) then
            call get_command_argument(j+1,arg)
            read(arg, '(i10)'), iters
            print '(a i4)', 'Number of iterations:',iters
        endif
      enddo

      do j=1, iters
#endif

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

#ifdef PROFILING
	      print '(a i4)',"End of run: ",j
	      if (i > 0) exit
      enddo
#endif

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
      
