CODE SEGMENT GLBLCNTL
C    
      integer :: fvsRtnCode,restartcode,stopptcode,stopptyear,
     -  jstash,jdstash,actualstoppt,stopptreached,firstWrite
      common /GLBLCNTL/ fvsRtnCode,restartCode,stopptcode,stopptyear,
     -  jstash,jdstash,actualstoppt,stopptreached,firstWrite
      
      character(len=250) :: keywordfile,restartfile,stopptfile
      common /GLBLCNTLC/ keywordfile,restartfile,stopptfile

C     fvsRtnCode= 0 if all is well, continue processing, 
C                 1 if there is a error, return to caller, 
C                 2 if EOF or STOP was found on the keyword file, return to caller.
C                  (not an error, but don not continue).      
C     restartcode= restart code. This value is the loction code where the 
C         processing ended. That is, it is the value of stopptcode when
C         the program returned to the caller. If -1, then processing
C         should stop.
C     stopptreached = 0 if not yet stopping, 1 if stopping (stop point reached)
C     actualstoppt= the value of stoppoint as it is accomplished. This
C         will never be -1, but it will be 0 if no stopping is done.
C     stopptcode= stoppoint request code. This is the location code from
C         where the program is being asked to return to the caller. 
C         codes are:
C          0 = Don't stop.
C          -1 = Stop at every stop point.
C          1 = Stop just before cuts, at the beginning of the cycle.
C          2 = Stop after growth has been compute, but prior to adding
C              the growth.
C          3 = Stop just before the estab routines are called.
C     stopptyear= stoppoint year.
C          codes are:
C          0 = Don't stop, even if stopptcode == -1.
C          -1 = Stop at every cycle.
C          >0 = Stop during the cycle that contains the year. 
C     firstWrite = 1 if put has not yet happened, 0 if it has
C     keywordfile= keyword file name.
C     restartfile= recovery file name. If blank, the program assumes that
C         its memory state is consistent with the last check point.
C     stopptfile= stoppoint file name. If blank, or [none], the program does 
C         not write a stoppoint file, it simply returns.
C     jstash= stoppoint file unit number, if -1, then no file is output.
C     jdstash= restart file unit number, if -1 then no file can be read.
C
C-----END SEGMENT