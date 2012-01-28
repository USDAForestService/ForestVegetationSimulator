CODE SEGMENT GLBLCNTL
C    
      integer :: fvsRtnCode,restartcode,minorstopptcode,
     -  minorstopptyear,jstash,jdstash,actualstoppt,
     -  firstWrite,maxStoppts,actualstopyear,
     -  majorstopptyear,majorstopptcode,oldstopyr,stopstatcd,
     -  originalRestartCode
      common /GLBLCNTL/ fvsRtnCode,restartCode,minorstopptcode,
     -  minorstopptyear,jstash,jdstash,actualstoppt,
     -  firstWrite,maxStoppts,actualstopyear,
     -  majorstopptyear,majorstopptcode,oldstopyr,stopstatcd,
     -  originalRestartCode
      
      character(len=250) :: keywordfile,restartfile,stopptfile
      common /GLBLCNTLC/ keywordfile,restartfile,stopptfile

C     fvsRtnCode= 0 if all is well, continue processing, 
C                 1 if there is a error, return to caller, 
C                 2 if EOF or STOP was found on the keyword file, return to caller.
C                  (not an error, but don not continue).      
C     restartcode= restart code. This value is the loction code where the 
C         processing ended. That is, it is the value of minor|major stopptcode when
C         the program returned to the caller. If -1, then processing
C         should stop.
C     actualstoppt= the value of stoppoint as it is accomplished. This
C         will never be -1, but it will be 0 if no stopping is done.
C     actualstopyear= the value of stopptyear as it is accomplished. This
C         will never be -1, but it will be 0 if no stopping is done.
C     maxStoppts= the maximum value of minorstopptcode permitted.
C     majorstopptcode= stoppoint request code (stopWithStore, see next definition).
C     minorstopptcode= stoppoint request code (stopWithoutStore).
C         The "stopptcode" is the location code from where the program 
C         is being asked to return to the caller. 
C         codes are:
C          0 = Don't stop.
C          -1 = Stop at every stop point.
C          1 = Stop just before cuts, at the beginning of the cycle.
C          2 = Stop after growth has been compute, but prior to adding
C              the growth.
C          3 = Stop just before the estab routines are called.
C     majorstopptyear= stopptyear request code (stopWithStore, see next definition).
C     minorstopptyear= stopptyear request code (stopWithoutStore).
C          codes are:
C          0 = Don't stop, even if minorstopptcode == -1.
C          -1 = Stop at every cycle.
C          >0 = Stop during the cycle that contains the year. 
C     firstWrite = 1 if putstd has not yet happened once, 0 if it has
C     keywordfile= keyword file name.
C     restartfile= restart file name. If blank, the program assumes that
C         its memory state is consistent with the last check point.
C     stopptfile= stoppoint file name. If blank, or [none], the program does 
C         not write a stoppoint file, it simply returns.
C     jstash= stoppoint file unit number, if -1, then no file is output.
C     jdstash= restart file unit number, if -1 then no file can be read.
C     oldstopyr= if restarting form a restart file, it is the stop year 
C                from the last run. -1 otherwise. This value is used to 
C                to signal if the program is running form a restart file
C                verses from a keyword file.
C     stopstatcd = 0=no stop has been "caused" (this is the initial condition).
C                  1=stop was caused by a stopWithStore
C                  2=stop was caused by a simulation end signal
C                  3=stop was caused by a stopWithoutStore
C                  4=stop was caused by a the reload of a stand
C    originalRestartCode = restartCode used when a stopWithStore was done.
C
C-----END SEGMENT
