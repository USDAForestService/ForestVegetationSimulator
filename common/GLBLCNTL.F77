CODE SEGMENT GLBLCNTL
C    
      integer :: fvsRtnCode,restartcode,minorstopptcode,
     -  minorstopptyear,jstash,jdstash,firstWrite,maxStoppts,
     -  originalRestartCode,majorstopptyear,majorstopptcode,
     -  oldstopyr,stopstatcd,readFilePos,seekReadPos
      common /GLBLCNTL/ fvsRtnCode,restartCode,minorstopptcode,
     -  minorstopptyear,jstash,jdstash,firstWrite,maxStoppts,
     -  originalRestartCode,majorstopptyear,majorstopptcode,
     -  oldstopyr,stopstatcd,readFilePos,seekReadPos
      
      character(len=250) :: keywordfile,restartfile,stopptfile
      common /GLBLCNTLC/ keywordfile,restartfile,stopptfile

C     fvsRtnCode= -1 no start up has happened.
C                 0 if all is well, continue processing, 
C                 1 if there is a error, return to caller, 
C                 2 if EOF on or STOP was found on the keyword file
C                   or on the restart file. 
C                  (not an error, but don not continue).      
C     restartcode= restart code. This value is the loction code 
C         where the processing ended. That is, it is the value 
C         of minor|major stopptcode when the program returned 
C         to the caller. If -1, then processing should stop.
C     maxStoppts= the maximum value of minorstopptcode permitted.
C     majorstopptcode= stoppoint request code (stopWithStore, see next definition).
C     minorstopptcode= stoppoint request code (stopWithoutStore).
C         The "stopptcode" is the location code from where the program 
C         is being asked to return to the caller. 
C         codes are:
C          0 = Don't stop.
C          -1 = Stop at every stop point.
C          1 = Stop just before the first call to the Event Monitor.
C          2 = Stop just after the first call to the Event Monitor.
C          3 = Stop just before the second call to the Event Monitor.
C          4 = Stop just after the second call to the Event Monitor.
C          5 = Stop after growth has been computed, but prior to 
C              adding the growth.
C          6 = Stop just before the estab routines are called.
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
C    readFilePos=The file position where the current stand starts, can 
C        be used to recall the stand for another alternative.
C    seekReadPos=A semaphore, readFilePos for first read, -1 for rest of current
C        stand. This support re-reading a stand. 
C
C-----END SEGMENT
