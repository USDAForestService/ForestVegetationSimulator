CODE SEGMENT GLBLCNTL
C----------
C COMMON $Id$
C----------
C
      character(len=250) :: keywordfile,restartfile,stopptfile
C    
      integer :: firstWrite,fvsRtnCode,jdstash,jstash,majorstopptcode,
     &  majorstopptyear,maxStoppts,minorstopptcode,minorstopptyear,
     &  originalRestartCode,oldstopyr,readFilePos,restartcode,
     &  seekReadPos,stopstatcd
C
      common /GLBLCNTLC/ keywordfile,restartfile,stopptfile
C
      common /GLBLCNTL/ firstWrite,fvsRtnCode,jdstash,jstash,
     &  majorstopptcode,majorstopptyear,maxStoppts,minorstopptcode,
     &  minorstopptyear,oldstopyr,originalRestartCode,readFilePos,
     &  restartCode,seekReadPos,stopstatcd
C
C----------
C  VARIABLE DEFINITIONS:
C----------
C           firstWrite = 1 if putstd has not yet happened once, 0 if it has
C           fvsRtnCode = -1 no start up has happened.
C                         0 if all is well, continue processing, 
C                         1 if there is a error, return to caller, 
C                         2 if STOP was found on the keyword file
C                           or EOF on the restart file. 
C                           (not an error, but do not continue).      
C              jdstash = restart file unit number, if -1 then no file can be 
C                        read.
C               jstash = stoppoint file unit number, if -1, then no file is 
C                        output.
C      majorstopptcode = stoppoint request code (stopWithStore).
C      majorstopptyear = stopptyear request code (stopWithStore).
C           maxStoppts = the maximum value of minorstopptcode permitted.
C      minorstopptcode = stoppoint request code (stopWithoutStore).
C                        The "stopptcode" is the location code from where the  
C                        program is being asked to return to the caller. 
C                        codes are:
C                        0 = Never stop.
C                       -1 = Stop at every stop point.
C                        1 = Stop just before the first call to the  
C                            Event Monitor.
C                        2 = Stop just after the first call to the
C                            Event Monitor.
C                        3 = Stop just before the second call to the
C                            Event Monitor.
C                        4 = Stop just after the second call to the
C                            Event Monitor.
C                        5 = Stop after growth has been computed, but prior to 
C                            adding the growth.
C                        6 = Stop just before the estab routines are called.
C                        7 = Stop before missing value dubbing.
C      minorstopptyear = stopptyear request code (stopWithoutStore).
C                        codes are:
C                        0 = Never stop, even if minorstopptcode == -1.
C                       -1 = Stop at every cycle.
C                       >0 = Stop during the cycle that contains the year. 
C         keywordfile = keyword file name.
C           oldstopyr = if restarting form a restart file, it is the stop year 
C                       from the last run. -1 otherwise. This value is used to 
C                       to signal if the program is running form a restart file
C                       verses from a keyword file.
C originalRestartCode = restartCode used when a stopWithStore was done.
C         readFilePos = The file position where the current stand starts, can 
C                       be used to recall the stand for another alternative.
C         restartcode = restart code. This value is the loction code 
C                       where the processing ended. That is, it is the value 
C                       of minor|major stopptcode when the program returned 
C                       to the caller. If -1, then processing should stop.
C         restartfile = restart file name. If blank, the program assumes that
C                       its memory state is consistent with the last check
C                       point.
C         seekReadPos = A semaphore, readFilePos for first read, -1 for rest of 
C                       current stand. This support re-reading a stand. 
C          stopptfile = stoppoint file name. If blank, or [none], the program 
C                       does not write a stoppoint file, it simply returns.
C          stopstatcd = 0=no stop has been "caused" (this is the initial
C                       condition).
C                       1=stop was caused by a stopWithStore
C                       2=stop was caused by a simulation end signal
C                       3=stop was caused by a stopWithoutStore
C                       4=stop was caused by a the reload of a stand
C
C-----END SEGMENT
