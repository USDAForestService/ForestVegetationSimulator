CODE SEGMENT GLBLCNTL
C----------
C  **GLBLCNTL--COMMON   DATE OF LAST REVISION:  10/21/2011
C----------
C    
      integer :: fvsRtnCode,restartcode,checkptcode,checkptyear,
     -  jstash,jdstash
      common /GLBLCNTL/ fvsRtnCode,restartCode,checkptcode,checkptyear,
     -  jstash,jdstash
      
      character(len=250) :: keywordfile,restartfile,checkptfile
      common /GLBLCNTLC/ keywordfile,restartfile,checkptfile

C     fvsRtnCode= 0 if all is well, continue processing, 
C                 1 if there is a error, return to caller, 
C                 2 if EOF or STOP was found on the keyword file, return to caller.
C                  (not an error, but don not continue).      
C     restartcode= restart code. This value is the loction code where the 
C         processing ended. That is, it is the value of checkptcode when
C         the program returned to the caller.
C     checkptcode= checkpoint request code. This is the location code from
C         where the program is being asked to return to the called.
C     checkptyear= checkpoint year, if zero "0", then every cycle.

C     keywordfile= keyword file name.
C     restartfile= recovery file name. If blank, the program assumes that
C         its memory state is consistent with the last check point.
C     checkptfile= checkpoint file name. If blank, or [none], the program does 
C         not write a checkpoint file, it simply returns.
C     jstash= checkpoint file unit number, if -1, then no file is output.
C     jdstash= restart file unit number, if -1 then no file can be read.
C
C-----END SEGMENT
