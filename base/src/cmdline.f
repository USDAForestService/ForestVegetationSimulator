      subroutine cmdLine(theCmdLine,lenCL)
      implicit none
      
      include "GLBLCNTL.F77"
      
      integer :: i,n,irtn,ieq,iend,lenCL,oldstopyr
      character(len=256) arg
      character(len=*) theCmdLine
      character(len=1024) cmdLcopy
      
      keywordfile = " "
      stopptfile = " "
      fvsRtnCode = 0
      restartcode = 0
      stopptcode = 0
      actualstoppt = 0
      stopptreached = 0
      firstWrite = 1
            
c     the file unit numbers also act as switches, if the are -1, then
c     there is no attached file.

      jstash = -1
      jdstash = -1

      if (lenCL > 0) then
        cmdLcopy=theCmdLine(:lenCL)
      else
        cmdLcopy = " "
        n = command_argument_count()
        i = 0
        do 
          i = i+1
          if (i > n) exit
          call get_command_argument(i,arg,iend,irtn)
          if (irtn /= 0) exit
          if (i == 1) then
            cmdLcopy = arg(:iend) // " "
          else
            cmdLcopy = trim(cmdLcopy)// " " // arg(:iend) 
          endif
        enddo
        lenCL = len_trim(cmdLcopy)
      endif   

      if (lenCL == 0) return
      
      i = 1
      do
        if (i >= lenCL) exit
        i = index(cmdLcopy(i:),"--")+i-1
        if (i == 0) exit
        ieq = index(cmdLcopy(i:),"=")
        if (ieq == 0) exit
        ieq = ieq+i-1
        iend = index(cmdLcopy(ieq+1:)," ")
        if (iend == 0) then 
          iend=lenCL
        else
          iend=iend+ieq-1
        endif
        select case (cmdLcopy(i:ieq))

        case ("--keywordfile=")
          keywordfile = cmdLcopy(ieq+1:iend)

        case ("--stoppoint=")
          stopptfile = cmdLcopy(ieq+1:iend)
          read (stopptfile,*) stopptcode,stopptyear

          ieq=index(stopptfile,",")
          stopptfile = stopptfile(ieq+1:)
          ieq=index(stopptfile,",")
          if (ieq == 0) stopptfile=" "
          stopptfile = stopptfile(ieq+1:)

          if (stopptfile == " ") stopptfile="[none]"

          print *,"Stop point code=",stopptcode,
     -            " year=",stopptyear,
     -            " output= ",stopptfile(:len_trim(stopptfile))

        case ("--restart=")
          restartcode = 1
          if (ieq+1 <= iend) restartfile = cmdLcopy(ieq+1:iend)

        end select
        i=iend+1
      enddo

      if (restartcode /= 0) then
      
        if (keywordfile /= " ") then
          print *,"Specifying a keyword file conflicts with",
     -            " specifying a restart file; it is ignored."
          keywordfile = " "
        endif

        jdstash=72
        open (unit=jdstash,form="unformatted",status="old",
     >        file=restartfile(:len_trim(restartfile)),err=30)
        goto 40
   30   continue
        print *,"Restart open error on file=",trim(restartfile)
        fvsRtnCode = 1
        return        
   40   continue
        read (jdstash) restartcode,oldstopyr,i,keywordfile(:i)
        print *,"Restarting from year= ",oldstopyr,
     -          " using stop point code= ",restartcode
      endif


      if (stopptcode /= 0 .and. stopptfile /= "[none]") then
        jstash=71
        open (unit=jstash,form="unformatted",status="replace",
     >        file=stopptfile(:len_trim(stopptfile)),err=10)
        goto 20
   10   continue
        print *,"Checkpoint open error on file=",trim(stopptfile)
        fvsRtnCode = 1
        return
   20   continue
      endif
      return
      end
      
      
      subroutine getstoppointcodes (spptcd,spptyr)
      implicit none
      
      include "GLBLCNTL.F77"
      
      integer :: spptcd,spptyr
      spptcd = stopptcode
      spptyr = stopptyear
      return
      end

      subroutine setstoppointcodes (spptcd,spptyr)
      implicit none
      
      include "GLBLCNTL.F77"
      
      integer :: spptcd,spptyr
      stopptcode = spptcd 
      stopptyear = spptyr 
      return
      end


      subroutine getrestartcode (restrtcd)
      implicit none
      
      include "GLBLCNTL.F77"
      
      integer :: restrtcd
      restrtcd = restartcode
      return
      end
      
      subroutine fvsRestart (restrtcd)
      implicit none
      
      include "GLBLCNTL.F77"
      
      integer :: restrtcd
      
      if (jdstash /= -1) call getstd
      if (fvsRtnCode /= 0) restartcode = -1
      restrtcd = restartcode
      return
      end
      
      subroutine getkeywrd (fn,nch)
      implicit none
      
      include "GLBLCNTL.F77"
      
      integer :: nch
      character(len=*) fn
      fn = keywordfile(:nch)
      return
      end

      subroutine setfvsRtnCode (rtnCode)
      implicit none
      
      include "GLBLCNTL.F77"
      
      integer :: rtnCode
      fvsRtnCode = rtnCode
      return
      entry getfvsRtnCode (rtnCode)
      rtnCode = fvsRtnCode 
      return
      end
      
      
      subroutine fvsStopPoint (LOCODE,ISTOPDONE)
      implicit none
      
      include "PRGPRM.F77"
      include "GLBLCNTL.F77"
      include "CONTRL.F77"
      
      integer :: LOCODE,ISTOPDONE,i
      
      ISTOPDONE = 0
      stopptreached = 0
      IF (stopptcode == 0) return
      IF (stopptcode > 0 .and. stopptcode /= LOCODE) return
      
      IF (stopptyear == 0) return
      IF (LOCODE > 0 .and. 
     -   (stopptyear < iy(icyc) .or. stopptyear >= iy(icyc+1)))
     -      return
      
      actualstoppt = LOCODE

c     If the program is "stopping", the store the data, but only if
c     there is a file (or "location") to store it. If there is no
c     location, the it means the fvs is returning to its caller without
c     storing the state of the system. At this point, the restartcode
c     needs to be set so that when FVS is recalled, the program will
c     start computing from where it left off.
      
      IF (jstash /= -1) then
      	if (firstWrite == 1) then      	
         i=len_trim(keywordfile)
         write (jstash) actualstoppt,stopptyear,i,keywordfile(:i)
         call flush(jstash)
         firstWrite = 0
        endif
        call putstd
      else
        restartcode = LOCODE
      endif
      ISTOPDONE = 1
      stopptreached = 1
      return

      entry getAmStopping (ISTOPDONE)
      ISTOPDONE = stopptreached
      return
      end


     
