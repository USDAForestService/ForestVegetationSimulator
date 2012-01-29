      subroutine cmdLine(theCmdLine,lenCL)
      implicit none
      
      include "GLBLCNTL.F77"
      
      integer :: i,n,irtn,ieq,iend,lenCL
      logical fstat
      character(len=256) arg
      character(len=*) theCmdLine
      character(len=1024) cmdLcopy
      
      keywordfile = " "
      maxStoppts = 3
      stopptfile = " "
      fvsRtnCode = 0
      restartcode = 0
      minorstopptcode = 0
      minorstopptyear = 0
      majorstopptcode = 0
      majorstopptyear = 0
      actualstoppt = 0
      actualstopyear = 0   
      stopstatcd = 0
      originalRestartCode = 0
      
      oldstopyr = -1
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
          read (stopptfile,*) majorstopptcode,majorstopptyear
          if (majorstopptcode < -1) majorstopptcode = -1
          if (majorstopptcode > maxStoppts) majorstopptcode=maxStoppts
          ieq=index(stopptfile,",")
          stopptfile = stopptfile(ieq+1:)
          ieq=index(stopptfile,",")
          if (ieq == 0) stopptfile=" "
          stopptfile = stopptfile(ieq+1:)

          if (stopptfile == " ") stopptfile="[none]"

c         when there is no "output", then downgrade the stop to minor

	        if (stopptfile == "[none]") then
          	minorstopptcode = majorstopptcode
          	minorstopptyear = majorstopptyear
          	majorstopptcode = 0
          	majorstopptyear = 0
          endif

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
        inquire(unit=jdstash,opened=fstat)
        if (fstat) close(jdstash)
        inquire(file=restartfile(:len_trim(restartfile)),number=i)
        if (i > 0) close(i)
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

c       store the last used restart code that was used to store all the stands.    

        originalRestartCode = restartcode
        stopstatcd = 1

      endif

      if (majorstopptcode /= 0 .and. stopptfile /= "[none]") then
        jstash=71
        inquire(unit=jstash,opened=fstat)
        if (fstat) close(jstash)
        inquire(file=stopptfile(:len_trim(stopptfile)),number=i)
        if (i > 0) close(i)
        open (unit=jstash,form="unformatted",status="replace",
     >        file=stopptfile(:len_trim(stopptfile)),err=10)
        goto 20
   10   continue
        print *,"Stop point open error on file=",trim(stopptfile)
        fvsRtnCode = 1
        return
   20   continue
      endif
      
c     open/reopen the keyword/output file.

      call filopn

      return
      end
      
      block data setglblcntl
      include "GLBLCNTL.F77"
      data fvsRtnCode/-1/
      end      
      
      subroutine getstoppointcodes (spptcd,spptyr)
      implicit none
      
      include "GLBLCNTL.F77"
      
      integer :: spptcd,spptyr
      spptcd = minorstopptcode
      spptyr = minorstopptyear
      return
      end

      subroutine setstoppointcodes (spptcd,spptyr)
      implicit none
      
      include "GLBLCNTL.F77"
      
      integer :: spptcd,spptyr
      minorstopptcode = spptcd
      minorstopptyear = spptyr 
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

c     if the current return code is not zero, then no restart is reasonable.
      
      if (fvsRtnCode /= 0) then
        restartcode = -1
        return
      endif
            
      select case (stopstatcd)
      case (0) ! no stopping was done
        restartcode = 0
      case (1,2) ! stopWithStore OR simulation end signal on the last stand

        if (jdstash /= -1) then
          call getstd
          restartcode = -1 ! signal return to caller
          stopstatcd = 4
        else ! stop with store without reloading
        	stopstatcd = 0
          restartcode = 0 ! should force reading from the keyword file
        endif
        
      case (3) ! stopWithoutStore
        stopstatcd = 0
        restartcode = restartcode
      case (4) ! stopWithStore, second call.
        stopstatcd = 0
        restartcode = originalRestartCode
      end select   
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
      
c     note that this routine is called during the simulation      
      
      include "PRGPRM.F77"
      include "GLBLCNTL.F77"
      include "CONTRL.F77"
      
      integer :: LOCODE,ISTOPDONE,i
      
      ISTOPDONE = 0
      actualstopyear = 0
      actualstoppt = 0
      stopstatcd = 0

      if (LOCODE == -1) then
        stopstatcd = 2
        ISTOPDONE = 1
        return
      endif

      if (majorstopptyear == 0 .or. majorstopptcode == 0) goto 100

c     set up the test code. Use the most restrictive (the one
c     that results in the most stoppage (major or minor).

      if (firstWrite == 1) then
        IF (majorstopptyear == -1) then
          majorstopptyear = iy(icyc)
        endif
        IF (majorstopptcode == -1) then 
          majorstopptcode = restartcode + 1
          IF (majorstopptcode > maxStoppts) majorstopptcode=1
        endif
      endif
      actualstopyear = majorstopptyear
      actualstoppt = majorstopptcode         

      IF (actualstoppt == 0) goto 100
      IF (actualstoppt > 0 .and. actualstoppt /= LOCODE) goto 100
      
      IF (actualstopyear == 0) goto 100
      IF (LOCODE > 0 .and. 
     -   (actualstopyear < iy(icyc) .or. 
     -    actualstopyear >= iy(icyc+1))) goto 100
      
c     If the program is "stopping", the store the data
      
      IF (jstash /= -1) then
      	if (firstWrite == 1) then
         i=len_trim(keywordfile)
         write (jstash) actualstoppt,actualstopyear,i,
     -                  keywordfile(:i)
         call flush(jstash)
         firstWrite = 0
        endif
        call putstd
      endif

      stopstatcd = 1 ! stop was caused by stopWithStore
      restartcode = LOCODE
      ISTOPDONE = 1
      return
      
  100 continue
      if (minorstopptyear == 0 .or. minorstopptcode == 0) return
        
      actualstopyear = 0
      actualstoppt = 0
      IF (minorstopptyear == -1) then
        actualstopyear = iy(icyc)
      else
        actualstopyear = minorstopptyear
      endif  
      IF (minorstopptcode == -1) then 
        actualstoppt = restartcode + 1
        IF (actualstoppt > maxStoppts) actualstoppt=1
      else
        actualstoppt = minorstopptcode         
      endif
      
      IF (actualstoppt == 0) return
      IF (actualstoppt > 0 .and. actualstoppt /= LOCODE) return
      
      IF (actualstopyear == 0) return
      IF (LOCODE > 0 .and. 
     -   (actualstopyear < iy(icyc) .or. 
     -    actualstopyear >= iy(icyc+1))) return

      stopstatcd = 3 ! stop was caused by stopWithoutStore
      restartcode = LOCODE
      ISTOPDONE = 1
      return

C     This entry is called from places in FVS or its routines to see if 
C     the stop point was found and "returning" is in progress.

      entry getAmStopping (ISTOPDONE)
      if (stopstatcd > 0) then
        ISTOPDONE = 1
      else
      	ISTOPDONE = 0
      endif
      return
      end


     
