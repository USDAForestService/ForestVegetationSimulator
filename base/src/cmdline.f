      subroutine cmdLine(theCmdLine,lenCL)
      implicit none
      
      include "GLBLCNTL.F77"
      
      integer :: i,n,irtn,ieq,iend,lenCL
      character(len=256) arg
      character(len=*) theCmdLine
      character(len=1024) cmdLcopy
      
      keywordfile = " "
      checkptfile = " "
      fvsRtnCode = 0
      restartcode = 0
      checkptcode = 0
      
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
            cmdLcopy = trim(cmdLcopy) // arg(:iend) // " "
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

        case ("--checkpoint=")
          checkptfile = cmdLcopy(ieq+1:iend)
          read (checkptfile,*) checkptcode,checkptyear

          ieq=index(checkptfile,",")
          checkptfile = checkptfile(ieq+1:)
          ieq=index(checkptfile,",")
          if (ieq == 0) checkptfile=" "
          checkptfile = checkptfile(ieq+1:)

          if (checkptfile == " ") checkptfile="[none]"
          if (checkptcode > 3 .or. checkptcode < 0) then
            print *,"Checkpoint code error, set to zero (off)"
            checkptcode = 0
          else
            print *,"Checkpoint code=",checkptcode,
     -              " year=",checkptyear,
     -              " output= ",checkptfile(:len_trim(checkptfile))
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
        open (unit=jdstash,form="unformatted",status="old",
     >        file=restartfile(:len_trim(restartfile)),err=30)
        goto 40
   30   continue
        print *,"Restart open error on file=",trim(restartfile)
        fvsRtnCode = 1
        return        
   40   continue
        read (jdstash) restartcode,checkptyear,i,keywordfile(:i)
        print *,"Restarting from year= ",checkptyear,
     -          " using checkpoint code= ",restartcode
      endif


      if (checkptcode /= 0 .and. checkptfile /= "[none]") then
        jstash=71
        open (unit=jstash,form="unformatted",status="replace",
     >        file=checkptfile(:len_trim(checkptfile)),err=10)
        goto 20
   10   continue
        print *,"Checkpoint open error on file=",trim(checkptfile)
        fvsRtnCode = 1
        return
   20   continue
        i=len_trim(keywordfile)
        write (jstash) checkptcode,checkptyear,i,keywordfile(:i)
        call flush(jstash)
      endif
      return
      end
      
      
      subroutine getcheckpointcode (ckptcd,ckptyr)
      implicit none
      
      include "GLBLCNTL.F77"
      
      integer :: ckptcd,ckptyr
      ckptcd = checkptcode
      ckptyr = checkptyear
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
      
      
      subroutine fvsCheckPoint (LOCODE,ICKTAKEN)
      implicit none
      
      include "PRGPRM.F77"
      include "GLBLCNTL.F77"
      include "CONTRL.F77"
      
      integer :: LOCODE,ICKTAKEN
      
      ICKTAKEN = 0
      IF (checkptcode /= LOCODE) return
      IF (checkptyear /= 0) then
        IF(checkptyear.lt.iy(icyc).or.checkptyear.ge.iy(icyc+1))return
      ENDIF
      IF (jstash /= -1) call putstd
      restartcode = checkptcode    
      ICKTAKEN = 1
      return
      end

     
