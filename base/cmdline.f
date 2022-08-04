C----------
C BASE $Id: cmdline.f 3950 2022-04-01 11:51:52Z nickcrookston $
C----------
c     This is a collection of routines that provide an interface to
c     elements of the FVS command line parameters, including the stop/
c     restart facilities. These routines are most useful with the shared
c     library version of FVS but they are also called from within FVS.
c
c     Note that not all of the routines are designed to be part of the API

c     Created in 2011 and 2012 by Nick Crookston, RMRS-Moscow
c     In 2019 additional interface routines were added to allow for C functions
c     that are part of the API to call fortran functions with character strings.
c     The fortran routines that can be called by C have an upper case C added to 
c     the routine name. See apisubs.c for the C functions that are used to call 
c     the fortran routines. Only routines with character data have both languages.

      
#ifdef CMPgcc
      subroutine fvsSetCmdLineC(theCmdLine,lenCL,IRTNCD)
     -           bind(c, name="fvsSetCmdLineC") 
      use iso_c_binding      
      implicit none

      integer(c_int), bind(c) :: lenCL,IRTNCD
      character(c_char), dimension(255), bind(c) :: theCmdLine      

      integer i
      character passCmdLine*255
      
      do i=1,lenCL
        passCmdLine(i:i)=theCmdLine(i)
      enddo
      call fvsSetCmdLine(passCmdLine,lenCL,IRTNCD)
      return
      end
#endif
      
      subroutine fvsSetCmdLine(theCmdLine,lenCL,IRTNCD)
      implicit none

      include "GLBLCNTL.F77"

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE :: FVSSETCMDLINE
!DEC$ ATTRIBUTES ALIAS : 'FVSSETCMDLINE' :: FVSSETCMDLINE
!DEC$ ATTRIBUTES REFERENCE :: theCmdLine,lenCL,IRTNCD

      integer :: i,n,irtn,ieq,iend,lenCL,IRTNCD
      logical fstat
      character(len=256) arg
      character(len=lenCL) theCmdLine
      character(len=1024) cmdLcopy

c     make sure the files are closed if resetting with the cmdLine.
c     (this is only done if a none-zero return or restart code is set)

      if (fvsRtnCode /= 0 .or. restartcode /= 0) call FILClose

c     initialize the multiple report routine (this does not open a file)

      call genrpt
      
      keywordfile = " "
      maxStoppts = 7
      stopptfile = " "
      fvsRtnCode = 0
      restartcode = 0
      minorstopptcode = 0
      minorstopptyear = 0
      majorstopptcode = 0
      majorstopptyear = 0
      stopstatcd = 0
      originalRestartCode = 0
      readFilePos = -1
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

      if (lenCL == 0) goto 100

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

        case ("--restart=")
          restartcode = 1
          if (ieq+1 <= iend) restartfile = cmdLcopy(ieq+1:iend)

        end select
        i=iend+1
      enddo

      if (restartcode /= 0) then

        if (keywordfile /= " ") then
          print *,"Specifying a keyword file conflicts with",
     -         " using a restart file; keyword file is ignored."
          keywordfile = " "
        endif

        jdstash=72
        inquire(unit=jdstash,opened=fstat)
        if (fstat) close(jdstash)
        inquire(file=restartfile(:len_trim(restartfile)),number=i)
        if (i > 0) close(i)
        open (unit=jdstash,form="unformatted",status="old",err=30,
     >        access="stream",file=restartfile(:len_trim(restartfile)))
        goto 40
   30   continue
        print *,"Restart open error on file=",trim(restartfile)
        fvsRtnCode = 1
        return
   40   continue
        read (jdstash,end=42) restartcode,oldstopyr,i,keywordfile(:i)
        goto 43
   42   continue
        print *,"Premature end of data on file=",trim(restartfile)
        fvsRtnCode = 1
        return
   43   continue
        write (*,44) restartfile(:len_trim(restartfile)),
     >          oldstopyr,restartcode
   44   format (" Restarting from file=",A," Year=",I5,
     >          " Stop point code=",I2)

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
        open (unit=jstash,form="unformatted",status="replace",err=10,
     >        access="stream",file=stopptfile(:len_trim(stopptfile)))
        goto 20
   10   continue
        print *,"Stop point open error on file=",trim(stopptfile)
        fvsRtnCode = 1
        IRTNCD = fvsRtnCode
        return
   20   continue
        write (*,21) majorstopptcode,
     >     majorstopptyear," File= ",trim(stopptfile)
      else
        if (majorstopptcode /= 0) write (*,21) majorstopptcode,
     >    majorstopptyear," Will stop without saving data."
   21   format (" Stop point code=",I2," Year=",I5,A,A)
      endif

  100 continue

c     open/reopen the keyword/output file.

      call filopn
      CALL fvsGetRtnCode(IRTNCD)
      IF (IRTNCD.NE.0) RETURN

      return
      end

      block data setglblcntl
      include "GLBLCNTL.F77"
      data fvsRtnCode/-1/
      end

      subroutine fvsGetStoppointCodes (spptcd,spptyr)
      implicit none

      include "GLBLCNTL.F77"

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE :: FVSGETSTOPPOINTCODES
!DEC$ ATTRIBUTES ALIAS : 'FVSGETSTOPPOINTCODES' :: FVSGETSTOPPOINTCODES
!DEC$ ATTRIBUTES REFERENCE :: spptcd,spptyr

      integer :: spptcd,spptyr
      spptcd = minorstopptcode
      spptyr = minorstopptyear
      return
      end

      subroutine fvsSetStoppointCodes (spptcd,spptyr)
      implicit none

      include "GLBLCNTL.F77"

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE :: FVSSETSTOPPOINTCODES
!DEC$ ATTRIBUTES ALIAS : 'FVSSETSTOPPOINTCODES' :: FVSSETSTOPPOINTCODES
!DEC$ ATTRIBUTES REFERENCE :: spptcd,spptyr

      integer :: spptcd,spptyr
      minorstopptcode = spptcd
      minorstopptyear = spptyr
      return
      end


      subroutine fvsGetRestartCode (restrtcd)
      implicit none

      include "GLBLCNTL.F77"

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE :: FVSGETRESTARTCODE
!DEC$ ATTRIBUTES ALIAS : 'FVSGETRESTARTCODE' :: FVSGETRESTARTCODE
!DEC$ ATTRIBUTES REFERENCE :: restrtcd

      integer :: restrtcd
      if (fvsRtnCode == 0) then
        restrtcd = restartcode
      else
        restrtcd = 0
      endif
      return
      entry clearrestartcode
      restartcode = 0
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
          inquire (unit=jdstash,pos=readFilePos) ! save position where stand is stored
          seekReadPos = readFilePos ! start reading form here.
          call getstd
          restartcode = -originalRestartCode ! signal return to caller
          stopstatcd = 4
        else ! stop with store without reloading
          stopstatcd = 0
          restartcode = 0 ! should force reading from the keyword file
        endif

      case (3) ! stopWithoutStore
        stopstatcd = 0
      case (4) ! stopWithStore, second call.
        stopstatcd = 0
        restartcode = originalRestartCode
      end select
      restrtcd = restartcode
cc      print *,"at the end of fvsRestart, stopstatcd=",stopstatcd,
cc     -        " restrtcd=",restrtcd
      return
      end


      subroutine fvsRestartLastStand(restrtcd)
      implicit none

      include "GLBLCNTL.F77"

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE :: FVSRESTARTLASTSTAND
!DEC$ ATTRIBUTES ALIAS:'FVSRESTARTLASTSTAND' :: FVSRESTARTLASTSTAND
!DEC$ ATTRIBUTES REFERENCE :: restrtcd

      integer :: restrtcd
      if (readFilePos == -1) then
        call fvsSetRtnCode (1)
      else
        seekReadPos = readFilePos ! start reading form here.
        call getstd
        ! signal return to caller, will be reset when fvsRestart is called.
        restartcode = -1 
        stopstatcd = 4
      endif
      restrtcd = fvsRtnCode
      return
      end

      subroutine fvsGetKeywordFileName (fn,mxch,nch)
      implicit none

      include "GLBLCNTL.F77"

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE :: FVSGETKEYWORDFILENAME
!DEC$ ATTRIBUTES ALIAS:'FVSGETKEYWORDFILENAME' :: FVSGETKEYWORDFILENAME
!DEC$ ATTRIBUTES REFERENCE :: fn,mxch,nch

      integer :: mxch,nch
      character(mxch) fn

C     nch of 251 is key to save, not load, filename
      if (nch == 251) then
        keywordfile = fn
        return
      else
        fn = " "
      endif

      if (mxch < 1) return
      
      nch = min(mxch,len_trim(keywordfile))
      if (nch > 0) fn = keywordfile(:nch)
      return
      end

      subroutine fvsSetRtnCode (rtnCode)
      implicit none
      include "GLBLCNTL.F77"
      integer :: rtnCode

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE :: FVSSETRTNCODE
!DEC$ ATTRIBUTES ALIAS : 'FVSSETRTNCODE' :: FVSSETRTNCODE
!DEC$ ATTRIBUTES REFERENCE :: rtnCode

      fvsRtnCode = rtnCode

C     if in an error state, close the files.

      if (fvsRtnCode /= 0) call filclose
      return
      end

      subroutine fvsGetRtnCode (rtnCode)

      include "GLBLCNTL.F77"
      integer :: rtnCode

!DEC$ ATTRIBUTES DLLEXPORT,C,DECORATE :: FVSGETRTNCODE
!DEC$ ATTRIBUTES ALIAS : 'FVSGETRTNCODE' :: FVSGETRTNCODE
!DEC$ ATTRIBUTES REFERENCE :: rtnCode

      rtnCode = fvsRtnCode
      return
      end


      subroutine fvsStopPoint (LOCODE,ISTOPDONE)
      implicit none

c     note that this routine is called during the simulation

      include "PRGPRM.F77"
      include "GLBLCNTL.F77"
      include "CONTRL.F77"

      integer :: LOCODE,ISTOPDONE,i,tmpyr

      ISTOPDONE = 0
      stopstatcd = 0
cc      print *,"in fvsStopPoint,LOCODE",LOCODE
      if (LOCODE == -1) then
        restartcode = 100
        stopstatcd = 2
        ISTOPDONE = 1
        return
      endif
      if (LOCODE == 0) return

cc      print *,"in fvsStopPoint,majorstopptyear=",majorstopptyear,
cc     -  " majorstopptcode=",majorstopptcode," icyc=",icyc,"iy(icyc)=",
cc     -  iy(icyc)," +1=",iy(icyc+1)

      if (majorstopptyear == 0 .or. majorstopptcode == 0) goto 100

      IF (majorstopptcode > 0 .and.
     -    majorstopptcode /= LOCODE) goto 100

      IF (majorstopptyear > 0 .and.
     -   (majorstopptyear < iy(icyc) .or.
     -    majorstopptyear >= iy(icyc+1))) goto 100

      IF (majorstopptyear > 0) then
        tmpyr=majorstopptyear
      else
        tmpyr=iy(icyc)
      endif

c     If the program is "stopping", then store the data (unless there is

      IF (jstash /= -1) then
        if (firstWrite == 1) then
         i=len_trim(keywordfile)
         write (jstash) LOCODE,tmpyr,i,keywordfile(:i)
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
cc      print *,"in fvsStopPoint,minorstopptyear=",minorstopptyear,
cc     -  " minorstopptcode=",minorstopptcode," icyc=",icyc,"iy(icyc)=",
cc     -  iy(icyc)," iy(icyc+1)=",iy(icyc+1)

      if (minorstopptcode == 0) return

      IF (minorstopptcode > 0 .and. minorstopptcode /= LOCODE) return

      IF (minorstopptyear > 0 .and.
     -   (minorstopptyear < iy(icyc) .or.
     -    minorstopptyear >= iy(icyc+1))) return

      stopstatcd = 3 ! stop was caused by stopWithoutStore
      restartcode = LOCODE
      ISTOPDONE = 1
cc      print *,"in fvsStopPoint,ISTOPDONE=",ISTOPDONE
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


