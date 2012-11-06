      subroutine ECSTATUS(ICYC, NCYC, IY, beforeCuts)
C----------
C **ECSTATUS--ECON  DATE OF LAST REVISION: 06/18/2009
C----------
C Author Fred Martin, WA DNR,
C Determines ECON and PRETEND status each cycle.
C  Pretend mode is "on" only if active in the first year of a cycle.

C Called after each call to EVMON in GRINC to obtain activities (econStart and pretend) triggered by events during a cycle

C 10/12/2012 Removed saved variables & created new commons

C Variables from FVS
C   beforeCuts - 0 = ECSTATUS being called before CUTS, 1 = ECSTATUS being called after CUTS
C   ICYC       - current cycle number.
C   NCYC       - number of cycles in simulation.
C   IDT        - eventYear, year an Event Monitor activity is scheduled to take place.
C   IY         - array of actual simulation years, 1=inventory year, 2=end 1st cycle, 3=end 2nd cycle, etc.

      implicit none

      include 'PRGPRM.F77'
      include 'ECNCOM.F77'
      include 'ECNCOMSAVES.F77'

      integer               :: evntCnt, i, IACTK, IDT, parmsCnt
      integer, dimension(1) :: pretend   = (/PRETEND_ACTIVITY/),
     &                         econStart = (/ECON_START_YEAR/)
      integer, intent(in)   :: beforeCuts, ICYC, NCYC
      integer, intent(in), dimension(NCYC) :: IY

      logical, intent(out) :: isPretend

      real, dimension(1)       :: pretendDuration(1)
      real, dimension(3)       :: strtParms(3)

      if (.not.isEconToBe) return

!    Determine 1st econStartYear to ensure PRETEND is not activated before ECON starts
      if (econStartYear > IY(ICYC)) then                                 !Intial econStartYear = 9999
         call OPFIND (1, econStart, evntCnt)                             !1=#event types, activity type, returns evntCnt=#found events
         if (evntCnt > 0) then
            do i = 1, evntCnt                                            !Get last ECON start specs for a given cycle
               call OPGET (i, 3, IDT, IACTK, parmsCnt, strtParms)        !Entry in OPFIND, 1=event #, 2=parms dimension, returns IDT, IACTK, parmsCnt, & parms
               call OPDONE(i, IDT)                                       !Entry in OPFIND, i=event id, IDT=date accomplished
            end do
            econStartYear    = IDT
            pretendStartYear = econStartYear
            discountRate     = strtParms(1)
            sevInput         = strtParms(2)
            doSev            = .FALSE.
            if (strtParms(3) > 0.0) doSev = .TRUE.
         end if
      end if

!    Capture all PRETEND activites to preserve even before ECON starts
      call OPFIND(1, pretend, evntCnt)                                   !1=array size of pretend actions, returns evntCnt for this cycle
      do i = 1, evntCnt
         CALL OPGET(i, 1, IDT, IACTK, parmsCnt, pretendDuration)         !Entry in OPFIND, i=event #, 1=parms dimension, returns IDT, IACTK, parmsCnt, & parms
         if (IACTK == PRETEND_ACTIVITY) then
            if (IDT + beforeCuts > pretendEndYear)
     &                               pretendStartYear = IDT + beforeCuts !Ensure continuation of PRETEND status across multiple keywords
            pretendEndYear = NINT(pretendDuration(1)) + IDT+beforeCuts-1 !Duration uses last activity
            CALL OPDONE (i, IDT + beforeCuts)
         end if
      end do

      if (beforeCuts == 0) then                                          !isPretendActive set prior to cuts.f for entire cycle
         if (pretendStartYear >= econStartYear) then                     !Ensure pretend is not activated prior to start of ECON
            if (pretendStartYear <= IY(ICYC) .and.
     &                                  pretendEndYear >= IY(ICYC)) then !Only set pretend activitiy if econ active in 1st year of a cycle
               isPretendActive = .TRUE.
            else
               isPretendActive = .FALSE.
            end if
         end if
      end if

      return


      entry getIsPretendActive(isPretend)
         isPretend = isPretendActive
      return


!    Set "saved/common" variables to an intial state before each simulation
!    Called in ecininit.f
      entry setPretendStatus()
         isPretendActive  = .FALSE.
         pretendStartYear = 0
         pretendEndYear   = 0
      return

      end
