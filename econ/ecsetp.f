      SUBROUTINE ECSETP(IY)
C----------
C ECON $Id$
C----------
C Author Fred Martin, WA DNR,

C Initialize ECON start if necessary and creates sorted indexes for accumulating harvest revenues from keywords.

C Called from MAIN in cycle=0, once for each simulated stand after reading all keywords.

C Variables from FVS
C  IY     - array of actual simulation years, 1=inventory year, 2=end 1st cycle, 3=end 2nd cycle, etc.
C  KODE   - return code, where: 0 implies no errors, 1 implies the referenced activity could not be found.
C  MAXSP  - maximum number of species codes used by a specific variant, from PRGPRM.F77.

      implicit none

      include 'PRGPRM.F77'
      include 'ECNCOM.F77'

      integer             :: i, j, KODE
      integer, intent(in) :: IY(*)

      real, dimension(3)  :: parms = 0.0                                 !1=maximum number of Event Monitor parms

      if (.not.isEconToBe) then
         econStartYear = 9999
         return
      end if

!    Check for absence of STRTECON keywords and if none submitted, set econStartYear to beginning of FVS
      if (econStartYear == -9999) then                                   !No valid STRTECON keyword read
         call OPNEW(KODE, IY(1), ECON_START_YEAR, 3, parms)
         econStartYear = 9999
      end if

!    Load sorted indexes to harvest revenue array values, in descending order of DIB or DBH
      do i = 1, MAXSP
         do j = 1, MAX_REV_UNITS
            if (hrvRevCnt(i,j) > 0) then
***               hrvRevDiaIndx(i,j,:) = hrvRevDia(i,j,:)  DON'T SEE NEED FOR THIS   !Case if hrvRevCnt(i,j)=1
               call RDPSRT(hrvRevCnt(i,j), hrvRevDia(i,j,:),
     &                                     hrvRevDiaIndx(i,j,:), .TRUE.)
            end if
         end do
      end do

      return
      end
