      SUBROUTINE DBSINIT
      IMPLICIT NONE
C
C $Id$
C
      INCLUDE 'DBSCOM.F77'
C
C
      CASEID    = ""

      ISUMARY  = 0
      ICOMPUTE = 0
      IATRTLIST= 0
      ITREELIST= 0
      ICUTLIST = 0
      IDM1     = 0
      IDM2     = 0
      IDM3     = 0
      IDM5     = 0
      IDM6     = 0
      IPOTFIRE = 0
      IFUELS   = 0
      ITREEIN  = 0
      IRGIN    = 0
      IFUELC   = 0
      ICMRPT   = 0
      ICHRPT   = 0
      ICLIM    = 0
      IBURN    = 0
      IMORTF   = 0
      ISSUM    = 0
      ISDET    = 0
      IADDCMPU = 0
      I_CMPU   = 0
      ISTRCLAS = 0
      IBMMAIN  = 0
      IBMBKP   = 0
      IBMTREE  = 0
      IBMVOL   = 0
      ICANPR   = 0
      ISPOUT6  = 0
      ISPOUT17 = 0
      ISPOUT21 = 0
      ISPOUT23 = 0
      ISPOUT30 = 0
      ISPOUT31 = 0
      IDWDVOL  = 0
      IDWDCOV  = 0
      IRD1     = 0
      IRD2     = 0
      IRD3     = 0

      RETURN
      END

      SUBROUTINE DBSACTV(LACTV)
      LOGICAL LACTV
      LACTV = .TRUE.
      RETURN
      END

