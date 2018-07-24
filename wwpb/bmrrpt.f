      SUBROUTINE RRPPPT (WK3, IPNT, ILIMIT)
C----------
C WWPB $Id$
C----------
*  Purpose:
*     Write the quick root rot and stem rust model data for a given stand.
*  This is part of the Parallel Processing Extension, and freely 
*  adapted from MSPPPT.FOR 
*----------------------------------------------------------------------
*
*  Call list definitions:
*     ILIMIT: (I)  Size of print buffer WK3.
*     IPNT:   (IO) Pointer to curent element in print buffer WK3.
*     WK3:    (IO) Work array used as a print buffer.
*
*  Local variable definitions:
*     I:      Loop counter.
*
*  Common block variables and parameters:
*     LRRON:  From RRBMCM; TRUE if RR spread rate is > 0
*     LSRON:  From RRBMCM; TRUE if SR spread rate is > 0
*     BMRRSR: From RRBMCM; Periodic RR spread rate (0-1) 
*     BMSRSR: From RRBMCM; Periodic SR spread rate (0-1) 
*     BMRR:   From RRBMCM; Proportion RR infection of record (0-1) 
*     BMSR:   From RRBMCM; Proportion SR infection of record (0-1) 
*
***********************************************************************

C.... Parameter statements.

      PARAMETER (MXL=2, MXR=2, MXI=0)

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'BMRRCM.F77'

C.... Variable declarations.
      
      LOGICAL LOGICS (MXL)
      REAL    WK3(MAXTRE), REALS(MXR)
      
C.... Write the logical scalars.

      LOGICS (1) = LRRON
      LOGICS (2) = LSRON
      CALL LFWRIT (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)
      
C.... Write the real scalars.

      REALS (1) = BMRRSR
      REALS (2) = BMSRSR
      CALL BFWRIT (WK3, IPNT, ILIMIT, REALS, MXR, 2)

C.... Write the real arrays, if active.
      
      IF (LRRON) CALL BFWRIT (WK3, IPNT, ILIMIT, BMRR, ITRN, 2)
      IF (LSRON) CALL BFWRIT (WK3, IPNT, ILIMIT, BMSR, ITRN, 2)      

      RETURN
      END
