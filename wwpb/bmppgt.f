      SUBROUTINE BMPPGT (IPNT, ILIMIT)
      
***********************************************************************
*  **BMPPGT--WWPB   DATE OF LAST REVISION:  07/03/94
*----------------------------------------------------------------------
*  Purpose:
*     Get the damage code information for the given stand.
*  This is part of the Parallel Processing Extension, and freely 
*  adapted from MSPPGT.FOR. In this case, damage code information is
*  only useful from the initialization. Therefore, if the master cycle
*  is more than 2, the variable is not saved or restored.
*----------------------------------------------------------------------
*
*  Call list definitions:
*     ILIMIT: (I)  Size of print buffer WK3.
*     IPNT:   (IO) Pointer to curent element in print buffer WK3.
*
*  Local variable definitions:
*
*  Common block variables and parameters:
*     LBMDAM:   From BMCOM; TRUE if damage code in inventory
*     WK3:      From ARRAYS; Work array used as a print buffer.
*
***********************************************************************

C.... Parameter statements.

      PARAMETER (MXL=1, MXR=0, MXI=0)

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.

      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PPCNTL.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'BMCOM.F77'

C.... Variable declarations.

      LOGICAL LOGICS(MXL), LX

C     GET THE LOGICAL SCALAR. THIS IS .TRUE. IF THE MASTER
C     CYCLE IS 2 OR LESS, .FALSE. OTHERWISE.

      CALL LFREAD (WK3, IPNT, ILIMIT, LOGICS, MXL, 2)
      LX = LOGICS (1)

C     GET THE LOGICAL ARRAYS.

      IF (LX) CALL LFREAD (WK3,IPNT,ILIMIT,LBMDAM,ITRN,   2)

      RETURN
      END
