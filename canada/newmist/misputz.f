      SUBROUTINE MISPUTZ(ITREE,IDMR)
      IMPLICIT NONE
C----------
C  $Id$
C----------
*  **MISPUTZ--MS  Date of last revision:  03/06/01
*----------------------------------------------------------------------
*  Purpose:
*     Sets dwarf mistletoe rating (stored in array IMIST) for the
*  current tree record to the value passed in IDMR, and zeros
*  elements in 3 DM arrays.
*     Entry point MISPUT omits zeroing DM arrays, and is the same
*  as the previous (before Sept. 1993) MISPUT code.
*----------------------------------------------------------------------
*
*  Call list definitions:
*     ITREE:  (I) Current tree record number.
*     IDMR:   (I) Mistletoe rating to put into IMIST for current tree.
*
*  Local variable definitions:
*     DEBUG:  Logical flag to turn debug on or off.
*
*  Common block variables and parameters:
*     ACTIVE: From DMCOM; parameter.
*     BPCNT:  From DMCOM; parameter.
*     BRKPNT: From DMCOM;
*     CRTHRD: From DMCOM; parameter.
*     DMINF:  From DMCOM;
*     DMKLDG: From DMCOM;
*     ICYC:   From CONTRL; cycle index number.
*     IMIST:  From MISCOM; array containing tree record DMR's.
*     JOSTND: From CONTRL; unit number of stand output.
*     PBRKPT: From DMCOM;
*
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'

C.... Common include files.

      INCLUDE 'CONTRL.F77'
      INCLUDE 'MISCOM.F77'
      INCLUDE 'DMCOM.F77'

C.... Variable declarations.

      LOGICAL DEBUG
      INTEGER I, J, K, KCOUNT, ITREE, IDMR

C.... Data statements.

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISPUTZ',7,ICYC)

      IF(DEBUG) WRITE(JOSTND,100) ICYC
  100 FORMAT(' Begin MISPUTZ: Cycle = ',I5)

C.... Set zeros for initial values of DM variables.

      DO I=1,BPCNT
        BRKPNT(ITREE,I) = 0.0
        PBRKPT(ITREE,I) = 0.0
      ENDDO
      DO I=1,CRTHRD
        DO J=1,DEAD_BC
          DMINF(ITREE,I,J) = 0.0
        ENDDO
      ENDDO
      DO I=1,CRTHRD
        DO J=1,ACTIVE
          DO K=1,MAXBC
            DMINF_BC(ITREE,I,J,K) = 0.0
          ENDDO
        ENDDO
      ENDDO

      ENTRY MISPUT(ITREE,IDMR)

C.... Check for debug.

      CALL DBCHK(DEBUG,'MISPUT',6,ICYC)

      IF(DEBUG) WRITE(JOSTND,110) ICYC
  110 FORMAT(' Begin MISPUT: Cycle = ',I5)

C.... Check for valid dwarf mistletoe rating.

      IF (IDMR .GE. 0 .AND. IDMR .LE. 6) THEN

C....    Put value into IMIST for current tree record number.

        IMIST(ITREE)=IDMR
        DMRATE(ITREE)=IDMR
        KCOUNT=0

        DO I = 1,BPCNT
          KCOUNT = KCOUNT + 1
          BRKPNT(ITREE,I) = DMKLDG(KCOUNT)
          KCOUNT = KCOUNT + 1
          PBRKPT(ITREE,I) = DMKLDG(KCOUNT)
        ENDDO

        DO I = 1,CRTHRD
          DO J = 1,DEAD_BC
            KCOUNT = KCOUNT + 1
            DMINF(ITREE,I,J) = DMKLDG(KCOUNT)
          ENDDO
        ENDDO

        DO I = 1,CRTHRD
          DO J = 1,ACTIVE
            DO K = 1,MAXBC
              KCOUNT = KCOUNT + 1
              DMINF_BC(ITREE,I,J,K) = DMKLDG(KCOUNT)
            ENDDO
          ENDDO
        ENDDO

      ELSE

C....    Print error message and return.

        WRITE(JOSTND,900) ITREE,IDMR
  900   FORMAT (/1X,' *** MISPUT. Invalid DM rating input.  ',
     &           'ITREE=',I5,'; IDMR=',I3,'.  0 used.')
        GO TO 9000
      ENDIF

C.... Common return.

 9000 CONTINUE

      IF(DEBUG) WRITE(JOSTND,9010) ICYC
 9010 FORMAT(' End MISPUT(Z): Cycle = ',I5)

      RETURN
      END
