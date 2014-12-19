      SUBROUTINE RDSTP (ISL,IISP,DEN,DIAM,RTD)
      IMPLICIT NONE
C----------
C  **RDSTP       LAST REVISION:  09/03/14
C----------
C
C  ADD INSIDE INFECTED STUMPS TO STUMP ARRAYS. ONLY ADD THE INFECTED PORTION
C     OF THE ROOT SYSTEM TO THE ROOT ARRAYS.
C
C  CALLED BY :
C     RDPRIN  [ROOT DISEASE]
C     RDEND   [ROOT DISEASE]
C     RDIN    [ROOT DISEASE]
C     RDSADD  [ROOT DISEASE]
C
C  CALLS     :
C     NONE
C
C  PARAMETERS :
C     ISL    - (I ) Stump size class.
C     IISP   - (I ) Tree species of current tree record.
C     DEN    - (I ) Number of infected trees in current tree record
C                   that were cut.
C     DIAM   - (I ) Diameter of stump.
C     RTD    - (I ) Root radius of the live tree
C     ROTD   - (I ) Infected root radius of the stump
C
C  Revision History:
C     28-JUN-2002 Lance R. David (FHTET)
C        Previous revision date noted was March 2, 1995.
C        Changed (TST + 1E-6) to just TST in equations below because it
C        is not possible for TST to be zero at this point.
C   09/03/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
C.... PARAMETER INCLUDE FILES
C
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'RDPARM.F77'
C
C.... COMMON INCLUDE FILES
C
      INCLUDE 'CONTRL.F77'
      INCLUDE 'RDCOM.F77'  
      INCLUDE 'RDADD.F77'
C
C.... Local variables
C
      LOGICAL DEBUG
      INTEGER IDI, IISP, IS, ISL, IST
      REAL    DEN, DIAM, RTD, TST, ROTD
C
C.... Check for DEBUG.
C
      CALL DBCHK(DEBUG,'RDSTP',5,ICYC)
      IF (DEBUG) WRITE (JOSTND,100) ICYC,ISL,IISP,DEN,DIAM,RTD
  100 FORMAT (' Begin RDSTP : ICYC, ISL, IISP, DEN, DIAM, RTD = ',
     &          I5, I5, I5, F8.3, F8.3, F8.3)  
     
C     EXIT ROUTINE IF HAVE NO INFECTED STUMPS TO ADD
      
      IF (DEN .LE. 0.0) RETURN     

      IDI = MAXRR
      IF (MAXRR .LT. 3) IDI = IDITYP(IRTSPC(IISP))
C
C     ALLOW DISEASE TYPE TO BE USER SET UPON INITIALIZATION
C
      IF (ISTFLG .EQ. 1) IDI = IRRSP
      
      IST = AMAX0(1,ISTEP)
      IS  = ISPS(IRTSPC(IISP))
      ROTD = RTD * PCOLO(IRTSPC(IISP),IDI)
      TST = PROBDA(IDI,IS,ISL,IST) + DEN

      IF (DEBUG) WRITE (JOSTND,200) IST,IS,ROTD,TST
  200 FORMAT (' In RDSTP : IST, IS, ROTD, TST = ',
     &          2(' ',I5), 3(' ', F8.3))

C     changed (TST + 1E-6) to just TST in equations below because it
C     is not possible for TST to be zero at this point. lrd 28-JUN-02
C
C     DBHDA and ROOTDA is being accumulated as a weighted average
C     of what is already in the class and what is being added.

      DBHDA(IDI,IS,ISL,IST) = ((DBHDA(IDI,IS,ISL,IST) *
     &        PROBDA(IDI,IS,ISL,IST)) + (DIAM * DEN)) / TST
      ROOTDA(IDI,IS,ISL,IST) = ((ROOTDA(IDI,IS,ISL,IST) *
     &        PROBDA(IDI,IS,ISL,IST)) + (ROTD * DEN)) / TST
C     JRAGED(IDI,IS,ISL,IST) = IYEAR
      PROBDA(IDI,IS,ISL,IST) = PROBDA(IDI,IS,ISL,IST) + DEN

      IF (DEBUG) WRITE (JOSTND,900) IDI,IS,ISL,IST
  900 FORMAT (' In RDSTP : IDI, IS, ISL, IST = ', 4I5)
      IF (DEBUG) WRITE (JOSTND,901) DBHDA(IDI,IS,ISL,IST),
     &         ROOTDA(IDI,IS,ISL,IST), PROBDA(IDI,IS,ISL,IST)
  901 FORMAT (' End RDSTP : DBHDA, ROOTDA, PROBDA = ', 3(' ',F10.5))

      RETURN
      END
