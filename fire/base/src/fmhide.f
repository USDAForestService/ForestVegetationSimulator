      SUBROUTINE FMHIDE (ISTD, IYR)
      IMPLICIT NONE
C----------
C  **FMHIDE  FIRE--DATE OF LAST REVISION:  09/29/09
C----------
C
C     CALLED FROM: FMMAIN 
C     CALLS:   CFVOL
C
C  PURPOSE:                                 
C     PRINT SOME VARIABLES FOR DEBUGGING/CHECKING.  AT SOME POINT, WE
C     MAY WANT TO PLACE THESE INTO PROPER FILES.
C
C  CALL LIST DEFINITIONS:
C     ISTD:    CURRENT STAND
C     IYR:     CURRENT YEAR
C
C  LOCAL VARIABLE DEFINITIONS:   
C
C  COMMON BLOCK VARIABLES AND PARAMETERS:
C
C**********************************************************************

C.... PARAMETER STATEMENTS.

C.... PARAMETER INCLUDE FILES.

      INCLUDE 'PRGPRM.F77'
C      INCLUDE 'PPEPRM.F77'
      INCLUDE 'FMPARM.F77'

C.... COMMON INCLUDE FILES.

      INCLUDE 'FMCOM.F77'

C.... VARIABLE DECLARATIONS.  
      REAL     SNVIS, SNVIH   
      REAL     TOTD(2), TOTV(2)
      INTEGER  IYR,ISTD,I,JCL

      IF (JCOUT .LE. 0) RETURN
      
      TOTD(1) = 0.0
      TOTD(2) = 0.0
      TOTV(1) = 0.0
      TOTV(2) = 0.0

      DO 100 I = 1, NSNAG

         IF ((DENIS(I)+DENIH(I)) .LE. 0.0) GOTO 100
         
C        CALCULATE SNAG VOLUME
         
         SNVIS = 0.0
         SNVIH = 0.0
         
         IF (DENIH(I) .GT. 0.0) THEN
           CALL FMSVOL (I, HTIH(I), SNVIH,.FALSE.,0)
           SNVIH = SNVIH*DENIH(I)
         END IF
         
         IF (DENIS(I) .GT. 0.0) THEN
           CALL FMSVOL (I, HTIS(I), SNVIS,.FALSE.,0)
           SNVIS = SNVIS*DENIS(I)
         END IF

C        SUM VOLUMES AND DENSITIES OF SNAGS <> 12 INCHES
        
         JCL = 2
         IF (DBHS(I) .LT. 12.0) JCL = 1
            
         TOTD(JCL) = TOTD(JCL) + DENIS(I) + DENIH(I)
         TOTV(JCL) = TOTV(JCL) + SNVIS + SNVIH

  100 CONTINUE   

      WRITE(JCOUT,300) IYR,ISTD,TOTD(1),TOTD(2),TOTV(1),TOTV(2),
     &               (CWDNEW(1,I),I=1,11),(CWDNEW(2,I),I=1,11)
  300 FORMAT(I4,1X,I4,1X,2(F6.1,1X),2(F6.0,1X),1X,11(F7.2,1X),
     &       11(F6.2,1X)) 
  
      DO 400 I=1,MXFLCL
         CWDNEW(1,I) = 0.0
         CWDNEW(2,I) = 0.0
  400 CONTINUE
    
      RETURN
      END

