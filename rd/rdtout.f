      SUBROUTINE RDTOUT
      IMPLICIT NONE
C----------
C RD $Id$
C----------
C
C  PRINTS A SUMMARY OF THE DISEASE INFORMATION WHICH WAS READ IN
C  FROM THE TREELIST.
C
C  CALLED BY :
C     INITRE  [PROGNOSIS] -NOT ANY MORE 
C     RDMN1   [ROOT DISEASE]
C
C  CALLS     :
C     NONE
C
C  PARAMETERS :
C     NONE
C
C  COMMON BLOCK VARIABLES :
C     STCUT:   From ANCOM;
C
C
C  LOCAL VARIABLES :
C
C
C  Revision History:
C   06/12/13 Lance R. David (FMSC)
C     Modified for removal of "print control" column 1.
C   09/04/14 Lance R. David (FMSC)
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
      INCLUDE 'RDARRY.F77'
      INCLUDE 'RDADD.F77'
      INCLUDE 'METRIC.F77'

      INTEGER IDI, J
      CHARACTER*1 DITYPE(4)
C
C.... DATA statements
C
      DATA DITYPE/'P','S','A','W'/
C
C     WRITE HEADING FOR OUTPUT
C
      WRITE (JOSTND,1000)
 1000 FORMAT (/,130('-'))
      WRITE (JOSTND,1010)
 1010 FORMAT (/,42X,'SUMMARY OF DISEASE INFORMATION READ IN ON',
     &       ' INPUT')
      WRITE (JOSTND,1000)

      DO 90 IDI=MINRR,MAXRR
         WRITE (JOSTND,1020) DITYPE(IDI)
 1020    FORMAT (/,11X,'DISEASE TYPE: ',A1)

         IF (RRTINV) THEN
            WRITE (JOSTND,1030) RINNF(IDI), RINUF(IDI), RISTU(IDI)
 1030       FORMAT (11X, 'NUMBER OF INFECTED TREE RECORDS IN ',
     &          'DISEASE PATCHES=  ', F6.0/
     &          11X, 'NUMBER OF UNINFECTED TREE RECORDS IN ',
     &          'DISEASE PATCHES=', F6.0/
     &          11X, 'NUMBER OF INFECTED STUMP RECORDS IN ',
     &          'DISEASE PATCHES= ', F6.0)
         ENDIF
         WRITE (JOSTND,1040)
         WRITE (JOSTND,1050)
         WRITE (JOSTND,1060)

 1040    FORMAT (/,11X, '        NUMBER OF INFECTED STUMPS         ')
 1050    FORMAT (/,11X, 'SIZE CLASS      RESINOUS      NON-RESINOUS')
 1060    FORMAT (11X,   '------------------------------------------')

         DO 50 J = 1,5
            IF (LMTRIC) THEN
              IF (J.LT.5) THEN
                 WRITE (JOSTND,1071)
     &            NINT(STCUT(J)*INTOCM), NINT(STCUT(J+1)*INTOCM),
     &            PROBDA(IDI,1,J,1), PROBDA(IDI,2,J,1)
              ELSE
                 WRITE (JOSTND,1081) NINT(STCUT(J)*INTOCM),
     &            PROBDA(IDI,1,J,1), PROBDA(IDI,2,J,1)
              ENDIF
            ELSE
              IF (J.LT.5) THEN
                 WRITE (JOSTND,1070)
     &            NINT(STCUT(J)), NINT(STCUT(J+1)),
     &            PROBDA(IDI,1,J,1), PROBDA(IDI,2,J,1)
              ELSE
                 WRITE (JOSTND,1080) NINT(STCUT(J)),
     &            PROBDA(IDI,1,J,1), PROBDA(IDI,2,J,1)
              ENDIF 
            ENDIF  
 1070       FORMAT (12X,I2,'-',I3,'"',6X,F5.0,10X,F5.0)
 1080       FORMAT (12X,'  >',I3,'"',6X,F5.0,10X,F5.0)
 1071       FORMAT (11X,I3,'-',I3,' CM',5X,F5.0,10X,F5.0)
 1081       FORMAT (11X,'   >',I3,' CM',5X,F5.0,10X,F5.0)
   50    CONTINUE

   90 CONTINUE
C
C     PRINT DELIMITER FOR END OF OUTPUT
C
      WRITE (JOSTND,1000)

      RETURN
      END
