      REAL FUNCTION RDRANN(J)
      IMPLICIT NONE
C----------
C RD $Id: rdrann.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C
C  THIS FUNCTION RETURNS A UNIFORM RANDOM NUMBER TO THE
C  ROOT DISEASE MODEL CALLING ROUTINE.
C
C  CALLED BY :
C     RDCENT  [ROOT DISEASE]
C     RDCLOC  [ROOT DISEASE]
C     RDINSD  [ROOT DISEASE]
C     RDSPRD  [ROOT DISEASE]
C     RDSPL1  [ROOT DISEASE]
C     RDSPL2  [ROOT DISEASE]
C
C  CALLS     :
C     NONE
C
C  PARAMETERS :
C     J      -
C
C  Revision History :
C   11/06/89 - Last revision date.
C   09/02/14 Lance R. David (FMSC)
C     Added implicit none and declared variables.
C
C----------------------------------------------------------------------
C
COMMONS
C
      INCLUDE 'PRGPRM.F77' 
      INCLUDE 'RDPARM.F77'

      INCLUDE 'RDADD.F77'
C
COMMONS
C
      INTEGER J
      
      S1 = DMOD(16807D0*S0,2147483647D0)
      RDRANN = REAL(S1 / 2147483648D0)
      S0 = S1

      RETURN
      END
