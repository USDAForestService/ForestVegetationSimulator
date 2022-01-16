      SUBROUTINE PTINT(IPLTNO, NINC, XAXIS, ANEW)
      IMPLICIT NONE
C----------
C  **PTINT         DATE OF LAST REVISION:  08/22/14
C----------
C
C
C Revision History
C   02/08/88 Last noted revision date.
C   07/02/10 Lance R. David (FMSC)
C     Added IMPLICIT NONE.
C   08/22/14 Lance R. David (FMSC)
C     Function names were used as variable names.
C     changed variable INT to INC
C     changed variable AINT to AINC
C     changed variable NINT to NINC
C----------
      INTEGER IP, IPLTNO, NINC, NINT1, NP

      REAL ANEW(10), OLD(10), DIFF(10), AINC, DIFAX, INC,
     &     OLDAX, XAXIS
C
      NP = 10
      IF (NINC .LE. 1) GO TO 420
      AINC = FLOAT(NINC)
C
C     ** COMPUTE DIFFERENCES
      DO 390 IP = 1,NP
      DIFF(IP) = (ANEW(IP) - OLD(IP))/AINC
  390 CONTINUE
      DIFAX = (XAXIS - OLDAX)/AINC
C
C     **  INTERPOLATE PLOT VALUES
      NINT1 = NINC - 1
      DO 410 INC = 1,NINT1
      OLDAX = OLDAX + DIFAX
      DO 400 IP = 1,NP
      OLD(IP) = OLD(IP) + DIFF(IP)
  400 CONTINUE
      CALL PTARY (IPLTNO, OLDAX, OLD)
  410 CONTINUE
  420 CONTINUE
C
C     ** SAVE OLD VALUES FOR PLOT
      DO 430 IP = 1,NP
      OLD(IP) = ANEW(IP)
  430 CONTINUE
      OLDAX = XAXIS
C
C     ** SPOOL NEW VALUES
      CALL PTARY(IPLTNO, XAXIS, ANEW)
C
      RETURN
      END
