C----------
C VOLUME $Id$
C----------
C_______________________________________________________________________
C
      SUBROUTINE R4D2H (VOLEQ,HTTOT,DBHOB,DRC,FCLASS,VOL,ERRFLAG)
C_______________________________________________________________________
C
!..    This routine wil calculate cubic volumes using the equations 
!..    developed by David Chojnacky (Research Paper INT-339).

      USE DEBUG_MOD
      
      IMPLICIT NONE

!REV  Created ??? 
!REV  Revised TDH 01/12/2010
!REV  Fixed bug, missing '.' in species 066 equation

!**********************************************************************

      CHARACTER*10 VOLEQ
      REAL HTTOT,DBHOB,DRC,VOL(15),D2H
      INTEGER ERRFLAG,MSTEM,FCLASS, I
      
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 10) ' -->Enter R4D2H'
   10    FORMAT (A)   
      END IF
      
c     look for drc, if not use dbhob
      IF (DBHOB .LE. 0.0 .AND. DRC.LE.0.0) THEN
         ERRFLAG = 3
         GO TO 1000
      ENDIF
      IF( HTTOT .LE. 0.0)THEN
         ERRFLAG = 4
         GO TO 1000
      ENDIF

      DO 100, I=1,15
         VOL(I) = 0
  100 CONTINUE

      IF(DRC .GT. 0) THEN
         D2H = DRC*DRC*HTTOT
      ELSE
         D2H = DBHOB*DBHOB*HTTOT
      ENDIF
      
      IF (DEBUG%MODEL) THEN        
           WRITE  (LUDBG, 110)' DRC     HTTOT     DBHOB     D2H'
  110      FORMAT (A)
           WRITE  (LUDBG, 115)DRC, HTTOT, DBHOB, D2H
  115      FORMAT (4F8.1)  
        ENDIF

      IF(FCLASS .GT. 0) THEN
         MSTEM = 1
      ELSE
         MSTEM = 0
      ENDIF
c     find species, do the calculation
c     Western Juniper
      IF (VOLEQ(8:10).EQ.'064')THEN
        VOL(1) = (-0.22048 + 0.125468*D2H**(1./3.)+0.100092*MSTEM)**3.
        VOL(4) = VOL(1)
c     Rocky Mountain Juniper
      ELSEIF (VOLEQ(8:10).EQ.'066')THEN
        VOL(1) = (0.02434+0.119106*D2H**(1./3.))**3.
        VOL(4) = VOL(1)
        
        IF (DEBUG%MODEL) THEN        
           WRITE  (LUDBG, 175)' D2H ', D2H,' VOL(1)',VOL(1)
  175      FORMAT (A,F8.1,A,F8.1)  
        ENDIF
        
c     Utah Juniper
      ELSEIF (VOLEQ(8:10).EQ.'065')THEN
         IF(VOLEQ(2:3) .EQ. '01')THEN
            VOL(1) = (-0.08728+0.135420*D2H**(1./3.)-0.019587*MSTEM)**3.
            VOL(4) = VOL(1)
         ELSE
            VOL(1) = (-0.13386+0.133726*D2H**(1./3.)+0.036329*MSTEM)**3.
            VOL(4) = VOL(1)
         ENDIF
c     Single Leaf Pinyon Pine
      ELSEIF (VOLEQ(8:10).EQ.'133')THEN
        VOL(1) = (-0.14240 + 0.148190*D2H**(1./3.)-0.016712*MSTEM)**3.
        VOL(4) = VOL(1)
c     Pinyon Pine
      ELSEIF (VOLEQ(8:10).EQ.'106')THEN
        VOL(1) = (-0.20296 + 0.150283*D2H**(1./3.)+0.054178*MSTEM)**3.
        VOL(4) = VOL(1)
c     Mountain Mahogany
      ELSEIF (VOLEQ(8:10).EQ.'475')THEN
        VOL(1) = (-0.13363 + 0.128222*D2H**(1./3.)+0.080208*MSTEM)**3.
        VOL(4) = VOL(1)
c     Other Hardwoods
      ELSEIF (VOLEQ(8:10).EQ.'998')THEN
        VOL(1) = (-0.13822 + 0.121850*D2H**(1./3.))**3.
        VOL(4) = VOL(1)
      ENDIF

 1000 RETURN
      END