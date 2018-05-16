      SUBROUTINE BMAGDW (ISTD,IYR)
***********************************************************************
*  **BMAGDW  Date of last revision:  June 23, 1994
*----------------------------------------------------------------------
*  Purpose:
*     Transfer some SDWP() to DDWP (falldown)
*     Age 'SDWP()' and 'DDWP()' compartments by one year, using decay rates
*     taken from the fire model
*     Zero out DWPHOS() 
*----------------------------------------------------------------------
*
*  Local variable definitions:
*     I:      Loop counter for host/non=host species
*     J:      Loop counter for size classes
*     K:      Loop counter for dead woody pool age classes
*     PTRANS: Array containing proportions of dead woody pool stuff
*             surviving to next age class.
*
*  Common block variables and parameters:
*     DDWP:   From BMCOM;  Downed Dead Woody Pool (TONS)
*     DWPHOS: From BMCOM;  Dead Woody Pool (Host Trees only) (TPA)
*     SDWP:   From BMCOM;  Standing Dead Woody Pool (TOT CU FT)
*     V2T:    From BMPRM;  Conversion factor from volume to tons
*
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... Common include files.

      INCLUDE 'BMCOM.F77'

C.... Variable declarations.

      INTEGER I, J, K 
      REAL FALL
      REAL X 
      REAL PTRANS(MXDWAG - 1)

C.... Data statements.

C.... Initializations

C.... Check for debug.

      IF(LBMDEB) WRITE(JBMBPR,10) IYR, ISTD
   10 FORMAT(' Begin BMAGDW: Year = ',I5, 'Stand = ', I6)

C.... Begin Routine
      
      
C.... The inverse is (roughly) the annual transfer out of each pool
      
      PTRANS(1) = 1.0 / 1.0
      PTRANS(2) = 1.0 / 1.0
      PTRANS(3) = 1.0 / 3.0
      PTRANS(4) = 1.0 / 5.0
      
C.... Decay dead pools and calculate falldown                        
C         JJ references small/large size classes in SDECRT and DDWP
      
      DO 200 J=1,(MXDWHZ+1)
         JJ = MIN0(J,MXDWSZ)

         DO 220 K=1,MXDWAG               
            FALL = 0.0         
            DO 222 I=1,MXDWPC
               SDWP(ISTD,I,J,K) = SDWP(ISTD,I,J,K) * SDECRT(JJ)
               FALL = FALL + SDWP(ISTD,I,J,K) * FALLRT(I)
C  Added 10/01.ajm.  I'm 95% certain this next line is needed.  Heretofore, felled
c  trees were added to DDWP but never subtracted from SDWP!
               SDWP(ISTD,I,J,K) = SDWP(ISTD,I,J,K) * (1-FALLRT(I))
  222       CONTINUE     
            IF (J .LE. MXDWSZ) 
     &              DDWP(ISTD,J,K) = DDWP(ISTD,J,K) * DDECRT(J)
            DDWP(ISTD,JJ,K) = DDWP(ISTD,JJ,K) + FALL * V2T
  220    CONTINUE
  200 CONTINUE

C     Age dead pools

      DO 300 J=1,(MXDWHZ+1)
         DO 320 K=(MXDWAG-1),1,-1
            DO 322 I=1,MXDWPC
      
               X = SDWP(ISTD,I, J, K) * PTRANS(K)
               SDWP(ISTD,I, J, K) = SDWP(ISTD,I, J, K) - X
               SDWP(ISTD,I, J, K + 1) = SDWP(ISTD,I, J, K + 1) + X

  322       CONTINUE      
            IF (J .LE. MXDWSZ) THEN
              X = DDWP(ISTD,J, K) * PTRANS(K)
              DDWP(ISTD,J,K) = DDWP(ISTD,J,K) - X
              DDWP(ISTD,J,K + 1) = DDWP(ISTD,J,K + 1) + X
            ENDIF
  320    CONTINUE
  300 CONTINUE
      
C.... Zero out the fresh host pool (Note that fresh host is now also added to the 
c     other dead woody pools when it is created since everything uses slightly
c     different array subscripts)
      
      DO 500 I=1,MXDWHC
         DO 520 J=1,MXDWHZ
            DWPHOS(ISTD,I, J) = 0.0
  520    CONTINUE
  500 CONTINUE
      
      IF(LBMDEB) WRITE(JBMBPR,99) IYR, ISTD
   99 FORMAT(' End BMAGDW: Year = ',I5, 'Stand = ', I6)

      RETURN
      END
