      SUBROUTINE SVINIT
      IMPLICIT NONE
C----------
C  **SVINIT--BASE  DATE OF LAST REVISION: 04/10/08
C----------
C
C     STAND VISUALIZATION GENERATION
C     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
C     D. ROBINSON   -- ESSA        -- MAY 2005
C
C     INITIALIZE VISUALIZATION VARIABLES
C
COMMONS
C
      INCLUDE 'PRGPRM.F77'

      INCLUDE 'SVDATA.F77'

      INCLUDE 'SVDEAD.F77'
C
COMMONS

      INTEGER I
      JSVOUT   =   0
      JSVPIC   =  91
      ICOLIDX  =   2
      IGRID    = 100
      IPLGEM   =   1
      IMORTCNT =   0
      IRPOLES  =   0
      IDPLOTS  =   0
      IMETRIC  =   0

C     LOAD SNAG RATES FOR DECAY, AND HEIGHT LOSS.
C     THESE COEFFICIENTS ARE THE NI-FFE DEFAULTS. THEN
C     WILL NOT BE VALID FOR MOST OTHER VARIANTS - DR/ESSA
C
C     04/10/08: The remaining decay coefficients below are
C     currently used just when estimating snag height at time
C     of death (in SVSNAD) for snags input via the treelist.
C     When the FFE and SVSNAD logic are reconciled for incoming
C     snags, the following coefficients can likely be eliminated.

      DO I=1,MAXSP
        IF (I.GE.1 .AND. I.LE.3) THEN
          YHFHTS(I) = 33
          YHFHTH(I) = 16
          HRATE(I)  =  0.9
        ELSEIF (I.GE.4 .AND. I.LE.9) THEN
          YHFHTS(I) = 27
          YHFHTH(I) = 13
          HRATE(I) =   1.1
        ELSE
          YHFHTS(I) = 30
          YHFHTH(I) = 14
          HRATE(I) =   1.0
        ENDIF
      ENDDO

      RETURN
      END
