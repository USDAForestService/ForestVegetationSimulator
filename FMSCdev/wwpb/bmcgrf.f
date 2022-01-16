      SUBROUTINE BMCGRF (ISTD,IYR,OLDGRF)
C----------
C WWPB $Id$
C----------
c     CALLED BY: BMDRV
***********************************************************************
*  **BMCGRF  Date of last revision:  June 14, 1994
*----------------------------------------------------------------------
*  Purpose:
*     Calculate the growth reduction factor (GRF) of each host
*     size class by multipling all stressors together.
*----------------------------------------------------------------------
*
*  Local variable definitions:
*     GRFDV:  Temporary variable containing GRF for each
*             driving variable
*     ICLS:   Loop counter over dbh size classes
*     TOTAL:  Accumulated total GRF for stand (across size classes)
*     TOTATT: proportion of total trees attacked
*     X:      temporary value containing the avg GRF/tree
*     OLDGRF: last year's GRF (used in calculating reproduction)
*         The following three added 6/99. AJM
*     DNCF1:  Coefficient used in the stand-level density effects GRF calculation.
*     DNCF2:  A second coeeficient used in same equation.
*     BASTD:  Total basal area of the stand (host plus non-host.)
*
*  Common block variables and parameters:
*     TOPKLL: From BMCOM; Proportion trees top-killed by Ips in each size class
*     GRFSTD: From BMCOM; stand-level GRF
*     MSBA:   FROM BMCOM; BA in each size class
*     NSCL:   From BMPRM; Number of dbh size classes
*     OTHATT: From BMCOM; Proportion of trees attacked but not killed in
*                         each size class
*     RROT:   From BMCOM; Proportion of trees in each size class
*                         infected with root disease
*     RVDSC:  From BMCOM: the Rating Value due to Drought, dimensioned by
*                         stand and Size Class.
*     RVDFOL: From BMCOM: the Rating Value due to defoliators
*     SCORCH: From BMCOM; Proportion of trees in size class severely
*                         scorched by fire
*     SRUST:  From BMCOM; Proportion of trees in each size class
*                         infected with stem rust
*     STRIKE: From BMCOM; Proportion of trees in each size class struck
*                         by lightning
*     RVDNST:  From BMCOM; Rating value due to stand density effects.
*        This last variable added 6/99 (AJM).
***********************************************************************

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'BMPRM.F77'

C.... Include files.

      INCLUDE 'BMCOM.F77'
      INCLUDE 'BMPCOM.F77'

C.... Variable declarations.

      INTEGER ICLS
      REAL GRFDV
      REAL OLDGRF(NSCL)
      REAL TOTAL
      REAL TOTATT
      REAL X
C...  The following added 6/28/99 for the new stand density rating value
C        calculation below.  AJM.
      REAL DNCF1
      REAL DNCF2

C..   Check for debug.

      IF(LBMDEB) WRITE(JBMBPR,10) IYR, ISTD
   10 FORMAT(' Begin BMCGRF: Year= ',I4, 'ISTD= ', I8)

c     First zero out the variable used in printing to the driving variables
c     output file:

      DO 20 IDV= 1,NUMRV
         DVRV(ISTD,IDV) = 0.0
   20 CONTINUE

C     Begin calculation by looping over sizeclass-level GRF factors.
      
      DO 100 ICLS= 1,NSCL
      
         IF (GRF(ISTD,ICLS) .GT. 0.0) THEN
            OLDGRF(ICLS) = GRF(ISTD,ICLS)
         ELSE
            OLDGRF(ICLS) = 1.0
         ENDIF
         GRF(ISTD,ICLS)= 1.0
         
C...     GRF from DM (near zero is DMR=6)

         GRFDV = 1.0 - (SDMR(ISTD,ICLS) / 6.5)
         IF (GRFDV .LE. 0.0) GRFDV = .01
         GRF(ISTD,ICLS)= GRF(ISTD,ICLS) * GRFDV
         DVRV(ISTD,6) = DVRV(ISTD,6) + GRFDV * BAH(ISTD,ICLS)
         
C....    GRF from root disease

         GRFDV= 1.0 - SRR(ISTD,ICLS)
         IF (GRFDV .LE. 0.0) GRFDV = .01
         GRF(ISTD,ICLS)= GRF(ISTD,ICLS) * GRFDV
         DVRV(ISTD,7) = DVRV(ISTD,7) + GRFDV * BAH(ISTD,ICLS)
      
C....    GRF from stem rusts

         GRFDV = 1.0 - SSR(ISTD,ICLS)
         IF (GRFDV .LE. 0.0) GRFDV = .01
         GRF(ISTD,ICLS)= GRF(ISTD,ICLS) * GRFDV
         DVRV(ISTD,8) = DVRV(ISTD,8) + GRFDV * BAH(ISTD,ICLS)
      
C...  GRF from other beetle attacks
C
         TOTATT = OTHATT(ISTD,ICLS) + TOPKLL(ISTD,ICLS)
         GRFDV = 1 - TOTATT

         IF (GRFDV .LE. 0.0) GRFDV = .01
         GRF(ISTD,ICLS)= GRF(ISTD,ICLS) * GRFDV
         DVRV(ISTD,4) = DVRV(ISTD,4) + GRFDV * BAH(ISTD,ICLS)

C      
C...  GRF from fire (differs between beetle species)
C         
         GRFDV = 1.0
         IF (PBSPEC .EQ. 1 .OR. PBSPEC .EQ. 2) THEN
            GRFDV = 1 - 0.5 * SCORCH(ISTD,ICLS)
         ELSEIF (PBSPEC .EQ. 3) THEN
            GRFDV = 1 - 0.99 * SCORCH(ISTD,ICLS)
         ENDIF   
         IF (GRFDV .LE. 0.0) GRFDV = .01
         GRF(ISTD,ICLS)= GRF(ISTD,ICLS) * GRFDV
         DVRV(ISTD,1) = DVRV(ISTD,1) + GRFDV * BAH(ISTD,ICLS)
C       
C...  GRF from lightning
C                            
         GRFDV = 1 - .99 * STRIKE(ISTD,ICLS)
         IF (GRFDV .LE. 0.0) GRFDV = .01
         GRF(ISTD,ICLS)= GRF(ISTD,ICLS) * GRFDV
         DVRV(ISTD,2) = DVRV(ISTD,2) + GRFDV * BAH(ISTD,ICLS)
      
c  GRF from drought
c
         GRFDV = RVDSC(ISTD,ICLS)
         IF (GRFDV .LE. 0.0) GRFDV = .01
********************************************
c...troubleshoot new compiling 3/00 ajm
C
C	 WRITE (81,110) ISTD, ICLS, RVDSC(ISTD,ICLS), GRFDV,
C     >   BAH(ISTD,ICLS), DVRV(ISTD,3), GRF(ISTD,ICLS)
C  110    FORMAT (I12, I4, 2F6.3, 2X, F7.3, 2X, F9.3, 2X, F10.3)
C**************************************************************
	 GRF(ISTD,ICLS)= GRF(ISTD,ICLS) * GRFDV
         DVRV(ISTD,3) = DVRV(ISTD,3) + GRFDV * BAH(ISTD,ICLS)
c

c

c  GRF from defoliators (notice the first line so that the GRF
c                        will not be 0 in the first year)
c
         IF (RVDFOL(ISTD,ICLS) .LE. 0.0) RVDFOL(ISTD,ICLS) = 1.0
         GRFDV = RVDFOL(ISTD,ICLS)
         IF (GRFDV .LE. 0.0) GRFDV = .01
         GRF(ISTD,ICLS)= GRF(ISTD,ICLS) * GRFDV 
         DVRV(ISTD,5) = DVRV(ISTD,5) + GRFDV * BAH(ISTD,ICLS)
         
C...  GRF from density effects

C   The following two lines commented out 6/25/99, AJM.
C   GRF due to stand density effects will be calculated at  the
C   stand level (not by size class as is done here).  See added lines below.
C
C        GRF(ISTD,ICLS)= GRF(ISTD,ICLS) * DNSTD(ISTD,ICLS)
C        DVRV(ISTD,9) = DVRV(ISTD,9) + DNSTD(ISTD,ICLS) * BAH(ISTD,ICLS)

C     GRF can be at most 0.01
         GRF(ISTD,ICLS)= MAX(1.0E-2, GRF(ISTD,ICLS))
c *********IF CHANGING this use of 0.01, change it also in BMDRGT and BMATCT.

  100 CONTINUE


      TOTAL = 0.0
      GRFSTD(ISTD)= 0.0
      DO 200 ICLS=1,NSCL
        X = BAH(ISTD,ICLS)
        TOTAL= TOTAL + X
        GRFSTD(ISTD)= GRFSTD(ISTD) + GRF(ISTD,ICLS) * X
  200 CONTINUE
C

      IF (TOTAL .GT. 1.0E-9) THEN
        GRFSTD(ISTD)= GRFSTD(ISTD) / TOTAL
        DO 210 IDV= 1,NUMRV
           DVRV(ISTD,IDV) = DVRV(ISTD,IDV) / TOTAL
  210   CONTINUE        
      ELSE
        GRFSTD(ISTD)= 1.0
        DO 215 IDV= 1,NUMRV
           DVRV(ISTD,IDV) = 1.0
  215   CONTINUE        
      ENDIF  
C *************************************************************************
C...The following section of code calculates stand-level GRF due to
C   stand density effects, and then updates the total stand GRF accordingly
C...Modified 6/25/99; and again 7/9/99... and again 7/19/99, and again 7/20

      DNCF1= -0.033
      DNCF2= 3.0

      BASTD(ISTD)= BAH(ISTD,NSCL+1) + BANH(ISTD,NSCL+1)
      RVDNST(ISTD)= 2 - (1.9 / (1 + (9 * EXP(BASTD(ISTD) * DNCF1)))
     & **DNCF2)

c...the following logical flag added so that keyword RVDENSE still works
c   with this new stand-density-RV function.  GFRFSTD will use the newly-
c   calculated RVDNST if keyword RVDENSE is NOT used (if RVDENSE  is used
c   LCDENS is set to false in BMPPIN.) AJM 1/31/00.

      IF(LCDENS) THEN
         GRFSTD(ISTD)=GRFSTD(ISTD) * RVDNST(ISTD)
         DVRV(ISTD,9) = RVDNST(ISTD)
      ELSE 
         DVRV(ISTD,9) = 1.0
      ENDIF
C *************************************************************************
c     temporarily disable grfstd()   may 4, 93
c                            
c      GRFSTD(ISTD)= 1.0      



      
C.... Common return

 9999 CONTINUE

      
      IF(LBMDEB) WRITE(JBMBPR,99)IYR
   99 FORMAT(' End BMCGRF: Year = ',I4)

      RETURN
      END
