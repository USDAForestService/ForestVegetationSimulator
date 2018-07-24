      SUBROUTINE BMSANI (IYR)
C----------
C WWPB $Id$
C----------
c     CALLED FROM BMDRV
c     CALLS:  BMSLSH
***********************************************************************
* **BMSANI    Date of last revision:  09/28/05
***********************************************************************
*             revised 7/28/05: added new var: REMBKP(ISTD)to bookkeep the BKP removed 
*             via sanitation cutting, by stand. In common.  For output.  AJM
*             REVISED 6/30/05.  TREE, BAH, and AREMS arrays should only be adjusted
*             when removing unattacked, low-RV trees.  COmmented out the adjustments 
*             happening to these arrays in the block where recently attacked trees
*             are being sanitized. AJM
*             Date of last revision 8/99(AJM).  Commented out block
*             referring to Target Volume removal method of calculating
*             amount to sanitize.  Target volume no longer a user-input
*             option.  The referred-to PRMS(6) does not exist.
* **BMSANI    Date of last revision:  October 28, 1998 (RNH, OCT98)
*             >>>>>Remove low RV trees from highst Size to lowest based on<<<<<
*             linearly varying efficiency
* **BMSANI    Date of last revision:  October 16, 1998 (RNH, OCT98)
*             Added target volume removal method for sanitize management
* **BMSANI    Date of last revision:  September 16, 1998 (RNH, May98)
*             Added target basal area method for sanitize management
* **BMSANI    Date of last revision:  August 5, 1998 (RNH, May98)
C             Passed MYLSTS, MAXSCS and MINSCS to BMKILL to track
C             Sanitize removals in BMKILL
* **BMSANI    Date of last revision:  June 21, 1994
* **BMSANI    Date of last revision:  May 6, 1998 (RNH, May98)
C            Major revision to run Sanitize as stand level activity,
C            similar to SALVAGE keyword.  Also modified BMPPIN to
C            pass user specified stand list, from keyword file or
C            from user specified file, to BMSANI like SALVAGE keyword
*
*  Sanitation cuts are a landscape-level action. If conditions are right,
*  the cut could occur anywhere in the landscape. All beetle-killed trees
*  and low RV trees within the given size range will be removed.
*
*  NOTE THAT WE MAY WANT TO CHANGE THIS TO MAKE IT STAND-ACTIVATED (LIKE PHEROMONES)
C            Done May98 by RNH
*
*  Definitions:
*     IPC:    Loop counter over pool types (fast, med, slow)
*     ISIZ:   Loop counter over size classes
*     MINSC:  Minimum size class for cut
*     MAXSC:  Maximum size class for cut
*     RVMAX:  Maximum RV that will be removed (of unattacked trees)
*     REMOVE: Amount of TPA removed from one size class of recent kills
*     SUM:    Total amount of standing dead volume (all classes)
*     VREMOV: Amount of standing dead volume removed over size classes
*
*  Common block variables and parameters:
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77'
      INCLUDE 'PPCNTL.F77'
      INCLUDE 'BMPRM.F77'
      INCLUDE 'BMCOM.F77'
      INCLUDE 'BMPCOM.F77' !AJM 9/05 FOR AAREMS AND VOLREM
C.... Variable declarations.

      INTEGER ISIZ, IPC
      INTEGER MINSC, MAXSC
      INTEGER DUM(1), INDEX(NSCL)
C
C     Add required Variables for stand level simulation (RNH May98)
C
      INTEGER MYLST(MXSTND), SCNT
C
      LOGICAL LOK
      REAL    REMPB, REMIPS
      REAL    REMOVE, RVMAX
      REAL    SUM
      REAL    VREMOV, TARVR
      REAL    PRMS(7), AA(NSCL)

      REAL    THRVOL, EFFC

      SAVE
C
C
      IF(LBMDEB) WRITE(JBMBPR,10) IYR, ISTD
   10 FORMAT(' Begin BMSANI: Year= ',I5, 'Stand= ', I6)

C     Initialization
C
C      Move to loop 500
C
C      IPC = IQPTYP(ISTD,1)
C      SUM = 0.0
C      VREMOV = 0.0

C     KLUDGE TO GET AROUND COUNTING OF NONSTOCKABLE STANDS.
C
C      IF (ICNT .GE. BMEND) ICNT = 0
C      ICNT = ICNT + 1
C
C      IF (ICNT .EQ. 1) THEN
C
C
C     Zero out MYLST array from last year (RNH 10Aug98)
C
      DO 11 IJK= 1, MXSTND
      MYLST(IJK)= 0.0
      VOLREM(IJK,1)=0.0 !NEW AJM 9/05
         DO 12 ISIZ= 1, NSCL
            AAREMS(IJK,ISIZ)= 0.0  !NEW AJM 9/05  THIS WILL BE JUST LIKE AREMS ONLY ZEROED ANNUALLY FOR USE IN BMOUT
   12    CONTINUE   
   11 CONTINUE
C
      IYR1= IYR
      NPRMS= 6
      MAXPRM = 7       !   From SALVAGE (RNH)
C
      CALL GPGET2 (308,IYR1,MAXPRM,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)
C       CALL GPGET2 (308, IYR1, 7, NPRMS, PRMS, 1, I, DUM, LOK)
C
C     Set LOKS to control Sanitize accounting in BMKILL (RNH Aug98)
C     LOKS should remain TRUE thoughout the cycle to trigger calculations
C     in BMKILL.  LOKS1 is set to FALSE in BMKILL to signal end of SANI action

        LOKS = LOK
        IF (LOKS) LOKS1 = .TRUE.
        IF (LOKS1) LOKS = .TRUE.

        IF (LOK) THEN

          MINSC = INT(PRMS(1))
          MAXSC = INT(PRMS(2))
          RVMAX = PRMS(3)
          THRVOL = PRMS(4)
          EFFC = PRMS(5)
          TARVR= PRMS(6)
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C     The following variables are set here to be passed to BMKILL to
C     track Sanitization removals in the PPE (MAXSCS, MINSCS,
C     MYLSTS) (RNH Aug98)
C
      MINSCS = MINSC
      MAXSCS = MAXSC
C
C     Append the MYLSTS array with the current years sanitiation
C     removals from MYLST
C
      IJ0= 1
      DO 19 IJ= 1, MXSTND
      IF(MYLSTS(IJ) .NE. 0) GO TO 19
      MYLSTS(IJ) = MYLST(IJ0)
      IJ0= IJ0 + 1
   19 CONTINUE
C++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
          IF (LBMDEB) WRITE (JBMBPR,71) MICYC, IYR1, MINSC, MAXSC,
     >                                  RVMAX, THRVOL, EFFC
   71     FORMAT (' BMSANI: MICYC=', I5,' IYR1=',I5,' MINSC=',I4,
     >      ' MAXSC=',I4,' RVMAX=',F5.2,' THRVOL=',F8.0,' EFFC=',F5.3)

         ELSE
          EFFC = 0.0
        ENDIF
C      ENDIF

      IF (EFFC .LE. 0.0) RETURN

C     Main loop to calculate stands +++++++++++++++++++++++++++++
C
C     The folling loop over SCNT was suggested from BMSALV
C
      DO 500 I = 1, SCNT
C
          ISTD = MYLST(I)
          IPC = IQPTYP(ISTD,1)
          IF (.NOT.STOCK(ISTD).OR.ISTD.LE.0) GOTO 500
C
C     Total recently killed volume or low RV volume available for cut
C
      VREMOV = 0.0
      SUM = 0.0
      SUMBA = 0.0
C
C  Calc. total basal area of stand (SBAT10)
C
C      SBAT10 = 0.0
C      DO 14 ISBA1 = 1, NSCL
C      BAHPNH = BAH(ISTD,ISBA1) + BANH(ISTD,ISBA1)
C      SBAT10 = SBAT10 + BAHPNH
C   14 CONTINUE
C
      DO 20 ISIZ= MINSC,MAXSC
         SUM = SUM + (PBKILL(ISTD,ISIZ) + ALLKLL(ISTD,ISIZ))
     &                         * TVOL(ISTD,ISIZ,1)*0.90
         IF (GRF(ISTD,ISIZ) .LE. RVMAX)
     &       SUM = SUM + TREE(ISTD,ISIZ,1) * TVOL(ISTD,ISIZ,1)*EFFC
C
C
   20 CONTINUE
C
C     If not enough volume to warrent a sanitation cut then
      IF (SUM .LT. THRVOL) GO TO 500
C
C     If the target basal area method of sanitiation is being used
C     TARBA < 999. then start calculations below.  Else, brance to
C     statement 35 to begin sanitation operations based on maximum
C     rating value method TARBA >= 999. (RNH sep98)
C
C      IF (TARVR .GE. 9999.) GO TO 35
C
C      WRITE (*,*) SUM,THRVOL
      GO TO 35
C                                                                                    !
C...  Above change made 8/99 (AJM)                                                   !
C                                                                                    !
                                                                                     !
C     New variables                                                                  !
C     NN        = number of size classes to be included in sanitization              !
C     AA(MAXSC) = scatch array to pass rating values of size classes                 !
C                 (MINSC TO MAXSC) to sort routine                                   !
C     TARVR     = stand target removal volume specified by user in input             !
C     INDEX(NN) = passed fromn sort routine this array holds the subscrips           !
C                 of AA in order of accending RV                                     !
C     INDRV     = subscript values from INDEX transformed to range                   !
C                 MINSC - MAXSC for use in other arrays                              !
C     SRVOL1    = temporary variable holding removal volumes, used to compare        !
C                 with TARVR                                                         !
C                                                                                    !
C     The following routine removes infected trees first.  Then the                  !
C     tree size classes are removed based on rating value, with the                  !
C     lowest rating value trees cut first, until the target removal volume           !
C     (TARVR) for the stand is reached.                                              !
C                                                                                    !
C     First take out all infected trees                                              !
C                                                                                    !
      REMLRV= 0.0                                                                    !
      SRVOL1= 0.0                                                                    !
                                                                                     !
      DO 22 ISIZ= MINSC,MAXSC                                                        !
C                                                                                    !
         REMPB = PBKILL(ISTD,ISIZ) * 0.90                                            !
         PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) - REMPB                               !
C                                                                                    !
         REMIPS = ALLKLL(ISTD,ISIZ) * 0.90                                           !
         ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) - REMIPS                              !
C                                                                                    !
C     Changed removal efficiency of beetle killed trees to 0.90 obove                !
C     Accounted for these removal in tree record before removing low                 !
C     RV trees.  calculated reduction in basal area properly (RNH19Aug)              !
C                                                                                    !
      TREE(ISTD,ISIZ,1) = TREE(ISTD,ISIZ,1) - REMPB - REMIPS                         !
      BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) - BAH(ISTD,ISIZ)*(REMPB + REMIPS)               !
C                                                                                    !
      SRVOL1= SRVOL1 +(REMPB + REMIPS)*TVOL(ISTD, ISIZ, 1)                           !
                                                                                     !
         REMOVE = REMPB + REMIPS + REMLRV                                            !
         VREMOV = VREMOV + REMOVE * TVOL(ISTD,ISIZ,1)                                !
                                                                                     !
C        Also remove some wood from the current standing dead pools because          !
c        the killed trees were added to these pools last year (so use subscript      !
c        2 which means age 1). Multiply REMOVE by the decay factor to account        !
c        for one year's decay. Note that SDWP is measured in volume.                 !
                                                                                     !
         JSC = L2D(ISIZ) + 1                                                         !
         JJ = MIN0(JSC,MXDWSZ)                                                       !
         SDWP(ISTD,IPC,JSC,2) = SDWP(ISTD,IPC,JSC,2) - (REMOVE - REMLRV)             !
     &                                  * TVOL(ISTD,ISIZ,1) * SDECRT(JJ)             !
                                                                                     !
         SREMOV(ISTD) = SREMOV(ISTD) + REMOVE                                        !
      AREMS(ISTD,ISIZ)= AREMS(ISTD,ISIZ) + REMPB + REMIPS                            !
C                                                                                    !
   22 CONTINUE                                                                       !
C                                                                                    !
C*************                                                                       !
C     Take out rating value ranking code and remove trees from largest               !
C     size class to lowest based on linearly varying efficiencyN (EFFCLN)            !
C     RNH October 28, 1998                                                           !
C*************                                                                       !
C                                                                                    !
   27 CONTINUE             ! Branch from end of loop if TARBA not met                !
C                                                                                    !
      DO 28 IJK= MAXSC, MINSC, -1                                                    !
C                                                                                    !
C     calculate efficiency based on size class.  Want maximum size class             !
C     cut at input efficiency and size class 3 cut at efficiency= 0.0                !
C     The efficiency for cutting size classes (EFFCLN) in between                    !
C     should vary linearly                                                           !
C                                                                                    !
      IF (IJK .GT. 3) THEN                                                           !
      EFFCLN= EFFC*(IJK-3)/(MAXSC-3)                                                 !
      ELSE                                                                           !
      EFFCLN= 1.0E-6                                                                 !
      ENDIF                                                                          !
C      WRITE(29,*) ' SIZCLASS= ',IJK,' EFFCLN= ',EFFCLN,' ISTD= ',ISTD               !
C      WRITE(29,*)' MINSC= ',MINSC,' MAXSC= ',MAXSC                                  !
C                                                                                    !
      IF (GRF(ISTD,IJK) .LE. RVMAX) THEN                                             !
C                                                                                    !
       IF (SRVOL1 .LT. TARVR) THEN                                                   !
C                                                                                    !
C     SRVOL2 is temporary storage for SRVOL1 in refiinement of removals              !
C     to meet TARVR                                                                  !
C                                                                                    !
       SRVOL2 = SRVOL1                                                               !
C                                                                                    !
       REMLRV= TREE(ISTD,IJK,1)*EFFCLN                                               !
C                                                                                    !
      IF(REMLRV .LE. 1.0E-6) REMLRV= 0.0                                             !
C                                                                                    !
C     Accumulate removals in SRVOL1                                                  !
C                                                                                    !
      SRVOL1= SRVOL1 + REMLRV*TVOL(ISTD, IJK, 1)                                     !
C                                                                                    !
C      If removal accumulates above TARVR then use efficiency to refine              !
C      removal volume to approximate TARVR                                           !
C                                                                                    !
        IF (SRVOL1 .GT. TARVR) THEN                                                  !
                                                                                     !
        EFFCLN1= (TARVR - SRVOL2)/(TREE(ISTD,IJK,1)*TVOL(ISTD,IJK,1))                !
C                                                                                    !
C       Recalculate REMLRV based on refined efficiency estimate                      !
C                                                                                    !
        REMLRV= TREE(ISTD,IJK,1)*EFFCLN1                                             !
        SRVOL1 = SRVOL2 + REMLRV*TVOL(ISTD,IJK,1)                                    !
        BAH(ISTD,IJK)= BAH(ISTD,IJK)*(1-EFFCLN1)                                     !
        IJK1= IJK                                                                    !
C                                                                                    !
c                                                                                    !
        GO TO 1010                                                                   !
C                                                                                    !
        END IF                                                                       !
C                                                                                    !
      IF(BAH(ISTD,IJK) .LE. 1.0E-6) BAH(ISTD,IJK) = 0.0                              !
C                                                                                    !
                                                                                     !
       BAH(ISTD,IJK)= BAH(ISTD,IJK) * (1 - EFFCLN)                                   !
C                                                                                    !
 1010 CONTINUE                                                                       !
C                                                                                    !
C+++++++++++++                                         ++++++++++++++                !
C      Accumulate removal data to pass to BMKILL for Sanitize accounting             !
C      in PPE (RNH Aug98)                                                            !
C                                                                                    !
       AREMS(ISTD,IJK)= AREMS(ISTD,IJK) + REMLRV                                     !
C                                                                                    !
C++++++++++++                                          ++++++++++++++                !
C                                                                                    !
       TREE(ISTD,IJK,1) = TREE(ISTD,IJK,1) - REMLRV                                  !
C                                                                                    !
       REMOVE = REMLRV                                                               !
       VREMOV = VREMOV + REMOVE * TVOL(ISTD,IJK,1)                                   !
C                                                                                    !
C        Also remove some wood from the current standing dead pools because          !
c        the killed trees were added to these pools last year (so use subscript      !
c        2 which means age 1). Multiply REMOVE by the decay factor to account        !
c        for one year's decay. Note that SDWP is measured in volume.                 !
C                                                                                    !
C         JSC = L2D(IJK) + 1                                                         !
C         JJ = MIN0(JSC,MXDWSZ)                                                      !
C         SDWP(ISTD,IPC,JSC,2) = SDWP(ISTD,IPC,JSC,2) - (REMOVE - REMLRV)            !
C     &                                  * TVOL(ISTD,IJK,1)*SDECRT(JJ)               !
C                                                                                    !
         SREMOV(ISTD) = SREMOV(ISTD) + REMOVE                                        !
C                                                                                    !
      ELSE                                                                           !
C                                                                                    !
C      Met TARVR criterium, finished with sanitize                                   !
C                                                                                    !
C      WRITE(29,*) ' SRVOL1= ',SRVOL1,' SREMOV= ', SREMOV(ISTD)                      !
C      WRITE(29,*) ' VREMOV= ',VREMOV                                                !
C                                                                                    !
       GO TO 50                                                                      !
       END IF                                                                        !
      END IF                                                                         !
   28 CONTINUE                                                                       !
C                                                                                    !
C     If loop through stand did not meet TARVR criterium then loop through again     !
C                                                                                    !
C                                                                                    !
      IF (SRVOL1 .LT. TARVR) GO TO 27                                                !
C                                                                                    !
C                                                                                    !
      GO TO 50                 !End of TARVR sanitize method loop                    !
C                                                                                    !
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++                     !
C                                                                                    !
C     Branch from TARBA test (ie method of sanitization)                             !
CC     Target stand removal volume method of Sanitization                             
   35 CONTINUE
C
C     Insert section of code from above defining EFFCLN (8/10/99, AJM).
C
C      IF (IJK .GT. 3) THEN
C       EFFCLN= EFFC*(IJK-3)/(MAXSC-3)
C       ELSE
C       EFFCLN= 1.0E-6
C      ENDIF
C
C     Maximum rating value mehtod of Sanitation
C     Remove trees from each appropriate size class based on the
C     desired efficiency
C
      DO 40 ISIZ= MINSC,MAXSC
C         REMPB = PBKILL(ISTD,ISIZ) * EFFC
         REMPB = PBKILL(ISTD,ISIZ) * 0.90
         PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) - REMPB                   ! REMOVE ATTACKED TREES

C         REMIPS = ALLKLL(ISTD,ISIZ) * EFFC
         REMIPS = ALLKLL(ISTD,ISIZ) * 0.90
         ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) - REMIPS                   !DITTO
C
C     New var for bookkeeping bkp removal (FOR OUTPUT). ajm 7/05
C     Note: this assumes that each dead beetle-killed tree removed via sanitation 
C     (i.e. the decrements from the PBKILL array) is fully occupied with BKP.  \

         REMBKP(ISTD) = REMBKP(ISTD) + REMPB * MSBA(ISIZ)                !WHERE IS THIS ZEROED?AJM

C     Changed removal efficiency of beetle killed trees to 0.90 obove
C     Accounted for these removal in tree record before removing low
C     RV trees.  calculated reduction in basal area properly (RNH19Aug)
C
C COMMENTING OUT THE NEXT THREE LINES AJM 6/30/05
C TREE AND BAH ARRAYS WERE ALREADY DECREMENTED IN BMISTD IF THE TREES WERE BEETLE-KILLED,
C WHICH THIS SECTION IS REMOVING.  LIKEWISE, AREMS IS BEING BOOKEPT *ONLY* FOR 
C POSTING TO BASE-FVS WK2 ARRAY IN BMKILL.  SINCE THESE TREES REMOVED VIA SANIT
C ARE ALREADY DEAD, THEY'VE ALREADY BEEN ADDED TO THE TPBK ARRAY, SO ARE ALREADY
C ACCOUNTED FOR.  REMOVALS POSTED TO WK2 ARRAY FROM SANI SHOULD ONLY REFLECT THOSE
C LIVE UNATTACKED TREES REMOVED VIA SANIT.  SEE NEXT IF...THEN BLOCK.
C
C      TREE(ISTD,ISIZ,1) = TREE(ISTD,ISIZ,1) - REMPB - REMIPS
C      BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) - MSBA(ISIZ)*(REMPB + REMIPS)
C      AREMS(ISTD,ISIZ)= AREMS(ISTD,ISIZ) + REMPB + REMIPS
C
C
         IF (GRF(ISTD,ISIZ) .LE. RVMAX) THEN                               !NOW REMOVE LOW RV TREES
C	    REMLRV = TREE(ISTD,ISIZ,1) * EFFCLN
            REMLRV = TREE(ISTD,ISIZ,1) * EFFC
C
C... Changed above formula [and commented out block above(ca lines 388-92)]
c     so that now efficiency is NOT modified by size class, but remains
c     constant at the user-specified level from field 7 in SANITIZE
c     keyword; default = 0.95 (set in BMPPIN).  AJM 8/11/99
C+++++++++++++                                         ++++++++++++++
C     Accumulate removal data to pass to BMKILL for Sanitize accounting
C     in PPE (RNH Aug98)
C
            AREMS(ISTD,ISIZ)= AREMS(ISTD,ISIZ) + REMLRV
            AAREMS(ISTD,ISIZ)= REMLRV                    ! NEW AJM 9/05 ANNUAL ACCUMULATOR REZEROED ANNUALLY (ABOVE) FOR OUTPUT
C            BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) * (1 - EFFCLN) ! COMMENT OUT AJM 6/30/05
C                           ! THIS SHOULD'VE BEEN CHANGED 8/99 WHEN EFFCLN WAS NIXED
            BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) * (1 - EFFC) ! ADD 6/30/05 AJM
C
C      BAH(ISTD,ISIZ)= BAH(ISTD,ISIZ) - MSBA(ISIZ)*EFFC
            TREE(ISTD,ISIZ,1) = TREE(ISTD,ISIZ,1) - REMLRV
         ELSE
            REMLRV = 0.0
         ENDIF
C
         REMOVE = REMLRV + REMPB + REMIPS
         VREMOV = VREMOV + REMOVE * TVOL(ISTD,ISIZ,1)

C        Also remove some wood from the current standing dead pools because
c        the killed trees were added to these pools last year (so use subscript
c        2 which means age 1). Multiply REMOVE by the decay factor to account
c        for one year's decay. Note that SDWP is measured in volume.

         JSC = L2D(ISIZ) + 1
         JJ = MIN0(JSC,MXDWSZ)
         SDWP(ISTD,IPC,JSC,2) = SDWP(ISTD,IPC,JSC,2) - (REMOVE - REMLRV)
     &                                  * TVOL(ISTD,ISIZ,1) * SDECRT(JJ)

         SREMOV(ISTD) = SREMOV(ISTD) + REMOVE

   40 CONTINUE
C
C+++++++++++++++++++++++++++++++++++++++++++++++++++++++
C
C     Branch from TARBA method
C
   50 CONTINUE
C
      VOLREM(ISTD,1) = VOLREM(ISTD,1) + VREMOV ! NEW ANNUAL ACCUMULATOR AJM 6/05
      CVOLREM(ISTD,1) = CVOLREM(ISTD,1) + VREMOV
C
C     Calculate amount of slash produced from sanitation

      CALL BMSLSH (IPC,1.0,VREMOV,ISTD)

C     Re-calculate GRF in case any low RV trees were removed.  
c                                        !note 7/28/05 ajm
                                         ! this step may not be necessary, because
      TOTAL = 0.0                        ! GRFs are recalculated after this sanitation (call to BMCGRF from BMDRV)
      GRFSTD(ISTD)= 0.0                  ! but before all of the BKP dynamics that needs the GRFs
      DO 200 ICLS=1,NSCL                 ! i.e. we don't need to derive this here and now
        X = BAH(ISTD,ICLS)
        TOTAL= TOTAL + X
        GRFSTD(ISTD)= GRFSTD(ISTD) + GRF(ISTD,ICLS) * X
  200 CONTINUE

      IF (TOTAL .GT. 1.0E-9) THEN
        GRFSTD(ISTD)= GRFSTD(ISTD) / TOTAL
      ELSE
        GRFSTD(ISTD)= 1.0
      ENDIF

      IF(LBMDEB) WRITE(JBMBPR,99) IYR, ISTD
   99 FORMAT(' End BMSANI: Year= ',I5, 'Stand= ', I6)
C
  500 CONTINUE
C
C     End of main stand loop +++++++++++++++++++++++++
C
      RETURN
      END
