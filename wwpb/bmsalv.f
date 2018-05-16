      SUBROUTINE BMSALV (IYR)
      
c     CALLED FROM BMDRV
c     CALLS:  BMSLSH 
***********************************************************************
* **BMSALV    Date of last revision:  09/28/05
C
C     Modified input data fields to coresspond with SANITISE keyword,
C     and modified SALVAGE keyword to consider minimum volume to
C     conduct Salvage operation (RNH May98)
C
*
*  Definitions:  
*     IAG:    Loop counter over age classes
*     IPC:    Loop counter over pool types (fast, med, slow)
*     ISC:    Loop counter over size classes
*     MINSC:  Minimum size class for cut
*     MAXSC:  Maximum size class for cut
*     REMOVE: Amount of standing dead volume removed for one age & size
*     SUM:    Total amount of standing dead volume (all classes)
C     THRVOL -min. Vol. to perform salvage (RNH MAy98)
*     VREMOV: Amount of standing dead volume removed over age and size classes
*
*  Common block variables and parameters:
***********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77' 
      INCLUDE 'PPCNTL.F77'
      INCLUDE 'BMPRM.F77'
      INCLUDE 'BMCOM.F77'
      INCLUDE 'BMPCOM.F77'

C.... Variable declarations.
      
      INTEGER ISC                                     
      INTEGER MINSC, MAXSC, MAXAGE 
      INTEGER SCNT, MYLST(MXSTND)
      INTEGER MINLIV, MAXLIV
      LOGICAL LOK
      REAL    PRMS(7)
      REAL    REMOVE
      REAL    VREMOV, EFFC, THRVOL
      
      SAVE
      
      IF(LBMDEB) WRITE(JBMBPR,10) IYR
   10 FORMAT(' Begin BMSALV: Year= ',I5)

C     Initializations

      IYR1 = 0
C      NPRMS = 4
C
C     Added one parameter so new total in 5 parameters (RNH May98)
      NPRMS = 5
      IYR1 = IYR
      MAXPRM = 7
      MAXLIV = -1
      MINLIV = -1
      CALL GPGET2 (318,IYR1,MAXPRM,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)      
      IF (LOK) THEN 
        MINSC = INT(PRMS(1))
        MAXSC = INT(PRMS(2))                                    
        MAXAGE = INT(PRMS(3))
        THRVOL = PRMS(4)       !  Min. Vol. to perform Salvage (RNH May98)
C        EFFC = PRMS(4)
C
C     Efficiency is now t 5th parameter in Field 7 of Keyword (RNH MAy98)
        EFFC = PRMS(5)
        IF (EFFC .GT. 1.0) EFFC = 1.0
        IF (EFFC .LT. 0.0) EFFC = 0.0
        
C       DON'T BOTHER DOING THE REST OF THE LOOP IF EFFICIENCY = 0        
        IF (EFFC .EQ. 0.0) RETURN

        DO 22 ISIZ = 1,NSCL
          IF (L2D(ISIZ) .EQ. MINSC .AND. MINLIV .EQ. -1) MINLIV = ISIZ
          IF (L2D(ISIZ) .EQ. MAXSC .AND. MAXLIV .EQ. -1) MAXLIV = ISIZ
   22   CONTINUE     
C
C     Beginning of main stand loop
C
        DO 200 I = 1, SCNT
          
          ISTD = MYLST(I)
          IF (.NOT.STOCK(ISTD).OR.ISTD.LE.0) GOTO 200
C
C     Check for Min. volume for salvage cut.  This routine was lifted
C     from an ESSA commented-out r0outine at the end of this module 
C     (RNH May98)          
C          
C          
      SUM = 0.0
C     Total standing dead volume available for cut
      DO 30 IPC= 1,MXDWPC
        DO 25 ISC= MINSC,MAXSC
C          DO 20 IAG= 1,MXDWAG   Fixed 10/01 ajm.  Only dead trees
c                                less than user-defined threshold should be
c                                added to eligibility pool!  User-entered age 
c                                {PRMS(3)=[MAXAGE]} is set to a class (1-5) in BMPPIN.
          DO 20 IAG= 1,MAXAGE
              SUM = SUM + SDWP(ISTD,IPC,ISC,IAG)
   20     CONTINUE
   25   CONTINUE
   30 CONTINUE                                  
      SUM = SUM * EFFC
      
c     If not enough volume to warrent a salvage cut then return
      IF (SUM .LT. THRVOL) GO TO 200
          
C         Remove some standing dead wood from various classes
          DO 50 IPC= 1,MXDWPC
            VREMOV = 0.0
            DO 45 ISC= MINSC,MAXSC
              DO 40 IAG= 1,MAXAGE
                REMOVE = SDWP(ISTD,IPC,ISC,IAG) * EFFC                  
                          
                VREMOV = VREMOV + REMOVE
                SDWP(ISTD,IPC,ISC,IAG) = SDWP(ISTD,IPC,ISC,IAG) - REMOVE
   40         CONTINUE
   45       CONTINUE
  
C           Calculate amount of slash produced from salvage

C            CALL BMSLSH (IPC,1,VREMOV,ISTD)
C           **** Integer argument "1" caused fortran error (arithmetic underflow...
C                passed 1E-45 into BMSLSH via TTREE). Changed to a real number in line below.
C                MJO March 1998

            CALL BMSLSH (IPC,1.0,VREMOV,ISTD)

            VOLREM(ISTD,2) = VOLREM(ISTD,2) + VREMOV      
   50     CONTINUE 
   
c         For bookkeeping purposes, make sure to remove some PBKILL and ALLKILL
c         since they would have been part of the removals in the youngest age class.
          
          DO 52 ISIZ= MINLIV,MAXLIV
             PBKILL(ISTD,ISIZ) = PBKILL(ISTD,ISIZ) * (1 - EFFC)
             ALLKLL(ISTD,ISIZ) = ALLKLL(ISTD,ISIZ) * (1 - EFFC)
   52     CONTINUE
      
  200   CONTINUE
      ENDIF
      
c      SUM = 0.0
C     Total standing dead volume available for cut
c      DO 30 IPC= 1,MXDWPC
c        DO 25 ISC= MINSC,MAXSC
c          DO 20 IAG= 1,MXDWAG    
c              SUM = SUM + SDWP(ISTD,IPC,ISC,IAG)
c    20     CONTINUE
c   25   CONTINUE
c   30 CONTINUE                                  
C      SUM = SUM * EFFC
      
c     If not enough volume to warrent a salvage cut then return
c      IF (SUM .LT. THRVOL) RETURN
        

      IF(LBMDEB) WRITE(JBMBPR,99) IYR
   99 FORMAT(' End BMSALV: Year= ',I5)
      

      RETURN
      END
