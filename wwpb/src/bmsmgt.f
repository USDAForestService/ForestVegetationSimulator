      SUBROUTINE BMSMGT (IYR)
      
c     CALLED FROM BMDRV
***********************************************************************
* **BMSMGT    Date of last revision:  June 22, 1994
*
*  Routine to remove fresh slash (created from FVS-level harvest or BM-
*  model salvage cut (but not from sanititation cut)). Model removes
*  user-defined proportion of DOWNED (only) dead wood from host and non-
*  host pools.
*
*  Definitions:  
*     ISC:    Loop counter over size classes
*     JSC:    Size class < 3 or > 3
*     PSLREM: Proportion of slash to remove that is < 3 or >3 in
*                 -given by keyword.
*
*  Common block variables and parameters:
***********************************************************************

C.... Parameter statements.

C.... Parameter include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PPEPRM.F77' 
      INCLUDE 'PPCNTL.F77'
      INCLUDE 'BMPRM.F77'
      INCLUDE 'BMCOM.F77'

C.... Variable declarations.
      
      INTEGER ISC, JSC
      INTEGER SCNT, MYLST(MXSTND)
      LOGICAL LOK
      REAL    PRMS(2)
      REAL    PSLREM(2)
      
      SAVE

      IF(LBMDEB) WRITE(JBMBPR,10) IYR
   10 FORMAT(' Begin BMSMGT: Year= ',I5)

C     Initializations

      IYR1 = 0
      NPRMS = 2
      IYR1 = IYR
      CALL GPGET2 (315,IYR1,7,NPRMS,PRMS,MXSTND,SCNT,MYLST,LOK)      
      IF (LOK) THEN 
        PSLREM(1) = PRMS(1)
        PSLREM(2) = PRMS(2)
        
        DO 200 I = 1, SCNT
          
          ISTD = MYLST(I)
          IF (.NOT.STOCK(ISTD) .OR. ISTD .LE. 0) GOTO 200
          
C         Remove some dead wood from downed dead wood, but only for the
c         current year.
c         (remember that DWPHOS is all > 3in, but is in 3 size classes and
c          DDWP is only <3 or > 3in)

          DO 45 ISC= 1, MXDWHZ                                    
            JSC = MIN0(ISC,MXDWSZ)

            DWPHOS(ISTD,2,ISC) = DWPHOS(ISTD,2,ISC) * (1 - PSLREM(2))

            IF (ISC .LE. MXDWSZ) 
     >          DDWP(ISTD,ISC,1) = DDWP(ISTD,ISC,1) * (1 - PSLREM(JSC))

   45     CONTINUE
  
  200   CONTINUE
      ENDIF
      
      IF(LBMDEB) WRITE(JBMBPR,99) IYR
   99 FORMAT(' End BMSMGT: Year= ',I5)
      
      RETURN
      END
