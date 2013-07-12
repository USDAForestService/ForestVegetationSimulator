      SUBROUTINE BRESTB(TIME,ITYP,ISSP)
C**********************************************************************
C  **BRESTB       DATE OF LAST REVISION:  06/21/2013
C----------------------------------------------------------------------
C  Purpose:
C  BRESTB sets the Blister Rust variables for new trees that are
C  added by the FVS REGENeration system. It performs the following
C  operations:
C     1) Assigns initial stock type from parameter ITYP which
C        is a hard-coded value in the CALL statement in the FVS
C        subroutine ESTAB.
C     2) Sets tree age from parameter TIME.
C     3) Sets all other tree-specific Blister Rust variables,
C        except "actual" stock type which is determined in
C        subroutine BRSTYP.  Trees assigned stock type 5 in
C        this routine, will be assigned stock type 1, 2, 3 or 4
C        in BRSTYP based on the current stocking mix.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  14-SEP-2000 Lance David (FHTET)
C     Transfered Glen Brink's July, 2000 modifications from older version
C     of blister rust source code:
C     Implemented blister rust host species index array, ISPBR.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed ISPBR to BRSPM.
C
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable declarations.

      LOGICAL DEBUG
      INTEGER ITYP,ISSP
      REAL CRLEN,TIME

C.... Is debug requested?

      CALL DBCHK(DEBUG,'BRESTB',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,10) ICYC
   10 FORMAT('Entering subroutine BRESTB: cycle = ',I2)

C.... If tree species is not host, no processing is performed.
C.... If Stock Type is 5, it will be reset in BRSTYP.

      IF(BRSPM(ISSP) .GT. 0) THEN
         UPMARK(ITRN)=10000.0
         RI(ITRN)=0.0
         BRPB(ITRN)=0.0
         ESTCAN(ITRN)=0.0
         ITCAN(ITRN)=0
         ILCAN(ITRN)=0
         BRAGE(ITRN)=TIME
         IIAGO=IFIX(TIME)
         ISTOTY(ITRN)=ITYP
         IBRTID(ITRN)=IDTREE(ITRN)
         ICRED(ITRN)=0
         IBRSTAT(ITRN)=0
         BRHT=HT(ITRN)*0.3048
         BRDBH=DBH(ITRN)*2.54

C....    The tree's ground diameter is calculated if the tree
C....    height is > or = 2.0 meters, otherwise ground diameter
C....    is DBH is used.

         IF(BRHT.LT.2.0) THEN
            BRGD(ITRN)=BRDBH
         ELSE
            BRGD(ITRN)=(100*BRHT*BRDBH)/(100*(BRHT-1.14))
            IF(BRGD(ITRN).LT.BRDBH) BRGD(ITRN)=BRDBH
         ENDIF

         IF(ICR(ITRN).GT.0) THEN

C....       Crown ratio is supplied; calculate height to base of crown.

            CRLEN=BRHT*(FLOAT(ICR(ITRN))/100.0)
            BRHTBC(ITRN)=(BRHT-CRLEN)*100.0
         ELSE

C....       Assume the base of crown is at ground level.

            BRHTBC(ITRN)=0.0
         ENDIF

C....    Call BRGI to calculate growth index and sum target up to the
C....    current cycle.

         CALL BRGI(IIAGO,BRHT,GIBR,TBSUM)
         GI(ITRN)=GIBR
         TSTARG(ITRN)=TBSUM
      ENDIF

C.... Common return.

      IF(DEBUG) WRITE(JOSTND,300) ICYC
  300 FORMAT('Leaving subroutine BRESTB: cycle = ',I2)
      RETURN
      END
