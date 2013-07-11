      SUBROUTINE BRTRIP(INUT,I,WT)
C**********************************************************************
C  **BRTRIP       DATE OF LAST REVISION:  06/21/2013
C----------------------------------------------------------------------
C  Purpose:
C  Triples the Blister Rust records.
C----------------------------------------------------------------------
C
C  Parameters passed:
C     INUT - array index of new tree (record) being created.
C     I    - array index of tree (record) from which new tree is being
C            created.
C     WT   - Weight being assigned to new tree to determine number of
C            trees being represented by this new record. When value is
C            0.6, the index values INUT and I should be the same because
C            the original tree PROB (number of tree represented) is
C            being adjusted.
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C
C  20-APR-1999 Lance David
C     Comments for code and parameters only.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable definitions.

      LOGICAL DEBUG

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRTRIP',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,112) ICYC
  112 FORMAT ('Entering subroutine BRTRIP: cycle = ',I2)

C.... The WT value of 0.6 the weight at which the original record is set
C.... when tripling occurs. This line should not be necessary. 20-APR-1999
C.... IF(WT.EQ.0.6) INUT=I

      UPMARK(INUT)=UPMARK(I)
      BRHTBC(INUT)=BRHTBC(I)
      GI(INUT)=GI(I)
      RI(INUT)=RI(I)
      BRGD(INUT)=BRGD(I)
      BRPB(INUT)=BRPB(I)*WT
      ESTCAN(INUT)=ESTCAN(I)
      TSTARG(INUT)=TSTARG(I)
      ITCAN(INUT)=ITCAN(I)
      ILCAN(INUT)=ILCAN(I)
      BRAGE(INUT)=BRAGE(I)
      ISTOTY(INUT)=ISTOTY(I)
      IBRTID(INUT)=IBRTID(I)
      ICRED(INUT)=ICRED(I)
      IBRSTAT(INUT)=IBRSTAT(I)

C.... Load cankers for new tree.

      NNCAN=ILCAN(I)

      DO 77 J=1,NNCAN
         DUP(J,INUT)=DUP(J,I)
         DOUT(J,INUT)=DOUT(J,I)
         GIRDL(J,INUT)=GIRDL(J,I)
         ISTCAN(J,INUT)=ISTCAN(J,I)
   77 CONTINUE

      IF(DEBUG) WRITE(JOSTND,114) ICYC
  114 FORMAT ('Leaving subroutine BRTRIP: cycle = ',I2)
      RETURN
      END
