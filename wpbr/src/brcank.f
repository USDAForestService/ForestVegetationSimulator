      SUBROUTINE BRCANK
C**********************************************************************
C  **BRCANK       DATE OF LAST REVISION:  04/03/2001
C----------------------------------------------------------------------
C  Purpose:
C   BRCANK reads the canker data provided in the keyword list or
C   canker data file.
C   This routine is called from the subroutine BRIN.
C----------------------------------------------------------------------
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C
C  14-APR-1999 Lance R. David
C     Added code to process comments marked with * or ! in
C     the canker data file. * = echoed. ! = skipped.
C  03-APR-2001 Lance R. David (FHTET)
C     Changed order of variables and added stock type to canker file
C     read.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'HTCAL.F77'
      INCLUDE 'ESTREE.F77'
      INCLUDE 'BRCOM.F77'

C.... Local variable declarations.

      CHARACTER*80 CREC
      LOGICAL DEBUG
      INTEGER IBRID,ISTK,JJ
      REAL    CAGE,CNKCNT,COUT,CUP,GGIRD

C.... See if we need to do some debug.

      CALL DBCHK(DEBUG,'BRCANK',6,ICYC)
      IF(DEBUG) WRITE(JOSTND,10) ICYC,ICIN,ICFMT
  10  FORMAT(' Entering subroutine BRCANK: cycle = ',I2,/,
     &       ' ICIN=',I3,' ICFMT=',A)

C.... Read canker records. Note that all canker records will be read but
C.... a maximum of 10 cankers/tree will be tracked in the lethal canker
C.... array.  Also, a total canker count record is required and must be
C.... the last record listed for the tree.  This count will be set in
C.... the total canker count array for the tree but not loaded into the
C.... Blister Rust canker arrays.
C.... Tree's age is set to Prognosis stand age in subroutine BRSOR if
C.... tree age is not supplied on the canker record.

C.... Top of the canker record loop.
C.... If reading from an external file, read until the end of the file
C.... is reached.  If reading from supplemental records following the
C.... CANKDATA keyword, read until a "-999" is reached.
C.... Comments may exist in the canker data file. Records with a "*" or "!"
C.... in column 1 are comments. Records with a "*" are written to FVS
C.... standard output file. Records with "!" are not, just skipped.

   90 CONTINUE

      READ (ICIN,91,END=400) CREC
   91 FORMAT (A)
      IF (CREC(1:1).EQ.'!') GOTO 90
      IF (CREC(1:1).EQ.'*') THEN
         WRITE (JOSTND,'(/,T13,A)') CREC
         GOTO 90
      ENDIF

      IBRID = 0
      ISTK  = 0
      CAGE  = 0.0
      CUP   = 0.0
      COUT  = 0.0
      GGIRD = 0.0
      CNKCNT= 0.0

C     READ(CREC,ICFMT) IBRID,UP,OUT,GGIRD,AGE,CNKCNT
      IF(DEBUG) WRITE(JOSTND,*) CREC
      READ(CREC,ICFMT) IBRID,ISTK,CAGE,CUP,COUT,GGIRD,CNKCNT
      IF(IBRID.EQ.-999) GO TO 400

C.... If necessary, convert distance up from feet to centimeters
C.... and distance out from inches to centimeters.

      IF(.NOT.LMETRIC) THEN
         CUP=CUP*30.48
         COUT=COUT*2.54
      ENDIF

C.... Keep track of tree records encountered. Canker data will be
C.... loaded into the blister rust arrays according to unique
C.... tree IDs and then sorted back into the same arrays according
C.... to the internal tree numbers generated in the FVS model.

      IF(INCAN.GT.0) THEN
         DO 110 JJ=1,INCAN
            IF(IBRID.EQ.IBRTID(JJ)) GO TO 120
  110    CONTINUE
      ENDIF
      INCAN=INCAN+1
      IF(INCAN.GT.MAXTRE) CALL ERRGRO(.FALSE.,13)
      IBRTID(INCAN)=IBRID
      JJ=INCAN
  120 CONTINUE

C.... If UP, OUT, and % girdle are all zeros, this record was
C.... used to provide the, stock type, age, and total canker count
C.... for this tree.

      IF(CUP.LE.0.0.AND.COUT.LE.0.0.AND.GGIRD.LE.0.0) THEN

C....    Set tree age if not 0.

         IF(CAGE.NE.0.0) BRAGE(JJ)=CAGE

C....    Set Stock type if provided.
C....    Default stock type is 5 which will be reassigned based on
C....    proportion of stock values in BRSTYP subroutine.

         IF(ISTK.GE.1 .AND. ISTK.LE.5) THEN
           ISTOTY(JJ)=ISTK
         ELSE
           ISTOTY(JJ)=5
         ENDIF

C....    ITCAN keeps a total count of cankers on the tree (lethal and
C....    non-lethal) no matter how many canker records are entered.
C....    Set ITCAN to the canker count number read from the canker
C....    list.  This canker count record is required to be the last one
C....    listed for the tree - that way we can make sure the count is
C....    at least at large as the number of lethal cankers read in.

         IF(CNKCNT.GT.0) ITCAN(JJ)=CNKCNT
         IF(ITCAN(JJ).LT.ILCAN(JJ)) ITCAN(JJ)=ILCAN(JJ)
         IF(DEBUG) WRITE(JOSTND,*) ' ID=',IBRTID(JJ),' AGE=',BRAGE(JJ),
     &     ' STOCK=',ISTOTY(JJ),' ITCAN=',ITCAN(JJ),' ILCAN=',ILCAN(JJ)
         GO TO 90
      ENDIF

C.... Increment the counter for "lethal cankers".

      ILCAN(JJ)=ILCAN(JJ)+1
      NN=ILCAN(JJ)
      IF(NN.GT.10) THEN

C....    A maximum of 10 cankers per tree will be tracked by the model
C....    in the "lethal cankers" array. If we've passed that maximum
C....    then subtract one.

         ILCAN(JJ)=ILCAN(JJ)-1
         GO TO 90
      ENDIF

C.... Load the canker arrays. This data will be sorted in subroutine
C.... BRSOR to keep the canker data in synch with FVS tree data.
C.... Percent girdle for branch cankers is set to zero and cankers
C.... cannot have a negative distance out.

      DUP(NN,JJ)=CUP

      IF(COUT.LT.0.0) THEN
         DOUT(NN,JJ)=0.0
      ELSE
         DOUT(NN,JJ)=COUT
      ENDIF

      IF(COUT.GT.0.0) THEN
         GIRDL(NN,JJ)=0.0
      ELSE
         GIRDL(NN,JJ)=GGIRD
      ENDIF

C.... Go read another canker record.

      GO TO 90

C.... Common return.

  400 CONTINUE
      IF(DEBUG) WRITE(JOSTND,300) ICYC
  300 FORMAT(' Leaving subroutine BRCANK: cycle = ',I2)
      RETURN
      END
