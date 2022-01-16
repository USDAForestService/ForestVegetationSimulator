      SUBROUTINE DMFINF (Ptr, Index)
      IMPLICIT NONE
C----------
C CANADA-NEWMIST $Id$
C----------
C **DMFINF --  DATE OF LAST REVISION: 02/16/96 
C----------
C Purpose:
C  The model frequently needs to access the treelist according to
C species and DM rating. This routine generates two objects: a triply
C indexed array, 'Ptr()', that holds references to the positions in 
C a second array, 'Index()', containing the relevant treelist
C records. A call to OPLIST generates the 'Index()' array.
C Subsequently, the key positions of 'Index()' are determined and
C stored in 'Ptr()'.
C
C
C Called by:
C
C     DMTREG 
C
C Other routines called:
C
C     OPSORT
C
C Argument list definitions:                        
C
C     INTEGER Ptr     (O) Matrix of **pointers to treelist, sorted
C                          by species and DM class:
C                          Index 1: species code.
C                          Index 2: DM rating.
C                          Index 3: FST is first position in 'Index()'
C                                   array; LST is last position in
C                                   array. This mapping is analagous
C                                   to the 'IND1()' array of the base
C                                   model, but with two levels of
C                                   indirection.
C     INTEGER Index   (O) Array containing pointers to the treelist.
C                          It is sorted by species and DM rating and
C                          is an analogue of the 'IND1()' array of
C                          the base model.
C
C Local variable definitions:
C
C     INTEGER i           Loop counter for species and for walk 
C                          along 'Index()' array.
C     INTEGER j           Loop counter for DM rating.
C     INTEGER k           Loop counter for third index of 'Ptr()'
C                          and for position in 'Index()' array.
C     INTEGER Sp          Current species code.
C     INTEGER DMR         Current DM rating.
C     INTEGER PrSp        Previous species code.
C     INTEGER PrDMR       Previous DM rating.
C
C
C Common block variables and parameters:
C
C     MAXSP   PRGPRM
C     MAXTRE  PRGPRM
C     FST     DMCOM
C     LST     DMCOM
C     ITRN    CONTRL
C     ISP     ARRAYS
C     DMRATE  DMCOM
C
C**********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'DMCOM.F77'

C Subroutine arguments.

      INTEGER   Ptr
      INTEGER   Index

      DIMENSION Ptr(MAXSP, 0:6, LST)
      DIMENSION Index(MAXTRE)

C Local variables.

      INTEGER i, j, k
      INTEGER Sp, DMR
      INTEGER PrSp, PrDMR

      do i = 1, MAXTRE
        Index(i) = 0
      enddo

C Zero pointer matrix

      DO 100 i = 1, MAXSP
        DO 110 j = 0, 6
          DO 120 k = FST, LST
            Ptr(i, j, k) = 0
  120     CONTINUE
  110   CONTINUE
  100 CONTINUE

C Load the two variables into the sorting array. The 'Index()' array
C returned by OPSORT is sorted first by species 'ISP()' then by DM 
C rating 'DMRATE()'.

      CALL OPSORT(ITRN, ISP, DMRATE, Index, .TRUE.)

C Walk down the sorted list, assigning the first and last from each
C category, as they are found. Changes in species code and DM rate
C are used to trigger assignments to the 'Ptr()' array.

      k = Index(1)
      Sp = ISP(k)
      DMR = DMRATE(k)
      PrSp = Sp
      PrDMR = DMR

      Ptr(Sp, DMR, FST) = 1
      DO 300 i = 2, ITRN
        k = Index(i)
        Sp = ISP(k)
        DMR = DMRATE(k)

        IF (Sp .NE. PrSp) THEN
          Ptr(PrSp, PrDMR, LST) = i - 1
          Ptr(Sp, DMR, FST) = i
          PrSp = Sp
          PrDMR = DMR
        ELSE
          IF (DMR .NE. PrDMR) THEN
            Ptr(Sp, PrDMR, LST) = i - 1
            Ptr(Sp, DMR, FST) = i
            PrDMR = DMR
          END IF
        END IF

  300 CONTINUE
      Ptr(Sp, DMR, LST) = ITRN

      RETURN
      END
