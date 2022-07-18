      SUBROUTINE DMBSHD (iCS, iz, ix, ivLen, ivCnt)
      IMPLICIT NONE
C----------
C CANADA-NEWMIST $Id$
C----------
C  **DMBSHD -- NISI  Date of last revision: April 7 1994
C----------------------------------------------------------------------
C Purpose:
C   The source tree produces a field of trajectories emanating from
C the position of an infection. These trajectories are recorded 
C using a target tree reference frame in which the infection
C originates from (X,Z) cell (1,10). The X dimension increases going
C away from the tree and the Z dimension increases from the ground
C up. After adjusting for the different reference frame of the
C target, each MESH band of the  target exists in some position in
C the reference frame of the source. For example, this could be in
C (5,2), which would mean that a portion of the crown of the target
C is 5 MESH units lateral to the piece of source producing the
C infection, and 8 units below. The job of this routine is to provide
C a list of all the trajectories that pass through (5,2), and a
C record of what (X,Z) positions they passed through to get there.
C These allow the intensification, between-tree loss and spread to
C be computed. This is done by using the 'ShdPtr()' to see if any
C paths went through this (X,Z). If so, then the 'iCS()' matrix is 
C loaded all the path and path-weight information, to be used by the
C calling routine. The use of 0:28 in the 2nd position of the 'iCS()'
C matrix simply reflects an empirical determination of the longest
C path ever observed when MESH was 1 meter. Most are shorter, and
C since MESH is usually 2 meters, much shorter.
C----------------------------------------------------------------------
C
C Called by:
C
C     DMADLV
C
C Other routines called:
C
C     [none]
C
C Argument list definitions:
C      
C     INTEGER    iCS   (O)  The path information, consisting of a
C                            triply indexed array:
C                            Index 1: for each unique path.
C                            Index 2: for each gridcell that the
C                                      path passes through.
C                            Index 3: for the (X,Z) location of
C                                      each cell.
C     INTEGER    iz    (I)  The z-position (source tree's reference 
C                            frame) of the target crown.
C     INTEGER    ix    (I)  The x-position (etc.) of the target crown
C     INTEGER    ivLen (O)  The length of each path passing through
C                            (iz,ix); this corresponds to the relevant
C                            length of iCS(-,2,-)
C     INTEGER    ivCnt (O)  The number of paths passing through (iz,ix)
C                            This corresponds to the relevant length
C                            of iCS(1,-,-)
C
C Local variable definitions:
C                         
C      INTEGER   i          Loop counter for the each unique path
C      INTEGER   j          Loop counter for the length of each path
C      INTEGER   StrtVl     Pointer to position of first cell in path
C      INTEGER   EndVal     Pointer to position of last cell in path
C      INTEGER*4 Ptr        Index of position in encoded paths
C
C Common block variables and parameters:
C
C     MXTRAJ     DMCOM
C     TOP1       DMCOM
C     XX         DMCOM                                       
C     ZZ         DMCOM
C     ShdPtr     DMCOM
C     Shd1       DMCOM
C
C**********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'DMCOM.F77' 
      
C Subroutine arguments.
      
      INTEGER    iCS
      INTEGER    iz
      INTEGER    ix
      INTEGER    ivLen
      INTEGER    ivCnt
      
      DIMENSION  iCS(MXTRAJ, 0:28, 2)
      DIMENSION  iVLen(MXTRAJ)

C Local variables

      INTEGER    i, j
      INTEGER    StrtVl, EndVal
      INTEGER*4  Ptr

C If no trajectories occur in this grid cell, 'Ptr' is zero.

      Ptr = ShdPtr(iz, ix)

      IF (Ptr .EQ. 0) THEN
         ivCnt = 0

      ELSE IF ((Ptr .GE. 1) .AND. (Ptr .LT. TOP1)) THEN

         ivCnt = Shd1(Ptr)
         Ptr = Ptr + 1

         DO 100 i = 1, ivCnt

            iCS(i, 0, XX) = Shd1(Ptr)
            Ptr = Ptr + 1

            StrtVl = Shd1(Ptr)
            Ptr = Ptr + 1

            EndVal = INT(Shd1(Ptr)/2)
            Ptr = Ptr + 1

            DO 300 j = StrtVl, EndVal
               iCS(i, j, XX) = Shd1(Ptr)
               Ptr = Ptr + 1
               iCS(i, j, ZZ) = Shd1(Ptr)
               Ptr = Ptr + 1
  300       CONTINUE

          ivLen(i) = EndVal

  100     CONTINUE

      END IF
 
      RETURN
      END
