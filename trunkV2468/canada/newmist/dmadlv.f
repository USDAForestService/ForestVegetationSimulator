      SUBROUTINE DMADLV(SrcInd, Cnt, SFld, IFld,  MshHt, Dist,
     &                      Level, Shd, Shd0, II, EB)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C  **DMADLV --  DATE OF LAST REVISION: 02/26/96        
C----------
C  Purpose:
C    Computes part (one of many "levels", hence the name) of the DM
C  Intensification vector of the infection source tree and the Spread
C  vector of the infection target tree. This involves accounting for
C  the different frames of reference of the two trees, as well as
C  slope and height-dependent opacity effects. The Spread and
C  Intensification vectors (ie: heights in each tree) are returned to
C  the calling routine, where they are further adjusted to account
C  for canopy size and distance between the two. As calculated here,
C  the units are not "complete" until the final geometrical
C  adjustments and summations take place in the calling routine.
C
C
C Called by:
C
C     DMTREG 
C
C Other routines called:
C
C     DMBSHD
C     DMRANN
C
C Argument list definitions:                        
C
C     INTEGER SrcInd  (I) Treelist index for the source tree.
C     INTEGER Cnt     (I) Number of these trees. This will only be
C                          greater than 1 in dense small treelists.
C     REAL    SFld    (0) Array into which the computed amount of
C                          spread (amount/MESH**3) is placed.
C     REAL    IFld    (0) Array into which the computed amount of
C                          intensification (amount/MESH**3) is 
C                          placed.
C     INTEGER MshHt   (I) Height (MESH) from which the source
C                          infection is acting.
C     INTEGER Dist    (I) Dstnce (MESH) between the source and
C                          target trees.
C     REAL    Level   (I) Infection (amount/cubic MESH) originating
C                          from the source tree.
C     REAL    Shd     (I) Nbr stand shading array
C     REAL    Shd0    (I) Index stand shading array
C     INTEGER II      (I) 0 if the Src-Target pair are in the same stand;
C                         otherwise 1
C     INTEGER EB      (I) Edge-band in which Target is found
C
C Local variable definitions:
C     
C     INTEGER h       Height of source (MESH), source reference
C                      frame.
C     INTEGER i       Loop counter, fixed ref frame (z-axis).
C     INTEGER j       Loop counter, fixed ref frame (x-axis).
C     INTEGER k       Loop counter,  list of trajectories into
C                      (u,v) coordinate space.
C     INTEGER m       Loop counter, members of 'k' list.
C     INTEGER n       Number of unique trajectories into (u,v).   
C     INTEGER u       Loop counter, tree ref frame (z-axis).
C     INTEGER v       Loop counter, tree ref frame (x-axis).
C     INTEGER HSZInd  Upper bound, tree ref frame (z-axis).
C     INTEGER LSZInd  Lower bound, tree ref frame (z-axis).
C     INTEGER HSXInd  Upper bound, tree ref frame (x-axis).
C     INTEGER LSXInd  Lower bound, tree ref frame (x-axis).
C     INTEGER HFZInd  Upper bound, fixed ref frame (z-axis).
C     INTEGER LFZInd  Lower bound, fixed ref frame (z-axis).
C     INTEGER HFXInd  Upper bound, fixed ref frame (x-axis).
C     INTEGER LFXInd  Lower bound, fixed ref frame (x-axis).
C     INTEGER CShd    List of x- and z-positions taken along each
C                      precomputed trajectory;
C                      Index 1: element in list.
C                      Index 2: array position in element.
C                      Index 3: x- and z-position.
C     INTEGER VecLen  Length of each 'CShd' list element.
C     REAL    x       Intermediate calculation.
C     REAL    y       Intermediate calculation.
C     REAL    Op      Opacity (/MESH) of host species.
C     REAL    Rad     Radius (MESH) of source tree.
C     REAL    VecWt   Scalar for amount of infection along this
C                      trajectory.
C     REAL    Loss    Intermediate calculation of infection loss
C                      via intensification or shading.
C
C Common block variables and parameters:
C
C     DMOPQ2  DMCOM
C     DMRDMX  DMCOM
C     MXHT    DMCOM    
C     MAXOFF  DMCOM     
C     MXTRAJ  DMCOM    
C     MXTHRX  DMCOM    
C     MXTHRZ  DMCOM
C     ORIGIN  DMCOM
C     RADIUS  DMCOM            
C     XX      DMCOM
C     ZZ      DMCOM 
C     ISP     ARRAYS  
C
C

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'DMCOM.F77'

C Subroutine arguments.

      INTEGER   SrcInd
      INTEGER   Cnt
      REAL      SFld
      REAL      IFld
      INTEGER   MshHt
      INTEGER   Dist
      REAL      Level
      REAL      Shd(MXHT)
      REAL      Shd0(MXHT)
	INTEGER   II
	INTEGER   EB

      DIMENSION SFld(MXHT)
      DIMENSION IFld(MXHT)

C Local variables.
     
      INTEGER   h, i, j, k, m, n, u, v, w

C The precomputed matrix, represented as row and column-specific
C lists in the call to DMBSHD, assumes a fixed matrix of
C MXTHRX by MXTHRZ for the "archetype" infection field. Since
C the actual location of an infection depends on its position in the
C source tree, the two coordinate systems must be reconciled by
C horizontal and vertical translations. These boundaries of the two
C coordinate systems are referenced by 'HSZInd. etc.' and 
C 'HFZInd, etc.' for the "fixed" coordinate system and the
C "tree" coordinate systems, respectively. 

      INTEGER HSZInd, LSZInd
      INTEGER HSXInd, LSXInd

      INTEGER HFZInd, LFZInd
      INTEGER HFXInd, LFXInd

C Shading function data is mapped into 'CShd'. The maximum
C values are derived as follows: Each row (1st index) of the matrix
C is a set of cells through which the trajectory passes. Inspection
C of the data set used to derive the function shows that for any
C (x,z) target cell, there are at most MXTRAJ unique trajectories.
C The value of MXTRAJ (and the underlying data) will change if the
C MESH definition is changed (for example, from 2 meters to 1 meter)
C or if the number of retained vectors is changed. The default (1993)
C data uses 40% of the vectors, to speed up the computation time.
C The columns (2nd index) are the maximum number of cells ((x,z)
C pairs) through which any trajectory will pass. The choice of 28 is
C slightly arbitrary, but large enough to accomodate a 1 meter MESH
C resolution for the longest-length trajectory list, based on
C inspection of the files that created 'dmdata.inc'. The 3rd index
C records the X (=XX=1) or Z (=ZZ=2) position of the trajectory.
C 'VecLen' records the actual number of elements in each row of the
C matrix.

      INTEGER CShd(MXTRAJ, 0:28, 2), VecLen(MXTRAJ)

      REAL    x, y, Op, Rad
      REAL    VecWt, Loss

C Find the opacity of the tree's foliage, in units of P(shading)
C per MESH**3. 

      Op = DMOPQ2(ISP(SrcInd))

C Find the upper and lower Z-indices of the 'SP()' array. If 'MshHt'
C is quite high or low on the tree, then parts of the distribution
C will be truncated when the 'SP()' array is mapped to the 'SFld()'
C array. This insures that the operations to the 'SFld()' array will
C always be within bounds. Note that 'MshHt' is measured in MESH
C units. Also, the calling subroutine prevents 'MshHt' from being
C greater than MXHT.

C Determine the lowest z-index for the tree coordinate system.

      LSZInd = 1
      i = ORIGIN - MshHt + 1
      IF (i .GT. LSZInd) LSZInd = i

C Determine the highest z-index for the tree coordinate system.

      HSZInd = MXTHRZ
      i = ORIGIN - MshHt + MXHT
      IF (i .LT. HSZInd) HSZInd = i

C Determine the lowest z-index for the tree coordinate system.
C This does not account for "seed rain" when the trajectories from
C the fixed system "run out" before hitting ground. This possibility
C is handled in the code.

      LFZInd = 1
      i = MshHt - ORIGIN + 1
      IF (i .GT. LFZInd) LFZInd = i

C Determine the highest z-index for the tree coordinate system.

      HFZInd = MshHt - ORIGIN + MXTHRZ
      i = MXHT
      IF (i .LT. HFZInd) HFZInd = i

C The origignal prototype allowed the x-dimension of the two matrices
C to shift. The method is now different, and the simplest code change
C involved processing only the x-value at fixed distance 'Dist'. ie:
C "process only one sampling ring". Other sampling rings are examined
C via other calls to this routine. The old loop structure is retained
C because "it ain't broke"; however, the J-loop is only passed
C through one time.

      LSXInd = Dist
      HSXInd = Dist

C     LSXInd = 1
C     HSXInd = MXTHRX

      LFXInd = LSXInd
      HFXInd = HSXInd

C Increment the infection field array with the crownthird DMR level,
C accounting for horizontal shading and the number of trees at
C distance 'Dist'. The 'u' and 'v' variables index into the fixed
C coordinate system as the tree coordinate system (the 'i' and 'j'
C indices) "moves" up the target tree.

      u = LSZInd
      DO i = LFZInd, HFZInd

!        IF (Shd(i) .GT. 0.0) THEN

          v = LSXInd
          DO j = LFXInd, HFXInd

C Create the 'ChellShd' list (disguised as a 3-index array) of
C trajectory paths arriving at position (u,v) in the fixed reference
C frame of the source infection. 'n' returns the number of items
C (unique trajectories) in the list.

            CALL DMBSHD(CShd, u, v, VecLen, n)

C Walk through the matrix computing actual shade-corrected
C transfer to the cell. The value in 'CShd(m,k,-)' is the
C number of occurences of the trajectory 'k' at height 'm'.
C Note that the 0'th column holds 'VecWt' on return, and that
C 'Shade' must be offset to provide the true height.
C
C Here is the dynamic: If the z-position is above 50 m (unlikely)
C then the information is not saved for the higher positions. If
C the x-position is less than the radius the trajectory contributes
C to intensification. If the x-position is on a cell straddling the
C radius of the tree, then intensification is a linear proportion of
C the amount straddled (Talk about dotting i's and crossing t's). If
C the x-position is greater than or equal to the radius, then it
C contributes to the spread field, if and only if is in the proper
C sampling ring; otherwise infection field is diminished by the
C presence of shading.

C Two issues are not addressed here: Shading loss is computed using
C the height 'h' of the source tree. The actual shading is probably
C closer to the average of the source and target tree heights. (All
C this is important *only* on slopes.) Second, the stochastic effect
C of multiple instances of a particular source tree record within a
C sampling ring are not included. At *very* high densities it is
C possible to have two (or more) source trees in a ring, derived
C from the same record. The model will put give all these trees the
C same offset relative to the target.

            DO k = 1, n
              VecWt = FLOAT(CShd(k, 0, XX)) * Level
              DO m = 1, VecLen(k)
                h = MshHt + CShd(k, m, ZZ) - ORIGIN
                IF ((h .GE. 1) .AND. (h .LE. MXHT)) THEN
                  Rad = DMRDMX(SrcInd, h, RADIUS)
                  x = FLOAT(CShd(k, m, XX))
                  y = x - Rad
                  IF (x .LE. Rad) THEN
                    Loss = VecWt * Op 
                    VecWt = VecWt - Loss
                    IFld(h) = IFld(h) + Loss
                  ELSE IF ((y .GT. 0.0) .AND. (y .LT. 1.0)) THEN
                    Loss = VecWt * Op * y 
                    VecWt = VecWt - Loss
                    IFld(h) = IFld(h) + Loss
                  ELSE
                    IF (CShd(k, m, XX) .EQ. Dist) THEN
                      Loss = VecWt * Op
                      SFld(h) = SFld(h) + Cnt * Loss
                    ELSE
	                IF (II .EQ. 1 .AND.
     >                    CShd(k, m, XX) .LT. (Dist-EB)) THEN
                        Loss = VecWt * Shd(h)
	                ELSE
                        Loss = VecWt * Shd0(h)
	                ENDIF
                      VecWt = VecWt - Loss
                    END IF
                  END IF
                END IF

C If the trajectory walk does not end at or before the ground (h=1),
C then "rain down" the remainder based on the last XX position 'x'.
C The value of 'x' is carried over from the last available position.        
         
                IF ((m .EQ. VecLen(k)) .AND. (i .EQ. LFZInd)
     >              .AND. (LFZInd .GT. 1)) THEN
                  DO w = i, 1, -1
                    Rad = DMRDMX(SrcInd, w, RADIUS)
                    y = x - Rad
                    IF (y .LE. Rad) THEN
                      Loss = VecWt * Op
                      VecWt = VecWt - Loss
                      IFld(w) = IFld(w) + Loss
                    ELSE IF ((y .GT. 0.) .AND. (y .LT. 1.)) THEN
                      Loss = VecWt * Op * y
                      VecWt = VecWt - Loss
                      IFld(w) = IFld(w) + Loss
                    ELSE
                      IF (CShd(k, m, XX) .EQ. Dist) THEN
                        Loss = VecWt * Op
                        SFld(w) = SFld(w) + Cnt * Loss
                      ELSE
	                IF (II .EQ. 1 .AND.
     >                    CShd(k, m, XX) .LT. (Dist-EB)) THEN
                          Loss = VecWt * Shd(h)
	                  ELSE
                          Loss = VecWt * Shd0(h)
	                  ENDIF
                        VecWt = VecWt - Loss
                      END IF
                    END IF
                  ENDDO
                END IF
              ENDDO
            ENDDO

          v = v + 1
          ENDDO

!       END IF    
        u = u + 1

      ENDDO

      RETURN
      END
