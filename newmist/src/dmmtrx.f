      SUBROUTINE DMMTRX
C--------------------------------------------------------------------
C **DMMTRX -- NISI Date of last revision: April 9, 1994
C--------------------------------------------------------------------
C Purpose:
C   This routine controls the three routines taken from the COVER
C model. Together, they estimate the radius and volume of treelist
C records. This routine calls the COVER model routines that compute
C the crown geometry needed to estimate opacity effect and spread and
C intensification. The basic information is crown radius (in MESH
C units; normally 2 meters) and volume in MESH height slices. The
C method is based on Moeur's COVER model.
C--------------------------------------------------------------------
C
C Called by:
C
C     DMTREG 
C
C Other routines called:
C
C     DMCW
C     DMSHAP
C     DMSUM
C
C     Note that DMCW and DMSHAP have modified *file* names, since
C     their actual contents vary according to the variant for which
C     they have modified. For example, the Northern Idaho variant 
C     has DMCWNI and DMSHAPNI.
C
C Argument list definitions:                        
C
C     [none]
C
C Local variable definitions:
C     
C     INTEGER   IDMSHP  Tree crown shape category
C     REAL      DMTRCW   Estimated tree crown diameter (feet)
C
C Common block variables and parameters:
C
C     MAXTRE    PRGPRM 
C
C**********************************************************************

      INCLUDE 'PRGPRM.F77'

C Local variables.

      REAL    DMTRCW(MAXTRE)
      INTEGER IDMSHP(MAXTRE)

C Find the tree crown width (feet) for every element of the treelist.
C Calling argument array 'DMTRCW' is filled on return.

      CALL DMCW(DMTRCW)

C Find the crown shape category of each element of the treelist. The
C calling argument array 'IDMSHP' is filled on return.

      CALL DMSHAP(DMTRCW, IDMSHP)

C Compute the radius and volume of each element of the treelist. This
C is estimated for each MESH-heigh band of each element, and computes
C in units of MESH Radius and MESH**3 volume. MESH is normally 2
C meters.

      CALL DMSUM(DMTRCW, IDMSHP)

      RETURN
      END
