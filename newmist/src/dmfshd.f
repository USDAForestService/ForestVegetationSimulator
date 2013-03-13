      SUBROUTINE DMFSHD
      IMPLICIT NONE
C----------
C  $Id$
C----------
C **DMFSHD --  DATE OF LAST REVISION: 02/16/96     
C----------
C Purpose:
C   This routine is responsible for estimating the amount of shading
C that occurs at each vertical 'layer' of the stand. It does this by
C simulating the position of each element of the treeslist, in
C proportion to its density, on a  100 x 100 grid of 1 meter squares.
C In order to simplify the computations at edges, an outside buffer
C makes to full simulation grid 121 x 121 meters. The (x,y) locations
C are random, with an implied Poisson distribution. The z-position
C and canopy information are provided by the base model and by the
C COVER extension. This information is widely used by the model, to
C estimate interception of seed and light-mediated effects. One of 
C its drawbacks may be that it makes its estimates independent of the
C spatial pattern specified by the model/user, which may vary from
C highly clumped to highly regular. Because canopy overlapping rules
C and effects are not known, this would change the computed opacity
C in ways that are not obvious. I suspect, though, that the
C *distribution* of opacities (which is not even addressed here) is
C well correlated with the local density of trees; this might be used
C to give better information than only the mean opacity (which is
C what *is* computed). 
C
C
C Called by:
C
C     DMTREG 
C
C Other routines called:
C
C     DMRANN
C
C Argument list definitions:                        
C
C     INTEGER Light (O)  Cumulative light remaining at each vertical
C                         level within the stand. 
C
C Local parameter and variable definitions:
C
C     INTEGER i          General loop counter
C     INTEGER j          General loop counter
C     INTEGER s          Loop counter for traversing the x-direction
C                         of the simulation grid
C     INTEGER t          Loop counter for traversing the y-direction
C                         of the simulation grid
C     INTEGER u          Loop counter over heights.
C     INTEGER v          Loop counter over treelist records.
C     INTEGER n          Number of trees of a particular treeslist
C                         record, to put on the grid.
C     INTEGER SLstLn     Length of 'ShdLst()' array.
C     INTEGER Grid       The dimension of the simulation grid
C     INTEGER LowIn      The first position inside the edge buffer.
C     INTEGER HighIn     The last position inside the edge buffer.
C     INTEGER ShdLst     List of heights in which canopy is present.
C     REAL    Tnumbr     Expected number of trees for a particular
C                         treelist record.
C     REAL    Frac       Fractional remainder of 'Tnumbr'.
C     REAL    d          Dstnce (meters) from center of simulated
C                         tree to the edge of its canopy.
C     REAL    x          X-position (grid cell) of simulated tree.
C     REAL    y          Y-position (grid cell) of simulated tree.
C     REAL    sum        summation of opacity on grid.
C     REAL    xp         X-position within canopy of simulated tree.
C     REAL    yp         Y-position within canopy of simulated tree.
C     REAL    Rad        Radius (m) of simulated tree at a given
C                         height.
C     REAL    Opq        Opacity of tree species (blockage per meter)
C     REAL    RND        Uniform {0,1} random number.
C     REAL    Cells      Cells along the side of the simulation grid.
C     REAL    G          The grid itself!
C
C Common block variables and parameters:
C
C     MXHT    DMCOM
C     ITRN    CONTRL
C     DMRDMX  DMCOM
C     RADIUS  DMCOM
C     PROB    ARRAYS
C     MESH    DMCOM
C     SQM2AC  DMCOM
C     DMOPAQ  DMCOM
C     ISP     ARRAYS
C     Shade   DMCOM
C
C**********************************************************************
 
      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'DMCOM.F77'

C Local variables and Parameters.

      INTEGER Grid
      INTEGER LowIn
      INTEGER HighIn
      REAL    Cells

      PARAMETER (Grid = 121)
      PARAMETER (LowIn = 11)
      PARAMETER (HighIn = 110)
      PARAMETER (Cells=100.0)

      INTEGER i, j, s, t, u, v
      INTEGER n, SLstLn
      INTEGER ShdLst(MXHT)          
      REAL    Tnumbr, Frac
      REAL    d, x, y, sum, xp, yp, Rad
      REAL    Opq
      REAL    RND
      REAL    G(Grid, Grid)

C Zero the 'Shade()' array, then find which of the height categories
C actually have something in them.

      i = 0
      DO 300 u = 1, MXHT
        Shade(u) = 0.
        DO 400 v = 1, ITRN
          IF (PROB(v) .GT. 0.01 .AND.
     >        DMRDMX(v, u, RADIUS) .GT. 0.1) THEN
            i = i + 1
            ShdLst(i) = u
            GOTO 350
          END IF
  400   CONTINUE
  350 CONTINUE
  300 CONTINUE

      SLstLn = MIN0(i, MXHT)

C This code remains at the 1 meter grid cell (horizontal plane) 
C resolution. Walk through heights having some shading. Note that 
C 'Rad' is converted from MESH to meters.

      DO 500 u = 1, SLstLn

        v = ShdLst(u)

        DO 700 i = 1, Grid
          DO 800 j = 1, Grid
            G(i, j) = 0.
  800     CONTINUE
  700   CONTINUE

        DO 900 i = 1, ITRN

          Rad = DMRDMX(i, v, RADIUS) * FLOAT(MESH)

          IF (Rad .GT. 0.0) THEN

C Determine number of trees to place on grid; then choose for the
C fractional remainder.
      
            TNumbr = PROB(i) * SQM2AC * FLOAT(Grid) ** 2
            n = INT(TNumbr)
            Frac = TNumbr - n
      
            CALL DMRANN(RND)
            IF (RND .LE. Frac) n = n + 1                       

C Locate each simulated radius on the grid and fill up cells within
C the radius of each disc. ('xp' and 'yp' are actually not exactly at
C P(i)...; they are -0.5 from there. Since their comparison points
C 'x' and 'y' are also shifted, it makes no difference.) Note that
C the decision to overlay two disks replaces less dense "canopy
C foliage" with more opaque. I don't know if tree-tree competition
C really acts in this way.

            Opq = DMOPAQ(ISP(i))

            DO 1000 j = 1, n
      
              CALL DMRANN(RND)
              x = FLOAT(INT(RND * FLOAT(Grid)) + 1)
              CALL DMRANN(RND)
              y = FLOAT(INT(RND * FLOAT(Grid)) + 1)

              DO 1100 s = INT(x - Rad), INT(x + Rad)
                DO 1200 t = INT(y - Rad), INT(y + Rad)
                  IF ((s .GT. 0) .AND. (s .LE. Grid) .AND.
     &                (t .GT. 0) .AND. (t. LE. Grid)) THEN
                    xp = FLOAT(s)
                    yp = FLOAT(t)
                    d = SQRT((xp - x) ** 2 + (yp - y) ** 2)
                    IF ( (d .LE. Rad) .AND. (G(s,t) .LT. Opq) ) 
     &                   G(s, t) = Opq
                  END IF
 1200           CONTINUE
 1100         CONTINUE
 1000       CONTINUE

          END IF

  900   CONTINUE

C  Walk through the inner grid, adding up areas containing tree
C radii. The mean value is taken as the shading. Following that,
C 'Shade()' is converted to meter units, then to MESH units.

        sum = 0.0
        DO i = LowIn, HighIn
          DO j = LowIn, HighIn
            sum = sum + G(i, j)
          ENDDO
        ENDDO

        Shade(v) = sum
        Shade(v) = Shade(v) / (Cells ** 2)
        Shade(v) = 1.0 - ((1.0 - Shade(v)) ** MESH)

        IF (Shade(v) .GT. 1.0) Shade(v) = 1.0
        IF (Shade(v) .LT. 0.0) Shade(v) = 0.0

  500 CONTINUE

C Define 'Light()' extinction as the cumulative effect of shading
C levels above the current level.

      Light(MXHT) = 1.0 - Shade(MXHT)
      DO i = MXHT-1,1,-1
        Light(i) = Light(i + 1) * (1.0 - Shade(i))
      ENDDO

      RETURN
      END
