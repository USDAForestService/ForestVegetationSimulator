C********************************************************************
C  **DMCOM  Date of last revision:  12/20/03
C--------------------------------------------------------------------
C Purpose:
C   Common block variables related to the NISI dwarf mistletoe
C routines.
C--------------------------------------------------------------------
C********************************************************************
C Symbolic names for array indices:
C
C     RADIUS  1st element of 3rd index of 'DMRDMX()' array.
C     VOLUME  2nd element of 3rd index of 'DMRDMX()' array.
C     XX      1st element of 3rd index of 'CShd()' array.
C     ZZ      2nd element of 3rd index of 'CShd()' array.
C     FST     1st element of 3rd index of 'DMSPtr()' [DMTREG]
C              array.
C     LST     2nd element of 3rd index of 'DMSPtr()' [DMTREG]
C              array.
C     CRTHRD  Number of parts in crown thirds.
C     BPCNT   Number of breakpoints in crown thirds.

C     IMMAT     1st element of 3rd index of 'DMINF()' array.
C     LATENT    2nd element of 3rd index of 'DMINF()' array.
C     SUPRSD    3rd element of 3rd index of 'DMINF()' array.
C     ACTIVE    4th element of 3rd index of 'DMINF()' array.
C     DEAD_BC   5th element of 3rd index of 'DMINF()' array.

C     KNT     1st element of 2nd index of 'SrcLst()' [DMTREG]
C              array.
C     INDX    2nd element of 2nd index of 'SrcLst()' [DMTREG]
C              array.
C********************************************************************

      INTEGER RADIUS, VOLUME
      INTEGER XX, ZZ
      INTEGER FST, LST
      INTEGER CRTHRD
      INTEGER BPCNT
      INTEGER IMMAT, LATENT, SUPRSD, ACTIVE, DEAD_BC
      INTEGER KNT, INDX

      PARAMETER(KNT=1,    INDX=2)
      PARAMETER(RADIUS=1, VOLUME=2)
      PARAMETER(XX=1,     ZZ=2)
      PARAMETER(FST=1,    LST=2)

      PARAMETER(CRTHRD = 3)
      PARAMETER(BPCNT = CRTHRD + 1)

C     LIFE HISTORY POOLS

      PARAMETER(IMMAT     = 1)
      PARAMETER(LATENT    = 2)
      PARAMETER(SUPRSD    = 3)
      PARAMETER(ACTIVE    = 4)
      PARAMETER(DEAD_BC   = 5)

C********************************************************************
C Fixed parameters and common array boundaries:
C
C     MESH    The size (MESH) of spatial grid cells.
C     MXTRAJ  The maximum number of trajectories that will *ever*
C              arrive at any grid cell. This value must be reviewed
C              whenever a new trajectory data set is put in DMBLKD.
C     ORIGIN  The cell (MESH) from which spread field trajectories
C              emmanate.
C     MXHT    The maximum height (MESH) of the stand.
C     MXTHRX  The maximum lateral distance of seed x-travel (MESH)
C               given a 20 m origin.
C     MXTHRZ  The maximum vertical distance of seed z-travel (MESH)
C               given a 20 m origin.
C     TOP1    The length of the *black box* array Shd1 that
C              holds all the encoded trajectory information.
C     DSTLEN  The length of the cumulative binomial-family
C              distribution.
C
C ORIGIN,...,MXTHRZ must all be evenly divisible by MESH.
C********************************************************************

      INTEGER MESH
      INTEGER MXTRAJ
      INTEGER ORIGIN
      INTEGER MXHT
      INTEGER MXTHRX
      INTEGER MXTHRZ
      INTEGER TOP1
      INTEGER DSTLEN

      PARAMETER(MESH=2)
      PARAMETER(MXTRAJ=1)
      PARAMETER(MXTHRX=14/MESH)
      PARAMETER(MXTHRZ=26/MESH)
      PARAMETER(ORIGIN=20/MESH)
      PARAMETER(MXHT=50/MESH)
      PARAMETER(TOP1=1496)
      PARAMETER(DSTLEN=1000)

C********************************************************************
C Some numerical constants.
C
C     TRAJWT  The weight of each observation in the shading array;
C              this is based on the number of paths used in the
C              trajectory simulation: 1000.
C     SQM2AC  The conversion factor for acres to square meters.
C     FPM     The number of feet in one meter.
C     PIE     The usual meaning of PI (NB: "PI" is a different
C              variable used elsewhere in the base model.)
C     HLFPIE  The usual meaning of PI / 2.
C     TWOPIE  PI * 2.
C     DMTINY  An arbitrarily small number that is near enough to
C              zero (in single precision) to mark the stop of some
C              calculations.
C********************************************************************

      REAL    TRAJWT
      REAL    SQM2AC
      REAL    FPM
      REAL    PIE
      REAL    HLFPIE
      REAL    TWOPIE
      REAL    DMTINY

      INTEGER MAXBC

      PARAMETER(TRAJWT=1./1000.)
      PARAMETER(SQM2AC=1./4046.8564)
      PARAMETER(FPM=3.2808)
      PARAMETER(PIE=3.14159)
      PARAMETER(HLFPIE=1.57080)
      PARAMETER(TWOPIE=6.283185)
      PARAMETER(DMTINY=1.0E-6)

      PARAMETER(MAXBC=5) ! max biological control systems

C     BIOCONTROL PARAMETERS FROM MISBCI KEYWORD

      TYPE BIOCONTROL_DESCRIPTION
        SEQUENCE
        INTEGER     Spp
        REAL        Mort(ACTIVE)
        REAL        Suprs(ACTIVE)
        REAL        Yr(ACTIVE)
        REAL        HfLf(ACTIVE)
      END TYPE

C********************************************************************
C
C Common block variables.
C
C  >> LOGICAL VARIABLES <<
C
C     LOGICAL  LDETAIL    .TRUE. if detailed outputs are requested
C     LOGICAL  NTDn       .TRUE. after the crownthirds have been
C                          initialized.
C     LOGICAL  DCDn       .TRUE. after the inventory damage codes
C                          have been read.
C     LOGICAL  NEWMOD     .TRUE. if the NISI model is in effect.
C     LOGICAL  ZPdn       .TRUE. if the lifehistory pools have been
C                          initialized.
C
C  >> DM RATING <<
C
C     INTEGER DMDMR       Default crownthird DM assignments: bottom
C                          up assignment of DM.
C                          Index 1: The six DM categories.
C                          Index 2: Upper (=1), Mid (=2) and Lower
C                                   (=3) crown third.
C     INTEGER DMRATE      The NISI DM category assigned to each
C                          treelist record. The range is from zero
C                          to six (Hawksworth's DMR).
C
C  >> TRAJECTORY INFORMATION <<
C
C     INTEGER*2 Shd1      The encoded trajectory information. More
C                          details on the coding are given in
C                          DMBSHD.
C     INTEGER*4 ShdPtr    Matrix of pointers to the 'Shd1()' array.
C                          There is a pointer for each of the X-
C                          and Z- positions of the trajectory
C                          reference frame. Each grid cell is a
C                          square of length MESH (normally 2
C                          meters).
C                          Index 1: Grid cells on the Z- (vertical)
C                                   axis. Ordering is from the
C                                   ground (=1) up.
C                          Index 2: Grid cells on the X-
C                                   (horizontal) axis. Ordering is
C                                   from the stem (=1) outward.
C
C  >> CROWN GEOMETRY, OPACITY & LIGHT <<
C
C     REAL    DMRDMX      Array holding crown geometry information
C                          for each MESH thickness slice of each
C                          treelist record.
C                          Index 1: The treelist record.
C                          Index 2: The height of the piece of
C                                   crown, measured in bands of
C                                   MESH thickness, from the ground
C                                   (=1) upward.
C                          Index 3: The estimated radius (=RADIUS)
C                                   and volume (=VOLUME) of the
C                                   relevant part of the crown.
C                                   Units are MESH, normally 2
C                                   meters.
C     REAL    DMOPQM      Multiplier that is applied to all the
C                          species-specific opacity estimates.
C                          This factor can be used to tie relative
C                          estimates of opacity to empirical
C                          measurement.
C     REAL    DMOPAQ      The relative opacity (PP = LP = 1.0 for
C                          the NI variant) of each species. Units
C                          measure the proportion of seeds that
C                          are *not* intercepted during a 1 meter
C                          flight path through the canopy of the
C                          relevant tree species.
C     REAL    DMOPQ2      Like 'DMOPAQ()', but expressed in MESH
C                           units.
C     REAL    DMLtRx      The coefficients for the light-driven
C                          forward and backward maturation
C                          effects. Up to four points can be used
C                          to describe the shape of each response
C                          surface. The x- (proportion of light)
C                          and y- (proportion changing state;
C                          either by going forward from latent to
C                          flowering, or backward from flowering
C                          to latent) are {0,1}. Each sequence of
C                          points must be strictly increasing on
C                          the x-axis.
C                          Index 1: The tree species.
C                          Index 2: Foward (=1) and backward (=2).
C                          Index 3: X- (=1) and Y- (=2) points.
C                          Index 4: The number of each point.
C     INTEGER DMLtnp      The number of points that have been
C                          chosen to represent the forward and
C                          backward reactions in 'DMLtRx()'.
C                          Index 1: The tree species.
C                          Index 2: The number of points for the
C                                   forward (=1) and the backward
C                                   (=2) reactions.
C
C  >> LIFE HISTORY <<
C
C     INTEGER DMKTUN      A species-specific multiplier that can
C                          be used to remove treelist records of
C                          a given DM category from contributing
C                          to spread.
C     REAL    DMETUN      A species-specific multiplier that can
C                          be used to increase or decrease the
C                          magnitude of spread and intensification
C                          simultaneously. It works *in addition
C                          to* the effects of 'DMSTUN()' and
C                          'DMITUN()'.
C     REAL    DMSTUN      A species-specific multiplier that can
C                          be used to increase or decrease the
C                          magnitude of spread.
C     REAL    DMITUN      A species-specific multiplier that can
C                          be used to increase or decrease the
C                          magnitude of intensification.
C     INTEGER DMFLWR      The species-specific average time
C                          (years) between the establishment of a
C                          successful infection and the earliest
C                          possible flowering under optimal
C                          mistletoe conditions.
C     REAL    DMCAP       The species-specific maximum density of
C                          infections that re permitted to occupy
C                          crown. Units are "DM equivalent", which
C                          means that they are measured the same
C                          way as DMR: the number of infections
C                          required per MESH**3, to give an
C                          observed DMR of 'x'. In other words,
C                          a way to skirt the issue of how many
C                          plants it takes to give a DMR.
C     REAL    DMDETH      The species-specific annual death rate
C                          of DM infections. The units are:
C                          'proportion dying per year',
C                          independent of tree or branch death;
C                          units are dimensionless {0,1}.
C     REAL    DMSURV      The species-specific complement of
C                          'DMDETH()'. Units are dimensionless:
C                          '1.0 - {0,1}'.
C
C >> SPATIAL EFFECTS <<
C
C     REAL    DMCLMP      The amount of clumping found in the
C                          stand. Units are the variance/mean
C                          ratio for *all* species combined,
C                          and may be fixed or dynamic, depending
C                          on keyword control.
C     REAL    DMALPH      The DM-dependent autocorrelation term,
C                          independent of species. Implementation
C                          details can be found in DMOPTS.
C     REAL    DMBETA      The distance-dependent autocorrelation
C                          term, independent of species.
C                          Implementation details can be found in
C                          DMOPTS.
C     INTEGER DMRDFF      Matrix of differences between DM
C                          categories. This is a simple efficient
C                          way to record the absolute difference
C                          between any two DM categories.
C                          Index 1: The 0:6 DM categories.
C                          Index 2: The 0:6 DM categories.
C     REAL    SF          Scaling factor required to adjust
C                          neighborhood densities of each DM
C                          category in each sampling ring.
C                          Implementation details can be found
C                          in DMAUTO.
C                          Index 1: The 0:6 DM categories.
C                          Index 2: The sampling rings.
C
C  >> INFECTIONS <<
C
C     REAL    NewSpr      The amount of new infection originating
C                          from spread. Units are "DM equivalents"
C                          as described above for 'DMCAP()'. This
C                          is required in a common block only so
C                          that it can be printed in the detailed
C                          output file.
C                          Index 1: The treelist record.
C                          Index 2: The crown third.
C     REAL    NewInt      The amount of new infection originating
C                          from intensification. Units are "DM
C                          equivalents"as described above for
C                          'DMCAP()'. This is required in a common
C                          block only so that it can be printed in
C                          the detailed output file.
C                          Index 1: The treelist record.
C                          Index 2: The crown third.
C     REAL    DMINF       The infection density (DMR/MESH**3) of
C                          each life history stage, in each crown
C                          third of each treelist record's canopy.
C                          Index 1: The treelist record.
C                          Index 2: The crown third.
C                          Index 3: The life history class:
C                                   immature (=IMMAT), latent
C                                   (=LATENT), suppressed
C                                   (=QACTV) and flowering
C                                   (=ACTIVE). plus the biocontrol
C                                   equivalents.
C
C  >> MORE CROWN GEOMETRY, OPACITY & LIGHT <<
C
C     REAL    Shade       Opacity per layer
C     REAL    Light       Heightwise light extinction array; {0,1}
C                          where 0 means no light and 1 means full
C                          sky illumination.
C     REAL    BrkPnt      The four breakpoints that divide each
C                          tree's canopy into three equal pieces:
C                          crown thirds. Units are MESH (normally
C                          2 meters), and apply to the current
C                          time step.
C     REAL    PBrkPt      The four breakpoints that divide each
C                          tree's canopy into three equal pieces:
C                          crown thirds. Units are MESH (normally
C                          2 meters), and apply to the previous
C                          time step.
C     REAL    CrArea      The area (acres) of each sampling
C                          *disc*. This value is used to predicted
C                          the mean number of trees expected in a
C                          sampling disc, and is subsequently
C                          modified to estimate a sampling *ring*.
C     REAL    Dstnce      The distance (meters) between the center
C                          (target tree) and the mid-point of each
C                          sampling ring.
C
C  >> KLUDGE <<
C
C  Temporary (93 Oct 4) variable for MISGET and MISPUT
C
C     REAL    DMKLDG      A quick fix to allow a number of
C                          variables to be passed quickly to the
C                          MISGET and MISPUT routines: each
C                          treelist records breakpoints, previous
C                          breakpoints, and the infection levels
C                          of each life history stage in each
C                          crown third of the canopy.
C
C  >> RANDOM NUMBERS <<
C
C     DOUBLE  DMS0        Random number modulus 0
C     DOUBLE  DMS1        Random number modulus 1
C     REAL    DMSS        Starting seed.
C
C********************************************************************

      LOGICAL LDETAIL
      LOGICAL NTDn
      LOGICAL DCDn
      LOGICAL NEWMOD
      LOGICAL ZPdn

      INTEGER DMDMR(0:6,CRTHRD)
      INTEGER DMRATE(MAXTRE)
      INTEGER Shd1(TOP1) !
      INTEGER ShdPtr(MXTHRZ, MXTHRX) !

      REAL    DMRDMX(MAXTRE, MXHT, VOLUME)
      REAL    DMOPQM
      REAL    DMOPAQ(MAXSP)
      REAL    DMOPQ2(MAXSP)

      REAL    DMLtRx(MAXSP,2,2,4)
      INTEGER DMLtnp(MAXSP,2)

      INTEGER DMKTUN(MAXSP)
      REAL    DMETUN(MAXSP)
      REAL    DMSTUN(MAXSP)
      REAL    DMITUN(MAXSP)

      INTEGER DMFLWR(MAXSP)
      REAL    DMCAP(MAXSP)
      REAL    DMDETH(MAXSP)
      REAL    DMSURV(MAXSP)

      REAL    DMCLMP
      REAL    DMALPH
      REAL    DMBETA
      INTEGER DMRDFF(0:6, 0:6)
      REAL    SF(0:6, MXTHRX)

      REAL    NewSpr(MAXTRE, CRTHRD)
      REAL    NewInt(MAXTRE, CRTHRD)
      REAL    DMINF(MAXTRE, CRTHRD, DEAD_BC)
      REAL    DMINF_BC(MAXTRE, CRTHRD, ACTIVE, MAXBC)
      REAL    Shade(MXHT), Light(MXHT)
      REAL    BrkPnt(MAXTRE, BPCNT)
      REAL    PBrkPt(MAXTRE, BPCNT)
      REAL    CrArea(MXTHRX)
      REAL    Dstnce(MXTHRX)

      REAL    DMKLDG(2*BPCNT + CRTHRD*DEAD_BC + CRTHRD*ACTIVE*MAXBC)

      COMMON /DMMIST/ LDETAIL,NTDn, DCDn
      COMMON /DMMIST/ NEWMOD, ZPdn
      COMMON /DMMIST/ DMDMR, DMRATE, DMRDMX
      COMMON /DMMIST/ DMOPAQ, DMOPQ2, DMOPQM
      COMMON /DMMIST/ DMLtRx,DMLtnp
      COMMON /DMMIST/ DMKTUN, DMETUN, DMSTUN, DMITUN
      COMMON /DMMIST/ DMFLWR, DMCAP
      COMMON /DMMIST/ DMDETH, DMSURV
      COMMON /DMMIST/ DMCLMP, DMALPH, DMBETA, DMRDFF, SF
      COMMON /DMMIST/ NewSpr, NewInt, DMINF, DMINF_BC
      COMMON /DMMIST/ Shade, Light
      COMMON /DMMIST/ BrkPnt, PBrkPt, CrArea, Dstnce
      COMMON /DMMIST/ Shd1, ShdPtr
      COMMON /DMMIST/ DMKLDG

C Common block for random number generator.

      REAL*8  DMS0, DMS1
      REAL    DMSS

      COMMON /DMRNCM/ DMS0, DMS1, DMSS

C Common block for biocontrol parameters

      TYPE (BIOCONTROL_DESCRIPTION) BC(MAXBC)

      COMMON /DMBCCM/ BC

C End segment.
