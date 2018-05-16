      SUBROUTINE DMTREG
      IMPLICIT NONE
C----------
C  $Id$
C----------
C **DMTREG -- NISI  Date of last revision: 12/20/03
C--------------------------------------------------------------------
C Purpose:
C   This routine is the main processing point for the NISI (New &
C Improved Spread & Intensification) Model. It carries out the
C following operations: ...
C--------------------------------------------------------------------
C
C Called by:
C
C     MISTOE
C
C Other routines called:
C
C     DMOPTS
C     DMMTRX
C     DMFBRK
C     DMFSHD
C     DMNTRD
C     DMFINF
C     DMNB
C     DMFDNS
C     DMSRC
C     DMTLST
C     DMAUTO
C     DMSAMP
C     DMSLST
C     DMSLOP
C     DMADLV
C     DMOTHR
C     DMCYCL
C     DMNDMR
C     MISDGF
C
C Argument list definitions:
C
C     [none]
C
C Local variable definitions:
C
C     INTEGER i         Loop counter for treelist records and for
C                        species codes.
C     INTEGER j         Loop counter for crown thirds and for species
C                        codes.
C     INTEGER k         Loop counter for DM categories.
C     INTEGER m         Loop counter for sampling rings.
C     INTEGER n         Loop counter for *source* DM categories.
C     INTEGER r         Loop councter for crown thirds and heights.
C     INTEGER s         Loop counter for MESH bands within crown
C                        thirds.
C     INTEGER u         Loop counter for treelist records, and the
C                        index to treelist recrords found in the
C                        list of *targets*.
C     INTEGER v         Loop counter for crown thirds, and the index
C                        to treelist records found in the list of
C                        *sources*.
C     INTEGER jj        Loop counter for treelist records that may
C                        have been left out of the S&I process.
C     INTEGER uu        Loop counter for the list of *targets*, and
C                        a loop counter for treelist records.
C     INTEGER vv        Loop counter for the list of *sources*, and
C                        a loop counter for treelist records.
C     INTEGER Scnt      The number of occurrences of a particular
C                        treelist record in a sample ring.
C     INTEGER Offset    MESH offset due to slope effect.
C     INTEGER LHt       Lower mesh band containing a breakpoint.
C     INTEGER UHt       Upper mesh band containing a breakpoint.
C     INTEGER DMSPtr    Matrix of pointers to treelist, sorted by
C                        species and DM class:
C                        Index 1: species code.
C                        Index 2: DM rating.
C                        Index 3: FST is first position in
C                                 'DMSInd()' array; LST is last
C                                 position in array. This mapping is
C                                 analagous to the 'IND1()' array of
C                                 of the base model, but with two
C                                 levels of indirection.
C     INTEGER DMSInd    Array containing pointers to the treelist.
C                        It is sorted by species and DM rating and
C                        is an analogue of the 'IND1()' array of
C                        the base model.
C     INTEGER Sample    The number of trees of a given DM category
C                        that will be placed in a sampling ring.
C     INTEGER SrcInd    Pointers to the treelist index occupying
C                        corresponding to the cumulative
C                        probability within the 'SrcCD()' array.
C                        These records are sorted by DM category
C                        into groups marked by the 'SrcPtr()' array.
C     INTEGER SrcPtr    Breakpoints demarcating the DM categories
C                        ordered within the 'SrcI() and 'SrcCD()'
C                        arrays. Each value marks the *last* entry
C                        in that category: e.g.: 'Sptr(3)' contains
C                        the position of the last position with DM
C                        rating 3; 'Sptr(2)+1' contains the first.
C     INTEGER SrcLst    List of trees selected to occupy the sampling
C                        sampling ring being processed.
C                        Index 1: The index value to the treelist
C                                 record.
C                        Index 2: The number of occurrences of the
C                                 record.
C     INTEGER TrgLst    The trees to be used as targets; the zero'th
C                        entry contains the number of trees in the
C                        list.
C     REAL    TotalD    The total density (trees/acre) of all trees
C                        of all species in the stand.
C     REAL    dgfx      Proportion of realized diameter growth. This
C                        is used to modify "seed" output.
C     REAL    MISDGF    Dummy parameter for MISDGF function call.
C     REAL    SrcCD     The cumulative probability of each DM group
C                        is computed by taking the relative
C                        density (trees/acre) of each treelist
C                        record and forming a cumulative
C                        distribution.
C     REAL    Dnsty     Trees/acres of each DM class; computed within
C                        a species loop and not accounting for the
C                        presence of any autocorrelation.
C     REAL    SDens     Like 'Dnsty', but after computing
C                        autocorrelation effects that may include
C                        differential density for different sampling
C                        rings.
C     REAL    NBC       Cumulative distribution for the sampling
C                        properties of each sampling ring without
C                        regard to species or DM category.
C     REAL    HtWt      The weight given to the infection within a
C                        slice of canopy. This is required only if
C                        slice does not occupy a full MESH band.
C     REAL    Level     DMR in crown third
C     REAL    x         Intermediate calculation.
C     REAL    Rad       The Radius (MESH) of a slice of canopy.
C     REAL    SprFld    Infection density field; produced in the
C                        *target* record.
C     REAL    IntFld    Infection density field; produced in the
C                        *source* record.
c
C Common block variables and parameters:
C
C     MAXTRE  PRGPRM
C     MAXSP   PRGPRM
C     LST     DMCOM
C     DSTLEN  DMCOM
C     INDX    DMCOM
C     MXTHRX  DMCOM
C     MXHT    DMCOM
C     DCDn    DMCOM
C     ITRN    CONTRL
C     DMRATE  DMCOM
C     CRTHRD  DMCOM
C     DMINF   DMCOM
C     ACTIVE  DMCOM
C     DMDMR   DMCOM
C     DMOPAQ  DMCOM
C     DMOPQM  DMCOM
C     DMOPQ2  DMCOM
C     MESH    DMCOM
C     DMSURV  DMCOM
C     DMDETH  DMCOM
C     NTDn    DMCOM
C     PROB    ARRAYS
C     ISCT    CONTRL
C     DMKTUN  DMCOM
C     KNT     DMCOM
C     BPCNT   DMCOM
C     BrkPnt  DMCOM
C     DMRDMX  DMCOM
C     VOLUME  DMCOM
C     RADIUS  DMCOM
C     TWOPIE  DMCOM
C     IND1    ARRAYS
C     PBrkPt  DMCOM
C
C********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'MISCOM.F77'
      INCLUDE 'DMCOM.F77'

C Local variables.

      INTEGER i,j,k,m,n,r,s,u,v,jj,uu,vv
      INTEGER Scnt
      INTEGER Offset
      INTEGER LHt,UHt
      INTEGER DMSPtr(MAXSP,0:6,LST)
      INTEGER DMSInd(MAXTRE)
      INTEGER Sample
      INTEGER SrcInd(MAXTRE)
      INTEGER SrcPtr(0:6)
      INTEGER SrcLst(0:DSTLEN,INDX)
      INTEGER TrgLst(0:MAXTRE)
      REAL    TotalD
      REAL    dgfx
      REAL    MISDGF
      REAL    SrcCD(MAXTRE)
      REAL    Dnsty(0:6)
      REAL    SDens(0:6)
      REAL    NBC(0:DSTLEN,MXTHRX)
      REAL    HtWt,Level,x,Rad
      REAL    SprFld(MXHT)
      REAL    IntFld(MXHT)

      REAL    DMHtWt

C On the first pass through, initialize the DMINF() with values from
C DMRATE(). For various reasons it is clumsy to attempt this anywhere
C else. Initial values for DMINF() are based on the user's (or
C default) values of DMDMR() and DMRATE().

      IF (.NOT. DCDn) THEN

        DO i = 1,ITRN
          IF (DMRATE(i) .GT. 0) THEN
            DO j = 1,CRTHRD
              DMINF(i,j,ACTIVE) = FLOAT(DMDMR(DMRATE(i),j))
            ENDDO
          ENDIF
        ENDDO

C Create values for DMOPQ2() based on the user's (or default) value
C for DMOPAQ(). This gives a MESH-based value that parallels the
C usage of the cubic meter-based DMOPAQ().

        DO i = 1,MAXSP
          DMOPAQ(i) = DMOPAQ(i) * DMOPQM
          DMOPQ2(i) = 1.0 - ((1.0 - DMOPAQ(i)) ** MESH)
          DMSURV(i) = 1.0 - DMDETH(i)
        ENDDO

        DCDn = .TRUE.

      ENDIF

C See if there are any option processor activities in this cycle.

      CALL DMOPTS

C Determine the radius and volume (MESH units) of each tree in the
C treelist at each height. 'DMRDMX()' is filled as a result.

      CALL DMMTRX

C Find the 4 breakpoints for each tree (1= top; 4= bottom of crown).
C The results are place in 'BrkPnt()'.

      CALL DMFBRK

C Compute the likelihood of shading at any height in the stand. This
C could be modified to reflect the sampling distribution; in which
C case it might be called numerous times to create grids of
C concentric disks.

      CALL DMFSHD

C Call DMNTRD on the cycle following the first entry, to map
C infection values to the new locations of each crown third.

      IF (NTDn) THEN
          CALL DMNTRD
        ELSE
          NTDn = .TRUE.
      ENDIF

C Zero arrays to hold new spread and intensification.

      DO u = 1,ITRN
        DO v = 1,CRTHRD
          NewSpr(u,v) = 0.0
          NewInt(u,v) = 0.0
        ENDDO
      ENDDO

C Scan the treelist and generate a matrix of pointers for the first
C and last records of each DMR class of each species. The indexes
C themselves are stored in 'DMSInd()', the pointer matrix is
C 'DMSPtr()'.

      CALL DMFINF(DMSPtr,DMSInd)

C Create the probability distribution for all stand trees around
C potential targets in sampling ring 'm'. The answer is returned in
C 'NBC()', a matrix representing the cumulative distribution for
C trees within ring 'm'. Usually the length of this vector (DSTLEN)
C will be long enough (for densities up to about 10,000/acre). If
C densities are higher than this, the compile-time length will have
C to be enlarged or the method changed.

      TotalD = 0.0
      DO i = 1,ITRN
        TotalD = TotalD + PROB(i)
      ENDDO

      DO m = 1,MXTHRX
        CALL DMNB(m,TotalD,NBC(0,m))
      ENDDO

C Loop over tree species.

      DO 100 j = 1,MAXSP

        IF (MISFIT(j) .EQ. 0 .OR. ISCT(j,1) .EQ. 0) GOTO 100

C Find the trees/acre density of each Target DMR class 'k' of species
C 'j'. The answer is returned in 'Dnsty()'. A vector is computed
C rather than one-at-a-time, because the SUM of DMR is needed to
C carry out DMAUTO.

        CALL DMFDNS(j,DMSPtr,DMSInd,PROB,Dnsty)

C Set up vector of Source trees and weights, so that random Source
C trees can be selected. The answer is returned as the paired vectors
C 'SrcInd()' and 'SrcCD()', which hold the treelist record and
C cumulative proportional probability associated with the record
C number, respectively. The offsets within these vectors are held by
C 'SrcPtr()'. This is all used by DMSLST.

        CALL DMSRC(j,Dnsty,DMSPtr,DMSInd,PROB,SrcInd,SrcCD,SrcPtr)

C Loop over the Target DMR classes.

        DO 200 k = 0,6

          IF (Dnsty(k) .LE. 0.0) GOTO 200

C A few loops from now we will walk through a list of Targets of
C species 'j' having DMR 'k'. If no Targets exist, fall through to
C the bottom of the Target loop. Otherwise, choose the players.

          CALL DMTLST(j,k,DMSPtr,DMSInd,PROB,TrgLst)

          IF (TrgLst(0) .EQ. 0) GOTO 200

          DO m = 1,MXTHRX  ! Loop over sampling rings

C Find the density for each Source DMR class in each ring,
C conditioned on the Target DMR. 'SDens()' is returned with the
C density of each Source DMR class computed using the alpha and beta
C autocorrelation terms.

            CALL DMAUTO(k,m,Dnsty,SDens)

            DO 400 n = DMKTUN(j),6 ! Loop over Source DMR classes

              IF (DMSPtr(j,n,FST) .EQ. 0) GOTO 400

              DO 500 uu = 1,TrgLst(0)

                u = TrgLst(uu)

C Find out how many Source trees of DMR 'n' to place in sampling
C ring 'm' surrounding the 'TrgLst()' record 'u' having DMR 'k'.
C The answer is returned in 'Sample'. If the sample size is zero,
C fall through to the bottom of the loop.

                CALL DMSAMP(TotalD,SDens(n),NBC(0,m),1.0,Sample)
                IF (Sample .EQ. 0) GOTO 500

C Select 'Sample' random infections with 'n' Source DMR. Treelist
C record indices are returned in 'SrcLst()'; 'SRcLst(0,INDX)' holds
C the number of unique indices.

                CALL DMSLST(n,Sample,SrcInd,SrcCD,SrcPtr,SrcLst)

                DO vv = 1,SrcLst(0,INDX) ! Loop over the Source trees.

C Zero the spread fields about to be experienced by
C Source 'v' and Target 'u'.

                  DO r = 1,MXHT
                    SprFld(r) = 0.0
                  ENDDO

C Get the index to the source tree and the number of occurrences.

                  v    = SrcLst(vv,INDX)
                  Scnt = SrcLst(vv,KNT)

C Get factor modifying periodic diameter growth. This affects
C 'Level', the effective seed production of the infection.

                  dgfx = MISDGF(v,j)

C See if the site slope will shift the source-target relationship.
C The answer returned in 'Offset' is the difference (+ is above; - is
C below) in MESH height of the Source Tree relative to the Target. If
C Offset is > 0 and Scnt > 1 then this will compell all target trees
C to have only 1 Offset, and variation will be underestimated in some
C situations. It is also relevenat ONLY if the density of the
C particular source tree is extremely high; such that it will appear
C more than once in a sample ring.

                  CALL DMSLOP(m,Offset)

C Loop over crown thirds of Source trees. The 'r' index is to LOWER
C breakpoint in each third.
C
C Find the upper and lower MESH categories into which each of the
C crown thirds is mapped. For each category define a weight
C defining how to apportion the upper and lower infection level.
c Then walk through the crown third in MESH meter bands from
C 'LHt' to 'UHt'.
c
c Note that IntFld is still present in the DMADLV arg list but not
c assigned until the very end of the subroutine

                  DO r = 2,BPCNT
                    Level = DMINF(v,r-1,ACTIVE)
                    IF (Level .GT. 0.0) THEN
                      LHt = INT(BrkPnt(v,r)) + 1
                      UHt = MIN(MXHT,INT(BrkPnt(v,r-1)) + 1)
                      DO s = LHt,UHt
                        HtWt = DMHtWt(v,r,s,Lht,Uht)
                        x = Level * HtWt * DMRDMX(v,s,VOLUME) * dgfx
                        CALL DMADLV(v,Scnt,SprFld,IntFld,
     >                    (s+Offset),m,x,Shade,Shade,0,0)
                      ENDDO
                    ENDIF
                  ENDDO

C Spread assumes a simple centre-of-source to centre-of-target
C geometry. In reality the source is an infected disk, so the
C subtended angle varies with position within the disk. The effect
C is more pronounced when Source-Target distance is small. The bias
C from this assumption is usually very small.  Final interception
C depends on apparent size of target from source, and is further
C modified by the radius of the disk (actually should be something
C like the mean extinction along the integrated path length through
C the disk). Thicker trees will catch more seed this way. Note that
C interception is the Target 'u'.

                  DO r = 2,BPCNT
                    LHt = INT(BrkPnt(u,r)) + 1
                    UHt = MIN(MXHT,INT(BrkPnt(u,r-1)) + 1)
                    DO s = LHt,UHt
                      HtWt = DMHtWt(v,r,s,Lht,Uht)
                      Rad = DMRDMX(u,s,RADIUS)
                      x = ATAN(Rad / (FLOAT(m) - 0.5)) * TWOPIE
                      NewSpr(u,r-1) = NewSpr(u,r-1) +
     >                  (HtWt * x * SprFld(s) * Rad)
                    ENDDO
                  ENDDO

                ENDDO   ! end looping over Source trees
  500         CONTINUE ! end looping over Target trees
  400       CONTINUE  ! end looping over Source DMR classes
          ENDDO      ! end looping over ring samples
  200   CONTINUE    ! end looping over Target DMR classes

C     Intensification needs to be added separately, so that a common
c     distance (MXTHRX) is used and all trajectories are included

        DO jj = ISCT(j,1),ISCT(j,2)

          v = IND1(jj)

          DO r = 1,MXHT
            IntFld(r) = 0.0
          ENDDO

          dgfx = MISDGF(v,j)

c Note that SprFld is still in the Arg list, but not used because
c it has already been computed in the main loop

          DO r = 2,BPCNT
            Level = DMINF(v,r-1,ACTIVE)
            IF (Level .GT. 0.0) THEN
              LHt = INT(BrkPnt(v,r)) + 1
              UHt = MIN(MXHT,INT(BrkPnt(v,r-1)) + 1)
              DO s = LHt,UHt
                HtWt = DMHtWt(v,r,s,Lht,Uht)
                x = Level * HtWt * DMRDMX(v,s,VOLUME) * dgfx
                CALL DMADLV(v,1,SprFld,IntFld,s,MXTHRX,x,
     >               Shade,Shade,0,0)
              ENDDO
            ENDIF
          ENDDO

C 'IntFld()' is implicitly a volume-based amount. It is scaled by
C 'HtWt' to account for the variable height across breakpoints.

          DO r = 2,BPCNT
            LHt = INT(BrkPnt(v,r))   + 1
            UHt = MIN(MXHT,INT(BrkPnt(v,r-1)) + 1)
            DO s = LHt,UHt
              IF (IntFld(s) .GT. 0.0) THEN
                HtWt = DMHtWt(v,r,s,Lht,Uht)
                NewInt(v,r-1) = NewInt(v,r-1) +
     &            (IntFld(s) * HtWt * DMRDMX(v,s,VOLUME))
              ENDIF
            ENDDO
          ENDDO
        ENDDO

C End of un-intensified patch.

        CALL DMOTHR(j)

C End of looping over species.

  100 CONTINUE

C Iterate each year within model timestep, then compute new DMR index
C after growth cycle.

      CALL DMCYCL
      CALL DMNDMR

C Cycle complete. Map breakpoints to PBrkPt() before growth occurs.

      DO u = 1,ITRN
        DO v = 1,BPCNT
          PBrkPt(u,v) = BrkPnt(u,v)
        ENDDO
      ENDDO

      RETURN
      END

C------------------------------------------------------------------

C     PRIVATE FUNCTION TO COMPUTE WEIGHT OF SLICE WITHIN CROWNTHIRD
C
C     THE UNUSUAL IF/ELSE IF/ LOGIC IS THE RESULT OF THE FACT THAT
C     SPECIAL WEIGHTING IS ONLY NEEDED FOR BREAKPOINTS 2 AND 3 (THE INNER
C     BREAKPOINTS). THE CALCULATION OF THE VOLUME AND RADIUS OF THE TOP
C     AND BOTTOM PIECES ALREADY ACCOUNTS FOR THE FACT THAT A TRUNCATED
C     SECTION MAY BE INVOLVED.

      REAL FUNCTION DMHtWt(v,r,s,UHt,LHt)

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'DMCOM.F77'

      INTEGER v,r,s
      INTEGER UHt,LHt
      REAL    HtWt

      IF ((LHt .EQ. s) .AND. (r .LT. BPCNT)) THEN
        HtWt = 1.0 - (BrkPnt(v,r) - INT(BrkPnt(v,r)))
      ELSE IF ((UHt .EQ. s) .AND. (r .GT. 2)) THEN
        HtWt = BrkPnt(v,r-1) - INT(BrkPnt(v,r-1))
      ELSE
        HtWt = 1.0
      END IF

      DMHtWt = HtWt

      RETURN
      END
