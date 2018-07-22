      SUBROUTINE BRINIT
      IMPLICIT NONE
C----------
C WPBR $Id$
C----------
C  Purpose:
C  Initializes the Blister Rust model variables.
C----------------------------------------------------------------------
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C  27-MAY-1999 Lance David
C     Added initialization of RIAF (rust index adjustment factor).
C  03-JUN-1999 Lance David
C     Added logical variables BRTHDR and BRCHDR for canker and tree
C     list main header printing control.
C  13-DEC-2000 Lance R. David (FHTET)
C     Changed stock type resistance factors.
C  06-MAR-2001 Lance R. David (FHTET)
C     Added initialization of pathological pruning variable, LPATPR.
C     Added initialization for branch and bole canker growth rates.
C  14-MAR-2001 Lance R. David (FHTET)
C     DFACT variable expanded to array by species and stock type.
C  24-APR-2001 Lance R. David (FHTET)
C     Added species dimension to PRPSTK and RESIST arrays.
C  01-APR-2001 Lance R. David (FHTET)
C     Expanded tree category scalar variables to arrays.
C  03-MAY-2001 Lance R. David (FHTET)
C     Initialization loops for variables expanded to species arrays.
C  09-MAY-2001 Lance R. David (FHTET)
C     Changed use of FVS parameter MAXSP to WPBR parameter NBRSP for
C     BR species-specific arrays.
C  11-MAY-2001 Lance R. David (FHTET)
C     Added accumulator for total historical mortality (TBRHMR).
C  16-MAY-2001 Lance R. David (FHTET)
C     Canker growth rate arrays (BOGRTH,BRGRTH) initialization.
C  06-NOV-2002 Lance R. David (FHTET)
C     Initial values for RI based on exposure time Gaussian function 
C     (variables: MINRI, MAXRI, PKAGE, RISHP).
C  11-NOV-2006 Lance R. David (FHTET)
C     Added call to reset random number generator.
C  15-MAY-2006 Lance R. David (FHTET)
C     Added BROUT keyword variables LBRDBH and LBRSUM.
C  12-JUN-2006 Lance R. David (FHTET)
C     Moved RIBUS initialization from brblkd.
C**********************************************************************

C.... Common include files.

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'BRCOM.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'CONTRL.F77'

      INTEGER I4, I, J, K, L

C.... Reset the random number seed generator. WK6(1) is just used
C.... as a dummy variable as it is not utilized in the process.

      CALL BRNSED(.FALSE., WK6(1))

C.... Initializations of logical variables from keyword options.

      LBRSUM  = .TRUE.
      LBRDBH  = .FALSE.
      LMETRIC = .FALSE.
      LDFACT  = .FALSE.
      LPATPR  = .FALSE.
      LPRUN   = .FALSE.
      LCLEN   = .FALSE.
      LPRGO   = .FALSE.
      LEXGO   = .FALSE.
      BRYES   = .FALSE.
      BRTHDR  = .TRUE.
      BRTL    = .FALSE.
      BRCHDR  = .TRUE.
      BRCL    = .FALSE.
      BRCMP   = .FALSE.
      CKINIT  = .FALSE.

C.... Pruning and excising guidelines.

      SRATE(1) = 0.9
      SRATE(2) = 0.5
      HTPRPR   = 0.50
      EXDMIN   = 3.0*2.54
      HTMAX(1) = 8.0*30.48
      HTMAX(2) = 6.0*30.48
      GIRMAX   = 50.0
      GIRMRT   = 100.0       ! Changed from 90.0 7-nov-02
      HTMIN    = 3.0*2.54
      OUTDST   = 6.0*2.54
      OUTNLD   = 24.0*2.54

C.... Stand deviation factor and Rust Index Adjustment Factor.

      DO I4 = 1,NBRSP
        DFACT(I4,1)=0.33
        DFACT(I4,2)=0.33
        DFACT(I4,3)=0.33
        DFACT(I4,4)=0.33
        RIAF(I4)   =1.0
        LBROUT(I4) =.FALSE.
      END DO

C.... Inactivation rates.

      RATINV(1)=0.05
      RATINV(2)=0.01

C.... Default growth index, rust index, ribes population variables, etc.

      GIDEF    = 15.0
      RIBPRP(1)= 0.0
      RIBPRP(2)= 0.5
      RIBPRP(3)= 0.5
      RIDEF    = 0.015
      RIMETH   = 0
      RIBUS(1,1) = 0.0
      RIBUS(1,2) = 200.0
      RIBUS(1,3) = 200.0
      RIBUS(2,1) = 0.0
      RIBUS(2,2) = 75.0
      RIBUS(2,3) = 75.0

C.... These variables are for exposure time (age) based RI calculations
C.... and are initialized to zero. Equation-specific defaults are set
C.... during rustindx keyword processing.
      MINRI = 0.0
      MAXRI = 0.0
      PKAGE = 0.0
      PKSHP = 0.0


C.... Branch canker radial growth and Bole canker diameter growth rates (cm)
C.... First array dimension is BR species, second dimension is stock type.
C.... White Pine branch
      BRGRTH(1,1) = 5.0
      BRGRTH(1,2) = 5.0
      BRGRTH(1,3) = 5.0
      BRGRTH(1,4) = 5.0
C.... White Pine bole
      BOGRTH(1,1) = 4.5
      BOGRTH(1,2) = 4.5
      BOGRTH(1,3) = 4.5
      BOGRTH(1,4) = 4.5
C.... Sugar Pine branch
      BRGRTH(2,1) = 5.0
      BRGRTH(2,2) = 5.0
      BRGRTH(2,3) = 5.0
      BRGRTH(2,4) = 5.0
C.... Sugar Pine bole
      BOGRTH(2,1) = 4.5
      BOGRTH(2,2) = 4.5
      BOGRTH(2,3) = 4.5
      BOGRTH(2,4) = 4.5

C.... Canker count.
      INCAN  = 0

      DO I4 = 1,NBRSP

C....    Tree count.
         BRNTRECS(I4) = 0

C....    Stock type resistance factors.

         RESIST(I4,1)=1.00
         RESIST(I4,2)=0.33
         RESIST(I4,3)=0.17
         RESIST(I4,4)=0.11

C....    Default proportions of white pine stock types which
C....    make up the total white pine population.

         PRPSTK(I4,1)=1.0
         PRPSTK(I4,2)=0.0
         PRPSTK(I4,3)=0.0
         PRPSTK(I4,4)=0.0

C....    Stand average summary variables which are used in subroutine
C....    BRSTAT.  Also summary variables for the 2" DBH class table.

         AVTCPT(I4)=0.0
         AVLCPT(I4)=0.0
         AVECPT(I4)=0.0
         PITCA(I4)=0.0
         PILCA(I4)=0.0
         STSUM(I4)=0.0
      END DO

      DO I=1,10
         D2AVT(I)=0.0
         D2AVL(I)=0.0
         D2AVE(I)=0.0
         D2PIT(I)=0.0
         D2PIL(I)=0.0
      END DO

C.... Initialize tree category accumulation variables which are
C.... used in subroutine BRTSTA.  Also summary variables for the
C.... 2" DBH class table.

      DO I4 = 1,NBRSP
         AVGGI(I4) =0.0
         AVGRI(I4) =0.0
         AVGSTS(I4)=0.0
         TBRHST(I4)=0.0
         TBRCLN(I4)=0.0
         TBRNOL(I4)=0.0
         TBRPRN(I4)=0.0
         TBREXC(I4)=0.0
         TBRNOS(I4)=0.0
         TBRGIR(I4)=0.0
         TBRMRT(I4)=0.0
         TBRHMR(I4)=0.0
         THPROB(I4)=0.0
         TRETN(I4) =0.0
      END DO

      DO I=1,10
         D2CLN(I)=0.0
         D2NOL(I)=0.0
         D2PRN(I)=0.0
         D2EXC(I)=0.0
         D2NOS(I)=0.0
         D2GIR(I)=0.0
         D2DED(I)=0.0
         D2WP(I)=0.0
      END DO

C.... Initialize tree-specific variables for maximum number of trees.

      DO 30 J=1,MAXTRE
         UPMARK(J)=10000.0
         BRHTBC(J)=0.0
         GI(J)=0.0
         RI(J)=0.0
         BRGD(J)=0.0
         BRPB(J)=0.0
         ESTCAN(J)=0.0
         TSTARG(J)=0.0
         ITCAN(J)=0
         ILCAN(J)=0
         BRAGE(J)=0.0
         ISTOTY(J)=0
         IBRTID(J)=0
         ICRED(J)=0
         IBRSTAT(J)=0
         LEXMLT(J)=.FALSE.

C....    Initialize canker-specific variables for the maximum number
C....    of cankers.

         DO 25 K=1,10
            ISTCAN(K,J)=0
            GIRDL(K,J)=0.0
            DUP(K,J)=0.0
            DOUT(K,J)=0.0
   25    CONTINUE
   30 CONTINUE

C.... Initialize blister rust summary output variables loaded in
C.... subroutine BRSUM (each WPBR species, 18 values every cycle).

      DO 46 J=1,NBRSP
        DO 45 K=1,18
           DO 40 L=1,41
              BROUT(J,K,L)=0.0
   40      CONTINUE
   45   CONTINUE
   46 CONTINUE

C.... Initialize blister rust summary output variables for the 2" DBH
C.... class table, loaded in BRSUM (10 2" DBH classes, 13 categories of
C.... information, for up to 41 cycles).

      DO 60 I=1,10
         DO 55 J=1,13
            DO 50 K=1,41
               BRDBHO(I,J,K)=0.0
   50       CONTINUE
   55    CONTINUE
   60 CONTINUE

C.... Common return.

      RETURN
      END
