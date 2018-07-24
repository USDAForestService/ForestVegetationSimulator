      SUBROUTINE DMOPTS
      IMPLICIT NONE      
C----------
C CANADA-NEWMIST $Id$
C----------
C **DMOPTS -- NISI  Date of last revision: April 10 1994 
C----------------------------------------------------------------------
C Purpose:
C  Users are provided three date-sensitive options in the model:
C DMCLMP, DMALPH and DMBETA. This routine checks to see if any of
C those keywords has been scheduled for this cycle, and performs
C necessary calculations if required. The calculation of the 'SF()'
C array mirrors the calculation performed in DMINIT.
C----------------------------------------------------------------------
C
C Called by:
C
C     DMTREG 
C
C Other routines called:
C
C     OPFIND
C     OPGET
C     OPDONE
C
C Argument list definitions:                        
C
C     [none]
C
C Local variable definitions:
C
C     INTEGER   i         Loop counter for various things: options,
C                          trees, plots and DM categories.
C     INTEGER   j         Loop counter for sampling rings.
C     INTEGER   n         Counter for actual number of sample points
C                          encountered.
C     INTEGER   p         Sample plot number.
C     INTEGER   NTODO     The number of activities scheduled for the
C                          option in the current cycle.
C     INTEGER   IDATE     Dummy value containing the date for which
C                          the OP call is scheduled.
C     INTEGER   IACTK     Dummy value holding the completion status
C                          of the OP call.
C     INTEGER   NPAR      Dummy value containing the number of
C                          parameters returned from the OP call.
C     REAL      Parm      The array in which option parameters are
C                          returned.
C     REAL      Mean      The mean density of trees.
C     REAL      Var       The variance of the density, computed
C                          either by the Dispersion index or by
C                          the variance of the sample points.
C     REAL      x         Scalar used in calculation of sample ring
C                          modified density; also used to hold
C                          trees/acre density of individual records.
C     REAL      tmp       Temporary calculation for sample ring
C                          density scaling.
C     REAL      PltDns   Dnsty (trees/acre) for each sample point.
C
C Common block variables and parameters:
C
C     MXTHRX    DMCOM
C     MAXPLT    PRGPRM
C     IY        CONTRL
C     ICYC      CONTRL
C     DMALPH    DMCOM
C     DMBETA    DMCOM
C     SF        DMCOM
C     IPTINV    PLOT
C     ITRE      ARRAYS
C     PROB      ARRAYS
C     PI        PLOT
C
C**********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'PLOT.F77'
      INCLUDE 'CONTRL.F77'
      INCLUDE 'ARRAYS.F77'
      INCLUDE 'DMCOM.F77'

C Local variables.
      
      INTEGER   i, n, p
      INTEGER   NTODO, IDATE, IACTK, NPAR
      INTEGER   MYACTS(2)
      REAL      Parm(2)
      REAL      Mean, Var, x      
      REAL      PltDns(MAXPLT)
      
      DATA MYACTS /2008,2009/

C See if options have been specified for DMALPH and/or DMBETA. A
C Parm() value of -999 means "don't change the existing value".

      NTODO = 0
      CALL OPFIND(1, MYACTS(2), NTODO)
      IF (NTODO .NE. 0) THEN
        DO 8 I = 1,NTODO
          CALL OPGET(I, 2, IDATE, IACTK, NPAR, Parm)
          CALL OPDONE(I,IY(ICYC))
          IF(Parm(1) .NE. -999.0) DMALPH = Parm(1)
          IF(Parm(2) .NE. -999.0) DMBETA = Parm(2)
    8   CONTINUE
        CALL DMNAC(DMALPH,DMBETA)
      ENDIF

 
C Determine if DMCLMP has been specified. If not, the stems/acre
C dispersion among plots is used. Plots are not assumed to be in any
C order, even though I suspect that they probably are.

      NTODO = 0
      CALL OPFIND(1, MYACTS(1), NTODO)
      IF (NTODO .NE. 0) THEN
        DO 19 I = 1, NTODO
          CALL OPGET(I, 1, IDATE, IACTK, NPAR, Parm)
          CALL OPDONE(I, IY(ICYC))
          DMCLMP = Parm(1)
   19   CONTINUE 
      
      ELSE 
      
        IF (IPTINV .GT. 1) THEN
        
          DO 20 i=1, MAXPLT   
            PltDns(i) = 0.0
   20     CONTINUE
      
          Mean = 0.0
          DO 21 i = 1, ITRN 
            p = ITRE(i)
            x = PROB(i)
            PltDns(p) = PltDns(p) + x
            Mean = Mean + x
   21     CONTINUE        
          Mean = Mean / PI 
      
          Var = 0.0 
          n = 0
          DO 22 i = 1, MAXPLT
            IF (PltDns(i) .GT. 0.0) THEN
              Var = Var + (PltDns(i) - Mean)**2
              n = n + 1
            END IF
   22     CONTINUE                                    
 
          IF (n .GT. 1) THEN 
            Var = Var / (FLOAT(n) - 1.0)
            DMCLMP = Var / Mean
          END IF
          
        END IF
                 
      ENDIF

      RETURN
      END
