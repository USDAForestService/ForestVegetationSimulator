      SUBROUTINE DMRANN (SEL)
      IMPLICIT NONE
C----------
C  $Id$
C----------
C **DMRANN -- NISI  Date of last revision: April 12 1994 
C----------------------------------------------------------------------
C Purpose:
C   This routine is based on the uniform random number found in
C ESRANN, coded by Nicholas Crookston. It is used in many places
C to simulate random processes.
C----------------------------------------------------------------------
C
C Called by:
C
C     DMINIT    [ENTRY DMRNSD] 
C     MISIN     [ENTRY DMRNSD]
C     DMFSHD
C     DMNDMR
C     DMSAMP
C     DMSLOP
C     DMSLST
C     DMTLST
C     
C Other routines called:
C
C     [none]
C
C Argument list (and ENTRY point) definitions:                        
C
C     INTEGER   SEL   (O) Uniform random number.
C     LOGICAL   LSET  (I) Logical value specifying whether the
C                          generator is being reseeded (.TRUE.).
C     REAL      SEED  (I) Value to reseed the random
C                          sequence. 
C
C Local variable definitions:
C
C     [none]
C
C Common block variables and parameters:
C
C     DMS0      DMCOM
C     DMS1      DMCOM
C     DMSS      DMCOM
C
C**********************************************************************

      INCLUDE 'PRGPRM.F77'
      INCLUDE 'DMCOM.F77'
      
C Argument list (including ENTRY point) variables.

      REAL      SEL
      LOGICAL   LSET
      REAL      SEED

      DMS1=DMOD(16807D0*DMS0,2147483647D0)
      SEL=DMS1/2147483648D0
      DMS0=DMS1
      RETURN

C You may reseed the generator by supplying an real-valued odd
C number with LSET = .TRUE. If LSET = .FALSE. a call to DMRNSD will
C cause the random number generator to start over.

      ENTRY DMRNSD (LSET, SEED)   
      IF (LSET) GOTO 10
      SEED=DMSS
      DMS0=SEED
      RETURN
   10 CONTINUE
      IF (AMOD(SEED,2.0).EQ.0.) SEED=SEED+1
      DMSS=SEED
      DMS0=SEED
      RETURN
      END
