C.... Code segment BMRRCM
C----------
C WWPB $Id$
C----------
************************************************************************
*  **BMRR  Date of last revision:  JUNE 22, 1994
*-----------------------------------------------------------------------
*  Purpose:
*     Common block variables related to the Stand level Root Disease and
*     Stem Rust embedded in Westwide Pine Beetle Model.
*-----------------------------------------------------------------------
*
*     BMRRSR:   Decadal spread rate of RR
*     BMSRSR:   Decadal spread rate of SR
*     BMRR:     Proportion of stems infected by RR.
*     BMSR:     Proportion of stems infected by SR.
*
***********************************************************************

      LOGICAL LRRON, LSRON
      REAL    BMRRSR, BMSRSR
      REAL    BMRR(MAXTRE), BMSR(MAXTRE)

      COMMON /BMRRCM/ LRRON, LSRON, BMRRSR, BMSRSR, BMRR, BMSR

C.... End segment.
