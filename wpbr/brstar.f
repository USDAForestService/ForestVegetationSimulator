      SUBROUTINE BRSTAR(HT,STAR)
      IMPLICIT NONE
C----------
C WPBR $Id$
C----------
C  Purpose:
C  Calculates the value of the sum of target area for a tree.
C----------------------------------------------------------------------
C
C  Parameters
C     Passed: HT     - Tree's height for current year in meters
C                      (current height + proportion of height growth)
C     Returned: STAR - Current sum of target area for the tree
C                      (thousands of needles)
C
C  Revision History:
C
C  dd-MMM-YYYY programmer_name
C     description of change or update.
C
C**********************************************************************
C.... Local variable declarations.

      REAL HT, STAR, CFA

C.... Calculate total needles on crown in thousands at given age (sum
C.... target area).

      STAR=EXP(2.1717+(1.3633*ALOG(HT))-(0.13758/(HT**2)))+0.02

      IF(HT.LE.5.0) THEN

C....    Calculate correction factor for trees less than 5 meters tall.

         CFA=(0.69-(3.58*ALOG(HT))+(12.3*ALOG(HT**2))+(19.7*ALOG(HT**3))
     &      +(7.76*ALOG(HT**4)))/(1-(4.59*ALOG(HT))+(11.53*ALOG(HT**2))
     &      +(21.03*ALOG(HT**3))+(7.7*ALOG(HT**4))+(0.3*ALOG(HT**5)))

C....    Apply the correction factor to the needle total.

         STAR=STAR*CFA
      ENDIF

      RETURN
      END
