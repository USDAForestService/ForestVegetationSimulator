CODE SEGMENT BWECOM
C ----------
C WSBWE $Id$
C ----------
C
C  --> VARIABLES FOR THE BUDWORM DAMAGE MODEL
C
C Revision History:
C   03-MAY-00 Lance David (FHTET)
C     Reduced size of array TABLE from 111 to 25, the actual number
C     of Defoliation Model keywords.
C   12-MAY-00  Lance David (FHTET)
C     Change variable ISIZE to TABSZ because ISIZE was used
C     elsewhere in the model for size classes. The was a problem for serial
C     processing of multiple stands, second set of budworm keywords failed.
C   16-AUG-01 Lance R. David (FHTET)
C     Added DSEEDD and DSEEDR variables for Damage model random number seed.
C   10-AUG-2006 Lance R. David (FHTET)
C     Reorganizing and removing obsolete variable in preparation for
C     linkage with PPE.
C   01-28-09 - Lance R. David (FMSC)
C      Common blocks added for character and double precision variables
C      that were in common BWECOM due to compiler complaint (mixed 
C      variable types in common) reported by Don Robinson (ESSA).
C   07/09/10 Lance R. David (FMSC)
c      Declared BWFINT for IMPLICIT NONE addition to subroutines.
C---------------------------------------------------------------------------

      CHARACTER*8 TABLE(25)
      INTEGER  IBWSPM(MAXSP),IBWYR1,IBWYR2,ICUMYR,IPRBYR,ITODO,
     &         IYRCUR,JOWSBW,NCROWN,NCUMYR,NHOSTS,NTODO,TABSZ
      REAL  BWFINT,DSEEDR
      DOUBLE PRECISION DSEEDD
      LOGICAL LBWDAM,LBWPDM,LCALBW,LDEFOL,LTOPK

      COMMON /BWECOM/ BWFINT,DSEEDR,IBWSPM,IBWYR1,IBWYR2,
     &   ICUMYR,IPRBYR,ITODO,IYRCUR,JOWSBW,LBWDAM,LBWPDM,LCALBW,
     &   LDEFOL,LTOPK,NCROWN,NCUMYR,NHOSTS,NTODO,TABSZ

      COMMON /BWECOMC/ TABLE

      COMMON /BWECOMD/ DSEEDD
C
C   LCALBW = TRUE IF BUDLITE MODEL IS CALLED
C   LDEFOL = TRUE IF USER SUPPLIES ANNUAL DEFOLIATION RATES
C   LTOPK =  TRUE IF TOP-KILL IS TO BE CALCULATED & APPLIED
C   LBWDAM = TRUE IF "DAMAGE" OUTPUT IS TO BE PRINTED
C   LBWPDM = TRUE IF "PERDAM" OUTPUT IS TO BE PRINTED
C
C   IBWSPM =  [set to 11 for Blue Mountains variant]
C   TABLE =  LOOKUP TABLE FOR BUDWORM KEYWORDS
C   TABSZ =  NUMBER OF BUDWORM KEYWORDS (dimension of array TABLE)
C   NHOSTS = NUMBER OF HOST SPECIES
C   NCROWN = NUMBER OF CROWN LEVELS
C   JOWSBW = FILE NUMBER FOR BUDWORM OUTPUT
C   IBWYR1 = FIRST YEAR IN BUDWORM SIMULATION
C   IBWYR2 = LAST YEAR IN BUDWORM SIMULATION
C   IYRCUR = CURRENT YEAR
C   IPRBYR =
C   ICUMYR =
C   NCUMYR =
C   ITODO  =
C   NTODO  =
C   BWFINT = NUMBER OF YEARS IN THE BUDWORM PORTION OF A CYCLE
C   DSEEDD = RANDOM NUMBER SEED VALUE FOR DAMAGE MODEL -DOUBLE PRECISION-
C   DSEEDR = RANDOM NUMBER SEED VALUE FOR DAMAGE MODEL -REAL-
C --------END SEGMENT
