      MODULE DEBUG_MOD

!...  Module to store NVEL debug data

      IMPLICIT  NONE

!CREV  Created TDH 04/01/09
!CREV  Revised TDH 11/18/09
!CREV  added VEDBG unit number for debugging the getvoleq subroutine

      TYPE :: DEBUG_INDICATORS
         LOGICAL   INPUT             ! LBP input debug
         LOGICAL   DLL               ! LBP DLL debug
         LOGICAL   MODEL             ! LBP model debug
         LOGICAL   ANALYSIS          ! LBP analysis debug
         LOGICAL   EXTERNAL          ! Ext subroutine call debug
         LOGICAL   VOLEQ            ! LBP design debug
      END TYPE DEBUG_INDICATORS

!      CHARACTER DEBUG_FILE_NAME*160
!      CHARACTER SA_DEBUG_FILE_NAME*160
!      LOGICAL   DEBUG_FILE_OPEN    / .FALSE. /
      LOGICAL   ANY_DEBUG          / .FALSE. /
      
      INTEGER   LUDBG /  1 /  ! Debug report unit number
      INTEGER   VEDBG /  3 /  ! Vol Eq report unit number
      INTEGER   LUOUT /  2 /  ! Ouput report unit number
      TYPE(DEBUG_INDICATORS) DEBUG

      END MODULE DEBUG_MOD