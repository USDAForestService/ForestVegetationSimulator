!--------------------------------------------------------------------------
! BASE version.f
!
! Provides the FVSVERSION module, which exposes build provenance metadata
! injected at compile time by bin/makefile via -D preprocessor flags.
!
! Subroutines:
!   fvsGetVersion(version, nch)   Short version string for --version flag
!   fvsGetBuildInfo(info, nch)    Full provenance block for --build-info flag
!
! Both subroutines are also callable via the shared library API from R,
! Python, or any language with a Fortran or C FFI.
!
! All metadata derives from the FVS source repo at build time.
! Re-run `make` to refresh after tagging or committing.
!
! A -dirty suffix on version or commit indicates an uncommitted working
! tree. A -dirty binary should never appear in production or deliverable
! contexts.
!--------------------------------------------------------------------------

MODULE FVSVERSION
  USE ISO_C_BINDING
  IMPLICIT NONE

#ifndef FVS_GIT_VERSION
#define FVS_GIT_VERSION "unknown"
#endif
#ifndef FVS_GIT_ORG
#define FVS_GIT_ORG     "unknown"
#endif
#ifndef FVS_GIT_HASH
#define FVS_GIT_HASH    "unknown"
#endif
#ifndef FVS_GIT_DATE
#define FVS_GIT_DATE    "unknown"
#endif
#ifndef FVS_GIT_BRANCH
#define FVS_GIT_BRANCH  "unknown"
#endif
#ifndef FVS_GIT_REMOTE
#define FVS_GIT_REMOTE  "unknown"
#endif

  CHARACTER(LEN=*), PARAMETER :: FVSVER_VERSION = FVS_GIT_VERSION
  CHARACTER(LEN=*), PARAMETER :: FVSVER_ORG     = FVS_GIT_ORG
  CHARACTER(LEN=*), PARAMETER :: FVSVER_HASH    = FVS_GIT_HASH
  CHARACTER(LEN=*), PARAMETER :: FVSVER_DATE    = FVS_GIT_DATE
  CHARACTER(LEN=*), PARAMETER :: FVSVER_BRANCH  = FVS_GIT_BRANCH
  CHARACTER(LEN=*), PARAMETER :: FVSVER_REMOTE  = FVS_GIT_REMOTE

  ! Stable buffer size constants for C/ctypes callers.
  ! Callers should allocate at least these many bytes.
  ! FVS_VERSION_BUFLEN covers: tag + ' (' + org + ')' with room to spare.
  ! FVS_BUILDINFO_BUFLEN covers: 6 labeled fields at ~80 chars each.
  INTEGER(KIND=C_INT), PARAMETER :: FVS_VERSION_BUFLEN   = 64
  INTEGER(KIND=C_INT), PARAMETER :: FVS_BUILDINFO_BUFLEN = 512

  CONTAINS

 !------------------------------------------------------------------------
  ! FVSGETVERSION
  !
  !   buf     (OUT) Caller-allocated buffer
  !   buflen  (IN)  Size of buf in bytes; allocate FVS_VERSION_BUFLEN
  !                 to guarantee no truncation
  !   nch     (OUT) Actual length written, not including null terminator.
  !                 If nch == buflen the string was truncated.
  !
  ! Fortran callers: declare a fixed-length local and pass LEN(buf):
  !
  !   CHARACTER(LEN=FVS_VERSION_BUFLEN) :: buf
  !   INTEGER :: nch
  !   CALL fvsGetVersion(buf, FVS_VERSION_BUFLEN, nch)
  !   WRITE(*,'(A)') buf(1:nch)
  !
  ! C/ctypes callers: allocate FVS_VERSION_BUFLEN bytes and pass its size.
  !------------------------------------------------------------------------
  SUBROUTINE fvsGetVersion(buf, buflen, nch)
    CHARACTER(KIND=C_CHAR), INTENT(OUT) :: buf(*)
    INTEGER(KIND=C_INT),    INTENT(IN)  :: buflen
    INTEGER(KIND=C_INT),    INTENT(OUT) :: nch

    CHARACTER(LEN=FVS_VERSION_BUFLEN) :: tmp
    INTEGER :: i

    tmp = FVSVER_VERSION // ' (' // FVSVER_ORG // ')'
    nch = MIN(LEN_TRIM(tmp), INT(buflen))
    DO i = 1, nch
      buf(i) = tmp(i:i)
    END DO
    IF (nch < buflen) buf(nch+1) = C_NULL_CHAR
  END SUBROUTINE fvsGetVersion

  !------------------------------------------------------------------------
  ! FVSGETBUILDINFO
  !
  !   buf     (OUT) Caller-allocated buffer
  !   buflen  (IN)  Size of buf in bytes; allocate FVS_BUILDINFO_BUFLEN
  !                 to guarantee no truncation
  !   nch     (OUT) Actual length written, not including null terminator.
  !                 If nch == buflen the string was truncated.
  !
  ! Fortran callers:
  !
  !   CHARACTER(LEN=FVS_BUILDINFO_BUFLEN) :: buf
  !   INTEGER :: nch
  !   CALL fvsGetBuildInfo(buf, FVS_BUILDINFO_BUFLEN, nch)
  !   WRITE(*,'(A)') buf(1:nch)
  !------------------------------------------------------------------------
  SUBROUTINE fvsGetBuildInfo(buf, buflen, nch)
    CHARACTER(KIND=C_CHAR), INTENT(OUT) :: buf(*)
    INTEGER(KIND=C_INT),    INTENT(IN)  :: buflen
    INTEGER(KIND=C_INT),    INTENT(OUT) :: nch

    CHARACTER(LEN=FVS_BUILDINFO_BUFLEN) :: tmp
    INTEGER :: i

    tmp = 'Version: ' // FVSVER_VERSION // CHAR(10) // &
          'Org:     ' // FVSVER_ORG     // CHAR(10) // &
          'Remote:  ' // FVSVER_REMOTE  // CHAR(10) // &
          'Branch:  ' // FVSVER_BRANCH  // CHAR(10) // &
          'Commit:  ' // FVSVER_HASH    // CHAR(10) // &
          'Date:    ' // FVSVER_DATE
    nch = MIN(LEN_TRIM(tmp), INT(buflen))
    DO i = 1, nch
      buf(i) = tmp(i:i)
    END DO
    IF (nch < buflen) buf(nch+1) = C_NULL_CHAR
  END SUBROUTINE fvsGetBuildInfo

END MODULE FVSVERSION
