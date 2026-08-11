PROGRAM MAIN
  USE FVSVERSION
  IMPLICIT NONE

  INTEGER rtnCode, lenCL, i, arglen, argstat
  CHARACTER(LEN=32)                   :: arg
  CHARACTER(LEN=FVS_VERSION_BUFLEN)   :: vbuf
  CHARACTER(LEN=FVS_BUILDINFO_BUFLEN) :: ibuf
  INTEGER(KIND=C_INT)                 :: nch

  ! Check for version flags before anything else.
  ! These exit immediately without initializing the simulation - no 
  ! keyfile or tree data required.
  IF (COMMAND_ARGUMENT_COUNT() .GT. 0) THEN
    CALL GET_COMMAND_ARGUMENT(1, arg, arglen, argstat)

    IF (TRIM(arg) .EQ. '--version') THEN
      CALL fvsGetVersion(vbuf, FVS_VERSION_BUFLEN, nch)
      WRITE(*,'(A)') vbuf(1:nch)
      STOP
    ELSE IF (TRIM(arg) .EQ. '--build-info') THEN
      CALL fvsGetBuildInfo(ibuf, FVS_BUILDINFO_BUFLEN, nch)
      WRITE(*,'(A)') ibuf(1:nch)
      STOP
    END IF
  END IF

  lenCL = 0
  CALL fvsSetCmdLine(' ', lenCL, rtnCode)
  IF (rtnCode .NE. 0) GOTO 10

  DO
    CALL FVS(rtnCode)
    IF (rtnCode .NE. 0) EXIT
  ENDDO

10 CONTINUE

  CALL fvsGetICCode(i)

  IF (i .EQ. 0) STOP

  GO TO (11,12,13,14,15), i
11 CONTINUE
  STOP 10
12 CONTINUE
  STOP 20
13 CONTINUE
  STOP 30
14 CONTINUE
  STOP 40
15 CONTINUE
  STOP 50
END
