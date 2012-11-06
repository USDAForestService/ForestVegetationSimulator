      SUBROUTINE DBSDRIV(JOSTND)
      IMPLICIT NONE
C
C  $Id$
C
C     PURPOSE: TO LIST THE ODBC DRIVERS ON THE MACHINE
C     INPUT: JOSTND  - OUTPUT FILE REFERENCE NUMBER
C
      INCLUDE 'DBSCOM.F77'
      
#ifdef _WINDLL
      !DEC$ ATTRIBUTES DLLIMPORT :: _FVSSQLSETENVATTR
      !DEC$ ATTRIBUTES DLLIMPORT :: _FVSSQLALLOCHANDLE
      !DEC$ ATTRIBUTES DLLIMPORT :: _FVSSQLFREEHANDLE      
#endif
      !DEC$ ATTRIBUTES ALIAS:'_FVSSQLSETENVATTR' :: FVSSQLSETENVATTR
      !DEC$ ATTRIBUTES ALIAS:'_FVSSQLALLOCHANDLE' :: FVSSQLALLOCHANDLE
      !DEC$ ATTRIBUTES ALIAS:'_FVSSQLFREEHANDLE' :: FVSSQLFREEHANDLE
C
      INTEGER(SQLHENV_KIND):: EnvHndl
      INTEGER(SQLSMALLINT_KIND)::DriverComplete
      INTEGER(SQLUSMALLINT_KIND) :: direction
      INTEGER(SQLSMALLINT_KIND) :: driverStrLen,attrStrLen
      INTEGER JOSTND,driverCount,i,i1
      CHARACTER(LEN=256) driver,attr

      driverCount = 0
C
C     ALLOCATE AND ENVIRONMENT HANDLE
C
      iRet = fvsSQLAllocHandle(SQL_HANDLE_ENV, SQL_NULL_HANDLE, EnvHndl)
      IF (iRet.ne.0) GOTO 100
C
C     SET ODBC VERSION TO USE 3.X
C
      iRet = fvsSQLSetEnvAttr(EnvHndl, SQL_ATTR_ODBC_VERSION,
     -  SQL_OV_ODBC3)
      IF (iRet.ne.0) GOTO 100

      direction = SQL_FETCH_FIRST     
      do
       iRet = fvsSQLDrivers(EnvHndl, direction,
     >          driver, len(driver), driverStrLen,
     >          attr, len(attr), attrStrLen)
       if (iRet.ne.0) exit
       driverCount = driverCount+1
       print *
       print *, "name:",driver(:driverStrLen)
       WRITE (JOSTND,20) driverCount,driver(:driverStrLen)
   20  FORMAT(/T13,'DRIVER ',I2,'; NAME: ',A,'; ATTRIBUTES:')
       IF (attrStrLen.GT.0) THEN
         i1=1
         do i=1,attrStrLen
           IF (iachar(attr(i:i)).EQ.0) then
             print *, "attr:",attr(i1:(i-1))
             WRITE (JOSTND,30) attr(i1:(i-1))
   30        FORMAT (T13,A)
             i1=i+1
           endif
         enddo
         if (i1.eq.1) then ! maybe there are no null terminators
           print *, "attr:", attr(:attrStrLen)
           WRITE (JOSTND,30) attr(:attrStrLen)
         endif  
       ENDIF
       direction = SQL_FETCH_NEXT
      enddo
      iRet = fvsSQLFreeHandle(SQL_HANDLE_ENV, EnvHndl)
  100 CONTINUE
      IF (driverCount.EQ.0) WRITE (JOSTND,110) 
  110 FORMAT (/T13,I3,'NO DRIVERS FOUND')
      RETURN
      END
     
 
