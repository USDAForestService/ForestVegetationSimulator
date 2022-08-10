! last modified on 01-18-2013
C 01/18/2013 added GETFIAVOLEQ
      SUBROUTINE GETVOLEQ2(REGN,FORSTC,DISTC,SPEC,PRODC,VOLEQC,ERRFLAG)
      !DEC$ ATTRIBUTES DLLEXPORT::GETVOLEQ2
!      !DEC$ ATTRIBUTES ALIAS:'_GETVOLEQ2@28' :: GETVOLEQ2

!REV  Created TDH 04/01/09 
!REV  Revised iii ../../..
           
      USE CHARMOD 
      USE DEBUG_MOD
      
      IMPLICIT NONE
      
      INTEGER         REGN   
      TYPE(CHAR256):: FORSTC
      TYPE(CHAR256):: DISTC
      TYPE(CHAR256):: PRODC
      INTEGER         SPEC
      TYPE(CHAR256):: VOLEQC
      INTEGER         ERRFLAG
      
!     Local Variables
      CHARACTER*2 VAR
      CHARACTER(FORSTC%LENGTH)  FORST
      CHARACTER(DISTC%LENGTH)   DIST
      CHARACTER(VOLEQC%LENGTH)  VOLEQ
      CHARACTER(PRODC%LENGTH)   PROD
      
!---------------------------------------------------------------------

!      IF (DEBUG%VOLEQ) THEN
!         OPEN (UNIT=VEDBG, FILE='VEDebug.txt', STATUS='UNKNOWN')
!	       WRITE (VEDBG,1)'Debugging NVEL'
!   1     FORMAT(A)
!         WRITE  (VEDBG, 15) ' -->Enter GETVOLEQ2'
!   15    FORMAT (A)   
!   		END IF
   		
      FORST   = FORSTC%STR(1:FORSTC%LENGTH)
      DIST    = DISTC%STR(1:DISTC%LENGTH)
      PROD    = PRODC%STR(1:PRODC%LENGTH)
      VOLEQ   = VOLEQC%STR(1:VOLEQC%LENGTH)
   		 
   		
   		 
      VAR = '  '
      CALL VOLEQDEF(VAR,REGN,FORST,
     +			DIST,SPEC,PROD,VOLEQ,ERRFLAG)
      
!      WRITE  (VEDBG, 16)var, forst, dist, prod, spec, voleq
!   16 FORMAT (4A, 2X, I4, A)    
      
      
      
      !conVert Char(*) back to CHAR256 for return to C++ wrapper including
      !a null terminator required by C strings.  Tack on the null terminator
      CALL CREATE_C_STRING(FORSTC, FORST)
      CALL CREATE_C_STRING(DISTC, DIST)
      CALL CREATE_C_STRING(VOLEQC, VOLEQ)
      CALL CREATE_C_STRING(PRODC, PROD)
      
      IF (LEN_TRIM(VOLEQC%STR) .LT. 10) THEN
         ERRFLAG = 1
         RETURN
      ENDIF
      
!      IF (DEBUG%VOLEQ) THEN
!         WRITE  (VEDBG, 25) ' <--Exit GETVOLEQ2'
!   25    FORMAT (A)   
   
!         CLOSE(VEDBG)
!   		END IF
   		
      RETURN
      END SUBROUTINE GETVOLEQ2

!*************************************************************************
!	GETVOLEQ     - subroutine 
!
      subroutine GETVOLEQ(REGN,FORST,DIST,SPEC,PROD,VOLEQ,ERRFLAG)
      ! Expose subroutine VERNUM to users of this DLL
      !
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::GETVOLEQ
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG ::GETVOLEQ
!      !DEC$ ATTRIBUTES ALIAS:'_GETVOLEQ@44' :: GETVOLEQ
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'GETVOLEQ'::GETVOLEQ
      
     
      
      
 !     CHARACTER*(1) FORST,PROD,DIST
      CHARACTER*(*) :: FORST,PROD,DIST

      CHARACTER*(*):: VOLEQ
      CHARACTER*2 VAR
      CHARACTER*10 nvoleq
	    INTEGER SPEC,ERRFLAG,REGN
	    
!	    print *, '--> enter getvoleq'
!	    print *, '    regn = ',regn, 'forst = ', forst
!	    print *, '    dist = ', dist
!	    print *, '*****************************'
!	    print *, '    spec = ', spec,'prod = ', prod, 'voleq = ', voleq 
         VAR = '  '
         CALL VOLEQDEF(VAR,REGN,FORST,DIST,SPEC,PROD,VOLEQ,ERRFLAG)

      RETURN
      end subroutine GETVOLEQ


!**************************************************************************

!	GETVOLEQ     - subroutine 
!
      SUBROUTINE GETVOLEQ3(REGN,FORSTI,DISTI,SPEC,PRODI,VOLEQI,ERRFLAG)
      ! Expose subroutine VERNUM to users of this DLL
      !
      !DEC$ ATTRIBUTES DLLEXPORT::GETVOLEQ3
!      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG ::GETVOLEQ
!      !DEC$ ATTRIBUTES ALIAS:'_GETVOLEQ@44' :: GETVOLEQ
      
       IMPLICIT NONE
      
      CHARACTER*256,INTENT(INOUT):: FORSTI,PRODI,DISTI, VOLEQI
      CHARACTER*(2) :: FORST,PROD,DIST

      CHARACTER*(10):: VOLEQ
      CHARACTER*2 VAR
	    INTEGER SPEC,ERRFLAG,REGN
	   
	    FORST   = FORSTI(1:3)
	    DIST    = DISTI(1:3)
	    PROD    = PRODI(1:3)
      VOLEQ   = VOLEQI(1:11)
	   
         VAR = '  '
         CALL VOLEQDEF(VAR,REGN,FORST,DIST,SPEC,PROD,VOLEQ,ERRFLAG)
         
         FORSTI = FORST // char(0)
         DISTI = DIST // char(0)
         PRODI = PROD // char(0)
         VOLEQI = VOLEQ // char(0)

      RETURN
      end subroutine GETVOLEQ3


!**************************************************************************
c Added this routine to be used by biomass library c#
      SUBROUTINE GETVARIANTCS(REGN,FORSTI,DISTI,VARI)
      !DEC$ ATTRIBUTES DLLEXPORT::GETVARIANTCS
       IMPLICIT NONE
      
      CHARACTER*256,INTENT(INOUT):: FORSTI,DISTI,VARI
      CHARACTER*(2) :: FORST,DIST

      CHARACTER*2 VAR
	INTEGER REGN
	   
	   FORST   = FORSTI(1:3)
	   DIST    = DISTI(1:3)
         CALL GETVARIANT(REGN,FORST,DIST,VAR)
         FORSTI = FORST // char(0)
         DISTI = DIST // char(0)
         VARI = VAR // char(0)
      RETURN
      END SUBROUTINE GETVARIANTCS
      
!*************************************************************************
      subroutine GETFIAVOLEQ(REGN,FORST,DIST,SPEC,VOLEQ,ERRFLAG)
c The routine used by Excel function to get FIA volume equation for PNW
      !DEC$ ATTRIBUTES STDCALL,REFERENCE, DLLEXPORT::GETFIAVOLEQ
      !DEC$ ATTRIBUTES MIXED_STR_LEN_ARG ::GETFIAVOLEQ
!      !DEC$ ATTRIBUTES ALIAS:'_GETVOLEQ@44' :: GETVOLEQ
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'GETFIAVOLEQ'::GETFIAVOLEQ
      
      CHARACTER*(*) :: FORST,DIST

      CHARACTER*(*):: VOLEQ
      CHARACTER*2 VAR
c      CHARACTER*10 nvoleq
	INTEGER SPEC,ERRFLAG,REGN
	    
!	    print *, '--> enter getFIAvoleq'
!	    print *, '    regn = ',regn, 'forst = ', forst
!	    print *, '    dist = ', dist
!	    print *, '*****************************'
!	    print *, '    spec = ', spec,'prod = ', prod, 'voleq = ', voleq 
         VAR = '  '
         CALL FIAVOLEQDEF(VAR,REGN,FORST,DIST,SPEC,VOLEQ,ERRFLAG)

      RETURN
      end subroutine GETFIAVOLEQ
C*********************************************************************************
      subroutine getvoleq_r(REGN,FORST,DIST,SPEC,VOLEQ,ERRFLAG)
C This subroutine is for R user to get default equation from vollib      !
C YW 02/10/2017
      !DEC$ ATTRIBUTES C,REFERENCE, DLLEXPORT::getvoleq_r
      !DEC$ ATTRIBUTES DECORATE, ALIAS:'getvoleq_r_'::getvoleq_r
      
      CHARACTER*(*) :: FORST,DIST

      CHARACTER*(*):: VOLEQ
      CHARACTER*2 VAR,PROD
	INTEGER SPEC,ERRFLAG,REGN
	    
         VAR = '  '
         CALL VOLEQDEF(VAR,REGN,FORST,DIST,SPEC,PROD,VOLEQ,ERRFLAG)

      RETURN
      end subroutine getvoleq_r
