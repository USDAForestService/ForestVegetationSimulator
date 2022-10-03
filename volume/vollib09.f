      SUBROUTINE VOLLIB09(REGN, FORSTC, VOLEQC, MTOPP, MTOPS, STUMP
     +,   DBHOB,
     &    DRCOB, HTTYPEC, HTTOT, HTLOG, HT1PRD, HT2PRD, UPSHT1, 
     +    UPSHT2, UPSD1,
     &    UPSD2, HTREF, AVGZ1, AVGZ2, FCLASS, DBTBH, BTR, I3, I7, I15,
     +    I20, I21,
     &    VOL,LOGVOLC,LOGDIAC,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     &    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPECC,PRODC,HTTFLL,LIVEC,
     &    BA,SI,CTYPEC,ERRFLAG, INDEB)

!... Main entry point into dll
    
      USE CHARMOD
      USE DEBUG_MOD
     
      IMPLICIT NONE

!... Expose subroutine VOLLIB09 to users of this DLL
      !DEC$ ATTRIBUTES DLLEXPORT::VOLLIB09
!      !DEC$ ATTRIBUTES NOMIXED_STR_LEN_ARG :: VOLLIB09 

!... Compiler adds and underscore to the beginning and @#bytes
!... to each external call...must create alias to "see" it
      !DEC$ ATTRIBUTES ALIAS:'_VOLLIB09@196' :: VOLLIB09
!
!   THERE ARE CALLS TO THE FOLLOWING SUBROUTINES
!     PROFILE - EXTERNAL 
!     DVE     - EXTERNAL
!     R4VOL  - EXTERNAL 
!     R6VOL  - EXTERNAL 
!     BLMVOL - EXTERNAL
!     R8VOL  - EXTERNAL
!     R10VOL - EXTERNAL
!     R12VOL - EXTERNAL 

!      !DEC$ ATTRIBUTES DLLIMPORT::VOLCAL

!     Parameters
      INTEGER         REGN
      TYPE(CHAR256):: FORSTC, VOLEQC
      REAL            MTOPP, MTOPS, STUMP, DBHOB, DRCOB
      TYPE(CHAR256):: HTTYPEC
      REAL            HTTOT
      INTEGER         HTLOG
      REAL            HT1PRD, HT2PRD, UPSHT1, UPSHT2, UPSD1, UPSD2
      INTEGER         HTREF
      REAL            AVGZ1, AVGZ2
      INTEGER         FCLASS
      REAL            DBTBH, BTR
      INTEGER         I3, I7, I15, I20, I21
      REAL            VOL(I15), LOGVOLC(I20, I7)
      REAL            LOGDIAC(I3,I21), LOGLEN(I20), BOLHT(I21)
      INTEGER         TLOGS
      REAL            NOLOGP,NOLOGS
      INTEGER         CUTFLG, BFPFLG, CUPFLG, CDPFLG, SPFLG
      TYPE(CHAR256):: CONSPECC, PRODC
      INTEGER         HTTFLL
      TYPE(CHAR256):: LIVEC
      INTEGER         BA, SI
      TYPE(CHAR256):: CTYPEC
      INTEGER         ERRFLAG
      INTEGER					INDEB
      INTEGER         IDIST
!     Local variables
      CHARACTER(FORSTC%LENGTH)  FORST
      CHARACTER(VOLEQC%LENGTH)  VOLEQ
      CHARACTER(HTTYPEC%LENGTH) HTTYPE
      CHARACTER(CONSPECC%LENGTH)CONSPEC
      CHARACTER(PRODC%LENGTH)   PROD
      CHARACTER(LIVEC%LENGTH)   LIVE
      CHARACTER(CTYPEC%LENGTH)  CTYPE
      CHARACTER*3     MDL,SPECIES
      CHARACTER*2     DIST,VAR   
      CHARACTER*10    EQNUM
      INTEGER         SPEC
      REAL            LOGVOL(I7,I20),LOGDIA(I21,I3) 

!     MERCH VARIABLES 
      REAL            THT1,   MAXLEN
!      INTEGER         ISFIRSTIME
      
!     TREE VARIABLES
      REAL            CR, TRIM
      INTEGER         SPCODE
  
!     OUTPUTS
      INTEGER         IFORST
    
!     ARRAYS
      INTEGER         I, J

!---------------------------------------------------------------------
      IF (INDEB .EQ. 1) THEN
      	 ANY_DEBUG = .TRUE.
      	 DEBUG%VOLEQ = .TRUE.
      	 DEBUG%MODEL = .TRUE.
      ELSE
      	 ANY_DEBUG = .FALSE.
      	 DEBUG%VOLEQ = .FALSE.
      	 DEBUG%MODEL = .FALSE.
      ENDIF
      
      IF (ANY_DEBUG) THEN
	       OPEN (UNIT=LUDBG, FILE='Debug.txt', STATUS='UNKNOWN')
	       WRITE (LUDBG,5)'Debugging NVEL'
   5     FORMAT(A)
      END IF
      
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 10) ' -->Enter VOLLIB09'
   10    FORMAT (A)   
   		END IF
      
      !Convert the CHAR256 types to Char(*) so I don't have to rename 
      !all references below
      !CHAR256 is a user defined type used for passing C strings back/
      !forth to Fortran
      !CHAR256 is equivalent to a C struct with an integer LENGTH,and
      !char[] STR
!--------------------------------------------------------------------
      FORST   = FORSTC%STR(1:FORSTC%LENGTH)
      VOLEQ   = VOLEQC%STR(1:VOLEQC%LENGTH)
      HTTYPE  = HTTYPEC%STR(1:HTTYPEC%LENGTH)
      CONSPEC = CONSPECC%STR(1:CONSPECC%LENGTH)
      PROD    = PRODC%STR(1:PRODC%LENGTH)
      LIVE    = LIVEC%STR(1:LIVEC%LENGTH)
      CTYPE   = CTYPEC%STR(1:CTYPEC%LENGTH)
!---------------------------------------------------
!Use array converter to reshape c arrays to fortran notation
      LOGVOL = RESHAPE(LOGVOLC, SHAPE(LOGVOL))
      LOGDIA = RESHAPE(LOGDIAC, SHAPE(LOGDIA))
      ERRFLAG = 0
      
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 15) REGN, VOLEQ, PROD, DBHOB, MTOPP,HTTOT
   15    FORMAT (2X, I2, 1X, A, 1X, A, F5.1, F5.1, F5.1)
      END IF
      
      CALL VOLINIT(REGN,FORST,VOLEQ,MTOPP,MTOPS,STUMP,DBHOB,
     +    DRCOB,HTTYPE,HTTOT,HTLOG,HT1PRD,HT2PRD,UPSHT1,UPSHT2,UPSD1,
     +    UPSD2,HTREF,AVGZ1,AVGZ2,FCLASS,DBTBH,BTR,I3,I7,I15,I20,I21,
     +    VOL,LOGVOL,LOGDIA,LOGLEN,BOLHT,TLOGS,NOLOGP,NOLOGS,CUTFLG,
     +    BFPFLG,CUPFLG,CDPFLG,SPFLG,CONSPEC,PROD,HTTFLL,LIVE,
     +    BA,SI,CTYPE,ERRFLAG,IDIST)
     
      !conVert Char(*) back to CHAR256 for return to C++ wrapper including
      !a null terminator required by C strings.  Tack on the null terminator
      CALL CREATE_C_STRING(FORSTC, FORST)
      CALL CREATE_C_STRING(VOLEQC, VOLEQ)
      CALL CREATE_C_STRING(HTTYPEC, HTTYPE)
      CALL CREATE_C_STRING(CONSPECC, CONSPEC)
      CALL CREATE_C_STRING(PRODC, PROD)
      CALL CREATE_C_STRING(LIVEC, LIVE)
      CALL CREATE_C_STRING(CTYPEC, CTYPE)
           
      !copy the logvol and logdia data back into the subroutine paramater for
      !return to c++
      LOGVOLC = RESHAPE(LOGVOL, SHAPE(LOGVOL))
      LOGDIAC = RESHAPE(LOGDIA, SHAPE(LOGDIA))
      
      IF (DEBUG%MODEL) THEN
         !WRITE  (LUDBG,*)'   BDFT    CUFT    TPWDCU  TPWDBF'
         WRITE  (LUDBG, 25)'ERRFLAG = ', ERRFLAG
   25    FORMAT (2X, A, 1X, I2)
      END IF
      
      
      IF (DEBUG%MODEL) THEN
         WRITE  (LUDBG, 30) ' <--Exit VOLLIB09'
   30    FORMAT (A)   
   		END IF
      
      IF (ANY_DEBUG) THEN
        CLOSE(LUDBG)
      ENDIF
      
      RETURN
      END SUBROUTINE VOLLIB09
      
!********************************************************************
      SUBROUTINE CREATE_C_STRING(C256STR, CHARSTR)

!...  covnert Char(*) back to CHAR256 for return to C++ wrapper 
!...  including a null terminator required by C strings.  Tack on the 
!...  null terminator
      
      USE CHARMOD
      
      IMPLICIT NONE 

!...  Created TDH 03/02/09
!...  Revised iii

          
!...  Parameters
      TYPE(CHAR256)::           C256STR
      CHARACTER(C256STR%LENGTH) CHARSTR
     
!--------------------------------------------------------------------
      C256STR%STR(1:(C256STR%LENGTH+1)) = CHARSTR//CHAR(0)
     
      RETURN
      END SUBROUTINE CREATE_C_STRING