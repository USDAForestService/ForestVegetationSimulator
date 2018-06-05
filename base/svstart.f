      SUBROUTINE SVSTART
      IMPLICIT NONE
C----------
C  $Id$
C----------
C
C     STAND VISUALIZATION GENERATION
C     N.L.CROOKSTON -- RMRS MOSCOW -- NOVEMBER 1998
C
C     BUILD THE INITIAL DISPLAY OF THE INITIAL TREES.
C
COMMONS
C
C
      INCLUDE 'PRGPRM.F77'
C
C
      INCLUDE 'FMPARM.F77'
C
C
      INCLUDE 'CONTRL.F77'
C
C
      INCLUDE 'ARRAYS.F77'
C
C
      INCLUDE 'FMCOM.F77'
C
C
      INCLUDE 'SVDATA.F77'
C
C
      INCLUDE 'SVRCOM.F77'
C
C
COMMONS
C

      LOGICAL DEBUG
      INTEGER I, ID, IFC, IH, ISVOBJ, J, K, L
C
      CALL DBCHK (DEBUG,'SVSTART',7,ICYC)
C
      IF (JSVOUT.EQ.0) RETURN
C
C     START THE SVRANN GENERATOR AT THE SAME PLACE AS THE
C     BASE MODEL GENERATOR.  THIS MAKES THE SVRANN GENERATOR
C     RESPOND TO THE RANNSEED KEYWORD.
C
      CALL RANNGET(SVS0)
C
C     SET UP THE PLOT GEOMETRY
C
      CALL SVGTPL
C
      NSVOBJ = 0
      DO ISVOBJ=1,MXSVOB
        IOBJTP(ISVOBJ) = 0
      ENDDO

C
C     PLACE THE INITIAL TREES.  NOTE THAT THIS CODE ASSUMES
C     THAT THERE ARE NO OBJECTS IN THE OBJECT LIST.
C
      CALL SVESTB(0)
C
C     If the FFE is active:
C     The FFE CWD pools have not been loaded yet (FMMAIN & FMCBA have
C     yet to be called); therefore, temporarily load some fire model
C     tree attributes, then call FMCBA to load the initial CWD.
C     This will enable CWD objects to be displayed in the SVS
C     initial inventory display.
C
      IF ( LFMON ) THEN
        DO I=1,ITRN
          FMPROB(I) = PROB(I)
          FMICR(I)  = ICR(I)
          FIRKIL(I) = 0.0
        ENDDO
        CALL FMCBA (IY(1),1)
      ENDIF
      IF ( DEBUG ) THEN
        WRITE(JOSTND,1020) ICYC
 1020   FORMAT (' ','IN SVSTART, ICYC=',I2,':', / ,
     &          ' ',T5,'CWD ARRAY (UNPILED ONLY):')
        DO IH=1,2
          DO ID=1,4
            WRITE(JOSTND,1040) (CWD(1,IFC,IH,ID),IFC=1,11), IH, ID
 1040       FORMAT(T5,11(F7.3,1X),'IH=',I1,', ID=',I1)
          ENDDO
        ENDDO
      ENDIF
C
C     OUTPUT THE INITIAL PICTURE
C
      CALL SVOUT(IY(1),0,'Inventory conditions')
C
C     If FFE variables were temporarily loaded for the initial SVS
C     display, reverse the process now, to avoid double-counting of
C     initial CWD loadings.
C
      IF ( LFMON ) THEN
        DO I=1,ITRN
          FMPROB(I) = 0.0
          FMICR(I)  = 0
        ENDDO
        DO I = 1,3
          DO J = 1,MXFLCL
            DO K = 1,2
              DO L = 1,5
                CWD(I,J,K,L) = 0.0
              ENDDO
            ENDDO
          ENDDO
        ENDDO
      ENDIF

C
C  Special processing for SO variant, when FFE is not active:
C
C  The SO variant is atypical in how the snagfall/decay coefficients
C  are loaded. For the SO variants, the snagfall/decay coefficients
C  are not loaded in FMVINIT. Their initialization is delayed until
C  FMCBA, because certain parameters are dependent on the forest
C  location code (whether you're in region 5 or 6), and the location
C  code either isn't set yet in FMVINIT, or, could change.
C  The following logic calls SOSNAG (an entry point in the SO/
C  FMCBA routine), to assign the missing snagfall/decay coefficients
C  when FFE is not active

      IF ( .NOT. LFMON ) THEN
        IF (VARIANT .EQ. 'SO' ) THEN
          CALL SNGCOE
        ENDIF
      ENDIF

C
C     QUICK CHECKS/DEBUG
C
      IF (DEBUG) CALL SVCDBH(WK3,0)
C
C
      RETURN
      END
