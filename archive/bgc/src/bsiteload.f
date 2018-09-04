      SUBROUTINE BSITELOAD(FVS_SLOPE,FVS_ASPECT,FVS_ELEV,FVS_TLAT,
     >                    FVS_KODTYP,FVS_KODFOR)
C----------
C  **SITELOAD  BGC--DATE OF LAST REVISION: 10/16/99
C----------
C
C     LOADS FVS SITE ATTRIBUTES INTO BGC MEMORY
C
C     CALLED FROM: BGCGROW
C
COMMONS
      INCLUDE 'ENTITY.F77'
      INCLUDE 'SITE.F77'
COMMONS
      INTEGER FVS_KODTYP, FVS_KODFOR 
C
C      print *,'in siteload'
C------------------------------------------------------------------------
C GET FVS SITE VARIABLES
C------------------------------------------------------------------------
C  FVS_SLOPE =   slope in % scaled to 0-1
C  FVS_ASPEC =   aspect in degrees
C  FVS_ELEV   =  elevation in 100's feet
C  FVS_TLAT =    latitude
C  FVS_KODTYP =  habitat type code
C  FVS_KODFOR =  Nat. Forest code
C------------------------------------------------------------------------
      SLOPE=FVS_SLOPE
      ASPECT=FVS_ASPECT*(360./6.28)     !convert radians to degrees
      LAT=FVS_TLAT
      HTYPE=FVS_KODTYP
      NFLOC=FVS_KODFOR
      ELEV=INT((FVS_ELEV*100.)/3.28)  ! Converts to meters
C
C
        WRITE(*,*) 
        WRITE(*,*) 'HTYPE= ',HTYPE
        WRITE(*,*) 'SLOPE= ',SLOPE
        WRITE(*,*) 'ASPECT= ',ASPECT
        WRITE(*,*) 'ELEV= ',ELEV
        WRITE(*,*) 'NFLOC= ',NFLOC
        WRITE(*,*) 'FVS_TLAT= ',FVS_TLAT
        WRITE(*,*) 'LAT= ',LAT
      RETURN
      END

