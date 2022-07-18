      SUBROUTINE BIOPEN
C      AMENDED 11/02. AJM. COMMENT OUT SOME OUTPUT "OPENS" FOR RELEASE VERSION
C
C
C      OPEN(1,FILE='INPUT.DAT',STATUS='OLD')   ! eliminated 
C      OPEN(60,FILE='CLIMATE.CLM',STATUS='OLD') ! in bgcinit.f
C      OPEN(61,FILE='BETA.DAT',STATUS='OLD')    ! in bgcinit.f
C      OPEN(62,FILE='SITE.DAT',STATUS='OLD')   ! in bgcinit.f
C      OPEN(63,FILE='STRUCT.OUT',STATUS='UNKNOWN')   ! in trnover.for
      OPEN(64,FILE='DAYSTAND.OUT',STATUS='UNKNOWN')   ! in gsv.for
C      OPEN(65,FILE='DAYENTY.OUT',STATUS='UNKNOWN')   ! in gsv.for
      OPEN(66,FILE='YRENTY.OUT',STATUS='UNKNOWN')   ! in gsv.for
C      OPEN(67,FILE='LAYER.OUT',STATUS='UNKNOWN')   ! in grow.for
      OPEN(68,FILE='DEAD.OUT',STATUS='UNKNOWN') !dead tree file    in kill.for
      OPEN(69,STATUS='SCRATCH')       !live tree scratch file
      OPEN(70,FILE='YRSTAND.OUT',STATUS='UNKNOWN')   ! in gsv.for
      OPEN(71,FILE='DAYWATR.OUT',STATUS='UNKNOWN')   ! in water.for
C      OPEN(72,FILE='TEST.OUT',STATUS='UNKNOWN')
C      OPEN(73,FILE='INCR.OUT',STATUS='UNKNOWN')    ! In update.for 
      OPEN(74,FILE='ENTYLIST.OUT',STATUS='UNKNOWN')   ! In gsv.for
      OPEN(75,FILE='FRACTIONS.OUT',STATUS='UNKNOWN') ! In BAlloca.f
C  Adding 2/01 ajm
      OPEN(76,FILE='VIGOR.OUT',STATUS='UNKNOWN')  !In BUpdate.f
C NEW OUTPUT FILE 6/19/03 FOR ANALYZING CROWN GEOMETRIES.
      OPEN(77,FILE='CROWNS.OUT',STATUS='UNKNOWN') !IN BStructN.f
c Anoteher new output file 4/04 AJM.  NPP, GPP, Gr and Maint resp, all by tissue pool by enty
      OPEN(78,FILE='NPP_GPP.OUT',STATUS='UNKNOWN') !IN BUpdate.f where vigor.out writen
      RETURN
      END

