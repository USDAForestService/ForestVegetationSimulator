      FUNCTION BRATIO(IS,D,H)
      IMPLICIT NONE
C----------
C WC $Id: bratio.f 0000 2018-02-14 00:00:00Z gedixon $
C----------
C
C  FUNCTION TO COMPUTE BARK RATIOS.  THIS ROUTINE IS VARIANT SPECIFIC
C  AND EACH VARIANT USES ONE OR MORE OF THE ARGUMENTS PASSED TO IT.
C
C  SPECIES LIST FOR WEST CASCADES VARIANT
C  1=SF,2=WF,3=GF,4=AF,5=RF,6=---,7=NF,8=YC,9=IC,10=ES,11=LP,12=JP,
C  13=SP,14=WP,15=PP,16=DF,17=RW,18=RC,19=WH,20=MH,21=BM,22=RA,23=WA,
C  24=PB,25=GC,26=AS,27=CW,28=WO,29=J,30=LL,31=WB,32=KP,33=PY,34=DG
C  35=HT,36=CH,37=WI,38=---,39=OT
C----------
      REAL BARKB(4,13),H,D,BRATIO,DIB
      INTEGER JBARK(39),IS
      REAL DANUW
C
      DATA JBARK/
     &  2,  2,  2,  2,  2,  2,  2,  5,  5,  10,
     & 10,  4,  4,  4,  3,  1,  5, 11, 12,  11,
     &  6,  9,  9,  6,  7,  9,  9,  8, 11,  10,
     & 13, 13, 13,  9,  9,  9,  9,  1, 10/
C----------
C  NOTE: COEFFICIENTS FOR SPECIES 312 & 431 IN PILLSBURY &
C  KIRKLEY ARE METRIC. INTERCEPT IS DIVIDED BY 2.54 TO CONVERT THESE
C  EQUATIONS TO ENGLISH.
C----------
      DATA BARKB/
     & 202.,  0.903563,  0.989388, 1.,
     &  15.,  0.904973,  1.0     , 1.,
     & 122.,  0.809427,  1.016866, 1.,
     & 116.,  0.859045,  1.0     , 1.,
     &  81.,  0.837291,  1.0     , 1.,
     & 312.,  0.08360 ,  0.94782 , 2.,
     & 431.,  0.15565 ,  0.90182 , 2.,
     & 815.,  0.8558  ,  1.0213 ,  1.,
     & 351.,  0.075256,  0.949670, 2.,
     & 108.,  0.9     ,  1.0     , 1.,
     & 242.,  0.949670,  1.0     , 1.,
     & 263.,  0.933710,  1.0     , 1.,
     & 101.,  0.933290,  1.0     , 1./
C----------
C  BARK COEFS
C  202,15,122,116,81 FROM WALTERS ET.AL. RES BULL 50
C  312,431      FROM PILLSBURY AND KIRKLEY RES NOTE PNW 414
C  242,93,108   FROM WYKOFF ET.AL. RES PAPER INT 133
C  815          FROM GOULD AND HARRINGTON
C
C  EQUATION TYPES
C  1  DIB = a * DOB ** b
C  2  DIB = a + bDOB
C  3  DIB = a*DOB = a * DOB ** b, (eQ.1), WITH b= 1.0
C  MODEL TYPE 1
C----------
C  DUMMY ARGUMENT NOT USED WARNING SUPPRESSION SECTION
C----------
      DANUW = H
C
C----------
      IF (D .GT. 0) THEN 
        IF(BARKB(4,JBARK(IS)) .EQ. 1.)THEN
          DIB=BARKB(2,JBARK(IS))*D**BARKB(3,JBARK(IS))
          BRATIO=DIB/D
        ELSEIF (BARKB(4,JBARK(IS)) .EQ. 2.)THEN
          DIB=BARKB(2,JBARK(IS)) + BARKB(3,JBARK(IS))*D
          BRATIO=DIB/D
        ELSE
          BRATIO= 0.9
        ENDIF
      ELSE
        BRATIO = 0.99
      ENDIF
C
      IF(BRATIO .GT. 0.99) BRATIO= 0.99
      IF(BRATIO .LT. 0.80) BRATIO= 0.80
C
      RETURN
      END
