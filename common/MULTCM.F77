CODE SEGMENT MULTCM
C----------
C COMMON $Id: MULTCM.F77 3950 2022-04-01 11:51:52Z nickcrookston $
C----------
C
      REAL   XDMULT(MAXSP),XHMULT(MAXSP),XMDIA1(MAXSP),XMDIA2(MAXSP),
     &       XMMULT(MAXSP),XRDMLT(MAXSP),XRHMLT(MAXSP)
C
      COMMON /MULTCM/ XDMULT,XHMULT,XMDIA1,XMDIA2,XMMULT,XRDMLT,XRHMLT
C
C----------
C  VARIABLE DEFINITIONS:
C----------
C  Species specifice multipers:
C  XDMULT -- diameter growth 
C  XHMULT -- height growth
C  XMDIA1 -- lower diameter limit for mortality multipier
C  XMDIA2 -- upper diameter limit for mortality multipier
C  XMMULT -- mortality 
C  XRDMLT -- small tree diameter growth
C  XRHMLT -- small tree height growth
C
C-----END SEGMENT
