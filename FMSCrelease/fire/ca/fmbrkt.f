      FUNCTION FMBRKT(DBH,ISP)
      IMPLICIT NONE
C----------
C FIRE-CA $Id$
C----------

C     COMPUTES THE BARK THICKNESS FOR USE IN THE FIRE-CAUSED MORTALITY
C     ROUTINE (FMEFF). DATA ARE FROM FOFEM V5.0 (REINHARDT ET AL. 2000)

COMMONS

      INCLUDE 'PRGPRM.F77'

COMMONS

      INTEGER ISP
      REAL    DBH,FMBRKT
      REAL    B1(MAXSP)

      DATA B1/
     >     0.081,  ! 1 = Port Orford cedar (pc)
     >     0.060,  ! 2 = incense cedar (ic)
     >     0.035,  ! 3 = western redcedar (rc)
     >     0.048,  ! 4 = white fir (wf)
     >     0.039,  ! 5 = California red fir (rf)
     >     0.039,  ! 6 = Shasta red fir (sh)
     >     0.063,  ! 7 = Douglas-fir (df)
     >     0.035,  ! 8 = western hemlock (wh)
     >     0.040,  ! 9 = mountain hemlock (mh)
     >     0.030,  !10 = whitebark pine (wb)
     >     0.030,  !11 = knobcone pine (kp)
     >     0.028,  !12 = lodgepole pine (lp)
     >     0.063,  !13 = Coulter pine (cp) ! PP
     >     0.030,  !14 = limber pine (lm)
     >     0.068,  !15 = Jeffrey pine (jp)
     >     0.072,  !16 = sugar pine (sp)
     >     0.035,  !17 = western white pine (wp)
     >     0.063,  !18 = ponderosa pine (pp)
     >     0.030,  !19 = Monterey pine (mp) ! KP
     >     0.033,  !20 = gray pine (gp)
     >     0.025,  !21 = western juniper (ju)
     >     0.025,  !22 = Brewer spruce (br) ! white spruce
     >     0.081,  !23 = giant sequoia (gs)
     >     0.025,  !24 = Pacific yew (py)
     >     0.063,  !25 = other softwoods (os) ! PP
     >     0.050,  !26 = coast live oak (lo)
     >     0.024,  !27 = canyon live oak (cy)
     >     0.033,  !28 = blue oak (bl)
     >     0.059,  !29 = Engelmann oak (eo)
     >     0.029,  !30 = Oregon white oak (wo)
     >     0.030,  !31 = California black oak (bo)
     >     0.043,  !32 = valley white oak (vo)
     >     0.034,  !33 = interior live oak (io)
     >     0.024,  !34 = bigleaf maple (bm)
     >     0.036,  !35 = California buckeye (bu) ! Ohio buckeye
     >     0.026,  !36 = red alder (ra)
     >     0.060,  !37 = pacific madrone (ma)
     >     0.045,  !38 = golden chinkapin (gc)
     >     0.062,  !39 = pacific dogwood (dg)
     >     0.042,  !40 = oregon ash (fl) ! Ash spp
     >     0.041,  !41 = walnut (wn)
     >     0.052,  !42 = tanoak (to)
     >     0.033,  !43 = California sycamore (sy) ! American sycamore
     >     0.044,  !44 = quaking aspen (as)
     >     0.044,  !45 = black cottonwood (cw)
     >     0.041,  !46 = willow (wi) ! Salix spp.
     >     0.025,  !47 = California nutmeg (cn) ! PY
     >     0.026,  !48 = California laurel (cl)
     >     0.030,  !49 = other hardwoods (oh) ! BO
     >     0.081/  !50 = coast redwood (RW)

      FMBRKT = DBH*B1(ISP)

      RETURN
      END
