      SUBROUTINE BPENMAN(TAIR,VPD,RAD,GC,GS,DAYLEN,PENMON)
C ------------------------------------------
C This is the Penmon-Monteith function
C ------------------------------------------
      REAL DAYCONST, DAYLEN, RAD, RAD1, GAMMA, TAIR, T1, T2
      REAL SVP1, SVP2, SLOPE, CP, PA, RA, RS, GC, GS
      REAL XLAT, XTRANS, VPD, PENMON
      DAYCONST=.85
C convert (kj/m2/day) to (Ave. watts/m2).      
      RAD1=RAD*1000.0/(DAYLEN*DAYCONST)
      GAMMA=0.646+0.0006*TAIR
      T1=TAIR+0.5
      T2=TAIR-0.5
      SVP1=6.1078*EXP((17.269*T1)/(237.0+T1))
      SVP2=6.1078*EXP((17.269*T2)/(237.0+T2))
      SLOPE=SVP1-SVP2
      CP=1.01E+3
      PA=1.292-0.00428*TAIR
      RA=1.0/GC
      RS=1.0/GS
      XLAT=(2.501-0.0024*TAIR)*1.0E+6
      XTRANS=((SLOPE*RAD1)+(CP*PA)*(VPD/RA))/(SLOPE+GAMMA*(1.0+RS/RA))
      PENMON=XTRANS/(XLAT*1000.0)
      RETURN
      END


