!== last modified  4-9-2002
      subroutine r6fix(DBHOB,FCLASS,tlh,tth,httype,logvol)
c        subroutine applying PNW fix for bdft appraisal
c           Created Sept 22, 1997
      real DBHOB,logvol(7,20),tlh,tth,tthln,tlhln 
      real bf3216,logtmp
      integer FCLASS,I
      character*1 httype                         
      
      if(httype.eq.'l' .or. httype.eq.'L') then
          
          tlhln = ALOG(tlh*10)
          bf3216 = 0.4017+(0.1450*tlhln)-(0.0025*DBHOB)-(0.0009*FCLASS)
 
      else

          tthln = ALOG(tth)
          bf3216=0.1909+(0.0006*DBHOB)+(0.1349*tthln)-(0.0002*FCLASS)
      
      endif
          
      do 100, I=1,20
          logtmp = logvol(1,i)*bf3216
          logvol(1,i)=anint(logtmp)
 100  continue    
          
      return
      end
          
          
 
      