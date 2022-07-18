
all : pnt01 pnt02

pnt01 : 
	@echo Test pnt01, basic functionality 
	-../../bin/FVSpn --keywordfile=pnt01.key
	grep -v "\-999" pnt01.sum      > pnt01.tmp
	grep -v "\-999" pnt01.sum.save > pnt01.tmp.save
	-diff -w pnt01.tmp pnt01.tmp.save
	rm pnt01.tmp pnt01.tmp.save
	@echo Test pnt01 ended
	
pnt02 : 
	@echo Test pnt02, stop/restart, output should match pnt01
	-../../bin/FVSpn --keywordfile=pnt01.key --stoppoint=2,2040,pnt.stop
	-../../bin/FVSpn --restart=pnt.stop
	grep -v "\-999" pnt01.sum      > pnt01.tmp
	grep -v "\-999" pnt01.sum.save > pnt01.tmp.save
	-diff -w pnt01.tmp pnt01.tmp.save
	rm pnt01.tmp pnt01.tmp.save
	@echo Test pnt02 ended
		 
 
 

