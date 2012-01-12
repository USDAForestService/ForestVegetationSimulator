
all : pnt01 

pnt01 : 
	@echo Test pnt01, basic functionality 
	-../../bin/FVSpnc < pnt01.rsp
	grep -v "\-999" pnt01.sum      > pnt01.tmp
	grep -v "\-999" pnt01.sum.save > pnt01.tmp.save
	diff pnt01.tmp pnt01.tmp.save
	rm pnt01.tmp pnt01.tmp.save
	@echo Test pnt01 passed
	
 
 
 

