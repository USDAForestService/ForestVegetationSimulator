
all : snt01 

snt01 : 
	@echo Test snt01, basic functionality 
	-../../bin/FVSiec < snt01.rsp
	grep -v "\-999" snt01.sum      > snt01.tmp
	grep -v "\-999" snt01.sum.save > snt01.tmp.save
	diff snt01.tmp snt01.tmp.save
	rm snt01.tmp snt01.tmp.save
	@echo Test snt01 passed
	
 
 
 

