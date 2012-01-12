
all : iet01 

iet01 : 
	@echo Test iet01, basic functionality 
	-../../bin/FVSiec < iet01.rsp
	grep -v "\-999" iet01.sum      > iet01.tmp
	grep -v "\-999" iet01.sum.save > iet01.tmp.save
	diff iet01.tmp iet01.tmp.save
	rm iet01.tmp iet01.tmp.save
	@echo Test iet01 passed
	
 
 
 

