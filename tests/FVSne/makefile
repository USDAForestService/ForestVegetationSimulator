
all : net01 

net01 : 
	@echo Test net01, basic functionality 
	-../../bin/FVSne < net01.rsp
	grep -v "\-999" net01.sum      > net01.tmp
	grep -v "\-999" net01.sum.save > net01.tmp.save
	diff -w net01.tmp net01.tmp.save
	rm net01.tmp net01.tmp.save
	@echo Test net01 ended
	
 
 
 

