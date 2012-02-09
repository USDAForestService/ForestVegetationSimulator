
all : iet01 iet02

iet01 : 
	@echo Test iet01, basic functionality 
	-../../bin/FVSiec < iet01.rsp
	grep -v "\-999" iet01.sum      > iet01.tmp
	grep -v "\-999" iet01.sum.save > iet01.tmp.save
	-diff -w iet01.tmp iet01.tmp.save
	rm iet01.tmp iet01.tmp.save
	@echo Test iet01 ended
	
 
iet02 : 
	@echo Test iet02, test use of command line options 
	-../../bin/FVSiec --keywordfile=iet02.key --stoppoint=2,2040,iet02.stop
	-../../bin/FVSiec --restart=iet02.stop
	-diff -w iet02.sum iet02.sum.save
	@echo Test iet02 ended
	
 
 

