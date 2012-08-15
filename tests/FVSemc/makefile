
all : emt01 

emt01 : 
	@echo Test emt01, basic functionality 
	-../../bin/FVSemc --keywordfile=emt01.key
	grep -v "\-999" emt01.sum      > emt01.tmp
	grep -v "\-999" emt01.sum.save > emt01.tmp.save
	diff -w emt01.tmp emt01.tmp.save
	rm emt01.tmp emt01.tmp.save
	@echo Test emt01 ended
	
