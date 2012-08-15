
all : ect01 

ect01 : 
	@echo Test ect01, basic functionality 
	-../../bin/FVSecc --keywordfile=ect01.key
	grep -v "\-999" ect01.sum      > ect01.tmp
	grep -v "\-999" ect01.sum.save > ect01.tmp.save
	diff -w ect01.tmp ect01.tmp.save
	rm ect01.tmp ect01.tmp.save
	@echo Test ect01 ended
	
 
 
 

