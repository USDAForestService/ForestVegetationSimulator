
all : lst01 

lst01 : 
	@echo Test lst01, basic functionality 
	-../../bin/FVSls --keywordfile=lst01.key
	grep -v "\-999" lst01.sum      > lst01.tmp
	grep -v "\-999" lst01.sum.save > lst01.tmp.save
	diff -w lst01.tmp lst01.tmp.save
	rm lst01.tmp lst01.tmp.save
	@echo Test lst01 passed
	
 
 
 

