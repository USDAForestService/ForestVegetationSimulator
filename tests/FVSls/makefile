
all : lst01 lst02

lst01 : 
	@echo Test lst01, basic functionality 
	../../bin/FVSls --keywordfile=lst01.key
	grep -v "\-999" lst01.sum      > lst01.tmp
	grep -v "\-999" lst01.sum.save > lst01.tmp.save
	-diff -w lst01.tmp lst01.tmp.save
	rm lst01.tmp lst01.tmp.save
	@echo Test lst01 ended
	
 
lst02 : 
	@echo Test lst02, stop/restart, output should match lst01
	../../bin/FVSls --keywordfile=lst01.key --stoppoint=2,2040,lst.stop
	../../bin/FVSls --restart=lst.stop
	grep -v "\-999" lst01.sum      > lst01.tmp
	grep -v "\-999" lst01.sum.save > lst01.tmp.save
	-diff -w lst01.tmp lst01.tmp.save
	rm lst01.tmp lst01.tmp.save
	@echo Test lst02 ended
		 
 

