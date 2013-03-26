
all : cat01 cat02

cat01 : 
	@echo Test cat01, basic functionality 
	-../../bin/FVScac --keywordfile=cat01.key
	grep -v "\-999" cat01.sum      > cat01.tmp
	grep -v "\-999" cat01.sum.save > cat01.tmp.save
	-diff -w cat01.tmp cat01.tmp.save
	rm cat01.tmp cat01.tmp.save
	@echo Test cat01 ended
	
cat02 : 
	@echo Test cat02, stop/restart
	-../../bin/FVScac --keywordfile=cat01.key --stoppoint=2,2040,cat.stop
	-../../bin/FVScac --restart=cat.stop
	grep -v "\-999" cat01.sum      > cat01.tmp
	grep -v "\-999" cat01.sum.save > cat01.tmp.save
	-diff -w cat01.tmp cat01.tmp.save
	rm cat01.tmp cat01.tmp.save
	@echo Test cat02 ended

	
	
