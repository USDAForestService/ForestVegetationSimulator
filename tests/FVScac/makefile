
all : cat01 

cat01 : 
	@echo Test cat01, basic functionality 
	-../../bin/FVScac --keywordfile=cat01.key
	grep -v "\-999" cat01.sum      > cat01.tmp
	grep -v "\-999" cat01.sum.save > cat01.tmp.save
	diff -w cat01.tmp cat01.tmp.save
	rm cat01.tmp cat01.tmp.save
	@echo Test cat01 passed
	
