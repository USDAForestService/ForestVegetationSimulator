
all : ttt01 

ttt01 : 
	@echo Test ttt01, basic functionality 
	-../../bin/FVSttc --keywordfile=ttt01.key
	grep -v "\-999" ttt01.sum      > ttt01.tmp
	grep -v "\-999" ttt01.sum.save > ttt01.tmp.save
	diff -w ttt01.tmp ttt01.tmp.save
	rm ttt01.tmp ttt01.tmp.save
	@echo Test ttt01 ended
	
