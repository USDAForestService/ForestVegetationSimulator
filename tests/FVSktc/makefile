
all : ktt01 

ktt01 : 
	@echo Test ktt01, basic functionality 
	-../../bin/FVSktc --keywordfile=ktt01.key
	grep -v "\-999" ktt01.sum      > ktt01.tmp
	grep -v "\-999" ktt01.sum.save > ktt01.tmp.save
	diff -w ktt01.tmp ktt01.tmp.save
	rm ktt01.tmp ktt01.tmp.save
	@echo Test ktt01 passed
	
