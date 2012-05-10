
all : wst01 

wst01 : 
	@echo Test wst01, basic functionality 
	-../../bin/FVSwsc --keywordfile=wst01.key
	grep -v "\-999" wst01.sum      > wst01.tmp
	grep -v "\-999" wst01.sum.save > wst01.tmp.save
	diff -w wst01.tmp wst01.tmp.save
	rm wst01.tmp wst01.tmp.save
	@echo Test wst01 passed
	
