
all : cst01 

cst01 : 
	@echo Test cst01, basic functionality 
	-../../bin/FVScs --keywordfile=cst01.key
	grep -v "\-999" cst01.sum      > cst01.tmp
	grep -v "\-999" cst01.sum.save > cst01.tmp.save
	diff -w cst01.tmp cst01.tmp.save
	rm cst01.tmp cst01.tmp.save
	@echo Test cst01 ended
	
