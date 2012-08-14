
all : nct01 

nct01 : 
	@echo Test nct01, basic functionality 
	-../../bin/FVSncc --keywordfile=nct01.key
	grep -v "\-999" nct01.sum      > nct01.tmp
	grep -v "\-999" nct01.sum.save > nct01.tmp.save
	diff -w nct01.tmp nct01.tmp.save
	rm nct01.tmp nct01.tmp.save
	@echo Test nct01 passed
	
 
 
 

