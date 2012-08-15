
all : wct01 

wct01 : 
	@echo Test wct01, basic functionality 
	-../../bin/FVSwcc --keywordfile=wct01.key
	grep -v "\-999" wct01.sum      > wct01.tmp
	grep -v "\-999" wct01.sum.save > wct01.tmp.save
	diff -w wct01.tmp wct01.tmp.save
	rm wct01.tmp wct01.tmp.save
	@echo Test wct01 ended
	