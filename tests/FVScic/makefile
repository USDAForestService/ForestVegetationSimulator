
all : cit01 

cit01 : 
	@echo Test cit01, basic functionality 
	-../../bin/FVScic --keywordfile=cit01.key
	grep -v "\-999" cit01.sum      > cit01.tmp
	grep -v "\-999" cit01.sum.save > cit01.tmp.save
	diff -w cit01.tmp cit01.tmp.save
	rm cit01.tmp cit01.tmp.save
	@echo Test cit01 passed
	
