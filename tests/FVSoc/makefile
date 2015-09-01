
all : oct01 oct02

oct01 : 
	@echo Test oct01, basic functionality 
	-../../bin/FVSoc --keywordfile=oct01.key
	grep -v "\-999" oct01.sum      > oct01.tmp
	grep -v "\-999" oct01.sum.save > oct01.tmp.save
	-diff -w oct01.tmp oct01.tmp.save
	rm oct01.tmp oct01.tmp.save
	@echo Test oct01 ended
	
oct02 : 
	@echo Test oct02, stop/restart
	-../../bin/FVSoc --keywordfile=oct01.key --stoppoint=2,2020,oct.stop
	-../../bin/FVSoc --restart=oct.stop
	grep -v "\-999" oct01.sum      > oct01.tmp
	grep -v "\-999" oct01.sum.save > oct01.tmp.save
	-diff -w oct01.tmp oct01.tmp.save
	rm oct01.tmp oct01.tmp.save
	@echo Test oct02 ended

	
	
