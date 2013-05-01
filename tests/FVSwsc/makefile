
all : wst01
#all : wst01 wst02

wst01 : 
	@echo Test wst01, basic functionality 
	-../../bin/FVSws --keywordfile=wst01.key
	grep -v "\-999" wst01.sum      > wst01.tmp
	grep -v "\-999" wst01.sum.save > wst01.tmp.save
	-diff -w wst01.tmp wst01.tmp.save
	rm wst01.tmp wst01.tmp.save
	@echo Test wst01 ended

#wst02 : 
#	@echo Test wst02, stop/restart, output should match wst01
#	-../../bin/FVSwsc --keywordfile=wst01.key --stoppoint=2,2040,wst.stop
#	-../../bin/FVSwsc --restart=wst.stop
#	grep -v "\-999" wst01.sum      > wst01.tmp
#	grep -v "\-999" wst01.sum.save > wst01.tmp.save
#	-diff -w wst01.tmp wst01.tmp.save
#	rm wst01.tmp wst01.tmp.save
#	@echo Test wst02 ended
			
