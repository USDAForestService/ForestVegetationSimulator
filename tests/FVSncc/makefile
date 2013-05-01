
all : nct01
#all : nct01 nct02

nct01 : 
	@echo Test nct01, basic functionality 
	-../../bin/FVSnc --keywordfile=nct01.key
	grep -v "\-999" nct01.sum      > nct01.tmp
	grep -v "\-999" nct01.sum.save > nct01.tmp.save
	-diff -w nct01.tmp nct01.tmp.save
	rm nct01.tmp nct01.tmp.save
	@echo Test nct01 ended
	
#nct02 : 
#	@echo Test nct02, stop/restart, output should match nct01
#	-../../bin/FVSncc --keywordfile=nct01.key --stoppoint=2,2020,nct.stop
#	-../../bin/FVSncc --restart=nct.stop
#	grep -v "\-999" nct01.sum      > nct01.tmp
#	grep -v "\-999" nct01.sum.save > nct01.tmp.save
#	-diff -w nct01.tmp nct01.tmp.save
#	rm nct01.tmp nct01.tmp.save
#	@echo Test nct02 ended
		 
 
 

