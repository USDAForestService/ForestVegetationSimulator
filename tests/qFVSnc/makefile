
all : nct01 nct02 ffe

nct01 : 
	@echo Test nct01, basic functionality 
	--$(WINE) ../../bin/qFVSnc --keywordfile=nct01.key
	grep -v "\-999" nct01.sum      > nct01.tmp
	grep -v "\-999" nct01.sum.save > nct01.tmp.save
	-diff -w nct01.tmp nct01.tmp.save
	rm nct01.tmp nct01.tmp.save
	@echo Test nct01 ended

nct02 : 
	@echo Test nct02, stop/restart, output should match nct01
	--$(WINE) ../../bin/qFVSnc --keywordfile=nct01.key --stoppoint=2,2020,nct.stop
	--$(WINE) ../../bin/qFVSnc --restart=nct.stop
	grep -v "\-999" nct01.sum      > nct01.tmp
	grep -v "\-999" nct01.sum.save > nct01.tmp.save
	-diff -w nct01.tmp nct01.tmp.save
	rm nct01.tmp nct01.tmp.save
	@echo Test nct02 ended
		 
ffe : 
	@echo Test ffe
	--$(WINE) ../../bin/qFVSnc --keywordfile=ffe.key 
	grep -v "\-999" ffe.sum      > ffe.tmp
	grep -v "\-999" ffe.sum.save > ffe.tmp.save
	-diff -w ffe.tmp ffe.tmp.save
	rm ffe.tmp ffe.tmp.save
	@echo Test ffe ended
 

