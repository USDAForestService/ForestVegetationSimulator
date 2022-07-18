
all : sot01 sot02 sot03

sot01 : 
	@echo Test sot01, basic fusotionality 
	-$(WINE) ../../bin/qFVSso --keywordfile=sot01.key
	grep -v "\-999" sot01.sum      > sot01.tmp
	grep -v "\-999" sot01.sum.save > sot01.tmp.save
	-diff -w sot01.tmp sot01.tmp.save
	rm sot01.tmp sot01.tmp.save
	@echo Test sot01 ended
	
 
sot02 : 
	@echo Test sot02, stop/restart, output should match sot01
	-$(WINE) ../../bin/qFVSso --keywordfile=sot01.key --stoppoint=2,2040,sot.stop
	-$(WINE) ../../bin/qFVSso --restart=sot.stop
	grep -v "\-999" sot01.sum      > sot01.tmp
	grep -v "\-999" sot01.sum.save > sot01.tmp.save
	-diff -w sot01.tmp sot01.tmp.save
	rm sot01.tmp sot01.tmp.save
	@echo Test sot02 ended
	
 
sot03 : 
	@echo Test sot03, basic run additional treedata 
	-$(WINE) ../../bin/qFVSso --keywordfile=sot03.key
	grep -v "\-999" sot03.sum      > sot03.tmp
	grep -v "\-999" sot03.sum.save > sot03.tmp.save
	-diff -w sot03.tmp sot03.tmp.save
	rm sot03.tmp sot03.tmp.save
	@echo Test sot03 ended
	

