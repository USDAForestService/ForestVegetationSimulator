
all : sot01 sot02

sot01 : 
	@echo Test sot01, basic fusotionality 
	-../../bin/FVSso --keywordfile=sot01.key
	grep -v "\-999" sot01.sum      > sot01.tmp
	grep -v "\-999" sot01.sum.save > sot01.tmp.save
	-diff -w sot01.tmp sot01.tmp.save
	rm sot01.tmp sot01.tmp.save
	@echo Test sot01 ended
	
 
sot02 : 
	@echo Test sot02, stop/restart, output should match sot01
	-../../bin/FVSso --keywordfile=sot01.key --stoppoint=2,2040,sot.stop
	-../../bin/FVSso --restart=sot.stop
	grep -v "\-999" sot01.sum      > sot01.tmp
	grep -v "\-999" sot01.sum.save > sot01.tmp.save
	-diff -w sot01.tmp sot01.tmp.save
	rm sot01.tmp sot01.tmp.save
	@echo Test sot02 ended
		 
 

