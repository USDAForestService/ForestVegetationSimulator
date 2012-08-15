
all : sot01 

sot01 : 
	@echo Test sot01, basic fusotionality 
	-../../bin/FVSsoc --keywordfile=sot01.key
	grep -v "\-999" sot01.sum      > sot01.tmp
	grep -v "\-999" sot01.sum.save > sot01.tmp.save
	diff -w sot01.tmp sot01.tmp.save
	rm sot01.tmp sot01.tmp.save
	@echo Test sot01 ended
	
 
 
 

