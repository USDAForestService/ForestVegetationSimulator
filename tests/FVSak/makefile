
all : akt01 

akt01 : 
	@echo Test akt01, basic functionality 
	-../../bin/FVSak --keywordfile=akt01.key
	grep -v "\-999" akt01.sum      > akt01.tmp
	grep -v "\-999" akt01.sum.save > akt01.tmp.save
	diff -w akt01.tmp akt01.tmp.save
	rm akt01.tmp akt01.tmp.save
	@echo Test akt01 passed
	
