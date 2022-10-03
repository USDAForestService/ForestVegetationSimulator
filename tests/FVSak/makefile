
all : akt01 akt02

akt01 : 
	@echo Test akt01, basic functionality 
	../../bin/FVSak --keywordfile=akt01.key
	grep -v "\-999" akt01.sum      > akt01.tmp
	grep -v "\-999" akt01.sum.save > akt01.tmp.save
	-diff -w akt01.tmp akt01.tmp.save
	rm akt01.tmp akt01.tmp.save
	@echo Test akt01 ended

akt02 : 
	@echo Test akt02, stop/restart, should get same answer as akt01
	../../bin/FVSak --keywordfile=akt01.key --stoppoint=2,2040,akt.stop
	../../bin/FVSak --restart=akt.stop
	grep -v "\-999" akt01.sum      > akt01.tmp
	grep -v "\-999" akt01.sum.save > akt01.tmp.save
	-diff -w akt01.tmp akt01.tmp.save
	rm akt01.tmp akt01.tmp.save
	@echo Test akt02 ended

clean : 
	rm -rfv akt.stop akt01.out akt01.sng akt01.sum FVS_Data.db FVSOut.db
