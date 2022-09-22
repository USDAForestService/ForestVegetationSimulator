
all : emt01 emt02

emt01 : 
	@echo Test emt01, basic functionality 
	../../bin/FVSem --keywordfile=emt01.key
	grep -v "\-999" emt01.sum      > emt01.tmp
	grep -v "\-999" emt01.sum.save > emt01.tmp.save
	-diff -w emt01.tmp emt01.tmp.save
	rm emt01.tmp emt01.tmp.save
	@echo Test emt01 ended

emt02 : 
	@echo Test emt02, stop/restart, output should match emt01
	../../bin/FVSem --keywordfile=emt01.key --stoppoint=2,2040,emt.stop
	../../bin/FVSem --restart=emt.stop
	grep -v "\-999" emt01.sum      > emt01.tmp
	grep -v "\-999" emt01.sum.save > emt01.tmp.save
	-diff -w emt01.tmp emt01.tmp.save
	rm emt01.tmp emt01.tmp.save
	@echo Test emt02 ended

clean : 
	rm -rfv emt.stop emt01.out emt01.sng emt01.sum FVS_Data.db FVSOut.db