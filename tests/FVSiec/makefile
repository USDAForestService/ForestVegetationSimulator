
# $Id$

all : iet01 iet02 iet03  \
 ffe_svs climate climatePutGet maxActs 

iet01 : 
	@echo Test iet01, basic functionality 
	-../../bin/FVSiec < iet01.rsp
	grep -v "\-999" iet01.sum      > iet01.tmp
	grep -v "\-999" iet01.sum.save > iet01.tmp.save
	-diff -w iet01.tmp iet01.tmp.save
	rm iet01.tmp iet01.tmp.save
	@echo Test iet01 ended
	
 
iet02 : 
	@echo Test iet02, test use of stop/restart, same as iet01. 
	-../../bin/FVSiec --keywordfile=iet01.key --stoppoint=2,2040,iet02.stop
	-../../bin/FVSiec --restart=iet02.stop
	grep -v "\-999" iet01.sum      > iet01.tmp
	grep -v "\-999" iet01.sum.save > iet01.tmp.save
	-diff -w iet01.tmp iet01.tmp.save
	rm iet01.tmp iet01.tmp.save
	@echo Test iet02 ended
	
iet03 : 
	@echo Test iet03, simple test of ffe with stop/restart
	-../../bin/FVSiec --keywordfile=iet03.key --stoppoint=1,2003,iet03.stop
	-../../bin/FVSiec --restart=iet03.stop
	grep -v "\-999" iet03.sum      > iet03.tmp
	grep -v "\-999" iet03.sum.save > iet03.tmp.save
	-diff -w iet03.tmp iet03.tmp.save
	rm iet03.tmp iet03.tmp.save
	@echo Test iet03 ended

ffe_svs:
	@echo ffe_svs: Test FFE with SVS and stop/restart
	-../../bin/FVSiec --keywordfile=ffe_svs.key --stoppoint=1,2003,ffe_svs.stop
	-../../bin/FVSiec --restart=ffe_svs.stop
	-diff -w ffe_svs_007.svs ffe_svs_007.svs.save
	-diff -w ffe_svs_index.svs ffe_svs_index.svs.save
#	rm ffe_svs_*.svs ffe_svs_*.grd
	@echo Test ffe_svs ended
	

climate:
	@echo Test Climate-FVS 
	-../../bin/FVSiec --keywordfile=climate.key
	grep -v "\-999" climate.sum      > climate.tmp
	grep -v "\-999" climate.sum.save > climate.tmp.save
	-diff -w climate.tmp climate.tmp.save
	rm climate.tmp climate.tmp.save
	@echo Test Climate-FVS ended
	
climatePutGet:
	@echo Test Climate-FVS with PutGet
	-../../bin/FVSiec --keywordfile=climate.key --stoppoint=1,2040,climate.stop
	-../../bin/FVSiec --restart=climate.stop
	grep -v "\-999" climate.sum      > climate.tmp
	grep -v "\-999" climate.sum.save > climate.tmp.save
	-diff -w climate.tmp climate.tmp.save
	rm climate.tmp climate.tmp.save
	@echo Test Climate-FVS with PutGet ended

maxActs:
	@echo Test having many compute statements
	-../../bin/FVSiec --keywordfile=maxActs.key
	@echo This grep command should not find the string
	grep "OPTION/ACTIVITY STORAGE AREA IS FULL" maxActs.out 
	rm maxActs.out 
	@echo Test having many compute statements ended

