
# $Id$

all : iet01
#all : iet01 iet02 iet03 TestSpecialCostAndRevenue MinHarvValueOfTrees \
# climate climatePutGet

iet01 : 
	@echo Test iet01, basic functionality 
	-../../bin/FVSie < iet01.rsp
	grep -v "\-999" iet01.sum      > iet01.tmp
	grep -v "\-999" iet01.sum.save > iet01.tmp.save
	-diff -w iet01.tmp iet01.tmp.save
	rm iet01.tmp iet01.tmp.save
	@echo Test iet01 ended
	
 
#iet02 : 
#	@echo Test iet02, test use of stop/restart, same as iet01. 
#	-../../bin/FVSiec --keywordfile=iet01.key --stoppoint=2,2040,iet02.stop
#	-../../bin/FVSiec --restart=iet02.stop
#	grep -v "\-999" iet01.sum      > iet01.tmp
#	grep -v "\-999" iet01.sum.save > iet01.tmp.save
#	-diff -w iet01.tmp iet01.tmp.save
#	rm iet01.tmp iet01.tmp.save
#	@echo Test iet02 ended
	
#iet03 : 
#	@echo Test iet03, simple test of ffe with stop/restart
#	-../../bin/FVSiec --keywordfile=iet03.key --stoppoint=1,2003,iet03.stop
#	-../../bin/FVSiec --restart=iet03.stop
#	grep -v "\-999" iet03.sum      > iet03.tmp
#	grep -v "\-999" iet03.sum.save > iet03.tmp.save
#	-diff -w iet03.tmp iet03.tmp.save
#	rm iet03.tmp iet03.tmp.save
#	@echo Test iet03 ended

#TestSpecialCostAndRevenue :
#	@echo Test ECON TestSpecialCostAndRevenue
#	-../../bin/FVSiec --keywordfile=TestSpecialCostAndRevenue.key
#	-diff -w TestSpecialCostAndRevenue.out TestSpecialCostAndRevenue.out.save
#	@echo Test ECON TestSpecialCostAndRevenue ended
#	
#MinHarvValueOfTrees :
#	@echo Test ECON MinHarvValueOfTrees
#	-../../bin/FVSiec --keywordfile=MinHarvValueOfTrees.key
#	-diff -w MinHarvValueOfTrees.out MinHarvValueOfTrees.out.save
#	@echo Test ECON MinHarvValueOfTrees ended
#	
#climate:
#	@echo Test Climate-FVS 
#	-../../bin/FVSiec --keywordfile=climate.key
#	grep -v "\-999" climate.sum      > climate.tmp
#	grep -v "\-999" climate.sum.save > climate.tmp.save
#	-diff -w climate.tmp climate.tmp.save
#	rm climate.tmp climate.tmp.save
#	@echo Test Climate-FVS ended
#	
#climatePutGet:
#	@echo Test Climate-FVS with PutGet
#	-../../bin/FVSiec --keywordfile=climate.key --stoppoint=1,2040,climate.stop
#	-../../bin/FVSiec --restart=climate.stop
#	grep -v "\-999" climate.sum      > climate.tmp
#	grep -v "\-999" climate.sum.save > climate.tmp.save
#	-diff -w climate.tmp climate.tmp.save
#	rm climate.tmp climate.tmp.save
#	@echo Test Climate-FVS with PutGet ended
#	
# 
#
#
#
#