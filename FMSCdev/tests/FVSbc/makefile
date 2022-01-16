# $Id: makefile 734 2013-03-22 22:39:10Z ncrookston.fs@gmail.com $

all : StandStructure YSM-SkyRanch

StandStructure :
	@echo "Test StandStructure (4 stands)"
	-../../bin/FVSbc --keywordfile=StandStructure.key
	grep -v "\-999" StandStructure.sum      > StandStructure.tmp
	grep -v "\-999" StandStructure.sum.save > StandStructure.tmp.save
	-diff -w StandStructure.tmp StandStructure.tmp.save
	rm StandStructure.tmp StandStructure.tmp.save
	rm StandStructure.out StandStructure.sng
	@echo "Test StandStructure (4 stands) ended"

YSM-SkyRanch :
	@echo "Test YSM-SkyRanch (15 stands)"
	-../../bin/FVSbc --keywordfile=YSM-SkyRanch.key
	grep -v "\-999" YSM-SkyRanch.sum      > YSM-SkyRanch.tmp
	grep -v "\-999" YSM-SkyRanch.sum.save > YSM-SkyRanch.tmp.save
	-diff -w YSM-SkyRanch.tmp YSM-SkyRanch.tmp.save
	rm YSM-SkyRanch.tmp YSM-SkyRanch.tmp.save
	rm YSM-SkyRanch.out
	@echo "Test YSM-SkyRanch (15 stands) ended (Ignore STOP 20)"

