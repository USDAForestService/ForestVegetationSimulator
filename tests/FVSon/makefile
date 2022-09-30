# $Id: makefile 734 2013-03-22 22:39:10Z ncrookston.fs@gmail.com $

all : Hardwood

Hardwood :
	@echo "Test Hardwood (1 stands)"
	rm -f FVSOutHardwood.db
	rm -f Hardwood.out Hardwood.sng Hardwood.trl
	-../../bin/FVSon --keywordfile=Hardwood.key
	grep -v "\-999" Hardwood.sum      > Hardwood.tmp
	grep -v "\-999" Hardwood.sum.save > Hardwood.tmp.save
	-diff -w Hardwood.tmp Hardwood.tmp.save
	rm Hardwood.tmp Hardwood.tmp.save
	@echo "Hardwood output DB dump"
	rm -f FVSOutHardwoodDBdump.txt
	@echo "Hardwood output DB dump" > FVSOutHardwoodDBdump.txt
	-sqlite3 FVSOutHardwood.db ".tables" >> FVSOutHardwoodDBdump.txt
	@echo "FVS_Cases" >> FVSOutHardwoodDBdump.txt
	-sqlite3 FVSOutHardwood.db ".header on" ".mode csv" \
  "select StandID \
   from FVS_Cases" >> FVSOutHardwoodDBdump.txt
	@echo "FVS_Summary_East_Metric" >> FVSOutHardwoodDBdump.txt
	-sqlite3 FVSOutHardwood.db ".header on" ".mode col" \
  "select count(*)  \
   from FVS_Summary_East_Metric" >> FVSOutHardwoodDBdump.txt
	@echo "---FVS_CutList_East_Metric---" >> FVSOutHardwoodDBdump.txt
	-sqlite3 FVSOutHardwood.db ".header on" ".mode csv" \
  "select count(*) \
   from FVS_CutList_East_Metric" >> FVSOutHardwoodDBdump.txt
	-sqlite3 FVSOutHardwood.db ".header on" ".mode csv" \
  "select SpeciesFVS, count(SpeciesFVS), round(sum(TPH),2) \
   from FVS_CutList_East_Metric \
   group by SpeciesFVS \
   order by SpeciesFVS" >> FVSOutHardwoodDBdump.txt
	@echo "---FVS_TreeList_East_Metric---" >> FVSOutHardwoodDBdump.txt
	-sqlite3 FVSOutHardwood.db ".header on" ".mode csv" \
  "select count(*) \
   from FVS_TreeList_East_Metric" >> FVSOutHardwoodDBdump.txt
	-sqlite3 FVSOutHardwood.db ".header on" ".mode csv" \
  "select SpeciesFVS, count(SpeciesFVS), round(sum(TPH),2) \
   from FVS_TreeList_East_Metric \
   group by SpeciesFVS \
   order by SpeciesFVS" >> FVSOutHardwoodDBdump.txt
	@echo "---End of DB dump---" >> FVSOutHardwoodDBdump.txt
	-diff -w FVSOutHardwoodDBdump.txt FVSOutHardwoodDBdump.txt.save
	@echo "Test Hardwood (1 stand) ended"
