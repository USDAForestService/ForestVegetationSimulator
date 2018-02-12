
all : cst01 cst02

cst01 : 
	@echo Test cst01, basic functionality 
	-../../bin/qFVScs --keywordfile=cst01.key
	grep -v "\-999" cst01.sum      > cst01.tmp
	grep -v "\-999" cst01.sum.save > cst01.tmp.save
	-diff -w cst01.tmp cst01.tmp.save
	rm cst01.tmp cst01.tmp.save
	@echo Test cst01 ended

cst02 :
	@echo Test cst02, stop/restart, output should match cst01
	-../../bin/qFVScs --keywordfile=cst01.key --stoppoint=2,2040,cst.stop
	-../../bin/qFVScs --restart=cst.stop
	grep -v "\-999" cst01.sum      > cst01.tmp
	grep -v "\-999" cst01.sum.save > cst01.tmp.save
	-diff -w cst01.tmp cst01.tmp.save
	rm cst01.tmp cst01.tmp.save
	@echo Test cst02 ended
			
	
