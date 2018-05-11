
all : ktt01 ktt02

ktt01 : 
	@echo Test ktt01, basic functionality 
	-$(WINE) ../../bin/qFVSkt --keywordfile=ktt01.key
	grep -v "\-999" ktt01.sum      > ktt01.tmp
	grep -v "\-999" ktt01.sum.save > ktt01.tmp.save
	-diff -w ktt01.tmp ktt01.tmp.save
	rm ktt01.tmp ktt01.tmp.save
	@echo Test ktt01 ended

ktt02 : 
	@echo Test ktt02, stop/restart, output should match ktt01
	-$(WINE) ../../bin/qFVSkt --keywordfile=ktt01.key --stoppoint=2,2040,ktt.stop
	-$(WINE) ../../bin/qFVSkt --restart=ktt.stop
	grep -v "\-999" ktt01.sum      > ktt01.tmp
	grep -v "\-999" ktt01.sum.save > ktt01.tmp.save
	-diff -w ktt01.tmp ktt01.tmp.save
	rm ktt01.tmp ktt01.tmp.save
	@echo Test ktt02 ended
			
