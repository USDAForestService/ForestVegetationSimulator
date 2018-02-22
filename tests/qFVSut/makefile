
all : utt01 utt02

utt01 : 
	@echo Test utt01, basic functionality 
	-$(WINE) ../../bin/qFVSut --keywordfile=utt01.key
	grep -v "\-999" utt01.sum      > utt01.tmp
	grep -v "\-999" utt01.sum.save > utt01.tmp.save
	-diff -w utt01.tmp utt01.tmp.save
	rm utt01.tmp utt01.tmp.save
	@echo Test utt01 ended

utt02 : 
	@echo Test utt02, stop/restart, output should match utt01
	-$(WINE) ../../bin/qFVSut --keywordfile=utt01.key --stoppoint=2,2040,utt.stop
	-$(WINE) ../../bin/qFVSut --restart=utt.stop
	grep -v "\-999" utt01.sum      > utt01.tmp
	grep -v "\-999" utt01.sum.save > utt01.tmp.save
	-diff -w utt01.tmp utt01.tmp.save
	rm utt01.tmp utt01.tmp.save
	@echo Test utt02 ended
			
