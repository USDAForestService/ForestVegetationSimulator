
all : snt01 snt02 sndb

snt01 : 
	@echo Test snt01, basic functionality 
	-../../bin/qFVSsn --keywordfile=snt01.key 
	grep -v "\-999" snt01.sum      > snt01.tmp
	grep -v "\-999" snt01.sum.save > snt01.tmp.save
	-diff -w snt01.tmp snt01.tmp.save
	rm snt01.tmp snt01.tmp.save
	@echo Test snt01 ended
	
snt02 : 
	@echo Test snt02, stop/restart, output should match snt01
	-../../bin/qFVSsn --keywordfile=snt01.key --stoppoint=2,2020,snt.stop
	-../../bin/qFVSsn --restart=snt.stop
	grep -v "\-999" snt01.sum      > snt01.tmp
	grep -v "\-999" snt01.sum.save > snt01.tmp.save
	-diff -w snt01.tmp snt01.tmp.save
	rm snt01.tmp snt01.tmp.save
	@echo Test snt02 ended
		 
sndb : 
	@echo sn variant database read and write, tests a lot of tables
	rm -f snout.db
	-../../bin/qFVSsn --keywordfile=sn.key  
	-sqlite3 snout.db < snTablesTest.sql > snout.txt
	-diff -w snout.txt snout.txt.save
	
