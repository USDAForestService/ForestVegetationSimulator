#Test for proper SQL summary output
#
#The included fvstest.mdf file must be attached to an existing instance of SQL Server
#  The sqlcmd arguments assumet the instance is running on the local machine.
#
#  To attach it copy the .mdf and .ldf files to you SQLServer data folder
#  modify the path in attach_db below to reflect the correct data folder.
#  Finally, run make attach_db.  Alternatively you can use SQL server management
#  studio or any tool of you choosing.

all : purge_test dsn_test

attach_db :
	-cmd.exe /c "sqlcmd -S .\sqlexpress -d master \
		-Q \"exec sp_attach_db @dbname=N'fvstest', \
		@filename1=N'C:\data\sqlserver\MSSQL10_50.SQLEXPRESS\MSSQL\DATA\fvstest.mdf'\""
	
purge_test : 
	@echo Purge the test database of records for the expected standids
	-cmd.exe /c "sqlcmd -S .\sqlexpress -d fvstest -i purge_test.sql -U sfuser -P Trees123"

dsn_test : 
	@echo Test dsn_test, database output to a SQL Server database
	rm -f dsn_test.fvs_summary dsn_test.sum dsn_test.out
	-../../../bin/FVSpnc --keywordfile=dsn_test.key
	-cmd.exe /c "sqlcmd -S .\sqlexpress -d fvstest -i select_test.sql -U sfuser -P Trees123 -o dsn_test.fvs_summary -s "," -W -h -1"
	diff -w dsn_test.fvs_summary dsn_test.fvs_summary.save

	grep -v "\-999" dsn_test.sum      > dsn_test.tmp
	grep -v "\-999" dsn_test.sum.save > dsn_test.tmp.save
	diff -w dsn_test.tmp dsn_test.tmp.save
	rm dsn_test.tmp dsn_test.tmp.save

	@echo Test dsn_test ended
    
