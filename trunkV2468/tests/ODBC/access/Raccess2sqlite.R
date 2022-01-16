require(RODBC)
require(RSQLite)

setwd("C:\\fvs\\open-fvs\\trunk\\tests\\ODBC\\access")

acc = odbcConnectAccess2007("FVS_Data.mdb")
tables=sqlTables(acc)
tables=subset(tables,TABLE_TYPE=="TABLE")

if (nrow(tables) > 0) 
{
  lt = dbDriver("SQLite")
  unlink("../sqlite/FVS_Data.db",force=TRUE)
  ltdb = dbConnect(lt, dbname = "../sqlite/FVS_Data.db")
  for (table in tables$TABLE_NAME)
  {
    atab = sqlFetch(acc,table,as.is=TRUE)
    # does this table contain "FVSKeywords", if yes, change the db name.
    kw=match("FVSKeywords",colnames(atab))
    if (!is.na(kw)) for (row in 1:nrow(atab)) # change "mdb" and "accdb" to "db"
    {
      atab[row,kw]= sub("FVS_Data.mdb","FVS_Data.db",atab[row,kw])
      atab[row,kw]= sub("FVS_Data.accdb","FVS_Data.db",atab[row,kw])
    }
    dbWriteTable(ltdb, table, atab)
  }
}
dbDisconnect(ltdb)
close(acc)

