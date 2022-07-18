require(RSQLite)

lt = dbDriver("SQLite")
ltdb = dbConnect(lt, dbname = "FVS_Data.db")

stands = dbReadTable(ltdb, "FVS_StandInit", as.is=TRUE)
#fix up the "Variant" field:
stands$Variant = gsub(" ","",stands$Variant)

dbsKeys = dbReadTable(ltdb, "FVS_GroupAddFilesAndKeywords", as.is=TRUE)
dbsKeys[1,3] = gsub("\r\n","\n",dbsKeys[1,3])
dbsKeys[2,3] = gsub("\r\n","\n",dbsKeys[2,3])

vars = c("ie","sn","ne")
for (var in vars)
{
 fout = paste(var,".key",sep="")
 unlink (fout)
 apply(subset(stands,Variant==var),1,function (astand)
   {
     cat("StandCN\n",file=fout,append=TRUE)
     cat(astand["Stand_CN"],"\n",file=fout,append=TRUE)
     # only used with "plots"     
     # cat(dbsKeys[1,"FVSKeywords"],"\n\n",file=fout,append=TRUE)
     cat(dbsKeys[2,"FVSKeywords"],"\n\n",file=fout,append=TRUE)
     cat("Screen\nNumCycle       4.\nEchoSum\nProcess\n\n",file=fout,append=TRUE)
   })
   cat("Stop\n",file=fout,append=TRUE)
}

dbDisconnect(ltdb)



