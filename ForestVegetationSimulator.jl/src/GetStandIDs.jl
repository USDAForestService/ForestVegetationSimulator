function fvsGetStandIDs()
      sID=repeat(" ",26)
      sCN=repeat(" ",40)
      mID=repeat(" ",4)
      ncsID=0
      ncCN=0
      ncmID=0
      ccall((:fvsstandid_ , "libFVS_iec"), Void,(
            Cstring, #sID
            Cstring, #sCN
            Cstring, #mID
            Ref{Int32}, #ncsID
            Ref{Int32}, #ncCN
            Ref{Int32}, #ncmID
      ),sID,sCN,mID,ncsID,ncCN,ncmID)
      return[sID,sCN,mID,ncsID,ncCN,ncmID]
end