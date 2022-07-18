select s.standid,s.year,s.age
        ,s.tpa,s.ba,s.sdi,s.ccf,s.topht,s.qmd,s.tcuft,s.mcuft,s.bdft
        ,s.rtpa,s.rtcuft,s.rmcuft,s.rbdft
        ,s.atba,s.atsdi,s.atccf,s.attopht,s.atqmd
        ,s.prdlen,s.acc,s.mort,s.mai,s.fortyp,s.sizecls,s.stkcls
from fvs_summary as s
	inner join fvs_cases as c
		on s.caseid=c.caseid
    --limit records to the most recent cases in case the db was not purged
	inner join (
		select standid,max(rundatetime) as dt
		from fvs_cases
		group by standid
		) as foo
		on foo.StandID=c.StandID and c.RunDateTime=foo.dt
where s.standid in ('test_driver','test_filedsn','test_sysdsn')
order by standid,"year"
