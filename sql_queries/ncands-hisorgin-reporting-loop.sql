use NCANDS

set nocount on

declare #ncands_year char(2)
declare #sql varchar(max) 

set #ncands_year = '16'

-- create temporary table to store output with, ## is global for the session across connections
-- select(*) from ##temp once ##temp created 
-- USE UTF8 (65001) for encoding of weird characters in AFCARSID (linking w/FC file)
while #ncands_year >'01'
	begin 
		set #sql = '
					INSERT INTO #TEMPTABLE subyr, cethn, RptFIPS,
					case when rptsrc between 9 AND 12
					then 1
					else 0
					end as comm_report
					from CF'+#ncands_year+'.Contributed as temp1
					) a
				group by a.subyr, a.RptFIPS, a.cethn
				'
		set #ncands_year = #ncands_year - 1
	end

-- select a.subyr, a.cethn, count(*) as total_reports, sum(a.comm_report) as comm_report
-- from(
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc between 9 AND 12
-- 		then 1
-- 		else 0
-- 		end as comm_report
-- 		from CF16.Contributed as temp1
-- 	UNION ALL
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
-- 		then 1
-- 		else 0
-- 		end as comm_report
-- 		from CF15.Contributed as temp2
-- 	UNION ALL
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
-- 		then 1
-- 		else 0
-- 		end as comm_report
-- 		from CF14.Contributed as temp3
-- 	UNION ALL
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
-- 		then 1
-- 		else 0
-- 		end as comm_report		
-- 		from CF13.Contributed as temp4
-- 	UNION ALL
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
-- 		then 1
-- 		else 0
-- 		end as comm_report		
-- 		from CF12.Contributed as temp5
-- 	UNION ALL
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
-- 		then 1
-- 		else 0
-- 		end as comm_report		
-- 		from CF11.Contributed as temp6
-- 	UNION ALL
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
-- 		then 1
-- 		else 0
-- 		end as comm_report		
-- 		from CF10.Contributed as temp7
-- 	UNION ALL
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
-- 		then 1
-- 		else 0
-- 		end as comm_report		
-- 		from CF09.Contributed as temp8
-- 	UNION ALL
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
-- 		then 1
-- 		else 0
-- 		end as comm_report		
-- 		from CF08.Contributed as temp9
-- 	UNION ALL
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
-- 		then 1
-- 		else 0
-- 		end as comm_report		
-- 		from CF07.Contributed as temp10
-- 	UNION ALL
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
-- 		then 1
-- 		else 0
-- 		end as comm_report		
-- 		from CF06.Contributed as temp11
-- 	UNION ALL
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
-- 		then 1
-- 		else 0
-- 		end as comm_report		
-- 		from CF05.Contributed as temp12
-- 	UNION ALL
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
-- 		then 1
-- 		else 0
-- 		end as comm_report		
-- 		from CF04.Contributed as temp13
-- 	UNION ALL
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
-- 		then 1
-- 		else 0
-- 		end as comm_report		
-- 		from CF03.Contributed as temp14
-- 	UNION ALL
-- 	select subyr, cethn, RptFIPS,
-- 	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
-- 		then 1
-- 		else 0
-- 		end as comm_report		
-- 		from CF02.Contributed as temp15
