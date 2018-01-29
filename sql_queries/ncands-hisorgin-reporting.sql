use NCANDS

set nocount on

select a.subyr, a.RptFIPS, a.cethn, count(*) as total_reports, sum(a.comm_report) as comm_report
from(
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report
		from CF16.Contributed as temp1
	UNION ALL
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report
		from CF15.Contributed as temp2
	UNION ALL
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report
		from CF14.Contributed as temp3
	UNION ALL
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report		
		from CF13.Contributed as temp4
	UNION ALL
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report		
		from CF12.Contributed as temp5
	UNION ALL
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report		
		from CF11.Contributed as temp6
	UNION ALL
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report		
		from CF10.Contributed as temp7
	UNION ALL
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report		
		from CF09.Contributed as temp8
	UNION ALL
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report		
		from CF08.Contributed as temp9
	UNION ALL
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report		
		from CF07.Contributed as temp10
	UNION ALL
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report		
		from CF06.Contributed as temp11
	UNION ALL
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report		
		from CF05.Contributed as temp12
	UNION ALL
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report		
		from CF04.Contributed as temp13
	UNION ALL
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report		
		from CF03.Contributed as temp14
	UNION ALL
	select subyr, cethn, RptFIPS,
	case when rptsrc = 9 OR rptsrc = 10 OR rptsrc = 11 OR rptsrc = 12
		then 1
		else 0
		end as comm_report		
		from CF02.Contributed as temp15
	) a
group by a.subyr, a.RptFIPS, a.cethn