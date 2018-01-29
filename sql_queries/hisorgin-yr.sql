use AFCARS;

SET ANSI_WARNINGS OFF;

select count(HISORGIN) as TotalCases, FY,
	sum(case when HISORGIN = 1 then 1 else 0 end) IsLatino,
	sum(case when HISORGIN = 3 then 1 else 0 end) LatinoNA,
	count(distinct FIPSCODE) as Counties
from dbo.FosterCare
where(FY>2000)
group by FY
order by FY desc;

select count(HISORGIN) as TotalCases, STATE, 
	sum(case when HISORGIN = 1 then 1 else 0 end) IsLatino,
	sum(case when HISORGIN = 3 then 1 else 0 end) LatinoNA
from dbo.FosterCare
where(FY>2000)
group by STATE;