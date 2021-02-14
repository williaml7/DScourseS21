--Reading florida insurance csv data file into sql
--First, create an appropriate table

CREATE TABLE florida(
	policyID INTEGER,
	statecode TEXT,
	county TEXT,
	eq_site_limit REAL,
	hu_site_limit REAL,
	fl_site_limit REAL,
	fr_site_limit REAL,
	tiv_2011 REAL,
	tiv_2012 REAL,
	eq_site_deductible REAL,
	hu_site_deductible REAL,
	fl_site_deductible REAL,
	fr_site_deductible REAL,
	point_latitude REAL,
	point_longitude REAL,
	line TEXT,
	construction TEXT,
	point_granularity INTEGER
);

--Read csv file into memory

.mode csv
.import /home/ouecon045/DScourseS21/ProblemSets/PS3/FL_insurance_sample.csv florida

--Printing first 10 rows of data
--Note that sqlite in git bash seems to not detect the first row in the csv file as var names despite using '.mode csv'
--This is why I did LIMIT 11 instead of LIMIT 10

SELECT * FROM florida LIMIT 11;
	
--List which counties are in data set

SELECT DISTINCT county FROM florida;

--Calculate avg property appreciation from 2011 to 2012 

SELECT AVG(tiv_2012 - tiv_2011) FROM florida;

--Create frequency table of the construction var

SELECT construction, count(*) FROM florida GROUP BY construction;
