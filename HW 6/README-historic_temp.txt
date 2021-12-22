historic_temp.csv
Temperature readings from Weather stations in several countries from 1881 to 2005
ÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑÑ
Source Data:
File Generated: 02-Feb-2012 
Dataset Collection: GSN Monthly Summaries from NOAA
Retrieved from: http://berkeleyearth.org/source-files/

------------------------------------

This is (a subset of) the dataset for the "GSN Monthly Summaries" dataset as 
represented in the Berkeley Earth Surface Temperature Analysis data archive.

GSN Monthly Summaries RAW DATA

This dataset is a representation of the land surface temperature component of 
the GSN Monthly Summaries provided by NOAA.  The official dataset is maintained 
by NOAA’s National Climatic Data Center (NCDC; http://www.ncdc.noaa.gov/).

The original data files can be found via:
http://cdo.ncdc.noaa.gov/pls/plclimprod/cdomain.abbrev2id?datasetabbv=GSNMON

The Berkeley Earth Surface Temperature project downloads these reports and 
produces a new representation of the temperature data in the standard format 
used by the Berkeley project.

This dataset should contain all of the air temperature data in the original 
dataset as of the date it was acquired.  It will also contain metadata for 
station names, locations, and other identifiers.  In some cases, the original 
source may provide additional metadata that is not included here.

Note: There are many GSNMON stations with missing metadata.  It is frequently 
necessary to consult other sources to find the complete metadata.

------------------------------------

% The columns contained in this file are as follows:
% 
% 	Station ID: A unique integer identifying each station.  Metadata for the site 
% is contained in the associated files labeled "site_summary" or "site_detail".
% 
% 
% 	Date: The date of a measurement is expressed as a year and decimal fraction of 
% a year corresponding to the midpoint of the time period being represented.  
% 
% For example, in monthly data: 
% 
% 	January 2005 = 2005 + (1 - 0.5) / 12 = 2005.042
% 	June 2008 = 2008 + (6 - 0.5) / 12 = 2008.458
% 
% For example, in daily data: 
% 
% 	January 25, 2005 (Day 25) = 2005 + (25 - 0.5)/365 =2005.067
% 	June 3, 2008 (Day 155) = 2008 + (155 - 0.5)/366 = 2008.422
% 			[Note the use of 366, during leap years.]
% 
% With three decimal digits of precision, all days are uniquely identified.

ConvertDecDate.R provides a function to convert decimal dates to (year, month, day)
It was used to produce the columns:

%	Year: the converted year from Date in YYYY format
%	Month: the converted month from Date in 1-12 format

% 
% 	Temperature: The temperature as reported in degrees Celsius.
% 
%	AverageCelsiusTemperature: average temperature recorded for that date at that station
%	MaxCelsiusTemp: max temperature recorded for that date at that station
%	MinCelsiusTemp: max temperature recorded for that date at that station

%	Information about the Station (if available)
%	Station.Name
%	Latitude
%	Longitude
%	Country