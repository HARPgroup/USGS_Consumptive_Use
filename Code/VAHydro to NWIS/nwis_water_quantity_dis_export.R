#DISCHARGE 

#load library
library('httr')
library('sqldf')
library('dplyr')
library('tidyr')
options(scipen = 999)

#load variables
syear = 2019
eyear = 2019

startdate <- paste(syear, "-01-01",sep='')
enddate <- paste(eyear, "-12-31", sep='')

#LOAD CONFIG FILE
source(paste("/var/www/R/config.local.private", sep = ""))
localpath <- paste(github_location,"/USGS_Consumptive_Use", sep = "")

#LOAD from_vahydro() FUNCTION
source(paste(localpath,"/Code/VAHydro to NWIS/from_vahydro.R", sep = ""))
datasite <- "http://deq2.bse.vt.edu/d.dh"

cached = FALSE
# RETRIEVE WITHDRAWAL DATA
export_view <- paste0("ows-awrr-map-export/dmr_ann_mgy?ftype_op=%3D&ftype=&bundle%5B1%5D=transfer&ftype=&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate)
output_filename <- "dis_mgy_export.csv"
data_annual <- from_vahydro(datasite,export_view,localpath,output_filename, cached)

############################################  
# ##check to see if there are multiple dis_mgy entries for a single year
#   a <- sqldf("SELECT a.*
# FROM data a
# JOIN (SELECT MP_hydroid, Facility_hydroid, 'Water.Use.MGY' as mgy, COUNT(*)
# FROM data
# GROUP BY MP_hydroid
# HAVING count(*) > 1 ) b
# ON a.MP_hydroid = b.MP_hydroid
# ORDER BY a.MP_hydroid")
############################################

#filter out non ECHO features 
data_ann <- sqldf('SELECT *
              FROM data_annual
              WHERE Hydrocode LIKE "echo_%"')

#remove duplicates (keeps one row for each year)
data_ann <- distinct(data_ann, MP_hydroid, Year, .keep_all = TRUE)

#rename columns 
dis_mgy <- sqldf('SELECT MP_hydroid,
                          Hydrocode,
                          "Source.Type" AS Source_Type,
                          "MP.Name" AS MP_Name,
                          Facility_hydroid,
                          Facility AS Facility_Name,
                          "USE.Type" AS Use_Type,
                          Year,
                          "Water.Use.MGY" AS MGY,
                          Latitude,
                          Longitude,
                          "FIPS.Code" AS FIPS_code
                       FROM data_ann
                       ORDER BY Year
                       ') 

sqldf('SELECT sum(MGY)/365
      FROM dis_mgy 
      WHERE Use_Type NOT LIKE "%power%"')

#transform from long to wide df
dis_mgy_export <- spread(data = dis_mgy, key = Year, value = MGY,sep = "_")

#save file
#write.csv(dis_mgy_export,paste(localpath,"/discharge_annual.csv",sep=""), row.names = FALSE)

#####################################################################################
#####################################################################################
#####################################################################################

#monthly withdrawal export

# RETRIEVE WITHDRAWAL DATA
export_view <- paste0("ows-annual-report-map-exports-monthly-export/dmr_mon_mgm?ftype_op=%3D&ftype=&bundle%5B0%5D=transfer&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate)
output_filename <- "dis_mgm_export.csv"
data_monthly <- from_vahydro(datasite,export_view,localpath,output_filename, cached)

###################
# #check to see if there are multiple dis_mgy entries for a single year (should be multiples of 12)
#   a <- sqldf("SELECT a.*
# FROM data a
# JOIN (SELECT MP_hydroid, Facility_hydroid, 'Water.Use.MGY' as mgy, COUNT(*)
# FROM data
# GROUP BY MP_hydroid
# HAVING count(*) > 12 ) b
# ON a.MP_hydroid = b.MP_hydroid
# ORDER BY a.MP_hydroid")
###################

#filter out non ECHO features 
data_mon <- sqldf('SELECT *
              FROM data_monthly
              WHERE Hydrocode LIKE "echo_%"')

#remove duplicates (keeps one row for each combination of Month and year)
data_mon <- sqldf("SELECT *
               FROM data_mon
               GROUP BY MP_hydroid, Month, Year")

dis_mon_median <- sqldf('select MP_hydroid, median("Water.Use.MGM") as mon_med from data_mon group by MP_Hydroid')
data_flagged <- sqldf(
  'SELECT MP_hydroid, max(flag_data_qual) as flag_data_qual 
   from ( 
     SELECT a.*, 
     CASE 
       WHEN a."Water.Use.MGM" = 0 THEN 0
       WHEN b.mon_med = 0 THEN 0
       WHEN a."Water.Use.MGM" >= (100.0 * b.mon_med ) THEN 1
       ELSE 0
     END as flag_data_qual
     FROM data_mon as a 
     left outer join dis_mon_median as b
     on (a.MP_hydroid = b.MP_hydroid)
  ) as foo 
  group by MP_hydroid
  '
)

#exclude dalecarlia
#data <- data[-which(data$Facility=='DALECARLIA WTP'),]

#transform from long to wide df

dis_mgm_export <- spread(data = data_mon, key = Month, value = Water.Use.MGM, sep = "_",)
dis_mgm_export <- sqldf(
  "select a.*, b.flag_data_qual 
   from dis_mgm_export as a 
   left outer join data_flagged as b 
   on (a.MP_hydroid = b.MP_hydroid)
  "
)

#rename columns
dis_mgm <- sqldf('SELECT MP_hydroid,
                          Hydrocode,
                          "Source.Type" AS Source_Type,
                          "MP.Name" AS MP_Name,
                          Facility_hydroid,
                          Facility AS Facility_Name,
                          "Use.Type" AS Use_Type,
                          Latitude,
                          Longitude,
                          "FIPS.Code" AS FIPS_code,
                          Year,
                          flag_data_qual,
                          Month_1 AS Jan,
                          Month_2 AS Feb,
                          Month_3 AS Mar,
                          Month_4 AS Apr,
                          Month_5 AS May,
                          Month_6 AS Jun,
                          Month_7 AS Jul,
                          Month_8 AS Aug,
                          Month_9 AS Sep,
                          Month_10 AS Oct,
                          Month_11 AS Nov,
                          Month_12 AS Dec
                       FROM dis_mgm_export
                       ORDER BY MP_hydroid, Year
                       ') 
#save file
#write.csv(dis_mgm_export,paste(localpath,"/discharge_monthly.csv",sep=""), row.names = FALSE)

###################
# #QA check to see that the MGY from Annual Map Export matches the sum of all 12 months from Monthly Map Export
# dis_mgm_export$ann_sum <- rowSums(dis_mgm_export[13:24],na.rm = FALSE)
# 
# dis_join_no_match <- sqldf('SELECT a.*, b.MGY
#                  FROM dis_mgm_export a
#                  LEFT OUTER JOIN dis_mgy b
#                  ON a.Year = b.Year
#                  AND a.MP_hydroid = b.MP_hydroid
#                  WHERE round(a.ann_sum,3) != round(b.MGY,3)')
##################

#add annual MGY value onto monthly export
dis_join <- sqldf('SELECT a.*, b.MGY
                 FROM dis_mgm a
                 LEFT OUTER JOIN dis_mgy b
                 ON a.Year = b.Year
                 AND a.MP_hydroid = b.MP_hydroid')

#rename columns for consistent export to USGS
dis_join2 <- sqldf('SELECT "VA087" AS From_Agency_Code,
                          MP_hydroid AS Site_ID,
                          "USEPA" AS To_Agency_Code,
                          Facility_hydroid AS Facility_ID,
                          Year,
                          "USEPA" AS Data_Source_Code,
                          "RT" AS Water_Quantity_code,
                          Source_Type AS Site_Type,
                          "UNKN" AS Method_Code,
                          "N" AS Accuracy_Code,
                          "W" AS Data_Aging_Code,
                          Use_Type as Facility_Type,
                          "Y" AS Preferred_Flag,
                          "Mgal/yr" AS Annual_Reporting_Unit_Name,
                          MGY AS Annual_Value,
                          "Mgal/m" AS Monthly_Reporting_Unit_Name,
                          flag_data_qual,
                          Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
                 FROM dis_join')

#with power
sqldf('SELECT sum(Annual_Value)/365 AS total_MGD
      FROM dis_join2')

#without power
sqldf('SELECT sum(Annual_Value)/365 AS total_MGD
      FROM dis_join2
      WHERE Facility_Type NOT LIKE "%power%"')

#save file
write.csv(dis_join2, paste("U:/OWS/foundation_datasets/nwis/discharge_water_quantity_",format(Sys.time(), "%H-%M-%OS_%a_%b_%d_%Y"),".csv",sep=""), row.names = FALSE)