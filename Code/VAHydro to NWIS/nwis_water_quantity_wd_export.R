#WITHDRAWAL

#load library
library('httr')
library('sqldf')
library('dplyr')
library('tidyr')

#load variables
syear = 2021
eyear = 2022

##########################################################################
#LOAD CONFIG FILE
source(paste("/var/www/R/config.local.private", sep = ""))
localpath <- paste(github_location,"/USGS_Consumptive_Use", sep = "")

#LOAD from_vahydro() FUNCTION
source(paste(localpath,"/Code/VAHydro to NWIS/from_vahydro.R", sep = ""))
datasite <- "http://deq1.bse.vt.edu/d.dh"

# RETRIEVE WITHDRAWAL DATA
wd_annual_data <- list()

## year range
year_range <- format(seq(as.Date(paste0(syear,"/1/1")), as.Date(paste0(eyear,"/1/1")), "years"), format="%Y")

for (y in year_range) {
  print(paste0("PROCESSING YEAR: ", y))
  startdate <- paste(y, "-01-01",sep='')
  enddate <- paste(y, "-12-31", sep='')

#with power
export_view <- paste0("ows-awrr-map-export/wd_mgy?ftype_op=%3D&ftype=&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498")
#Issuing Authority = VWUDS, VWP, and GWP registrations
#without power
#export_view <- paste0("ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498")
output_filename <- "wd_mgy_export.csv"
wd_annual <- from_vahydro(datasite,export_view,localpath,output_filename)

wd_annual_data <- rbind(wd_annual_data, wd_annual)
}
############################################  
# #check to see if there are multiple wd_mgy entries for a single year
#   wd_mgy_entries <- ((eyear - syear) +1)
#   a <- sqldf(paste('SELECT a.*
# FROM wd_annual a
# JOIN (SELECT MP_hydroid, Facility_hydroid, "Water.Use.MGY" as mgy, COUNT(*)
# FROM wd_annual
# GROUP BY MP_hydroid
# HAVING count(*) > ',wd_mgy_entries,' ) b
# ON a.MP_hydroid = b.MP_hydroid
# ORDER BY a.MP_hydroid'))
############################################
  sqldf('SELECT sum("Water.Use.MGY")/365
      FROM wd_annual_data 
      WHERE "Use.Type" NOT LIKE "%power%"')
  
  #remove duplicates - GROUP BY USING MAX
  wd_ann <- sqldf('SELECT "MP_hydroid","Hydrocode","Source.Type","MP.Name","Facility_hydroid","Facility","Use.Type","Year",max("Water.Use.MGY") AS "Water.Use.MGY","Latitude","Longitude","Locality","FIPS.Code" 
               FROM wd_annual_data
               WHERE Facility != "DALECARLIA WTP"
               GROUP BY "MP_hydroid","Hydrocode","Source.Type","MP.Name","Facility_hydroid","Facility","Use.Type","Year","Latitude","Longitude","Locality","FIPS.Code"
                ORDER BY "Water.Use.MGY" DESC ')
  
#rename columns 
wd_mgy <- sqldf('SELECT MP_hydroid,
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
                       FROM wd_ann
                       ORDER BY Year
                       ') 

sqldf('SELECT sum(MGY)/365
      FROM wd_mgy 
      WHERE Use_Type NOT LIKE "%power%"')

#place into export data frame
wd_mgy_export <- spread(data = wd_mgy, key = Year, value = MGY,sep = "_")

#save file
#write.csv(wd_mgy_export,paste(localpath,"/withdrawal_annual.csv",sep=""), row.names = FALSE)

#####################################################################################
#####################################################################################
#####################################################################################

#monthly withdrawal export

wd_monthly_data <- list()

## year range
year_range <- format(seq(as.Date(paste0(syear,"/1/1")), as.Date(paste0(eyear,"/1/1")), "years"), format="%Y")

#increase time to pull from URL
getOption('timeout')
options(timeout = 300)

for (y in year_range) {
  print(paste0("PROCESSING YEAR: ", y))
  startdate <- paste(y, "-01-01",sep='')
  enddate <- paste(y, "-12-31", sep='')

# RETRIEVE WITHDRAWAL DATA
export_view <- paste0("ows-annual-report-map-exports-monthly-export/wd_mgm?ftype_op=%3D&ftype=&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498")
output_filename <- "wd_mgm_export.csv"
wd_monthly <- from_vahydro(datasite,export_view,localpath,output_filename)

wd_monthly_data <- rbind(wd_monthly_data, wd_monthly)
}

#all_monthly_data <- rbind(all_monthly_data, wd_monthly_data)

write.csv(wd_monthly_data,paste(export_path,"/withdrawal_annual_ALL.csv",sep=""), row.names = FALSE)
#exclude dalecarlia
wd_mon <- wd_monthly_data[-which(wd_monthly$Facility=='DALECARLIA WTP'),]

sqldf('SELECT sum("Water.Use.MGM")/365
      FROM wd_mon
      WHERE "Use.Type" NOT LIKE "%power%"')

###################
##check to see if there are multiple wd_mgm entries for a single year (should be multiples of 12)
  wd_mgm_entries <- ((eyear - syear) +1)*12
  a <- sqldf(paste('SELECT a.*
FROM wd_mon a
JOIN (SELECT MP_hydroid, Facility_hydroid, "Water.Use.MGM" as mgm, COUNT(*)
FROM wd_mon
GROUP BY MP_hydroid
HAVING count(*) >',wd_mgm_entries,') b
ON a.MP_hydroid = b.MP_hydroid
ORDER BY a.MP_hydroid'))
sqldf('SELECT sum("Water.Use.MGM")/365 AS dupe_MGD_total
      from a')
###################

sqldf('SELECT sum(MGY)/365
      FROM wd_mgy 
      WHERE Use_Type NOT LIKE "%power%"')

#remove duplicates - GROUP BY USING MAX
wd_mon <- sqldf('SELECT "MP_hydroid","Hydrocode","Source.Type","MP.Name","Facility_hydroid","Facility","Use.Type","Year","Month",max("Water.Use.MGM") AS "Water.Use.MGM","Latitude","Longitude","Locality","FIPS.Code" 
               FROM wd_mon
               WHERE Facility != "DALECARLIA WTP"
               GROUP BY "MP_hydroid","Hydrocode","Source.Type","MP.Name","Facility_hydroid","Facility","Use.Type","Month","Year","Latitude","Longitude","Locality","FIPS.Code"
                ORDER BY "Water.Use.MGM" DESC ')
#### #NOTE: LAL, THE WD_MON DATAFRAME IS PROBABLY BETTER SUITED FOR METEORLOGICAL DATA #####

sqldf('SELECT sum("Water.Use.MGM")/365
      FROM wd_mon
      WHERE "Use.Type" NOT LIKE "%power%"')

#transform from long to wide df
wd_mgm_export <- spread(data = wd_mon, key = Month, value = Water.Use.MGM, sep = "_",)

#rename columns
wd_mgm <- sqldf('SELECT MP_hydroid,
                          Hydrocode,
                          "Source.Type" AS Source_Type,
                          "MP.Name" AS MP_Name,
                          Facility_hydroid,
                          Facility AS Facility_Name,
                          "USE.Type" AS Use_Type,
                          Latitude,
                          Longitude,
                          Locality,
                          Year,
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
                       FROM wd_mgm_export
                       ORDER BY MP_hydroid, Year
                       ')

#save file
#write.csv(wd_mgm,paste(localpath,"/withdrawal_monthly.csv",sep=""), row.names = FALSE)

###################
###QA check to see that the MGY from Annual Map Export matches the sum of all 12 months from Monthly Map Export
wd_mgm_export$ann_sum <- rowSums(wd_mgm_export[13:24],na.rm = FALSE)

wd_join_no_match <- sqldf('SELECT a.*, b.MGY
                 FROM wd_mgm_export a
                 LEFT OUTER JOIN wd_mgy b
                 ON a.Year = b.Year
                 AND a.MP_hydroid = b.MP_hydroid
                 WHERE round(a.ann_sum,3) != round(b.MGY,3)')
##################

#add annual MGY value onto monthly export
wd_join <- sqldf('SELECT a.*, b.MGY
                 FROM wd_mgm a
                 LEFT OUTER JOIN wd_mgy b
                 ON a.Year = b.Year
                 AND a.MP_hydroid = b.MP_hydroid')

sqldf('SELECT sum(MGY)/365
      FROM wd_join
      WHERE Use_type NOT LIKE "%power%" ')

sqldf('SELECT count(MP_hydroid)
      from wd_join
      where Source_type LIKE "Well"')

#rename columns for consistent export to USGS
wd_join2 <- sqldf('SELECT "VA087" AS From_Agency_Code,
                          MP_hydroid AS Site_ID,
                          "VA087" AS To_Agency_Code,
                          Facility_hydroid AS Facility_ID,
                          Year,
                          "VA087" AS Data_Source_Code,
                          "WD" AS Water_Quantity_code,
                          Source_Type AS Site_Type,
                          "UNKN" AS Method_Code,
                          "N" AS Accuracy_Code,
                          "W" AS Data_Aging_Code,
                          Use_Type as Facility_Type,
                          "Y" AS Preferred_Flag,
                          "Mgal/yr" AS Annual_Reporting_Unit_Name,
                          MGY AS Annual_Value,
                          "Mgal/m" AS Monthly_Reporting_Unit_Name,
                          Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
                 FROM wd_join')
# 
# #save file
# write.csv(wd_join2, paste("U:/OWS/foundation_datasets/nwis/withdrawal_water_quantity_",format(Sys.time(), "%H-%M-%OS_%a_%b_%d_%Y"),".csv",sep=""), row.names = FALSE)


#save file
write.csv(wd_join2, paste(export_path,"/withdrawal_",syear,"-",eyear,"_water_quantity_",format(Sys.time(), "%H-%M-%OS_%a_%b_%d_%Y"),".csv",sep=""), row.names = FALSE)
