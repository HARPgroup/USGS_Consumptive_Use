#load library
library('httr')
library('sqldf')
library('dplyr')
library('tidyr')
#load variables
syear = 2018
eyear = 2019

  startdate <- paste(syear, "-01-01",sep='')
  enddate <- paste(eyear, "-12-31", sep='')
  
  localpath <- tempdir()
  filename <- "data.all.csv"
  destfile <- paste(localpath,filename,sep="\\") 
  
  
  #has 3 issuing authorities, does not include power
  download.file(paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), destfile = destfile, method = "libcurl")  
  data.all <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
  
  data <- data.all
  
  #check to see if there are multiple wd_mgy entries for a single year
#   a <- sqldf("SELECT a.*
# FROM data a
# JOIN (SELECT MP_hydroid, Facility_hydroid, 'Water.Use.MGY' as mgy, COUNT(*)
# FROM data
# GROUP BY MP_hydroid
# HAVING count(*) > 2 ) b
# ON a.MP_hydroid = b.MP_hydroid
# ORDER BY a.MP_hydroid")
  
  #remove duplicates (keeps one row for each year)
  data <- distinct(data, MP_hydroid, Year, .keep_all = TRUE)
  #exclude dalecarlia
  #data <- data[-which(data$Facility=='DALECARLIA WTP'),]

  #rename columns 
wd_mgy_export <- sqldf('SELECT MP_hydroid,
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
                       FROM data
                       ORDER BY Year
                       ') 
  #place into export data frame
wd_mgy_export2 <- spread(data = wd_mgy_export, key = Year, value = MGY,sep = "_")

#####################################################################################
#####################################################################################
#####################################################################################
#monthly withdrawal export

#load variables
 syear = 2018
 eyear = 2019

#set time range
 startdate <- paste(syear, "-01-01",sep='')
 enddate <- paste(eyear, "-12-31", sep='')

 #####################################################################################
#WIP - goal is to supply irregular length of time (for example, just months 1 through 6)
 #need to figure out how to rename those columns based on months given
 # #smonth <- 1
# #emonth <- 6
# startdate <- paste(syear,if (smonth %in% 1:9) {
#   paste0(0,smonth)
# } else {smonth},"01",sep='-')
#
# enddate <- paste(eyear,if (emonth %in% 1:9) {
#   paste0(0,emonth)
# } else {emonth},"31", sep='-')
#####################################################################################

#LOAD CONFIG FILE
source(paste("/var/www/R/config.local.private", sep = ""))
localpath <- paste(github_location,"/USGS_Consumptive_Use", sep = "")

#LOAD from_vahydro() FUNCTION
source(paste(localpath,"/Code/VAHydro to NWIS/from_vahydro.R", sep = ""))
datasite <- "http://deq2.bse.vt.edu/d.dh"


# RETRIEVE WITHDRAWAL DATA
export_view <- paste0("ows-annual-report-map-exports-monthly-export/wd_mgm?ftype_op=%3D&ftype=&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=77498&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate)
output_filename <- "wd_mgm_export.csv"
data <- from_vahydro(datasite,export_view,localpath = tempdir(),output_filename)

#check to see if there are multiple wd_mgy entries for a single year (should be multiples of 12)
  a <- sqldf("SELECT a.*
FROM data a
JOIN (SELECT MP_hydroid, Facility_hydroid, 'Water.Use.MGY' as mgy, COUNT(*)
FROM data
GROUP BY MP_hydroid
HAVING count(*) > 24 ) b
ON a.MP_hydroid = b.MP_hydroid
ORDER BY a.MP_hydroid")

#remove duplicates (keeps one row for each combination of Month and year)
data <- sqldf("SELECT *
               FROM data
               GROUP BY MP_hydroid, Month, Year")

#exclude dalecarlia
#data <- data[-which(data$Facility=='DALECARLIA WTP'),]

#rename columns 
wd_mgm_export <- sqldf('SELECT MP_hydroid,
                          Hydrocode,
                          "Source.Type" AS Source_Type,
                          "MP.Name" AS MP_Name,
                          Facility_hydroid,
                          Facility AS Facility_Name,
                          "USE.Type" AS Use_Type,
                          "Water.Use.MGM" AS MGM,
                          Latitude,
                          Longitude,
                          Locality,
                          Month,
                          Year
                       FROM data
                       ORDER BY MP_hydroid, Year, Month
                       ') 
#place into export data frame
wd_mgm_export <- spread(data = wd_mgm_export, key = Month, value = MGM, sep = "_",)

#rename columns
wd_mgm_export <- sqldf('SELECT MP_hydroid,
                          Hydrocode,
                          Source_Type,
                          MP_Name,
                          Facility_hydroid,
                          Facility_Name,
                          Use_Type,
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

#append Annual MGY value (from annual export df) to monthly export df

#save file
write.csv(wd_mgm_export,paste(localpath,"/withdrawal_monthly.csv",sep=""), row.names = FALSE)
