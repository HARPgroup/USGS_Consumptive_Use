#DISCHARGE 

#load library
library('httr')
library('sqldf')
library('dplyr')
library('tidyr')
options(scipen = 999)

#load variables
syear = 2018
eyear = 2019

startdate <- paste(syear, "-01-01",sep='')
enddate <- paste(eyear, "-12-31", sep='')

localpath <- tempdir()
filename <- "data.all.csv"
destfile <- paste(localpath,filename,sep="\\") 

#has 3 issuing authorities, includes power
download.file(paste("http://deq2.bse.vt.edu/d.alpha/ows-awrr-map-export/dmr_ann_mgy?ftype_op=%3D&bundle%5B1%5D=transfer&ftype=&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,sep=""), destfile = destfile, method = "libcurl")  
data.all <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")

data <- data.all

############################################  
# ##check to see if there are multiple wd_mgy entries for a single year
#   a <- sqldf("SELECT a.*
# FROM data a
# JOIN (SELECT MP_hydroid, Facility_hydroid, 'Water.Use.MGY' as mgy, COUNT(*)
# FROM data
# GROUP BY MP_hydroid
# HAVING count(*) > 2 ) b
# ON a.MP_hydroid = b.MP_hydroid
# ORDER BY a.MP_hydroid")
############################################

#filter out non ECHO features 
data <- sqldf('SELECT *
              FROM data
              WHERE Hydrocode LIKE "echo_%"')

#remove duplicates (keeps one row for each year)
data <- distinct(data, MP_hydroid, Year, .keep_all = TRUE)

#rename columns 
dis_mgy_export <- sqldf('SELECT MP_hydroid,
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
dis_mgy_export <- spread(data = dis_mgy_export, key = Year, value = MGY,sep = "_")

#save file
#write.csv(dis_mgy_export,paste(localpath,"/discharge_annual.csv",sep=""), row.names = FALSE)

#####################################################################################
#####################################################################################
#####################################################################################

#monthly withdrawal export

#################################################
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
#################################################

#LOAD CONFIG FILE
source(paste("/var/www/R/config.local.private", sep = ""))
localpath <- paste(github_location,"/USGS_Consumptive_Use", sep = "")

#LOAD from_vahydro() FUNCTION
source(paste(localpath,"/Code/VAHydro to NWIS/from_vahydro.R", sep = ""))
datasite <- "http://deq2.bse.vt.edu/d.alpha"


# RETRIEVE WITHDRAWAL DATA
export_view <- paste0("ows-annual-report-map-exports-monthly-export/dmr_mon_mgm?ftype_op=%3D&ftype=&bundle%5B0%5D=transfer&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate)
output_filename <- "wd_mgm_export.csv"
data <- from_vahydro(datasite,export_view,localpath,output_filename)

###################
# #check to see if there are multiple wd_mgy entries for a single year (should be multiples of 12)
#   a <- sqldf("SELECT a.*
# FROM data a
# JOIN (SELECT MP_hydroid, Facility_hydroid, 'Water.Use.MGY' as mgy, COUNT(*)
# FROM data
# GROUP BY MP_hydroid
# HAVING count(*) > 24 ) b
# ON a.MP_hydroid = b.MP_hydroid
# ORDER BY a.MP_hydroid")
###################

#filter out non ECHO features 
data <- sqldf('SELECT *
              FROM data
              WHERE Hydrocode LIKE "echo_%"')

#remove duplicates (keeps one row for each combination of Month and year)
data <- sqldf("SELECT *
               FROM data
               GROUP BY MP_hydroid, Month, Year")

#exclude dalecarlia
#data <- data[-which(data$Facility=='DALECARLIA WTP'),]

#transform from long to wide df
dis_mgm_export <- spread(data = data, key = Month, value = Water.Use.MGM, sep = "_",)

#rename columns
dis_mgm_export <- sqldf('SELECT MP_hydroid,
                          Hydrocode,
                          "Source.Type" AS Source_Type,
                          "MP.Name" AS MP_Name,
                          Facility_hydroid,
                          Facility AS Facility_Name,
                          "USE.Type" AS Use_Type,
                          Latitude,
                          Longitude,
                          "FIPS.Code" AS FIPS_code,
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
                       FROM dis_mgm_export
                       ORDER BY MP_hydroid, Year
                       ') 
#save file
#write.csv(dis_mgm_export,paste(localpath,"/discharge_monthly.csv",sep=""), row.names = FALSE)

###################
# #QA check to see that the MGY from Annual Map Export matches the sum of all 12 months from Monthly Map Export
# dis_mgm_export$ann_sum <- rowSums(dis_mgm_export[12:23],na.rm = FALSE)
# 
# dis_join_no_match <- sqldf('SELECT a.*, b."Water.Use.MGY" AS MGY
#                  FROM dis_mgm_export a
#                  LEFT OUTER JOIN "data.all" b
#                  ON a.Year = b.Year
#                  AND a.MP_hydroid = b.MP_hydroid
#                  WHERE round(a.ann_sum,3) != round(b."Water.Use.MGY",3)')
##################

#add annual MGY value onto monthly export
dis_join <- sqldf('SELECT a.*, b."Water.Use.MGY" AS MGY
                 FROM dis_mgm_export a
                 LEFT OUTER JOIN "data.all" b
                 ON a.Year = b.Year
                 AND a.MP_hydroid = b.MP_hydroid')
#save file
write.csv(dis_join, paste(localpath,"/discharge_water_quantity.csv",sep=""), row.names = FALSE)
