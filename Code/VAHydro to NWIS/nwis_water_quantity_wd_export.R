#load library
library('httr')
library('sqldf')
library('dplyr')
#load variables
syear = 2017
eyear = 2019

  startdate <- paste(syear, "-01-01",sep='')
  enddate <- paste(eyear, "-12-31", sep='')
  
  localpath <- tempdir()
  filename <- paste("data.all_",y,".csv",sep="")
  destfile <- paste(localpath,filename,sep="\\") 
  
  #has 3 issuing authorities, does not include power
  download.file(paste("http://deq2.bse.vt.edu/d.dh/ows-awrr-map-export/wd_mgy?ftype_op=not&ftype=power&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=",startdate,"&tstime%5Bmax%5D=",enddate,"&bundle%5B0%5D=well&bundle%5B1%5D=intake&dh_link_admin_reg_issuer_target_id%5B0%5D=65668&dh_link_admin_reg_issuer_target_id%5B1%5D=91200&dh_link_admin_reg_issuer_target_id%5B2%5D=77498",sep=""), destfile = destfile, method = "libcurl")  
  data.all <- read.csv(file=paste(localpath , filename,sep="\\"), header=TRUE, sep=",")
  
  data <- data.all
  
  #check to see if there are multiple wd_mgy entries for a single year
  a <- sqldf("SELECT a.*
FROM data a
JOIN (SELECT MP_hydroid, Facility_hydroid, 'Water.Use.MGY' as mgy, COUNT(*)
FROM data
GROUP BY MP_hydroid
HAVING count(*) > 1 ) b
ON a.MP_hydroid = b.MP_hydroid
ORDER BY a.MP_hydroid")
  
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
                       ') 
  #place into export data frame
wd_mgy_export <- reshape(data = wd_mgy_export, idvar = "MP_hydroid", timevar = "Year", v.names = "MGY", direction = "wide")














#monthly withdrawal export
#start for loop

#pull from monthly  map exports view

#remove duplicates caused by permit filter

#transform table from long to wide (move month column into a column for each month)

#place into export data frame

#append following iterations to the export data frame (just the year and wd_mgm value)

#end for loop
