# THIS FILE WAS CREATED FOR JOINING "withdrawal_facility.csv" AND "withdrawal_site.csv" TO "withdrawal_water_quantity.csv"
# TO ENSURE THE FACILITY AND SITE FILES ONLY CONTAIN FACILITIES AND SITES PRESENT IN THE WATER QUANTITY FILE

library(sqldf)

# #LOAD CONFIG FILE
# source(paste("/var/www/R/config.local.private", sep = ""))
# localpath <- paste(github_location,"/USGS_Consumptive_Use", sep = "")
localpath <- "C:/Users/jklei/Desktop/WUDR_export"


  
######################################################################
# WITHDRAWAL Faciliity FILE
site_filename <- "withdrawal_facility_22-01-34_Wed_Jun_17_2020.csv"
water_quantity_filename <- "withdrawal_water_quantity.csv"

#READ CSV FROM LOCAL DIRECTORY
facility_dataframe <- read.csv(file=paste(localpath,site_filename,sep="\\"), header=TRUE, sep=",")
water_quantity <- read.csv(file=paste(localpath,water_quantity_filename,sep="\\"), header=TRUE, sep=",")

#JOIN TO WATER QUANTITY FILE
fac_join_query <- paste('SELECT b.*
                 FROM "water_quantity" a
                 LEFT JOIN "facility_dataframe" b
                 ON a.Facility_ID = b.Facility_ID',
                    sep='')
fac_join <- sqldf(fac_join_query)

#remove duplicate rows, Though facilities will still show multiple times if they have multiple permits
fac_join <- unique(fac_join)
write.csv(fac_join,paste(localpath,"/withdrawal_facility_JOIN_",format(Sys.time(), "%H-%M-%OS_%a_%b_%d_%Y"),".csv",sep=""), row.names = FALSE)
######################################################################
# WITHDRAWAL Site FILE
site_filename <- "withdrawal_site_23-03-16_Wed_Jun_17_2020.csv"
water_quantity_filename <- "withdrawal_water_quantity.csv"

#READ CSV FROM LOCAL DIRECTORY
site_dataframe <- read.csv(file=paste(localpath,site_filename,sep="\\"), header=TRUE, sep=",")
water_quantity <- read.csv(file=paste(localpath,water_quantity_filename,sep="\\"), header=TRUE, sep=",")

site_join_query <- paste('SELECT b.*
                 FROM "water_quantity" a
                 LEFT JOIN "site_dataframe" b
                 ON a.Site_ID = b.Site_ID',
                    sep='')
site_join <- sqldf(site_join_query)

#remove duplicate rows, Though sites will still show multiple times if their facilities have multiple permits, or if they're linked to multiple facilities
site_join <- unique(site_join)
write.csv(site_join,paste(localpath,"/withdrawal_site_JOIN_",format(Sys.time(), "%H-%M-%OS_%a_%b_%d_%Y"),".csv",sep=""), row.names = FALSE)
######################################################################
######################################################################
######################################################################
######################################################################


