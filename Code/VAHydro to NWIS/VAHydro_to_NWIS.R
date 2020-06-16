
#LOAD CONFIG FILE
source(paste("/var/www/R/config.local.private", sep = ""))
localpath <- paste(github_location,"/USGS_Consumptive_Use", sep = "")

#LOAD from_vahydro() FUNCTION
source(paste(localpath,"/Code/VAHydro to NWIS/from_vahydro.R", sep = ""))
datasite <- "http://deq2.bse.vt.edu/d.dh"


# RETRIEVE AND PROCESS FACILITY AND SITE FILES
#####################################################################################################
# WITHDRAWAL FACILITY FILE
export_view <- "nwis_withdrawal_facility_export"
output_filename <- "withdrawal_facility.csv"
facility <- from_vahydro(datasite,export_view,localpath,output_filename)

#REMOVE LEADING/TRAILING WHITESPACE FROM VAHYDRO SPATIAL CONTAINMENT COLUMNS
facility$State <- trimws(facility$State)
facility$County <- trimws(facility$County)
#REMOVE LEADING "huc12_" FROM HUC_12 COLUMN
facility$HUC_12 <- gsub("huc12_","",facility$HUC_12)

write.csv(facility,paste(localpath,"/withdrawal_facility.csv",sep=""), row.names = FALSE)
#####################################################################################################
# WITHDRAWAL SITE FILE
export_view <- "nwis_withdrawal_site_export"
output_filename <- "withdrawal_site.csv"
site <- from_vahydro(datasite,export_view,localpath,output_filename)

#REMOVE LEADING/TRAILING WHITESPACE FROM VAHYDRO SPATIAL CONTAINMENT COLUMNS
site$County <- trimws(site$County)
site$HUC_12 <- gsub("huc12_","",site$HUC_12)

write.csv(site,paste(localpath,"/withdrawal_site.csv",sep=""), row.names = FALSE)
#####################################################################################################

