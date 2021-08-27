
# #LOAD CONFIG FILE
source(paste("/var/www/R/config.local.private", sep = ""))
localpath <- paste(github_location,"/USGS_Consumptive_Use", sep = "")
# localpath <- "C:/Users/jklei/Desktop/WUDR_export"


#LOAD from_vahydro() FUNCTION
source(paste(localpath,"/Code/VAHydro to NWIS/from_vahydro.R", sep = ""))
datasite <- "http://deq1.bse.vt.edu:81/d.dh"


# RETRIEVE AND PROCESS FACILITY AND SITE FILES
#####################################################################################################
# WITHDRAWAL FACILITY FILE
export_view <- "nwis_withdrawal_facility_export"
output_filename <- "withdrawal_facility.csv"
withdrawal_facility <- from_vahydro(datasite,export_view,localpath,output_filename)

#REMOVE LEADING/TRAILING WHITESPACE FROM VAHYDRO SPATIAL CONTAINMENT COLUMNS
withdrawal_facility$State <- trimws(withdrawal_facility$State)
withdrawal_facility$County <- trimws(withdrawal_facility$County)
#REMOVE LEADING "huc12_" FROM HUC_12 COLUMN
withdrawal_facility$HUC_12 <- gsub("huc12_","",withdrawal_facility$HUC_12)

write.csv(withdrawal_facility,paste(localpath,"/withdrawal_facility_",format(Sys.time(), "%H-%M-%OS_%a_%b_%d_%Y"),".csv",sep=""), row.names = FALSE)
#####################################################################################################
# WITHDRAWAL SITE FILE
export_view <- "nwis_withdrawal_site_export"
output_filename <- "withdrawal_site.csv"
withdrawal_site <- from_vahydro(datasite,export_view,localpath,output_filename)

#REMOVE LEADING/TRAILING WHITESPACE FROM VAHYDRO SPATIAL CONTAINMENT COLUMNS
withdrawal_site$County <- trimws(withdrawal_site$County)
withdrawal_site$HUC_12 <- gsub("huc12_","",withdrawal_site$HUC_12)

write.csv(withdrawal_site,paste(localpath,"/withdrawal_site_",format(Sys.time(), "%H-%M-%OS_%a_%b_%d_%Y"),".csv",sep=""), row.names = FALSE)
#####################################################################################################
#####################################################################################################
#####################################################################################################
#####################################################################################################
# DISCHARGE FACILITY FILE
export_view <- "nwis_discharge_facility_export"
output_filename <- "discharge_facility.csv"
discharge_facility <- from_vahydro(datasite,export_view,localpath,output_filename)

#REMOVE LEADING/TRAILING WHITESPACE FROM VAHYDRO SPATIAL CONTAINMENT COLUMNS
discharge_facility$State <- trimws(discharge_facility$State)
discharge_facility$County <- trimws(discharge_facility$County)
#REMOVE LEADING "huc12_" FROM HUC_12 COLUMN
discharge_facility$HUC_12 <- gsub("huc12_","",discharge_facility$HUC_12)

write.csv(discharge_facility,paste(localpath,"/discharge_facility_",format(Sys.time(), "%H-%M-%OS_%a_%b_%d_%Y"),".csv",sep=""), row.names = FALSE)
#####################################################################################################
# DISCHARGE SITE FILE
export_view <- "nwis_discharge_site_export"
output_filename <- "discharge_site.csv"
discharge_site <- from_vahydro(datasite,export_view,localpath,output_filename)

#REMOVE LEADING/TRAILING WHITESPACE FROM VAHYDRO SPATIAL CONTAINMENT COLUMNS
discharge_site$County <- trimws(discharge_site$County)
discharge_site$HUC_12 <- gsub("huc12_","",discharge_site$HUC_12)

write.csv(discharge_site,paste(localpath,"/discharge_site_",format(Sys.time(), "%H-%M-%OS_%a_%b_%d_%Y"),".csv",sep=""), row.names = FALSE)

