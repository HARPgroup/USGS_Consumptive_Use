##################################################################################################################################
#-----------------------------------------------------ECHO_QAQC.R----------------------------------------------------------------#

# Primary Author: Morgan McCarthy, M.S. Biological Systems Engineering, Virginia Tech

##################################################################################################################################
#-------------------------------------------------------Purpose------------------------------------------------------------------#

# This script is intended to compare time series flows from CWA regulated facilities in the ECHO Database (ECHOInterface.R) to their respective 
# VPDES Individual Permits. The Virginia Pollution Discharge Elimination System (VPDES) is administered 
# by Virginia's Department of Environmental Quality (VDEQ). Specifically, we are looking at Individual Permits 
# for municipal and industrial discharging facilities. An excel spreadsheet listing active VPDES Individual 
# Permits is found on the VDEQ website (https://www.deq.virginia.gov/Programs/Water/PermittingCompliance/PollutionDischargeElimination/PermitsFees.aspx#GGPs).

# Other flagging methdos are demonstrated in this script as well to note suspicious reported effluents. 

##################################################################################################################################
#------------------------------------------------Load Library and Options--------------------------------------------------------#

library(foreign) #allows for easy manipulation of *.dbf file types
library(rgdal) #extract files from file geodatabases-like in ArcMap
library(dplyr)#data manipulation package that speeds up grouping, summarizing, ordering. 
library(XML) #downloads and reads XML file types-used to access ECHORest Services to download facilities
library(RCurl) #downloads and interacts with web-based content
library(readxl) #reads excel files
library(jsonlite)
library(httr)
library(proj4)
library(data.table)
library(stringr)
library(plotly)
library(shiny)
library(lubridate)
library(RColorBrewer)
library(tibble)
library(tm)

path<-"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated"
options(scipen=999) #Disable scientific notation

##################################################################################################################################
#------------------------------------------------ECHO Discharge Data-------------------------------------------------------------#

#-----------------------------------------#
#------------ECHO Facilities--------------#

#Utilizes CWA ECHO Water Facility Search REST Services to obtain all discharging facilities within state of interest.
#Attributes can be indicated in the query to obtain customized results: [Metadata Link](https://echo.epa.gov/tools/web-services/facility-search-water#!/Metadata/get_cwa_rest_services_metadata)

VA_Facility_Pull<- function(state){
  Req_URL<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&qcolumns=1,2,3,4,5,10,14,15,21,22,23,24,25,26,27,60,61,63,65,67,84,91,95,97,204,205,206,207,209,210,224&passthrough=Y&p_st=",state)
  URL_Download<-getURL(Req_URL) #Download URL from above
  URL_Parse<-xmlParse(URL_Download)#parses the downloaded XML of facilities and generates an R structure that represents the XML/HTML tree-main goal is to retrieve query ID or QID
  QID<-xmlToList(URL_Parse)#Converts parsed query to a more R-like list and stores it as a variable
  QID<-QID$QueryID
  GET_Facilities<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qcolumns=1,2,3,4,5,10,14,15,21,22,23,24,25,26,27,60,61,63,65,67,84,91,95,97,204,205,206,207,209,210,224&passthrough=Y&qid=",QID)
  ECHO_Facilities<-read.csv(GET_Facilities,stringsAsFactors = F) #Important to note this returns all facilities, active or not
  ECHO_Facilities$CWPName<-toupper(ECHO_Facilities$CWPName)

  ECHO_Facilities<-subset(ECHO_Facilities, subset=ECHO_Facilities$CWPPermitTypeDesc=="NPDES Individual Permit")
  assign("ECHO_Facilities",ECHO_Facilities,envir = .GlobalEnv)
  
}
VA_Facility_Pull("VA")

table(ECHO_Facilities$CWPPermitTypeDesc)

#-----------------------------------------#
#-------Time series ECHO DMR Data---------# 
# (Monthly, Daily, Weekly, Annual Average) 
# **Rare to see averages other than monthly average**
# From ECHO_Timeseries.R Script: Uses Effluent Charts REST Services#

ECHO_Discharge<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Documentation/Imports/ECHO_timeseries_3_28.txt",header=T, sep="\t")

##################################################################################################################################
#---------------------------------------------------VPDES Permit Data------------------------------------------------------------#

#This section of code retrieves the coordinates and flow limits for each point source discharging facility with an individual VPDES permit. 
#General permits are written for a general class of discharges, while individual permits are for municipal and industrial facilities with effluent limitations and monitoring requirements.
#Coordinates for each unique outfall is found in the VDEQ's VEGIS Database 
#Facility Design Flows are found on the VDEQ website under Permitting & Compliance 

VPDES_Permit_Pull<- function(){
  #-----------------------------------------#
  #--------VPDES Outfall Geodatabase--------#
  
  #VPDES Geodatabase:http://www.deq.virginia.gov/mapper_ext/GIS_Datasets/VPDES_Geodatabase.zip
  # Note: Takes time to download ~ 2 minutes
  
  temp<-tempfile(fileext = ".zip")
  #Locations and attribute data about active outfalls in the State
  download.file("http://www.deq.virginia.gov/mapper_ext/GIS_Datasets/VPDES_Geodatabase.zip", destfile = temp)
  unzip(temp, exdir = path)
  #Explore what is in VPDES_Geodatabase.gdb
  ogrListLayers(paste0(path,"/VPDES_Geodatabase.gdb")) #Two layers: VPDES Outfalls and OpenFileGDB
  VPDES_Outfalls<-as.data.frame(readOGR(paste0(path,"/VPDES_Geodatabase.gdb"),layer="VPDES_OUTFALLS"))
  names(VPDES_Outfalls)[names(VPDES_Outfalls)=="OUTFALL_ID"]<-'OutfallID'
  names(VPDES_Outfalls)[names(VPDES_Outfalls)=="VAP_PMT_NO"]<-'Facility.ID'
  names(ECHO_Facilities)[names(ECHO_Facilities)=="SourceID"]<-"Facility.ID"#Need to rename to give a central columnn name for future joins
  
  #-----------------------------------------#
  #-----VPDES Individual Permit Database----#
  
  #Permits Website:http://www.deq.virginia.gov/Programs/Water/PermittingCompliance/PollutionDischargeElimination/PermitsFees.aspx#GGPs
  
  #Individual Permits updated as of January 2019--look for updates every few months
  #Warnings about unknown or uninitiliased columns: previous IP contact sheets named the columns differently. It doesn't hinder any processes though. 
  GET('https://www.deq.virginia.gov/Portals/0/DEQ/Water/PollutionDischargeElimination/VPDES%20Spreadsheets/VPDES%20IP%20Contact%20Flow%20for%20WEB%20Jan%202019.xlsx?ver=2019-01-23-151510-490', 
      write_disk(temp <- tempfile(fileext = ".xlsx")))
  VPDES_IP <- read_excel(temp)
  VPDES_IP<-VPDES_IP[!is.na(VPDES_IP$Facility),]
  VPDES_IP$`Design Flow (MGD)`<-as.numeric(VPDES_IP$`Design Flow (MGD)`)
  VPDES_IP<-VPDES_IP[!duplicated(VPDES_IP$`Permit Number`),] #getting rid of duplicates and looking at unique permits
  
  #It's important to note that the individual permit information just includes design and total flow
  #Summarize the information within the VPDES IP Spreadsheet
  VPDES_IP%>%
    dplyr::summarise(Sum_DesFlow=sum(`Design Flow (MGD)`,na.rm = T),Max_DesFlow=max(`Design Flow (MGD)`,na.rm = T),
                     ZeroCount_DesFlow=dplyr::count(VPDES_IP%>%dplyr::filter(`Design Flow (MGD)`==0)),
                     NACount_DesFlow=length(VPDES_IP$`Design Flow (MGD)`[is.na(VPDES_IP$`Design Flow (MGD)`)]),
                     Sum_TotalFlow=sum(`Total Flow`,na.rm = T),Max_TotalFlow=max(`Total Flow`,na.rm = T),
                     NACount_TotalFlow=length(VPDES_IP$`Total Flow`[is.na(VPDES_IP$`Total Flow`)])) #24 facilities in VPDES IP spreadsheet report having a design flow of 0 MGD, 2 NAs
  
  #-----------------------------------------#
  #-----VPDES General Permit Database-------#
  # GET('https://www.deq.virginia.gov/Portals/0/DEQ/Water/PollutionDischargeElimination/VPDES%20Spreadsheets/VPDES%20General%20Permits%20July%202018%20(3).xls?ver=2018-07-24-133254-740', 
  #     write_disk(temp <- tempfile(fileext = ".xls")))
  # 
  # VPDES_GP<-loadWorkbook(temp)
  # sheet_names<-getSheets(VPDES_GP)
  # names(sheet_names)<-sheet_names
  # VPDES_GP<-lapply(sheet_names, function(.sheet){readWorksheet(object=VPDES_GP, .sheet)})
  # rm(sheet_names)
  # 
  # #Have a lot of general permitted facilities starting with VAN: which are the nutrient trading GP
  # VPDES_GP<-as.data.frame(VPDES_GP$`Nutrient GP`)
  # VPDES_GP<-VPDES_GP[rowSums(is.na(VPDES_GP))==0,]
  # colnames(VPDES_GP)=VPDES_GP[1,]
  # VPDES_GP <- VPDES_GP[-c(1, 2, 3), ]
  
  
  #############################################################################################################################
  ###############################################Coordinates for Outfalls######################################################
  
  #Set projections for VPDES Outfall coordinates#
  VPDES_Coordinates<-VPDES_Outfalls[,c(15,16)]
  VPDES_Coordinates <- proj4::project(VPDES_Coordinates, proj="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", inverse=TRUE)
  
  #Replace coordinates in VPDES_Outfalls data frame
  VPDES_Outfalls$Longitude<-VPDES_Coordinates$x
  VPDES_Outfalls$Latitude<-VPDES_Coordinates$y
  
  #Have seperate data frame with VPDESID and coordinates
  VPDES_Coordinates<-subset(VPDES_Outfalls, select=c(1,17,18))
  colnames(VPDES_Coordinates)<-c("OutfallID","Outfall_Longitude","Outfall_Latitude")
  
  #Attach Outfall Coordinates to ECHO_Discharge 
  ECHO_Discharge$OutfallID<-gsub("echo_","", as.character(ECHO_Discharge$OutfallID))
  #ECHO_Discharge_c<-merge.data.frame(ECHO_Discharge,VPDES_Coordinates,by="OutfallID") #Removes outfalls if there is no match present
  ECHO_Discharge<-merge.data.frame(ECHO_Discharge,VPDES_Coordinates,by="OutfallID",all.x=T) #Keeps all outfalls even if doesn't match in VPDES IP
  
  #Attach Facility Coordinates to ECHO_Discharge
  ECHO_Facilities_Coordinates<-subset(ECHO_Facilities, select = c(2,11,12))
  colnames(ECHO_Facilities_Coordinates)<-c("Facility.ID","Facility_Latitude","Facility_Longitude")
  ECHO_Discharge<-merge.data.frame(ECHO_Discharge,ECHO_Facilities_Coordinates,by="Facility.ID",all.x=T)
  
  ECHO_Discharge$Outfall_Longitude<-ifelse(is.na(ECHO_Discharge$Outfall_Longitude),ECHO_Discharge$Facility_Longitude,ECHO_Discharge$Outfall_Longitude)
  ECHO_Discharge$Outfall_Latitude<-ifelse(is.na(ECHO_Discharge$Outfall_Latitude),ECHO_Discharge$Facility_Latitude,ECHO_Discharge$Outfall_Latitude)
  
  assign("ECHO_Discharge",ECHO_Discharge,envir = .GlobalEnv)
  assign("VPDES_IP",VPDES_IP,envir = .GlobalEnv)
}
VPDES_Permit_Pull()

outfalls<-ECHO_Discharge%>%dplyr::group_by(OutfallID)%>%dplyr::summarise(Type=first(Outfall_Type))
table(outfalls$Type)
#############################################################################################################################
#------------------------------------------------Flagging-------------------------------------------------------------------#

# This section of the code flags anamolous discharge entries provided by ECHO.
# Flagging is based on four criteria:

# 1: If the Facility reports a design flow of 0 MGD
    # Why does this matter? Because if the facility is reporting discharge, it's definitely designed to handle more than 0 MGD

# 2: If the DMR entry exceeds 100*the median outfall discharge for a given year
    # Why does this matter? It may indicate a unit conversion error

# 3: If the DMR entry exceeds 106*the median outfall discharge for a given year
    # Why does this matter? It may indicate a unit conversion error

# 4: If the DMR entry exceeds the facility's design flow (if df > 0)
    # Why does this matter? It may indicate a unit conversion error and shows the facility is discharging more than it can theoretically handle

DMR_flagging<- function(ECHO_Discharge){
  #-----------------------------------------------------------------#
  #-------------------Design Flow = 0-------------------------------#
  
  
  VPDES_DesignFlow<-subset(VPDES_IP,select=c(3,15))
  colnames(VPDES_DesignFlow)<-c("Facility.ID","DesignFlow_mgd")
  VPDES_DesignFlow<-arrange(VPDES_DesignFlow,Facility.ID)
  ECHO_Discharge<-merge.data.frame(ECHO_Discharge,VPDES_DesignFlow,by="Facility.ID",all.x=T)
  
  ECHO_Discharge<-subset(ECHO_Discharge,select=c(1,3,17,18,2,5,15,16,4,6,7,8,19,9,10,11,12,13))
  
  ECHO_Discharge$fac_flag_zerodesflow<-ifelse(ECHO_Discharge$DesignFlow_mgd==0,"fac_flag_zerodesflow",NA)
  
  #-----------------------------------------------------------------#
  #-------ECHO Measured Effluents > VPDES Design flow---------------#
  
  #Flag facilities that report measured effluent greater than the design flow 
  ECHO_Discharge$dmr_flag_desflow<-ifelse(ECHO_Discharge$Measured_Effluent>ECHO_Discharge$DesignFlow_mgd 
                                          & ECHO_Discharge$DesignFlow_mgd>0,"dmr_flag_desflow",NA)
  
  #-----------------------------------------------------------------#
  #------Measured Effluents > 100*Median Measured Effluent----------#
  #------Measured Effluents > 100,000*Median Measured Effluent------#
  #---------------Potential Unit Conversion Error-------------------#
  
  ECHO_Discharge$MP_Begin_Date<-as.Date(ECHO_Discharge$MP_Begin_Date,format="%Y-%m-%d")
  ECHO_Discharge$MP_End_Date<-as.Date(ECHO_Discharge$MP_End_Date,format="%Y-%m-%d")
  ECHO_Discharge<-ECHO_Discharge[order(ECHO_Discharge[,5],ECHO_Discharge[,13],decreasing=F),]
  ECHO_Discharge<-ECHO_Discharge%>%add_column(Year=substr(ECHO_Discharge$MP_Begin_Date,1,4), .before="MP_Begin_Date")
  
  #Summmarize measured effluent values from ECHO by OutfallID--Not by Facility#
  ECHO_Discharge_Summary<-ECHO_Discharge%>%
    dplyr::group_by(OutfallID,Year)%>% #important to note that we are summarizing discharge by outfall and year here 
    dplyr::summarise(Mean_ME=mean(as.numeric(Measured_Effluent), na.rm = T),
                     Median_ME=median(as.numeric(Measured_Effluent), na.rm = T),
                     Std_ME=sd(as.numeric(Measured_Effluent), na.rm = T))
  
  ECHO_Discharge<-merge(ECHO_Discharge,ECHO_Discharge_Summary,by=c("OutfallID","Year"),all.x=T)
  
  ECHO_Discharge$dmr_flag_units_100<-ifelse(ECHO_Discharge$Measured_Effluent>100*ECHO_Discharge$Median_ME 
                                            & ECHO_Discharge$Median_ME>0 ,"dmr_flag_units_100",NA)
  ECHO_Discharge$dmr_flag_units_1000000<-ifelse(ECHO_Discharge$Measured_Effluent>1000000*ECHO_Discharge$Median_ME 
                                            & ECHO_Discharge$Median_ME>0 ,"dmr_flag_units_1000000",NA)
  
  ECHO_Discharge<-ECHO_Discharge[-c(2)]
  
  #-----------------------------------------------------------------#
  #----------------------ECHO System Flags--------------------------#
  #The Effluent Charts located on the ECHO RESt Services present effluent limits, releases, and violations over time for the CWA wastewater discharge permits issued by NPDES.
  
  #There are codes that describe the type of alleged effluent or monitoring/reporting violation:
  #E90 - effluent violation
  #D90 - DMR overdue, with a numeric Limit
  #D80 - DMR Overdue, monitoring only required
  
  #Included under: Violation Code
  
  #-----------------------------------------------------------------#
  #--------------Format DMR Dataframe with Flags--------------------#
  ECHO_Discharge<-ECHO_Discharge[,c(2,3,4,5,1,6,7,8,10,21,22,23,12,11,13,14,15,16,17,18,19,20,24,25)]
  
  #-----------------------------------------------------------------#
  #------------Subset Flagged Entries into Dataframe----------------#
  # Flagged_DMR<-subset(ECHO_Discharge,subset= (!is.na(ECHO_Discharge$fac_flag_zerodesflow))|
  #                 (!is.na(ECHO_Discharge$dmr_flag_desflow))|
  #                 (!is.na(ECHO_Discharge$dmr_flag_units_100))|
  #                 (!is.na(ECHO_Discharge$dmr_flag_units_1000000))|
  #                 (!(ECHO_Discharge$Violation_Code=="")))
  # 
  # Overlapping_Flags<-subset(Flagged_DMR, subset= !is.na(Flagged_DMR$dmr_flag_units_100) & !(Flagged_DMR$Violation_Code=="")|
  #                             !is.na(Flagged_DMR$dmr_flag_units_100) & !is.na(Flagged_DMR$fac_flag_zerodesflow)|
  #                             !is.na(Flagged_DMR$dmr_flag_units_100) &!is.na(Flagged_DMR$dmr_flag_desflow)|
  #                             !is.na(Flagged_DMR$dmr_flag_units_1000000) & !(Flagged_DMR$Violation_Code=="")|
  #                             !is.na(Flagged_DMR$dmr_flag_units_1000000) & !is.na(Flagged_DMR$fac_flag_zerodesflow)|
  #                             !is.na(Flagged_DMR$dmr_flag_units_1000000) &!is.na(Flagged_DMR$dmr_flag_desflow)|
  #                             !(Flagged_DMR$Violation_Code=="") & !is.na(Flagged_DMR$fac_flag_zerodesflow)|
  #                             !(Flagged_DMR$Violation_Code=="") & !is.na(Flagged_DMR$dmr_flag_desflow)|
  #                             !is.na(Flagged_DMR$fac_flag_zerodesflow) & !is.na(Flagged_DMR$dmr_flag_desflow)|
  #                             !is.na(Flagged_DMR$dmr_flag_units_100)&!(Flagged_DMR$Violation_Code=="")&!is.na(Flagged_DMR$fac_flag_zerodesflow)&!is.na(Flagged_DMR$dmr_flag_desflow)&!is.na(Flagged_DMR$dmr_flag_units_1000000)|
  #                             !is.na(Flagged_DMR$dmr_flag_units_100)&!(Flagged_DMR$Violation_Code=="")&!is.na(Flagged_DMR$fac_flag_zerodesflow)|
  #                             !is.na(Flagged_DMR$dmr_flag_units_100)&!is.na(Flagged_DMR$fac_flag_zerodesflow)&!is.na(Flagged_DMR$dmr_flag_desflow)|
  #                             !is.na(Flagged_DMR$dmr_flag_units_100)&!(Flagged_DMR$Violation_Code=="")&!is.na(Flagged_DMR$dmr_flag_desflow)|
  #                             !is.na(Flagged_DMR$dmr_flag_units_1000000)&!(Flagged_DMR$Violation_Code=="")&!is.na(Flagged_DMR$fac_flag_zerodesflow)|
  #                             !is.na(Flagged_DMR$dmr_flag_units_1000000)&!is.na(Flagged_DMR$fac_flag_zerodesflow)&!is.na(Flagged_DMR$dmr_flag_desflow)|
  #                             !is.na(Flagged_DMR$dmr_flag_units_1000000)&!(Flagged_DMR$Violation_Code=="")&!is.na(Flagged_DMR$dmr_flag_desflow))
  # 
  
  assign("ECHO_Discharge",ECHO_Discharge,envir=.GlobalEnv)
}
DMR_flagging(ECHO_Discharge)

##################################################################################################################################
#---------------------------------------Trim and Attach Relevant Attributes------------------------------------------------------#
# Trim data to time period of choice and append relevatn attributes that will help describe water movement in teh state. 

attributes<- function(){
  #-----------------------------------------------------------------#
  #---------------Trim to Match VWUDS: 2010-2016--------------------#
  
  ECHO_2010_2017<-ECHO_Discharge
  ECHO_2010_2017<-ECHO_2010_2017[ECHO_2010_2017$MP_End_Date %within% interval("2010-01-01","2016-12-31"),]
  
  #-----------------------------------------------------------------#
  #------------------Attach Relevant Attributes---------------------#
  
  # Find default classified water sector using VPDES permit information
  Use_Type<-subset(VPDES_IP,select=c(3,7))
  colnames(Use_Type)<-c("Facility.ID","Use_Type")
  ECHO_2010_2017<-merge(ECHO_2010_2017,Use_Type,by="Facility.ID",all.x=T)
  
  # Select relevant facility attributes to help during classification of water sector and 
  # get better understanding of data
  ECHO_Facility_Attributes<-subset(ECHO_Facilities,select=c(2,4,5,6,7,8,9,15,16,17,18,19,20,29))
  colnames(ECHO_Facility_Attributes)<-c("Facility.ID","City","State","County","HUC7","FIPS","SIC","Facility_Type","Permit_Status","Issuing_Agency","Permit_Type","Permit_Effective_Date","Permit_Expiration_Date","Waterbody_GNIS_Name")
  ECHO_2010_2017<-merge(ECHO_2010_2017,ECHO_Facility_Attributes,by="Facility.ID",all.x=T)
  
  assign("ECHO_2010_2017",ECHO_2010_2017,envir = .GlobalEnv)
  
}
attributes()


n_distinct(ECHO_2010_2017$Facility.ID)
n_distinct(ECHO_2010_2017$OutfallID)
##################################################################################################################################
########################################Classification of Water/Economic Sector###################################################

#-----------------------------------------------------------------#
#-------------------Investigate Facility Type---------------------#

#---ECHO Defined Facility Type---#
# Sectors defined as either POTW, NON-POTW, or Federal
table(ECHO_2010_2017$Facility_Type)

#---VPDES Defined Facility Type---#
# Default classes are either municipal, industrial, or NA, which is not realistic
table(ECHO_2010_2017$Use_Type)

 
Fac_Type_VPDES<-ECHO_2010_2017%>%
  dplyr::group_by(Facility.ID)%>% 
  dplyr::summarise(Facility_Name=first(FacilityName),IssuingAgency=first(Issuing_Agency),
            PermitType=first(Permit_Type),City=first(City),ECHO_Use_Type=first(Facility_Type), 
            VPDES_Use_Type=first(Use_Type), SIC=first(SIC))



#------Remove Hydropower Facilities-------#
remove_hydropower<- function(){
  #------Remove Hydropower Facilities-------#
  
  # Why? regulations changed around 2010 and now they report to another source. They are considered less consumptive
  
  #--Find Hydropower--#
  Fac_Type_VPDES$Hydropower<-NA
  Fac_Type_VPDES$Hydropower<-as.character(Fac_Type_VPDES$Hydropower)
  for (i in 1:length(Fac_Type_VPDES$Facility.ID)){
    if(length(grep('HYDRO',Fac_Type_VPDES$Facility_Name[i]))>0){
      Fac_Type_VPDES$Hydropower[i]<-'hydro'
    }
  }
  #Bath County Power Station is also hydropower
  
  
  
  Fac_Type_VPDES$Hydropower[Fac_Type_VPDES$Facility.ID=="VA0053317"]<-'hydro'
  
  Fac_Type_VPDES<-Fac_Type_VPDES[!(Fac_Type_VPDES$Facility.ID=="VA0087084"|
                                     Fac_Type_VPDES$Facility.ID=="VA0087092"|
                                     Fac_Type_VPDES$Facility.ID=="VA0087106"|
                                     Fac_Type_VPDES$Facility.ID=="VA0087114"|
                                     Fac_Type_VPDES$Facility.ID=="VA0088765"|
                                     Fac_Type_VPDES$Facility.ID=="VA0090310"|
                                     Fac_Type_VPDES$Facility.ID=="VA0092738"|
                                     Fac_Type_VPDES$Facility.ID=="VA0053317"),]
  
  ECHO_2010_2017[ECHO_2010_2017$Facility.ID=="VA0087084"|
                     ECHO_2010_2017$Facility.ID=="VA0087092"|
                     ECHO_2010_2017$Facility.ID=="VA0087106"|
                     ECHO_2010_2017$Facility.ID=="VA0087114"|
                     ECHO_2010_2017$Facility.ID=="VA0088765"|
                     ECHO_2010_2017$Facility.ID=="VA0090310"|
                     ECHO_2010_2017$Facility.ID=="VA0092738"|
                     ECHO_2010_2017$Facility.ID=="VA0053317",]%>%
    dplyr::group_by(Year= substr(MP_Begin_Date,1,4))%>%
    dplyr::summarise(no_Facilities=n_distinct(Facility.ID),no_sources=n_distinct(OutfallID), entries=n(),
                     Sum_Discharges=sum(Measured_Effluent)/12,Mean_Withdrawal=mean(Measured_Effluent),
                     Median_Withdrawal=median((Measured_Effluent)))
  
  
  ECHO_2010_2017<-ECHO_2010_2017[!(ECHO_2010_2017$Facility.ID=="VA0087084"|
                                     ECHO_2010_2017$Facility.ID=="VA0087092"|
                                     ECHO_2010_2017$Facility.ID=="VA0087106"|
                                     ECHO_2010_2017$Facility.ID=="VA0087114"|
                                     ECHO_2010_2017$Facility.ID=="VA0088765"|
                                     ECHO_2010_2017$Facility.ID=="VA0090310"|
                                     ECHO_2010_2017$Facility.ID=="VA0092738"|
                                     ECHO_2010_2017$Facility.ID=="VA0053317"),]
  
  Fac_Type_VPDES<-Fac_Type_VPDES[-c(9)]
  
  assign("ECHO_2010_2017",ECHO_2010_2017,envir = .GlobalEnv)
  assign("Fac_Type_VPDES",Fac_Type_VPDES,envir = .GlobalEnv)
  
}
remove_hydropower()

table(Fac_Type_VPDES$VPDES_Use_Type)
#-----------------------------------Classification-------------------------------------------#
# Reclassify Water Use Sector first based on SIC code and then based on text mining techniques observed in VWUDS_QAQC.R script. 
# The rules below are ordered and were iteratively changed to reduce training error


#--------SIC Code Classification----------#
# Standard Industrial Classification (SIC) Codes can give us a better idea of what sector
# each facility belongs to

SIC_Code<- function(){
  # https://siccode.com/
  
  Fac_Type_VPDES$Reclass_Use_Type<-NA
  Fac_Type_VPDES$SIC<-str_pad(Fac_Type_VPDES$SIC, 4, pad="0")
  for(i in 1:length(Fac_Type_VPDES$Facility.ID)){
    paste0("Facility: ",i,"out of ",length(Fac_Type_VPDES$Facility.ID))
    #-----------Agriculture SIC Codes----------------#
    if(is.na(Fac_Type_VPDES$SIC[i])){
      Fac_Type_VPDES$Reclass_Use_Type[i]<-NA
      
    }else if(substr(Fac_Type_VPDES$SIC[i],0,4)=="0182")
    {Fac_Type_VPDES$Reclass_Use_Type[i]<-'Commercial'
      
    }else if(substr(Fac_Type_VPDES$SIC[i],0,2)=="01"| # Agricultural Production - Crops
       substr(Fac_Type_VPDES$SIC[i],0,2)=="07"| # Agricultural Services
       substr(Fac_Type_VPDES$SIC[i],0,2)=="08"| # Forestry
       substr(Fac_Type_VPDES$SIC[i],0,4)=="4971") # Irrigation Systems
      { 
      Fac_Type_VPDES$Reclass_Use_Type[i]<-'Agriculture/Irrigation'
      
    }else if(substr(Fac_Type_VPDES$SIC[i],0,2)=="09"|# Fishing, Hunting and Trapping
             substr(Fac_Type_VPDES$SIC[i],0,4)=="0273") #animal aquaculture
      {
      Fac_Type_VPDES$Reclass_Use_Type[i]<-'Aquaculture'
    #-----------Commercial SIC Codes----------------#
    }else if(substr(Fac_Type_VPDES$SIC[i],0,2)=="40"| # Railroad Transportation
             substr(Fac_Type_VPDES$SIC[i],0,2)=="41"| # Local and Suburban Transit and Interurban High Passenger
             substr(Fac_Type_VPDES$SIC[i],0,2)=="42"| # Motor Freight Transportation and Warehousing
             substr(Fac_Type_VPDES$SIC[i],0,2)=="43"| # United States Postal Service
             substr(Fac_Type_VPDES$SIC[i],0,2)=="44"| # Water Transportation
             substr(Fac_Type_VPDES$SIC[i],0,2)=="45"| # Transportation by Air
             substr(Fac_Type_VPDES$SIC[i],0,2)=="47"| # Transportation Services
             substr(Fac_Type_VPDES$SIC[i],0,2)=="48"| # Communication
             substr(Fac_Type_VPDES$SIC[i],0,4)=="4961"| # Steam and Air-Conditioning Supply
             substr(Fac_Type_VPDES$SIC[i],0,4)=="4959"| # Sanitary Services, Not Elsewhere Classified
             substr(Fac_Type_VPDES$SIC[i],0,2)=="50"| # Wholesale Trade-Durable Goods
             substr(Fac_Type_VPDES$SIC[i],0,2)=="51"| # Wholesale Trade-Nondurable Goods
             substr(Fac_Type_VPDES$SIC[i],0,2)=="52"| # Building materials, hardware, garden supply
             substr(Fac_Type_VPDES$SIC[i],0,2)=="53"| # general merchandise stores
             substr(Fac_Type_VPDES$SIC[i],0,2)=="54"| # Food Stores
             substr(Fac_Type_VPDES$SIC[i],0,2)=="55"| # Automotive Dealers and Gasoline Service Stations
             substr(Fac_Type_VPDES$SIC[i],0,2)=="56"| # Apparel and Accessory Stores
             substr(Fac_Type_VPDES$SIC[i],0,2)=="57"| # Home furniture, furnishings, and equipment stores
             substr(Fac_Type_VPDES$SIC[i],0,2)=="58"| # Eating and Drinking places
             substr(Fac_Type_VPDES$SIC[i],0,2)=="59"| # Misc Retail
             substr(Fac_Type_VPDES$SIC[i],0,2)=="60"| # Depository Instituitions
             substr(Fac_Type_VPDES$SIC[i],0,2)=="61"| # Non-Depository Credit Instituitions
             substr(Fac_Type_VPDES$SIC[i],0,2)=="62"| # Security and Commodity Brokers
             substr(Fac_Type_VPDES$SIC[i],0,2)=="63"| # Insurance Carriers
             substr(Fac_Type_VPDES$SIC[i],0,2)=="64"| # Insurance Agents, Brokers and Service
             substr(Fac_Type_VPDES$SIC[i],0,2)=="65"| # Real Estate
             substr(Fac_Type_VPDES$SIC[i],0,2)=="67"| # Holding and other investment offices
             substr(Fac_Type_VPDES$SIC[i],0,2)=="70"| # Hotels, camps, houses
             substr(Fac_Type_VPDES$SIC[i],0,2)=="72"| # personal services (includes laundry, barber, studios, etc.)
             substr(Fac_Type_VPDES$SIC[i],0,2)=="73"| # Business Services
             substr(Fac_Type_VPDES$SIC[i],0,2)=="75"| # Automotive Repair
             substr(Fac_Type_VPDES$SIC[i],0,2)=="76"| # Misc Repair
             substr(Fac_Type_VPDES$SIC[i],0,2)=="78"| # Motion Pictures
             substr(Fac_Type_VPDES$SIC[i],0,2)=="79"| # Amusement and Recreation Services
             substr(Fac_Type_VPDES$SIC[i],0,2)=="80"| # Health Services
             substr(Fac_Type_VPDES$SIC[i],0,2)=="81"| # Legal Services
             substr(Fac_Type_VPDES$SIC[i],0,2)=="82"| # Educational Services
             substr(Fac_Type_VPDES$SIC[i],0,2)=="83"| # Social Services
             substr(Fac_Type_VPDES$SIC[i],0,2)=="84"| # Museums, Art Galleries, Botanical
             substr(Fac_Type_VPDES$SIC[i],0,2)=="86"| # Membership Organizations
             substr(Fac_Type_VPDES$SIC[i],0,2)=="87"| # Engineering, Accounting, Research
             substr(Fac_Type_VPDES$SIC[i],0,2)=="91"| # Exectuive, Legilative, and General Government
             substr(Fac_Type_VPDES$SIC[i],0,2)=="92"| # Justice, Public Order, and Safety
             substr(Fac_Type_VPDES$SIC[i],0,2)=="93"| # Public Finance, Taxation, and Monetary
             substr(Fac_Type_VPDES$SIC[i],0,2)=="94"| # Administration of HR
             substr(Fac_Type_VPDES$SIC[i],0,2)=="95"| # Administration of Environmental Quality
             substr(Fac_Type_VPDES$SIC[i],0,2)=="96"| # Administration fo Economic Programs
             substr(Fac_Type_VPDES$SIC[i],0,2)=="97"| # National Security
             substr(Fac_Type_VPDES$SIC[i],0,4)=="0279"| # Animal testing
             substr(Fac_Type_VPDES$SIC[i],0,2)=="99") # Nonclassifiable Establishments
             {  
      Fac_Type_VPDES$Reclass_Use_Type[i]<-'Commercial'
      
    #------------Energy SIC Codes------------------#
    }else if(substr(Fac_Type_VPDES$SIC[i],0,3)=="491"| # Electric Serves: engaged in generation, transmission, and/or distribution of electric energy for sale
             substr(Fac_Type_VPDES$SIC[i],0,3)=="493" # Combination Electric and Gas, and other Utility
    ){
      Fac_Type_VPDES$Reclass_Use_Type[i]<-'Energy'
      
    #------------Industrial SIC Codes------------------#
    }else if(substr(Fac_Type_VPDES$SIC[i],0,2)=="10"| # Metal Mining
             substr(Fac_Type_VPDES$SIC[i],0,2)=="12"| # Bituminous Coal and Lignite Mining
             substr(Fac_Type_VPDES$SIC[i],0,2)=="13"| # Oil and Gas Extraction
             substr(Fac_Type_VPDES$SIC[i],0,2)=="14"| # Mining and Quarrying of Nonmetallic Minerals, Except Fuels
             substr(Fac_Type_VPDES$SIC[i],0,2)=="15"| # Building Construction General Contractors and Operative Builders
             substr(Fac_Type_VPDES$SIC[i],0,2)=="16"| # Heavy Construction other than Building Construction Contractors
             substr(Fac_Type_VPDES$SIC[i],0,2)=="17"| # Construction Special Trade Contractors
             substr(Fac_Type_VPDES$SIC[i],0,2)=="20"| # Food and Kindred Products
             substr(Fac_Type_VPDES$SIC[i],0,2)=="21"| # Tobacco Products
             substr(Fac_Type_VPDES$SIC[i],0,2)=="22"| # Textile Mill Products
             substr(Fac_Type_VPDES$SIC[i],0,2)=="23"| # Apparel and other finished products from fabrics
             substr(Fac_Type_VPDES$SIC[i],0,2)=="24"| # Lumber and Wood Products, except furtinure
             substr(Fac_Type_VPDES$SIC[i],0,2)=="25"| # Furniture and Fixtures
             substr(Fac_Type_VPDES$SIC[i],0,2)=="26"| # Paper and Allied Products
             substr(Fac_Type_VPDES$SIC[i],0,2)=="27"| # Printing, Publishing, and Allied Industries
             substr(Fac_Type_VPDES$SIC[i],0,2)=="28"| # Chemical and Allied Products
             substr(Fac_Type_VPDES$SIC[i],0,2)=="29"| # Petroleum Refining and Related Industries
             substr(Fac_Type_VPDES$SIC[i],0,2)=="30"| # Rubber and Misc. Plastics Products
             substr(Fac_Type_VPDES$SIC[i],0,2)=="31"| # Leather and Leather Products
             substr(Fac_Type_VPDES$SIC[i],0,2)=="32"| # Stone, Clay, Glass, and Concrete Products
             substr(Fac_Type_VPDES$SIC[i],0,2)=="33"| # Primary Metal Industries
             substr(Fac_Type_VPDES$SIC[i],0,2)=="34"| # Fabricated Metal Products
             substr(Fac_Type_VPDES$SIC[i],0,2)=="35"| # Industrial and Commercial Machinery 
             substr(Fac_Type_VPDES$SIC[i],0,2)=="36"| # Electronic and other Electrical Equipment and Components
             substr(Fac_Type_VPDES$SIC[i],0,2)=="37"| # Transportation Equipment
             substr(Fac_Type_VPDES$SIC[i],0,2)=="38"| # Measuring, Analyzing, and Controlling Instruments
             substr(Fac_Type_VPDES$SIC[i],0,2)=="39"| # Misc Manufacturing Industries
             substr(Fac_Type_VPDES$SIC[i],0,2)=="46"| # Pipelines, except Natural Gas
             substr(Fac_Type_VPDES$SIC[i],0,3)=="492"| # Gas Production and Distribution
             substr(Fac_Type_VPDES$SIC[i],0,4)=="4953" # Refuse Systems - industrial waste landfills
             ){
      Fac_Type_VPDES$Reclass_Use_Type[i]<-'Industrial'
 
      #------------Municipal SIC Codes------------------#
    }else if(substr(Fac_Type_VPDES$SIC[i],0,3)=="494"| # Water Supply - domestic, commercial, and industrial use
             substr(Fac_Type_VPDES$SIC[i],0,4)=="4959" # Sanitary Services
             
             ){
      Fac_Type_VPDES$Reclass_Use_Type[i]<-'Municipal'
    }else{
      Fac_Type_VPDES$Reclass_Use_Type[i]<-NA
    }
  }
  assign("Fac_Type_VPDES",Fac_Type_VPDES,envir = .GlobalEnv)
}
SIC_Code()

#-----Fuel Type Classification for Energy Facilities-------#
Fuel_Type_func<- function(){
  Fuel_Type<-read.csv("G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/Energy_Fuel_Types.csv",header=T)
  Fac_Type_VPDES$Fuel_Type<-(Fuel_Type$Fuel.Type[match(Fac_Type_VPDES$Facility.ID,Fuel_Type$VPDES)])
  Fac_Type_VPDES$Gen_Cap_MW<-(Fuel_Type$Net.Generating.Capacity..MW.[match(Fac_Type_VPDES$Facility.ID,Fuel_Type$VPDES)])
  assign("Fac_Type_VPDES",Fac_Type_VPDES,envir = .GlobalEnv)
}
Fuel_Type_func()
#-----Rule Based Classification-------#

Rule_Based<- function(){
  for (i in 1:length(Fac_Type_VPDES$Facility.ID)){
    if(is.na(Fac_Type_VPDES$Reclass_Use_Type[i])){
    if(length(grep('\\bPOWER\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
       length(grep('\\bPOWER STATION\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
       length(grep('\\bCOMBUSTION\\b',Fac_Type_VPDES$Facility_Name[i]))>0){
      Fac_Type_VPDES$Reclass_Use_Type[i]<-'Energy'
    }else if (length(grep('\\bMUNICIPAL\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bREVERSE OSMOSIS\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bSERVICE AREA\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bSERV AREA\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bREGIONAL WATER SYSTEM\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWASTE WATER\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWWTP\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWWTF\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWASTEWATER\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWASTEWATER Facility\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWT PLANT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bSERV AUTH\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWATER TREATMENT PLANT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWASTEWATER TREATMENT PLANT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bSEWAGE TREATMENT PLAN\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bSEWAGE TREATMEN\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bSEWAGE TREAT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bSEWAGE TREATMENT PLANT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bSEWAGE\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bSANITATION\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bSANITARY\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWATER RECLAMATION\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bPUMPING\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWTF\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWATER WORKS\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWATER UTILITIES\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bUTILITIES\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWWTREAT PLANT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bTRICKLING FILTER\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bFILTRATION PLANT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bPARK WATER SYSTEM\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWATER TREATMEN\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bWATER TREATMENT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bPOLLUTION CONTROL\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bPOLLUTION CONTR\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bCENTRAL SYSTEM\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bCOMBINED SEW SYSTEM\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bCOMBINED SEWER SYSTEM\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bMS4\\b",Fac_Type_VPDES$Facility_Name[i]))>0| #Municipal Separate Strom Sewer System
              length(grep("\\bTRAILER\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bMOBILE HOME\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bTRACT\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCOMMUNITY\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bHOMES\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bAPARTMENTS\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bSUBDIVISION\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bESTATES\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bESTATE\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bAUTHORITY\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCITY OF\\b",Fac_Type_VPDES$Facility_Name[i]))>0){
      Fac_Type_VPDES$Reclass_Use_Type[i]<-'Municipal'
    }else if (length(grep('\\bAIRPORT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep('\\bARMY\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bGOLF COURSE\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCOUNTRY CLUB\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCOUNTRY CLB\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCLUB\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bGOLF\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCOURSE\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCHURCH\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCENTER\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCOMPLEX\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bHEALTHCARE\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bSCHOOL\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bSCHOOLS\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bBEHAVIORAL\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCONFERENCE CENTER\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bRECREATION\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bLEARNING CENTER\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bELEMENTARY\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bINSTITUTE\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCOURTHOUSE\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bNAVAL\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bSPACE FLIGHT CENTER\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bEDUCATIONAL\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCEMETERY\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bREST AREA\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bRENTALS\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bINN\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bMUSEUM\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bBUILDING\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bUNIVERSITY\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bHOSPITAL\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bRESTAURANT\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCORRECTION CENTER\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bTRAINING CENTER\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bDETENTION CENTER\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCORRECTIONAL\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bREHABILITATION\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCAMPGROUND\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCORRECTION UNIT\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bTRAVEL CENTER\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bSTATE PARK\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bDEPARTMENT OF LABOR\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bRESORT\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bYMCA\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCOOPERATIVE\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bBUSCH GARDENS\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bRETREAT\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
              length(grep("\\bCAR WASH\\b",Fac_Type_VPDES$Facility_Name[i]))>0){
      Fac_Type_VPDES$Reclass_Use_Type[i]<-'Commercial'
    } else if (length(grep('\\bFARM\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bORNAMENTALS\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bIRRIGATION\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bPRODUCE\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bLAWN\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bCENTER PIVOT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bHOG\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bDAIRY\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bORCHARD\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bNURSERY\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bNURSERIES\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bVINEYARD\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep("\\bGREENHOUSE\\b",Fac_Type_VPDES$Facility_Name[i]))>0){
      Fac_Type_VPDES$Reclass_Use_Type[i]<-'Agriculture/Irrigation'
    } else if (length(grep("\\bFISHERIES\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep("\\bFISH\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep("\\bHATCHERY\\b",Fac_Type_VPDES$Facility_Name[i]))>0){
    Fac_Type_VPDES$Reclass_Use_Type[i]<-'Aquaculture'
    } else if (length(grep('\\bPLANT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bMINE\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bDEEP MINE\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bPAPER\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bCONCRETE\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bSAND AND GRAVEL\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bAMMUNITION\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bFACILITY\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bTERMINALS\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bTEXTILES\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bLUMBER\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bCONCENTRATOR\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bCOMPOSITES\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bCONSTRUCTION\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bQUARRY\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bPLT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bMOTORS\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bOVENS\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bPRODUCTS\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bTIMBER\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bCHEMICAL\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bCHEMICALS\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bNANOCHEMONICS\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bPRODUCTION\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bINDUSTRIES\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bINDUSTRIAL\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bINDUSTRIAL PARK\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bDEVELOPMENT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bWAREHOUSE\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bMANUFACTURING\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep("\\bLANDFILL\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bBREWERY\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
               length(grep('\\bPURINA\\b',Fac_Type_VPDES$Facility_Name[i]))>0){
      Fac_Type_VPDES$Reclass_Use_Type[i]<-'Industrial'
    }else{
      Fac_Type_VPDES$Reclass_Use_Type[i]<-NA
    }
    }
  }
  
  #If reclassification did not pick up on the pattern recognition, use the VPDES use type (aka default type)
  Fac_Type_VPDES$Reclass_Use_Type<-ifelse(is.na(Fac_Type_VPDES$Reclass_Use_Type),Fac_Type_VPDES$VPDES_Use_Type,Fac_Type_VPDES$Reclass_Use_Type)
  #If still NA, base off of if NON-POTW or POTW
  Fac_Type_VPDES$Reclass_Use_Type<-ifelse(is.na(Fac_Type_VPDES$Reclass_Use_Type)&Fac_Type_VPDES$ECHO_Use_Type=="NON-POTW","Industrial",Fac_Type_VPDES$Reclass_Use_Type)
  Fac_Type_VPDES$Reclass_Use_Type<-ifelse(is.na(Fac_Type_VPDES$Reclass_Use_Type)&Fac_Type_VPDES$ECHO_Use_Type=="POTW","Municipal",Fac_Type_VPDES$Reclass_Use_Type)
  
  #----Apply Reclassification------#
  Fac_Type_VPDES_m<-subset(Fac_Type_VPDES,select = c("Facility.ID","Reclass_Use_Type","Fuel_Type","Gen_Cap_MW"))
  ECHO_2010_2017<-merge(ECHO_2010_2017,Fac_Type_VPDES_m,by="Facility.ID",all.x=T)
  
  
  ECHO_2010_2017<-mutate_if(ECHO_2010_2017,is.factor,as.character)
  ECHO_2010_2017<-as.data.table(ECHO_2010_2017)
  
  assign("Fac_Type_VPDES",Fac_Type_VPDES,envir=.GlobalEnv)
  assign("ECHO_2010_2017",ECHO_2010_2017,envir=.GlobalEnv)
}
Rule_Based()

change_use<- function(VPDESID,new_sector){
  ECHO_2010_2017$Reclass_Use_Type[ECHO_2010_2017$Facility.ID == VPDESID]<- new_sector
  assign('ECHO_2010_2017',ECHO_2010_2017,envir=.GlobalEnv)
}
change_use("VA0090824","Industrial")
change_use("VA0092576","Industrial")
change_use("VA0002674","Municipal")
change_use("VA0025569","Municipal")
change_use("VA0088404","Municipal")
change_use("VA0084212","Municipal")
change_use("VA0064599","Municipal")
change_use("VA0091405","Municipal")
change_use("VA0077411","Commercial")
change_use("VA0059072","Municipal")
change_use("VA0023141","Municipal")
change_use("VA0090654","Commercial")
change_use("VA0083097","Energy")
change_use("VA0076473","Municipal")
change_use("VA0028363","Commercial")
change_use("VA0091278","Aquaculture")
change_use("VA0091260","Aquaculture")
change_use("VA0073245","Commercial")
change_use("VA0074047","Commercial")
change_use("VA0086584","Commercial")
change_use("VA0091006","Commercial")
change_use("VA0028371","Commercial")
change_use("VA0060429","Commercial")
change_use("VA0029556","Commercial")
change_use("VA0063606","Commercial")
change_use("VA0024163","Commercial")
change_use("VA0085588","Commercial")
change_use("VA0065714","Commercial")
change_use("VA0023141","Commercial")
change_use("VA0028029","Commercial")
change_use("VA0028461","Commercial")
change_use("VA0059161","Commercial")
change_use("VA0080993","Commercial")
change_use("VA0090131","Industrial")
change_use("VA0020729","Commercial")
change_use("VA0028363","Commercial")
change_use("VA0086584","Commercial")
change_use("VA0029556","Commercial")
change_use("VA0027481","Commercial")
change_use("VA0091707","Commercial")
change_use("VA0088498","Commercial")
change_use("VA0028371","Commercial")
change_use("VA0060429","Commercial")
change_use("VA0024147","Commercial")
change_use("VA0021067","Commercial")
change_use("VA0067938","Commercial")
change_use("VA0028461","Commercial")
change_use("VA0032034","Commercial")
change_use("VA0023141","Commercial")

##################################################################################################################################
#----------------------------------------------Resolving Anomalous Entries-------------------------------------------------------#
#---------------------------QA/QC---------------------------------#

#---Resolve Flagged Anomalous Data---#
resolve_flags<- function(){
  
  ECHO_2010_2017<-as.data.table(ECHO_2010_2017)
  #---Subset flagged entries in 2010-2016 DMR Dataframe---#
  Flagged_DMR_2010_2017<-subset(ECHO_2010_2017,subset= (!is.na(ECHO_2010_2017$fac_flag_zerodesflow))|
                                  (!is.na(ECHO_2010_2017$dmr_flag_desflow))|
                                  (!is.na(ECHO_2010_2017$dmr_flag_units_100))|
                                  (!is.na(ECHO_2010_2017$dmr_flag_units_1000000)))%>%arrange(desc(Std_ME))
  
  Flagged_DMR_2010_2017<-as.data.table(Flagged_DMR_2010_2017)
   # colSums(!is.na(Flagged_DMR_2010_2017))
   # Flagged_DMR_2010_2017[Flagged_DMR_2010_2017$DesignFlow_mgd==0]%>%dplyr::group_by(Facility.ID)%>%dplyr::summarise(n())
   # Flagged_DMR_2010_2017[Flagged_DMR_2010_2017$dmr_flag_desflow=="dmr_flag_desflow"]%>%dplyr::group_by(Facility.ID)%>%dplyr::summarise(n())
   # Flagged_DMR_2010_2017[Flagged_DMR_2010_2017$dmr_flag_units_100=="dmr_flag_units_100"]%>%dplyr::group_by(Facility.ID)%>%dplyr::summarise(n())
   # Flagged_DMR_2010_2017[Flagged_DMR_2010_2017$dmr_flag_units_1000000=="dmr_flag_units_1000000"]%>%dplyr::group_by(Facility.ID)%>%dplyr::summarise(n())

  #---Assign weights to flags to create a weighted value---#
  ECHO_2010_2017<-
    ECHO_2010_2017%>%
    tibble::add_column(weighted_flag="", .after="dmr_flag_units_1000000")%>%
    tibble::add_column(resolution="",.after="weighted_flag")
  
  ECHO_2010_2017$fac_flag_zerodesflow<-ifelse(!is.na(ECHO_2010_2017$fac_flag_zerodesflow=="fac_flag_zerodesflow"),0.15,0)
  ECHO_2010_2017$dmr_flag_units_100<-ifelse(!is.na(ECHO_2010_2017$dmr_flag_units_100=="dmr_flag_units_100"),1,0)
  ECHO_2010_2017$dmr_flag_units_1000000<-ifelse(!is.na(ECHO_2010_2017$dmr_flag_units_1000000=="dmr_flag_units_1000000"),1,0)
  ECHO_2010_2017$dmr_flag_desflow<-ifelse(!is.na(ECHO_2010_2017$dmr_flag_desflow=="dmr_flag_desflow"),0.5,0)
  
  ECHO_2010_2017$weighted_flag<-ECHO_2010_2017$fac_flag_zerodesflow+
    ECHO_2010_2017$dmr_flag_desflow+
    ECHO_2010_2017$dmr_flag_units_100+
    ECHO_2010_2017$dmr_flag_units_1000000

  table(ECHO_2010_2017$weighted_flag)
  
  #---If weighted value is greater than 1, set as a high concern for potential skewing---#
  ECHO_2010_2017$resolution<-ifelse(ECHO_2010_2017$weighted_flag>=1,1,0)
  
  ECHO_2010_2017[ECHO_2010_2017$resolution>=1]%>%dplyr::group_by(Facility.ID)%>%dplyr::summarise(n())
  
  
  
  # library(FFTrees)
  # 
  # ECHO_2010_2017.train<-subset(ECHO_2010_2017,select = c(1,2,5,8,11,14,16,17,21,22,23,24,25,26))
  # ECHO_2010_2017.test<-subset(ECHO_2010_2017,select = c(1,2,5,8,11,14,16,17,21,22,23,24,25,26))
  # Flag.fft<-FFTrees(formula = resolution ~ .,
  #                   data = ECHO_2010_2017.train,
  #                   data.test = ECHO_2010_2017.test,
  #                   main = "Anomalous Discharge",
  #                   decision.labels = c("Low Concern","High Concern"),
  #                   do.comp = FALSE)
  # 
  # Flag.fft
  # plot(Flag.fft, data="test")
  
  
  ECHO_2010_2017<-ECHO_2010_2017%>%add_column(Resolved_Measured_Effluent_NA="", .after="Measured_Effluent")
  ECHO_2010_2017$Resolved_Measured_Effluent_NA<-ifelse(ECHO_2010_2017$resolution==1,NA,ECHO_2010_2017$Measured_Effluent)
  ECHO_2010_2017<-ECHO_2010_2017%>%add_column(Resolved_Measured_Effluent_Med="", .after="Resolved_Measured_Effluent_NA")
  
  ECHO_2010_2017$Resolved_Measured_Effluent_Med<-ifelse(ECHO_2010_2017$resolution==1 & ECHO_2010_2017$Measured_Effluent>ECHO_2010_2017$Median_ME,ECHO_2010_2017$Median_ME,ECHO_2010_2017$Measured_Effluent)
  
  #----After analysis, Reston Lake AC has an erroneous value in 2017-07-01----#
  ECHO_2010_2017$Resolved_Measured_Effluent_Med[ECHO_2010_2017$Facility.ID=="VA0091995"&ECHO_2010_2017$MP_Begin_Date=="2017-07-01"]<-1.8
  ECHO_2010_2017$Resolved_Measured_Effluent_Med[ECHO_2010_2017$Facility.ID=="VA0091995"&ECHO_2010_2017$MP_Begin_Date=="2017-08-01"]<-1.8
  ECHO_2010_2017$Resolved_Measured_Effluent_Med[ECHO_2010_2017$Facility.ID=="VA0091995"&ECHO_2010_2017$MP_Begin_Date=="2017-09-01"]<-1.8
  
  ECHO_2010_2017$Resolved_Measured_Effluent_Med[ECHO_2010_2017$Facility.ID=="VA0050181"&ECHO_2010_2017$MP_Begin_Date=="2016-11-01"]<-0.40
  ECHO_2010_2017$Resolved_Measured_Effluent_Med[ECHO_2010_2017$Facility.ID=="VA0050181"&ECHO_2010_2017$MP_Begin_Date=="2016-12-01"]<-0.40
  ECHO_2010_2017$Resolved_Measured_Effluent_Med[ECHO_2010_2017$Facility.ID=="VA0050181"&ECHO_2010_2017$MP_Begin_Date=="2016-10-01"]<-0.40
  
  ECHO_2010_2017$Resolved_Measured_Effluent_Med[ECHO_2010_2017$OutfallID=="VA0029785007"&ECHO_2010_2017$MP_Begin_Date=="2016-05-01"]<-0.0216
  ECHO_2010_2017$Resolved_Measured_Effluent_Med[ECHO_2010_2017$OutfallID=="VA0029785006"&ECHO_2010_2017$MP_Begin_Date=="2015-12-01"]<-0.008640
  ECHO_2010_2017$Resolved_Measured_Effluent_Med[ECHO_2010_2017$OutfallID=="VA0029785006"&ECHO_2010_2017$MP_Begin_Date=="2015-11-01"]<-0.008640
  
  D_Months<-
    ECHO_2010_2017%>%
    dplyr::group_by(OutfallID,Year=substring(MP_Begin_Date,1,4))%>%
    dplyr::count(Month=substring(MP_End_Date,6,7))%>%
    dplyr::summarise(Mon_Reported=sum(n))
  
  ECHO_2010_2017<-
    ECHO_2010_2017%>%add_column(Year=substring(ECHO_2010_2017$MP_Begin_Date,1,4), .before="MP_Begin_Date")
  
  ECHO_2010_2017<-merge(ECHO_2010_2017,D_Months,by=c("OutfallID","Year"),all.x=T)
  
  #--Define Class Types for Important Attributes--#
  ECHO_2010_2017<-ECHO_2010_2017[order(ECHO_2010_2017[,1],ECHO_2010_2017[,18],decreasing=F),]
  ECHO_2010_2017$Resolved_Measured_Effluent_Med<-as.numeric(ECHO_2010_2017$Resolved_Measured_Effluent_Med)
  ECHO_2010_2017$MP_Begin_Date<-as.Date(ECHO_2010_2017$MP_Begin_Date,format="%Y-%m-%d")
  
  write.table(ECHO_2010_2017,paste0(path,"/ECHO_2010_2017_QAQC.txt"), sep="\t", row.names=F)
  
  assign("ECHO_2010_2017",ECHO_2010_2017,envir = .GlobalEnv)
  
}
resolve_flags()

#---Confirm Outfall Type with Drew Hammond from DEQ---#
# First limit scope to outfalls ending in 001. Why? We can assumed these are external outfalls while 101 are internal
outfall_define<- function(Facility.ID,outfall,outfall_type){
  ECHO_2010_2017$Outfall_Type[ECHO_2010_2017$OutfallID == paste0(Facility.ID,outfall)]<- outfall_type
  assign('ECHO_2010_2017',ECHO_2010_2017,envir=.GlobalEnv)
}
define_execute<- function(){
  # Surry Power Station
  outfall_define("VA0004090","001","COOL")
  outfall_define("VA0004090","101","INO")
  outfall_define("VA0004090","102","INO")
  outfall_define("VA0004090","103","INO")
  outfall_define("VA0004090","104","INO")
  outfall_define("VA0004090","105","INO")
  outfall_define("VA0004090","106","INO")
  outfall_define("VA0004090","107","INO")
  outfall_define("VA0004090","108","INO")
  outfall_define("VA0004090","109","INO")
  outfall_define("VA0004090","110","INO")
  outfall_define("VA0004090","111","INO")
  outfall_define("VA0004090","112","INO")
  outfall_define("VA0004090","113","INO")
  outfall_define("VA0004090","114","INO")
  outfall_define("VA0004090","115","INO")
  outfall_define("VA0004090","116","INO")
  outfall_define("VA0004090","117","INO")
  outfall_define("VA0004090","118","INO")
  outfall_define("VA0004090","119","INO")
  outfall_define("VA0004090","120","INO")
  outfall_define("VA0004090","121","INO")
  outfall_define("VA0004090","122","INO")
  
  # Chesterfield
  outfall_define("VA0004146","001","COOL")
  outfall_define("VA0004146","002","COOL")
  outfall_define("VA0004146","003","COOL")
  outfall_define("VA0004146","004","INO")
  outfall_define("VA0004146","005","INO")
  outfall_define("VA0004146","006","INO")
  outfall_define("VA0004146","007","INO")
  outfall_define("VA0004146","008","INO")
  outfall_define("VA0004146","009","INO")
  outfall_define("VA0004146","010","INO")
  outfall_define("VA0004146","011","INO")
  outfall_define("VA0004146","101","INO")
  outfall_define("VA0004146","104","INO")
  outfall_define("VA0004146","301","INO")
  outfall_define("VA0004146","302","INO")
  outfall_define("VA0004146","303","INO")
  outfall_define("VA0004146","304","INO")
  outfall_define("VA0004146","401","INO")
  outfall_define("VA0004146","402","INO")
  
  # Yorktown
  outfall_define("VA0004103","001","COOL")
  outfall_define("VA0004103","002","COOL")
  outfall_define("VA0004103","101","INO")
  outfall_define("VA0004103","102","INO")
  outfall_define("VA0004103","103","INO")
  outfall_define("VA0004103","104","INO")
  outfall_define("VA0004103","105","INO")
  outfall_define("VA0004103","106","INO")
  outfall_define("VA0004103","107","INO")
  outfall_define("VA0004103","108","INO")
  outfall_define("VA0004103","109","INO")
  outfall_define("VA0004103","110","INO")
  outfall_define("VA0004103","111","INO")
  outfall_define("VA0004103","112","INO")
  outfall_define("VA0004103","202","INO")
  outfall_define("VA0004103","203","INO")
  outfall_define("VA0004103","204","INO")
  outfall_define("VA0004103","205","INO")
}
define_execute()

outfalls<-ECHO_2010_2017%>%
  dplyr::group_by(OutfallID)%>%
  dplyr::summarise(Outfall_Type=first(Outfall_Type),Hydrocode=first(Facility.ID),Facility_Name=first(FacilityName),
                   Outfall_Longitude=first(Outfall_Longitude),Outfall_Latitude=first(Outfall_Latitude),
                   City=first(City),County=first(County),Use_Type=first(Reclass_Use_Type),Permit_Status=first(Permit_Status))

write.table(outfalls,file="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/Outfall_Types.txt",row.names=F,sep="|")


cumulative_outfalls<- function(VPDESID,outfall_list,external_outfall){
  
  # First subset facility of choice...subsetting being finicky
  
  facility<-ECHO_2010_2017[ECHO_2010_2017$Facility.ID==VPDESID,]
  
  # Then look at outfalls that cumulatively add to said external outfall
  outfall_i<-data.frame()
  outfalls<-data.frame()
  for (i in 1:length(outfall_list)){
    outfall_i<-subset(facility,facility$OutfallID==paste0(VPDESID,outfall_list[i]))
    outfalls<-rbind(outfalls,outfall_i)
  }

  # Sum the flow that is being contributed, remembering to remove NA values
  outfalls<-outfalls%>%dplyr::group_by(MP_Begin_Date)%>%
    dplyr::summarise(Resolved_Measured_Effluent_Med=sum(Resolved_Measured_Effluent_Med,na.rm=T))
  
  # Subtract from external outfall of choice
  ECHO_2010_2017$Resolved_Measured_Effluent_Med[ECHO_2010_2017$OutfallID==paste0(VPDESID,external_outfall)]<-
    ifelse(outfalls$MP_Begin_Date%in%ECHO_2010_2017$MP_Begin_Date[ECHO_2010_2017$OutfallID==paste0(VPDESID,external_outfall)],
           ECHO_2010_2017$Resolved_Measured_Effluent_Med[ECHO_2010_2017$OutfallID==paste0(VPDESID,external_outfall)]-
             outfalls$Resolved_Measured_Effluent_Med[match(ECHO_2010_2017$MP_Begin_Date[ECHO_2010_2017$OutfallID==paste0(VPDESID,external_outfall)],outfalls$MP_Begin_Date)])
  
  assign("ECHO_2010_2017",ECHO_2010_2017,envir = .GlobalEnv)
  
}
# Surry power station's 001 outfall includes flows from outfalls 101 through 122
cumulative_outfalls("VA0004090",list("101","102","103","104","105",
                                  "106","107","108","109","110",
                                  "111","112","113","114","115",
                                  "116","117","118","119","120",
                                  "121","122"),"001") # Surry Power Station's 001 is the cooling water outfall. It also includes discharge from outfalls 101 through 122

# Yorktown's 001 outfall contains flow from outfalls 101-112
cumulative_outfalls("VA0004103",list("101","102","103","104","105","106","107","108","109","110","111","112"),"001")
# Yorktown's 002 outfall contains flow from outfalls 202-205
cumulative_outfalls("VA0004103",list("202","203","204","205"),"002")

# This function narrows down the outfalls that are analyzed. Typically outfall 001 represents a facility,
# however that is not always the case. This function swaps out the 001 outfall for one that represents the actual discharge
# leaving the facility and going into open waters. 
outfall.switch<- function(ECHO_2010_2017){
  #--Facilties that are represented with a different outfall rather than 001--#
  
  # Surry's 001 outfall includes flow from 101-122
  
  North_Anna<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0052451101") # Outfall 001 includes stormwater flows but these flow aren't measured 
  Mecklenburg<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0084069101")
  Clinch<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0001015003")
  Chesterfield<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0004146001"| 
                         ECHO_2010_2017$OutfallID=="VA0004146002"|ECHO_2010_2017$OutfallID=="VA0004146003") # All three of these Outfalls are external
  
  Yorktown<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0004103001"|
                     ECHO_2010_2017$OutfallID=="VA0004103002")
  
  Glen_Lyn<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0000370005")
  Hopewell<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0066630101")
  Franklin<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0004162103")
  GP<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0003026002")
  AAT<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0002160101")
  
  Covington<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0003646003")
  Radford<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0000248006")
  
  Steel<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0001589005")
  NPN<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0004804021")
  Coursey<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0091251007")
  
  Montebello<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0091243002")
  
  Gore<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0090590007")
  
  JamesC<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0020681002")
  Solenis<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0003433002")
  
  Omega<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0003867002")
  
  Iluka<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0092436101")
  
  Nrail<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0001597002")
  corr<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0020702002")
  frama<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0004774004")
  basham<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0022802002")
  fork<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0024147002")
  church<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0084212003")
  san<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0089010002")
  pilot<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0088277002")
  J<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0089214003")
  dulles<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0089541021")
  broad<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0090263003")
  sheetz<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0090271002")
  coating<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0091871202")
  senior<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0092371002")
  J2<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0092657002")
  loves<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0092851101")
  
  loves2<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0090956101")
  frank<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0092142102")
  shen<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0088846006")
  lacey<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0077399002")
  boones<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0067245002")
  vims<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0071528004")
  ship<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0053813009")
  quantico<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0002151030")
  ashland<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0003492005")
  appox<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0005819004")
  waynes<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0025151002")
  warsaw<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0026891002")
  lincoln<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0029785007")
  alb<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0052655004")
  doswell<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0052906002")
  GD<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0073091003")
  crop<-subset(ECHO_2010_2017,ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"&ECHO_2010_2017$OutfallID=="VA0088374003")
  
  ECHO_2010_2017<-subset(ECHO_2010_2017, subset=ECHO_2010_2017$Permit_Type=="NPDES Individual Permit"
                         &str_sub(ECHO_2010_2017$OutfallID, start=-3)=="001"
                         &!ECHO_2010_2017$OutfallID=="VA0052451001"
                         &!ECHO_2010_2017$OutfallID=="VA0084069001"
                         &!ECHO_2010_2017$OutfallID=="VA0001015001"
                         &!ECHO_2010_2017$OutfallID=="VA0000370001"
                         &!ECHO_2010_2017$OutfallID=="VA0004162001"
                         &!ECHO_2010_2017$OutfallID=="VA0066630001"
                         &!ECHO_2010_2017$OutfallID=="VA0003026001"
                         &!ECHO_2010_2017$OutfallID=="VA0002160001")
  
  ECHO_2010_2017<-rbind(ECHO_2010_2017,North_Anna,Mecklenburg,Clinch,
                        Chesterfield,Yorktown,Glen_Lyn,Hopewell,Franklin,
                        GP,AAT,Covington,Radford,Steel,NPN,Coursey,Montebello,JamesC,Gore,Solenis,Omega,Iluka,
                        Nrail, corr, frama,basham,fork,church,san,pilot,J,dulles,broad,sheetz,coating,senior,J2,loves,
                        loves2,frank,shen,lacey,boones,vims,ship,quantico,ashland,appox,waynes,warsaw,lincoln,alb,doswell,GD,crop)
  
  assign("ECHO_2010_2017",ECHO_2010_2017,envir = .GlobalEnv)
}
outfall.switch(ECHO_2010_2017)

duplicated<-ECHO_2010_2017[duplicated(ECHO_2010_2017, by=c("OutfallID","MP_End_Date")),]
ECHO_2010_2017<-ECHO_2010_2017[!duplicated(ECHO_2010_2017, by=c("OutfallID","MP_End_Date")),]

outfalls_post<-ECHO_2010_2017%>%
  dplyr::group_by(OutfallID)%>%
  dplyr::summarise(Outfall_Type=first(Outfall_Type),Hydrocode=first(Facility.ID),Facility_Name=first(FacilityName),
                   Outfall_Longitude=first(Outfall_Longitude),Outfall_Latitude=first(Outfall_Latitude),
                   City=first(City),County=first(County),Use_Type=first(Reclass_Use_Type),Permit_Status=first(Permit_Status))

missing<-subset(outfalls,!outfalls$Hydrocode %in% outfalls_post$Hydrocode)


write.table(ECHO_2010_2017,paste0(path,"/ECHO_2010_2017_QAQC.txt"), sep="\t", row.names=F)

#-------Summarize Data frame------------#

Outfall_Discharges<-ECHO_2010_2017%>%
  dplyr::group_by(OutfallID,Year=substr(MP_Begin_Date,1,4))%>%
  dplyr::summarise(Facility_Name=first(FacilityName),
                   Facility.ID=first(Facility.ID),
                   Outfalls=n_distinct(OutfallID),
                   entries=n(),
                   Ave_Mon_Reported=mean(Mon_Reported),
                   Discharges_MGD=sum(Resolved_Measured_Effluent_Med,na.rm=T)/first(Mon_Reported),
                   Sector=first(Reclass_Use_Type))%>%arrange(desc(Discharges_MGD))


Facility_Discharges<-Outfall_Discharges%>%
  dplyr::group_by(Facility.ID,Year)%>%
  dplyr::summarise(Facility_Name=first(Facility_Name),
                   Sources=n_distinct(OutfallID),
                   entries=sum(entries),
                   Discharges_MGD=sum(Discharges_MGD,na.rm=T),
                   Sector=first(Sector))%>%arrange(desc(Discharges_MGD))

energy<-Facility_Discharges[Facility_Discharges$Sector=='Energy',]%>%
  dplyr::group_by(Year)%>%
  dplyr::summarise(no_Facilities=n_distinct(Facility.ID),no_sources=sum(Sources), entries=sum(entries),
                   Sum_Discharges_MGD=sum(Discharges_MGD,na.rm=T),Mean_Discharge_MGD=mean(Discharges_MGD,na.rm=T),
                   Median_Withdrawal_MGD=median(Discharges_MGD,na.rm=T))
energy<-energy[complete.cases(energy),]

library(tidyverse)
energy<-energy%>%
  mutate(pct_change=round((Sum_Discharges_MGD/lag(Sum_Discharges_MGD) -1 )*100,digits=1))

energy%>%summarise(mean(no_Facilities),mean(no_sources),mean(entries),mean(Sum_Discharges_MGD),mean(pct_change,na.rm=T))

non.energy<-Facility_Discharges[!Facility_Discharges$Sector=='Energy',]%>%
  dplyr::group_by(Year)%>%
  dplyr::summarise(no_Facilities=n_distinct(Facility.ID),no_sources=sum(Sources), entries=sum(entries),
                   Sum_Discharges_MGD=sum(Discharges_MGD,na.rm=T),Mean_Discharge_MGD=mean(Discharges_MGD,na.rm=T),
                   Median_Withdrawal_MGD=median(Discharges_MGD,na.rm=T))
non.energy<-non.energy[complete.cases(non.energy),]

non.energy<-non.energy%>%
  mutate(pct_change=round((Sum_Discharges_MGD/lag(Sum_Discharges_MGD) -1 )*100,digits=1))

non.energy%>%summarise(mean(no_Facilities),mean(no_sources),mean(entries),mean(Sum_Discharges_MGD),mean(pct_change,na.rm=T))
