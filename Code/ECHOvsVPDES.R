#############################################################################################################################
###########################################ECHO vs VPDES#####################################################################

#Author: Morgan McCarthy, M.S. Biological Systems Engineering, Virginia Tech

#############################################Purpose#########################################################################

#This script is intended to compare time series flows from CWA regulated facilities in the ECHO Database (ECHOInterface.R) to their respective 
#VPDES Individual Permits. The Virginia Pollution Discharge Elimination System (VPDES) is administered 
#by Virginia's Department of Environmental Quality (VDEQ). Specifically, we are looking at Individual Permits 
#for municipal and industrial discharging facilities. An excel spreadsheet listing active VPDES Individual 
#Permits is found on the VDEQ website (https://www.deq.virginia.gov/Programs/Water/PermittingCompliance/PollutionDischargeElimination/PermitsFees.aspx#GGPs).

#Other flagging methdos are demonstrated in this script as well to note suspicious reported effluents. 

############################################Library Initilization############################################################

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
Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jre1.8.0_181')
library(XLConnect)
library(tm)

#############################################################################################################################
##################################################Initialize#################################################################

state<-"VA"
path<-"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated"
options(scipen=999) #Disable scientific notation

Sys.setenv("plotly_username"="mccartma")
Sys.setenv("plotly_api_key"="dGx8GZvFOrsIZBu4JGfh")

#############################################################################################################################
##########################################ECHO Discharge Data################################################################

#-----------------------------------------#
#------------ECHO Facilities--------------#

#Utilizes CWA ECHO Water Facility Search REST Services to obtain all discharging facilities within state of interest.
#Attributes can be indicated in the query to obtain customized results: [Metadata Link](https://echo.epa.gov/tools/web-services/facility-search-water#!/Metadata/get_cwa_rest_services_metadata)

Req_URL<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&qcolumns=1,2,3,4,5,10,14,15,21,22,23,24,25,26,27,60,61,63,65,67,84,91,95,97,204,205,206,207,209,210&passthrough=Y&p_st=",state)
URL_Download<-getURL(Req_URL) #Download URL from above
URL_Parse<-xmlParse(URL_Download)#parses the downloaded XML of facilities and generates an R structure that represents the XML/HTML tree-main goal is to retrieve query ID or QID
QID<-xmlToList(URL_Parse)#Converts parsed query to a more R-like list and stores it as a variable
QID<-QID$QueryID
GET_Facilities<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qcolumns=1,2,3,4,5,10,14,15,21,22,23,24,25,26,27,60,61,63,65,67,84,91,95,97,204,205,206,207,209,210&passthrough=Y&qid=",QID)
ECHO_Facilities<-read.csv(GET_Facilities,stringsAsFactors = F) #Important to note this returns all facilities, active or not
ECHO_Facilities$CWPName<-toupper(ECHO_Facilities$CWPName)
#ECHO_Facilities_IP<-subset(ECHO_Facilities,subset = ECHO_Facilities$CWPPermitTypeDesc=="NPDES Individual Permit")
#ECHO_Facilities_activeIP<-subset(ECHO_Facilities,subset = ECHO_Facilities$CWPPermitTypeDesc=="NPDES Individual Permit" & #ECHO_Facilities$CWPPermitStatusDesc=="Effective")
rm(Req_URL,URL_Download,URL_Parse,GET_Facilities,QID,state) #Clear unnecessary variables

#-----------------------------------------#
#-------Time series ECHO DMR Data---------# 
#(Monthly, Daily, Weekly, Annual Average) 
#**Rare to see averages other than monthly average**#
#From ECHO_Timeseries.R Script: Uses Effluent Charts REST Services#

ECHO_Discharge<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Documentation/Imports/ECHO_timeseries_10_2.txt",header=T, sep="\t")

#############################################################################################################################
#############################################VPDES Permit Data###############################################################

#This section of code retrieves the coordinates and flow limits for each point source discharging facility with an individual VPDES permit. 
#General permits are written for a general class of discharges, while individual permits are for municipal and industrial facilities with effluent limitations and monitoring requirements.
#Coordinates for each unique outfall is found in the VDEQ's VEGIS Database 
#Facility Design Flows are found on the VDEQ website under Permitting & Compliance 

#-----------------------------------------#
#--------VPDES Outfall Geodatabase--------#

#VPDES Geodatabase:http://www.deq.virginia.gov/mapper_ext/GIS_Datasets/VPDES_Geodatabase.zip

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

#Individual Permits updated as of July 2018--look for updates every few months
#Warnings about unknown or uninitiliased columns: previous IP contact sheets named the columns differently. It doesn't hinder any processes though. 
GET('https://www.deq.virginia.gov/Portals/0/DEQ/Water/PollutionDischargeElimination/VPDES%20Spreadsheets/VPDES%20IP%20Contacts%20Location%20Flow%20Exported%20July%202018.xls?ver=2018-07-24-133750-360', 
    write_disk(temp <- tempfile(fileext = ".xls")))
VPDES_IP <- read_excel(temp, skip=3)
VPDES_IP<-VPDES_IP[!is.na(VPDES_IP$Facility),]
VPDES_IP$`Design Flow (MGD)`<-as.numeric(VPDES_IP$`Design Flow (MGD)`)
VPDES_IP<-VPDES_IP[!duplicated(VPDES_IP$`Permit Number`),] #getting rid of duplicates and looking at unique permits

#It's important to note that the individual permit information just includes design and total flow
#Summarize the information within the VPDES IP Spreadsheet
VPDES_IP%>%
  summarise(Sum_DesFlow=sum(`Design Flow (MGD)`,na.rm = T),Max_DesFlow=max(`Design Flow (MGD)`,na.rm = T),
            ZeroCount_DesFlow=count(VPDES_IP%>%filter(`Design Flow (MGD)`==0)),
            NACount_DesFlow=length(VPDES_IP$`Design Flow (MGD)`[is.na(VPDES_IP$`Design Flow (MGD)`)]),
            Sum_TotalFlow=sum(`Total Flow`,na.rm = T),Max_TotalFlow=max(`Total Flow`,na.rm = T),
            NACount_TotalFlow=length(VPDES_IP$`Total Flow`[is.na(VPDES_IP$`Total Flow`)])) #24 facilities in VPDES IP spreadsheet report having a design flow of 0 MGD, 2 NAs

#-----------------------------------------#
#-----VPDES General Permit Database-------#
GET('https://www.deq.virginia.gov/Portals/0/DEQ/Water/PollutionDischargeElimination/VPDES%20Spreadsheets/VPDES%20General%20Permits%20July%202018%20(3).xls?ver=2018-07-24-133254-740', 
    write_disk(temp <- tempfile(fileext = ".xls")))

VPDES_GP<-loadWorkbook(temp)
sheet_names<-getSheets(VPDES_GP)
names(sheet_names)<-sheet_names
VPDES_GP<-lapply(sheet_names, function(.sheet){readWorksheet(object=VPDES_GP, .sheet)})
rm(sheet_names)

#Have a lot of general permitted facilities starting with VAN: which are the nutrient trading GP
VPDES_GP<-as.data.frame(VPDES_GP$`Nutrient GP`)
VPDES_GP<-VPDES_GP[rowSums(is.na(VPDES_GP))==0,]
colnames(VPDES_GP)=VPDES_GP[1,]
VPDES_GP <- VPDES_GP[-c(1, 2, 3), ]

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

#Now we have a data frame containing time series data for each outfall located in ECHO with outfall and facility coordinates. 
rm(VPDES_Coordinates,temp,ECHO_Facilities_Coordinates,state)

#############################################################################################################################
##################################################Flagging###################################################################

#-----------------------------------------------------------------#
#-------------------Design Flow = 0-------------------------------#

VPDES_DesignFlow<-subset(VPDES_IP,select=c(3,15))
colnames(VPDES_DesignFlow)<-c("Facility.ID","DesignFlow_mgd")
VPDES_DesignFlow<-arrange(VPDES_DesignFlow,Facility.ID)
VA_Discharge_DMR<-merge.data.frame(ECHO_Discharge,VPDES_DesignFlow,by="Facility.ID",all.x=T)
VA_Discharge_DMR<-subset(VA_Discharge_DMR,select=c(1,3,16,15,2,13,14,17,4,5,6,7,8,9,10,11,12))
VA_Discharge_DMR$fac_flag_zerodesflow<-ifelse(VA_Discharge_DMR$DesignFlow_mgd==0,"fac_flag_zerodesflow",NA)

#-----------------------------------------------------------------#
#-------ECHO Measured Effluents > VPDES Design flow---------------#

#Flag facilities that report measured effluent greater than the design flow 
VA_Discharge_DMR$dmr_flag_desflow<-ifelse(VA_Discharge_DMR$Measured_Effluent>VA_Discharge_DMR$DesignFlow_mgd & VA_Discharge_DMR$DesignFlow_mgd>0,"dmr_flag_desflow",NA)

#-----------------------------------------------------------------#
#------Measured Effluents > 100*Median Measured Effluent----------#
#------Measured Effluents > 100,000*Median Measured Effluent------#
#---------------Potential Unit Conversion Error-------------------#

#Summmarize measured effluent values from ECHO by OutfallID--Not by Facility#
VA_Discharge_DMR_Summary<-VA_Discharge_DMR%>%
  dplyr::group_by(OutfallID)%>% #important to note that we are summarizing discharge by outfall here 
  summarise(Mean_ME=mean(Measured_Effluent, na.rm = T),Median_ME=median(Measured_Effluent, na.rm = T),Std_ME=sd(Measured_Effluent, na.rm = T))

VA_Discharge_DMR<-merge(VA_Discharge_DMR,VA_Discharge_DMR_Summary,by="OutfallID",all.x=T)

VA_Discharge_DMR$dmr_flag_units_100<-ifelse(VA_Discharge_DMR$Measured_Effluent>100*VA_Discharge_DMR$Median_ME,"dmr_flag_units_100",NA)
VA_Discharge_DMR$dmr_flag_units_1000000<-ifelse(VA_Discharge_DMR$Measured_Effluent>1000000*VA_Discharge_DMR$Median_ME,"dmr_flag_units_1000000",NA)

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
VA_Discharge_DMR<-VA_Discharge_DMR[,c(2,3,4,5,1,6,7,8,9,20,21,22,11,10,12,13,14,15,16,17,18,19,23,24)]
VA_Discharge_DMR$MP_Begin_Date<-as.Date(VA_Discharge_DMR$MP_Begin_Date,format="%Y-%m-%d")
VA_Discharge_DMR$MP_End_Date<-as.Date(VA_Discharge_DMR$MP_End_Date,format="%Y-%m-%d")
VA_Discharge_DMR<-VA_Discharge_DMR[order(VA_Discharge_DMR[,5],VA_Discharge_DMR[,16],decreasing=F),]

#write.table(VA_Discharge_DMR,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/ECHO Flagging/ECHO_Timeseries_Flagged.txt",sep="\t",row.names = F)
#write.csv(VA_Discharge_DMR,"G:/My Drive/USGS_ConsumptiveUse/Summer, 2018/Morgan/ECHO_Timeseries_Flagged.csv",row.names = F)

#-----------------------------------------------------------------#
#------------Subset Flagged Entries into Dataframe----------------#
Flagged_DMR<-subset(VA_Discharge_DMR,subset= (!is.na(VA_Discharge_DMR$fac_flag_zerodesflow))|
                (!is.na(VA_Discharge_DMR$dmr_flag_desflow))|
                (!is.na(VA_Discharge_DMR$dmr_flag_units_100))|
                (!is.na(VA_Discharge_DMR$dmr_flag_units_1000000))|
                (!(VA_Discharge_DMR$Violation_Code=="")))

Overlapping_Flags<-subset(Flagged_DMR, subset= !is.na(Flagged_DMR$dmr_flag_units_100) & !(Flagged_DMR$Violation_Code=="")|
                            !is.na(Flagged_DMR$dmr_flag_units_100) & !is.na(Flagged_DMR$fac_flag_zerodesflow)|
                            !is.na(Flagged_DMR$dmr_flag_units_100) &!is.na(Flagged_DMR$dmr_flag_desflow)|
                            !is.na(Flagged_DMR$dmr_flag_units_1000000) & !(Flagged_DMR$Violation_Code=="")|
                            !is.na(Flagged_DMR$dmr_flag_units_1000000) & !is.na(Flagged_DMR$fac_flag_zerodesflow)|
                            !is.na(Flagged_DMR$dmr_flag_units_1000000) &!is.na(Flagged_DMR$dmr_flag_desflow)|
                            !(Flagged_DMR$Violation_Code=="") & !is.na(Flagged_DMR$fac_flag_zerodesflow)|
                            !(Flagged_DMR$Violation_Code=="") & !is.na(Flagged_DMR$dmr_flag_desflow)|
                            !is.na(Flagged_DMR$fac_flag_zerodesflow) & !is.na(Flagged_DMR$dmr_flag_desflow)|
                            !is.na(Flagged_DMR$dmr_flag_units_100)&!(Flagged_DMR$Violation_Code=="")&!is.na(Flagged_DMR$fac_flag_zerodesflow)&!is.na(Flagged_DMR$dmr_flag_desflow)&!is.na(Flagged_DMR$dmr_flag_units_1000000)|
                            !is.na(Flagged_DMR$dmr_flag_units_100)&!(Flagged_DMR$Violation_Code=="")&!is.na(Flagged_DMR$fac_flag_zerodesflow)|
                            !is.na(Flagged_DMR$dmr_flag_units_100)&!is.na(Flagged_DMR$fac_flag_zerodesflow)&!is.na(Flagged_DMR$dmr_flag_desflow)|
                            !is.na(Flagged_DMR$dmr_flag_units_100)&!(Flagged_DMR$Violation_Code=="")&!is.na(Flagged_DMR$dmr_flag_desflow)|
                            !is.na(Flagged_DMR$dmr_flag_units_1000000)&!(Flagged_DMR$Violation_Code=="")&!is.na(Flagged_DMR$fac_flag_zerodesflow)|
                            !is.na(Flagged_DMR$dmr_flag_units_1000000)&!is.na(Flagged_DMR$fac_flag_zerodesflow)&!is.na(Flagged_DMR$dmr_flag_desflow)|
                            !is.na(Flagged_DMR$dmr_flag_units_1000000)&!(Flagged_DMR$Violation_Code=="")&!is.na(Flagged_DMR$dmr_flag_desflow))

#############################################################################################################################
###############################################Compiling and Exporting Flags#################################################

#-----------------------------------------------------------------#
#-------------------Design Flow = 0-------------------------------#

Flagged_Zero_DesFlow<-subset(VA_Discharge_DMR,subset=VA_Discharge_DMR$DesignFlow_mgd==0,
                             select=-c(17,16,15))%>%arrange(desc(Measured_Effluent-DesignFlow_mgd))
#Summarize by facility 
Flagged_Zero_DesFlow%>%
  group_by(FacilityName)%>%
  summarise(Design_Flow=mean(DesignFlow_mgd,na.rm=T), ECHO_Limit=mean(Permitted_Limit,na.rm = T),Max_ME=max(Measured_Effluent,na.rm=T),
            Min_ME=min(Measured_Effluent,na.rm=T),Mean_ME=mean(Measured_Effluent,na.rm = T),
            Median_ME=median(Measured_Effluent,na.rm=T), STD_ME=sd(Measured_Effluent,na.rm=T))

#write.table(Flagged_Zero_DesFlow,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/ECHO Flagging/Flagged_Zero_DesFlow.txt",sep="\t",row.names = F)
#write.csv(Zero_desflow,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/QA_QC/Zero_desflow.csv")

#---Import for VAHydro---#
colnames(VPDES_DesignFlow)<-c("FacilityID","DesignFlow_mgd")
ECHO_Facilities<-merge(ECHO_Facilities,VPDES_DesignFlow,by="FacilityID",all.x=T)

design_flow<-data.frame(hydrocode=paste0("echo_",ECHO_Facilities$FacilityID), varkey='design_flow',
                        propname='design_flow', propvalue=ECHO_Facilities$DesignFlow_mgd,
                        proptext="", propcode=ifelse(ECHO_Facilities$FacilityID%in%Flagged_Zero_DesFlow$Facility.ID,Flagged_Zero_DesFlow$fac_flag_zerodesflow,""),startdate="",enddate="")

#write.table(design_flow,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/ECHO_VAHydro Imports/design_flow.txt",sep="\t",row.names = F)

#-----------------------------------------------------------------#
#-------ECHO Measured Effluents > VPDES Design flow---------------#

#Sort by largest difference between the measured effluent and the design flow
Flagged_Design_Flow<-subset(VA_Discharge_DMR,subset=VA_Discharge_DMR$dmr_flag_desflow=="dmr_flag_desflow",
                            select=-c(18,17,16,15))%>%arrange(desc(Measured_Effluent-DesignFlow_mgd)) 
Flagged_Design_Flow%>%
  group_by(FacilityName)%>%
  summarise(Max_ME=max(Measured_Effluent, na.rm = T),Min_ME=min(Measured_Effluent,na.rm = T),
            Mean_ME=mean(Measured_Effluent, na.rm = T),Median_ME=median(Measured_Effluent, na.rm = T),
            STD_ME=sd(Measured_Effluent,na.rm = T),Design_Flow=median(DesignFlow_mgd),
            ECHO_Limit=median(Permitted_Limit,na.rm = T))%>%arrange(desc(STD_ME-Design_Flow))

#write.table(Flagged_Design_Flow,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/ECHO Flagging/Flagged_Exceeds_DesFlow.txt",sep="\t",row.names = F)
#write.csv(MEgreaterDF,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/QA_QC/MEgreaterDF.csv")

#---VAHydro Import---#
dmr_flag_desflow<-data.frame(hydrocode=paste0("echo_",Flagged_Design_Flow$OutfallID),varkey="dmr_flag_desflow",
                             entity_type='dh_timeseries',propname="dmr_flag_desflow",propvalue="",
                             proptext="Exceeds Facility Design Flow",propcode="dmr_flag_desflow",
                             startdate=Flagged_Design_Flow$MP_Begin_Date,enddate=Flagged_Design_Flow$MP_End_Date)

#write.table(dmr_flag_desflow,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/ECHO Flagging for VAHydro/dmr_flag_desflow.txt",sep="\t",row.names = F)

#-----------------------------------------------------------------#
#------Measured Effluents > 100*Median Measured Effluent----------#
#------Measured Effluents > 100,000*Median Measured Effluent------#
#---------------Potential Unit Conversion Error-------------------#

Flagged_Units<-subset(VA_Discharge_DMR,subset=VA_Discharge_DMR$dmr_flag_units_100=="dmr_flag_units_100"|VA_Discharge_DMR$dmr_flag_units_1000000=="dmr_flag_units_1000000",
                      select=-c(19,18,17,16,15))%>%arrange(desc(Measured_Effluent-Median_ME))

Flagged_Units<-Flagged_Units[,c(2,3,4,5,1,6,7,8,9,11,15,16,17,10,12,13,14,18,19)]

Flagged_Units%>%
  group_by(OutfallID)%>% #grouping by outfalls that are in the subsetted flagged units data frame
  summarise(FacilityName=first(FacilityName),Max_ME=max(Measured_Effluent,na.rm=T),Median_ME=first(Median_ME),
            Std_ME=first(Std_ME))%>%arrange(desc(Std_ME))

#write.table(Flagged_Units,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/ECHO Flagging/Flagged_Exceeds_100Median.txt",sep="\t",row.names = F)
#write.csv(Flagged_Units_Summarized,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/QA_QC/Flagged_Units_Summarized.csv")

#---Import into VAHydro---#
dmr_flag_units_100<-data.frame(hydrocode=paste0("echo_",Flagged_Units$OutfallID),varkey="dmr_flag_units_100",
                               entity_type="dh_timeseries",propname="dmr_flag_units_100",propvalue="",
                               proptext="Exceeds 100*Median Outfall Discharge",propcode="dmr_flag_units_100",
                               startdate=Flagged_Units$MP_Begin_Date,enddate=Flagged_Units$MP_End_Date)

units_1000000<-subset(Flagged_Units, subset = dmr_flag_units_1000000=="dmr_flag_units_1000000")
dmr_flag_units_1000000<-data.frame(hydrocode=paste0("echo_",units_1000000$OutfallID),varkey="dmr_flag_units_100",
                                   entity_type="dh_timeseries",propname="dmr_flag_units_100",propvalue="",
                                   proptext="Exceeds 10^6*Median Outfall Discharge",propcode="dmr_flag_units_100",
                                   startdate=units_1000000$MP_Begin_Date,enddate=units_1000000$MP_End_Date)

#write.table(dmr_flag_units_100,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/ECHO Flagging for VAHydro/dmr_flag_units_100.txt",sep="\t",row.names = F)
#write.table(dmr_flag_units_1000000,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/ECHO Flagging for VAHydro/dmr_flag_units_1000000.txt",sep="\t",row.names = F)

#-----------------------------------------------------------------#
#----------------------ECHO System Flags--------------------------#

Flagged_ECHO_Violations<-subset(VA_Discharge_DMR,subset = !(Violation_Code==""), 
                                select=c(2,3,4,5,1,6,7,8,9,11,10,12,13,14,15,16,17))

Flagged_ECHO_Violations$Violation_Code<-as.character(Flagged_ECHO_Violations$Violation_Code)
count(Flagged_ECHO_Violations,Violation_Code)

#write.table(Flagged_ECHO_Violations,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/ECHO Flagging/Flagged_ECHO_Violations.txt",sep="\t",row.names = F)
#write.csv(ECHO_Violations_Summary,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/QA_QC/ECHO_Violations_Summary.csv") 

#---Import into VAHydro---#
echo_flag<-data.frame(hydrocode=paste0("echo_",Flagged_ECHO_Violations$OutfallID),varkey="echo_flag",
                      entity_type="dh_timeseries",propname="echo_flag", 
                      propcode=as.character(Flagged_ECHO_Violations$Violation_Code),
                      startdate=Flagged_ECHO_Violations$MP_Begin_Date, enddate=Flagged_ECHO_Violations$MP_End_Date)

for(i in 1:length(echo_flag$hydrocode)){
  if(echo_flag$propcode[i]=="E90"){
    echo_flag$proptext[i]<-"Effluent Violation"
  }else if(echo_flag$propcode[i]=="D90"){
    echo_flag$proptext[i]<-"DMR Overdue, with a numeric limit"
  }else if(echo_flag$propcode[i]=="D80"){
    echo_flag$proptext[i]<-"DMR Overdue, monitoring only required"
  }
}


#write.table(echo_flag,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/ECHO Flagging for VAHydro/echo_flag.txt",sep="\t",row.names = F)

##################################################################################################################################
###########################################################Analysis###############################################################

#-----------------------------------------------------------------#
#---------------Trim to Match VWUDS: 2010-2017--------------------#

ECHO_2010_2017<-VA_Discharge_DMR
ECHO_2010_2017<-ECHO_2010_2017[ECHO_2010_2017$MP_Begin_Date %within% interval("2010-01-01","2017-12-31"),]

Use_Type<-subset(VPDES_IP,select=c(3,7))
  colnames(Use_Type)<-c("Facility.ID","Use_Type")
  ECHO_2010_2017<-merge(ECHO_2010_2017,Use_Type,by="Facility.ID",all.x=T)

ECHO_Facility_Attributes<-subset(ECHO_Facilities,select=c(2,4,5,6,7,8,15,16,17,18,19,20,29))
colnames(ECHO_Facility_Attributes)<-c("Facility.ID","City","State","County","HUC7","FIPS","Facility_Type","Permit_Status","Issuing_Agency","Permit_Type","Permit_Effective_Date","Permit_Expiration_Date","Waterbody_GNIS_Name")

ECHO_2010_2017<-merge(ECHO_2010_2017,ECHO_Facility_Attributes,by="Facility.ID",all.x=T)

ECHO_2010_2017$Outfall_Longitude<-ifelse(is.na(ECHO_2010_2017$Outfall_Longitude),ECHO_2010_2017$Facility_Longitude,ECHO_2010_2017$Outfall_Longitude)
ECHO_2010_2017$Outfall_Latitude<-ifelse(is.na(ECHO_2010_2017$Outfall_Latitude),ECHO_2010_2017$Facility_Latitude,ECHO_2010_2017$Outfall_Latitude)

#-----Summarize Before QA/QC------#
ECHO_2010_2017%>%
  summarise(Facilities=n_distinct(Facility.ID), Outfalls=n_distinct(OutfallID),
            Summed_Dis=sum(Measured_Effluent, na.rm = T), Ave_Dis=mean(Measured_Effluent,na.rm=T),
            Med_Dis=median(Measured_Effluent,na.rm=T))
#write.table(VA_Discharge_DMR,paste0(path,"/Detailed_DMR_2010_2018.txt"),sep="\t",row.names=F)

##################################################################################################################################
#########################################################Pre-QA/QC################################################################

#-----------------------------------------------------------------#
#-------------------Investigate Facility Type---------------------#

#---ECHO Defined Facility Type---#
ECHO_2010_2017%>%
  dplyr::group_by(Facility.ID)%>% #860 Facilities
  count(Facility_Type)
n_distinct(Fac_Type_ECHO[Fac_Type_ECHO$Facility_Type=="FEDERAL",])#24
n_distinct(Fac_Type_ECHO[Fac_Type_ECHO$Facility_Type=="POTW",])#290
n_distinct(Fac_Type_ECHO[Fac_Type_ECHO$Facility_Type=="NON-POTW",])#546

#---VPDES Defined Facility Type---#
Fac_Type_VPDES<-ECHO_2010_2017%>%
  dplyr::group_by(Facility.ID)%>% #736 Facilities
  count(Use_Type)
n_distinct(Fac_Type_VPDES[Fac_Type_VPDES$Use_Type=="Municipal",])#512-before
n_distinct(Fac_Type_VPDES[Fac_Type_VPDES$Use_Type=="Industrial",])#210-before 

Fac_Type_VPDES<-ECHO_2010_2017%>%
  dplyr::group_by(Facility.ID)%>% #736 Facilities
  summarise(Facility_Name=first(FacilityName),IssuingAgency=first(Issuing_Agency),PermitType=first(Permit_Type),City=first(City),ECHO_Use_Type=first(Facility_Type), VPDES_Use_Type=first(Use_Type))

#----Buzz Word Frequencies----#
Sector<-subset(Fac_Type_VPDES,subset=VPDES_Use_Type=="Industrial",select = c(2))

corpus <- Corpus(VectorSource(Sector))
skipWords <- function(x) removeWords(x, stopwords("english"))
funcs <- list(tolower, removePunctuation, removeNumbers, stripWhitespace, skipWords)
a <- tm_map(corpus, FUN = tm_reduce, tmFuns = funcs)
a.dtm1 <- TermDocumentMatrix(a, control = list(wordLengths = c(3,10)))
N<-10
findFreqTerms(a.dtm1, N)
m <- as.matrix(a.dtm1)
v <- sort(rowSums(m), decreasing=TRUE)
head(v, N)


#--Reclassify Water Use Sector Based on Buzz Words--#

Fac_Type_VPDES$Reclass_Use_Type<-NA
for (i in 1:length(Fac_Type_VPDES$Facility.ID)){
  if(length(grep('\\bPOWER\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
     length(grep('\\bPOWER STATION\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
     length(grep('\\bCOMBUSTION\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
     length(grep('\\bCLINCH RIVER PLANT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
     length(grep('\\bAPCO\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
     length(grep('\\bENERGY\\b',Fac_Type_VPDES$Facility_Name[i]))>0){
    Fac_Type_VPDES$Reclass_Use_Type[i]<-'Energy'
  }else if (length(grep('\\bMUNICIPAL\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
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
            length(grep('\\bSANITATION\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
            length(grep('\\bWATER RECLAMATION\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
            length(grep('\\bPUMPING\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
            length(grep('\\bWTF\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
            length(grep('\\bSTP\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
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
            length(grep("\\bGOLF COURSE\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
            length(grep("\\bCOUNTRY CLUB\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
            length(grep("\\bCOUNTRY CLB\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
            length(grep("\\bCLUB\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
            length(grep("\\bGOLF\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
            length(grep("\\bCOURSE\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
            length(grep("\\bCHURCH\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
            length(grep("\\bCOMPLEX\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
            length(grep("\\bSCHOOL\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
            length(grep("\\bSCHOOLS\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
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
             length(grep("\\bFISHERIES\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
             length(grep("\\bFISH\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
             length(grep("\\bHATCHERY\\b",Fac_Type_VPDES$Facility_Name[i]))>0|
             length(grep("\\bGREENHOUSE\\b",Fac_Type_VPDES$Facility_Name[i]))>0){
    Fac_Type_VPDES$Reclass_Use_Type[i]<-'Agriculture/Irrigation'
  } else if (length(grep('\\bPLANT\\b',Fac_Type_VPDES$Facility_Name[i]))>0|
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

#If reclassification did not pick up on the pattern recognition, use the VPDES use type 
Fac_Type_VPDES$Reclass_Use_Type<-ifelse(is.na(Fac_Type_VPDES$Reclass_Use_Type),Fac_Type_VPDES$VPDES_Use_Type,Fac_Type_VPDES$Reclass_Use_Type)
#If still NA, base off of if NON-POTW or POTW
Fac_Type_VPDES$Reclass_Use_Type<-ifelse(is.na(Fac_Type_VPDES$Reclass_Use_Type)&Fac_Type_VPDES$ECHO_Use_Type=="NON-POTW","Industrial",Fac_Type_VPDES$Reclass_Use_Type)

#----Apply Reclassification------#
Fac_Type_VPDES_m<-subset(Fac_Type_VPDES,select = c(1,8))
ECHO_2010_2017<-merge(ECHO_2010_2017,Fac_Type_VPDES_m,by="Facility.ID",all.x=T)

#---Plot Facility Type---#

colors_10_16 <- colorspace::diverge_hcl(4)

ECHO_2010_2017%>%
  group_by(Facility.ID)%>%
  summarise(Use_Type=first(Use_Type))%>%
  count(Use_Type)%>%
  plot_ly(labels=~Use_Type,values=~n,
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo= 'text',
          text = ~paste(n, ' Facilities'),
          marker = list(colors=colors_10_16,
                        line=list(color = '#FFFFFF', width =1)))%>%
  add_pie(hole = 0.6)%>%
  layout(title = 'Water Use Categories in VPDES (2010-2017)',
         xaxis = list(showgrid=F, zeroline=F, showticklabels=F),
         yaxis = list(showgrid=F, zeroline=F, showticklabels=F))

ECHO_2010_2017%>%
  group_by(Facility.ID)%>%
  summarise(Reclass_Use_Type=first(Reclass_Use_Type))%>%
  count(Reclass_Use_Type)%>%
  plot_ly(labels=~Reclass_Use_Type,values=~n,
          textposition = 'inside',
          textinfo = 'label+percent',
          insidetextfont = list(color = '#FFFFFF'),
          hoverinfo= 'text',
          text = ~paste(n, ' Facilities'),
          marker = list(colors=colors_10_16,
                        line=list(color = '#FFFFFF', width =1)))%>%
  add_pie(hole = 0.6)%>%
  layout(title = 'Water Use Categories post Reclassification in VPDES (2010-2017)',
         xaxis = list(showgrid=F, zeroline=F, showticklabels=F),
         yaxis = list(showgrid=F, zeroline=F, showticklabels=F))

#-----------------------------------------------------------------#
#--------------------Plot Patterns: Before QA/QC------------------#

#----------------------------------------------#
#--------Summed Discharges over Time----------#

ECHO_2010_2017%>%
  group_by(MP_Begin_Date)%>%
  summarise(Discharges=sum(as.numeric(Measured_Effluent),na.rm=T),
            nFacilities=n_distinct(as.character(Facility.ID)))%>%
  plot_ly(x = ~MP_Begin_Date)%>%
  add_trace(y=~nFacilities, name="Number of Facilities", type='bar',  marker = list(color = '#C9EFF9'))%>%
  add_trace(y = ~Discharges, name="Discharge (MGD)", type = 'scatter', mode = 'lines+markers',line = list(color = '#45171D'), marker= list(color = '#45171D'),yaxis="y2")%>%
  layout(title = "Summed Discharge (MGD) by Monitoring Date from 2010-2017 (Pre QA/QC)",
         yaxis = list(title="Number of Facilities"),
         yaxis2=list(
           tickfont =list(color="red"),
           overlaying = "y",
           side = "right",
           title = "Discharges (MGD)"
         ),
         xaxis = list(title = "Date (Months)"),
         legend =list(x=100,y=-0.2))


#---Year by year analysis---#
Discharge.by.Category.Year<-ECHO_2010_2017%>%
  group_by(Year=(substr(MP_Begin_Date,1,4)),Use_Type=Reclass_Use_Type)%>%
  summarise(Sum=sum(Measured_Effluent,na.rm = T),
            Average=mean(Measured_Effluent,na.rm=T),
            Median=median(Measured_Effluent,na.rm=T),
            Minimum=min(Measured_Effluent,na.rm=T),
            Maximum=max(Measured_Effluent,na.rm=T),
            no_submissions=n())

colors_discharge <- brewer.pal(9, "PuBu")
#-----Median------#

plot_ly(Discharge.by.Category.Year, x = ~Use_Type[1:2], y = ~Median[1:2], type='bar', name="2010", 
          text=c("2010","2010"),
          marker= list(color=colors_discharge[9]))%>%
  add_trace(y = ~Median[3:4], name= "2011", 
            text=c("2011","2011"),
            marker= list(color=colors_discharge[8]))%>%
  add_trace(y = ~Median[5:6], name= "2012", 
            text=c("2012","2012"),
            marker= list(color=colors_discharge[7]))%>%
  add_trace(y = ~Median[7:8], name= "2013",
            text=c("2013","2013"),
            marker= list(color=colors_discharge[6]))%>%
  add_trace(y = ~Median[9:10], name= "2014",
            text=c("2014","2014"),
            marker= list(color=colors_discharge[5]))%>%
  add_trace(y = ~Median[11:12], name= "2015",
            text=c("2015","2015"),
            marker= list(color=colors_discharge[4]))%>%
  add_trace(y = ~Median[13:14], name= "2016",
            text=c("2016","2016"),
            marker= list(color=colors_discharge[3]))%>%
  add_trace(y = ~Median[15:16], name= "2017",
            text=c("2017","2017"),
            marker= list(color=colors_discharge[2]))%>%
  layout(yaxis = list(title='Median Discharge (MGD)'), 
         xaxis = list(title='Water Sector'),
         title = "Median Discharge per Water Sector per Year",
         barmode='stack')

#-----Mean-------#

plot_ly(Discharge.by.Category.Year, x = ~Use_Type[1:2], y = ~Average[1:2], type='bar', name="2010", 
        text=c("2010","2010"),
        marker= list(color=colors_discharge[9]))%>%
  add_trace(y = ~Average[3:4], name= "2011", 
            text=c("2011","2011"),
            marker= list(color=colors_discharge[8]))%>%
  add_trace(y = ~Average[5:6], name= "2012", 
            text=c("2012","2012"),
            marker= list(color=colors_discharge[7]))%>%
  add_trace(y = ~Average[7:8], name= "2013",
            text=c("2013","2013"),
            marker= list(color=colors_discharge[6]))%>%
  add_trace(y = ~Average[9:10], name= "2014",
            text=c("2014","2014"),
            marker= list(color=colors_discharge[5]))%>%
  add_trace(y = ~Average[11:12], name= "2015",
            text=c("2015","2015"),
            marker= list(color=colors_discharge[4]))%>%
  add_trace(y = ~Average[13:14], name= "2016",
            text=c("2016","2016"),
            marker= list(color=colors_discharge[3]))%>%
  add_trace(y = ~Average[15:16], name= "2017",
            text=c("2017","2017"),
            marker= list(color=colors_discharge[2]))%>%
  layout(yaxis = list(title='Mean Discharge (MGD)'), 
         xaxis = list(title='Water Sector'),
         title = "Mean Discharge per Water Sector per Year",
         barmode='stack')



#-----------------------------------------------------------------#
#---------------------------QA/QC---------------------------------#

#---Subset flagged entries in 2010-2016 DMR Dataframe---#
Flagged_DMR_2010_2017<-subset(ECHO_2010_2017,subset= (!is.na(ECHO_2010_2017$fac_flag_zerodesflow))|
                                (!is.na(ECHO_2010_2017$dmr_flag_desflow))|
                                (!is.na(ECHO_2010_2017$dmr_flag_units_100))|
                                (!is.na(ECHO_2010_2017$dmr_flag_units_1000000))|
                                (!(ECHO_2010_2017$Violation_Code=="")))%>%arrange(desc(Std_ME))


#---Assign weights to flags to create a weighted value---#
ECHO_2010_2017<-ECHO_2010_2017%>%add_column(weighted_flag="", .after="dmr_flag_units_1000000")%>%add_column(resolution="",.after="weighted_flag")

ECHO_2010_2017$fac_flag_zerodesflow<-ifelse(!is.na(ECHO_2010_2017$fac_flag_zerodesflow=="fac_flag_zerodesflow"),0.25,0)
ECHO_2010_2017$dmr_flag_units_100<-ifelse(!is.na(ECHO_2010_2017$dmr_flag_units_100=="dmr_flag_units_100"),0.4,0)
ECHO_2010_2017$dmr_flag_units_1000000<-ifelse(!is.na(ECHO_2010_2017$dmr_flag_units_1000000=="dmr_flag_units_1000000"),0.4,0)
ECHO_2010_2017$dmr_flag_desflow<-ifelse(!is.na(ECHO_2010_2017$dmr_flag_desflow=="dmr_flag_desflow"),1,0)

ECHO_2010_2017$weighted_flag<-ECHO_2010_2017$fac_flag_zerodesflow+
                                    ECHO_2010_2017$dmr_flag_desflow+
                                    ECHO_2010_2017$dmr_flag_units_100+
                                    ECHO_2010_2017$dmr_flag_units_1000000

#---If weighted value is greater than 1, set as a high concern for potential skewing---#
ECHO_2010_2017$resolution<-ifelse(ECHO_2010_2017$weighted_flag>=1,1,0)

ECHO_2010_2017.train<-subset(ECHO_2010_2017,select = c(1,2,5,8,11,14,16,17,21,22,23,24,25,26))
ECHO_2010_2017.test<-subset(ECHO_2010_2017,select = c(1,2,5,8,11,14,16,17,21,22,23,24,25,26))


library(FFTrees)    

Flag.fft<-FFTrees(formula = resolution ~ .,
                  data = ECHO_2010_2017.train,
                  data.test = ECHO_2010_2017.test,
                  main = "Flagged Entry Decision Tree",
                  decision.labels = c("Low Concern","High Concern"),
                  do.comp = FALSE)
Flag.fft
plot(Flag.fft, data="test")

ECHO_2010_2017<-ECHO_2010_2017%>%add_column(Resolved_Measured_Effluent_NA="", .after="Measured_Effluent")
ECHO_2010_2017$Resolved_Measured_Effluent_NA<-ifelse(ECHO_2010_2017$resolution==1,NA,ECHO_2010_2017$Measured_Effluent)
ECHO_2010_2017<-ECHO_2010_2017%>%add_column(Resolved_Measured_Effluent_Med="", .after="Resolved_Measured_Effluent_NA")
ECHO_2010_2017$Resolved_Measured_Effluent_Med<-ifelse(ECHO_2010_2017$resolution==1,ECHO_2010_2017$Median_ME,ECHO_2010_2017$Measured_Effluent)

write.table(ECHO_2010_2017,paste0(path,"/ECHO_2010_2017_QAQC.txt"), sep="\t", row.names=F)

rm(ECHO_2010_2017.test,ECHO_2010_2017.train, Use_Type, ECHO_Facility_Attributes)
##################################################################################################################################
#########################################################Post-QA/QC################################################################

#-----------------------------------------------------------------#
#--------------------Plot Patterns: Post QA/QC------------------#

#----------------------------------------------#
#--------Summed Withdrawals over Time----------#


#--Nulled Resolution--#
ECHO_2010_2017%>%
  group_by(MP_Begin_Date)%>%
  summarise(Discharges=sum(as.numeric(Resolved_Measured_Effluent_NA),na.rm=T),
            nFacilities=n_distinct(as.character(Facility.ID)),
            n_na=sum(is.na(Resolved_Measured_Effluent_NA)))%>%
  plot_ly(x = ~MP_Begin_Date)%>%
  add_trace(y=~n_na, name="Number of Nulled Entries", type='bar',  marker = list(color = '#C9EFF9'))%>%
  add_trace(y = ~Discharges, name="Discharge (MGD)", type = 'scatter', mode = 'lines+markers',line = list(color = '#45171D'), marker= list(color = '#45171D'),yaxis="y2")%>%
  layout(title = "Summed Discharge (MGD) by Monitoring Date from 2010-2017 (Post QA/QC)",
         yaxis = list(title="Number of Nulled Entries"),
         yaxis2=list(
           tickfont =list(color="red"),
           overlaying = "y",
           side = "right",
           title = "Discharges (MGD)"
         ),
         xaxis = list(title = "Date (Months)"),
         legend =list(x=100,y=-0.2))

#--Replacing with Median Flow--#
ECHO_2010_2017%>%
  group_by(MP_Begin_Date)%>%
  summarise(Discharges=sum(as.numeric(Resolved_Measured_Effluent_Med),na.rm=T),
            nFacilities=n_distinct(as.character(Facility.ID)),
            n_na=sum(is.na(Resolved_Measured_Effluent_Med)))%>%
  plot_ly(x = ~MP_Begin_Date)%>%
  add_trace(y=~n_na, name="Number of Nulled Entries", type='bar',  marker = list(color = '#C9EFF9'))%>%
  add_trace(y = ~Discharges, name="Discharge (MGD)", type = 'scatter', mode = 'lines+markers',line = list(color = '#45171D'), marker= list(color = '#45171D'),yaxis="y2")%>%
  layout(title = "Summed Discharge (MGD) by Monitoring Date from 2010-2017 (Post QA/QC)",
         yaxis = list(title="Number of Nulled Entries"),
         yaxis2=list(
           tickfont =list(color="red"),
           overlaying = "y",
           side = "right",
           title = "Discharges (MGD)"
         ),
         xaxis = list(title = "Date (Months)"),
         legend =list(x=100,y=-0.2))


#---Year by year analysis---#
Resolved_Discharge.by.Category.Year<-ECHO_2010_2017%>%
  group_by(Year=(substr(MP_Begin_Date,1,4)),Use_Type=Reclass_Use_Type)%>%
  summarise(Sum=sum(Resolved_Measured_Effluent_NA,na.rm = T),
            Average=mean(Resolved_Measured_Effluent_NA,na.rm=T),
            Median=median(Resolved_Measured_Effluent_NA,na.rm=T),
            Minimum=min(Resolved_Measured_Effluent_NA,na.rm=T),
            Maximum=max(Resolved_Measured_Effluent_NA,na.rm=T),
            no_submissions=n())


colors_discharge <- brewer.pal(9, "PuBu")
#-----Median------#

plot_ly(Discharge.by.Category.Year, x = ~Use_Type[1:4], y = ~Median[1:4], type='bar', name="2010", 
        text=c("2010","2010","2010","2010"),
        marker= list(color=colors_discharge[9]))%>%
  add_trace(y = ~Median[5:8], name= "2011", 
            text=c("2011","2011","2011","2011"),
            marker= list(color=colors_discharge[8]))%>%
  add_trace(y = ~Median[9:12], name= "2012", 
            text=c("2012","2012","2012","2012"),
            marker= list(color=colors_discharge[7]))%>%
  add_trace(y = ~Median[13:16], name= "2013",
            text=c("2013","2013","2013","2013"),
            marker= list(color=colors_discharge[6]))%>%
  add_trace(y = ~Median[17:20], name= "2014",
            text=c("2014","2014","2014","2014"),
            marker= list(color=colors_discharge[5]))%>%
  add_trace(y = ~Median[21:25], name= "2015",
            text=c("2015","2015","2015","2015"),
            marker= list(color=colors_discharge[4]))%>%
  add_trace(y = ~Median[13:14], name= "2016",
            text=c("2016","2016"),
            marker= list(color=colors_discharge[3]))%>%
  add_trace(y = ~Median[15:16], name= "2017",
            text=c("2017","2017"),
            marker= list(color=colors_discharge[2]))%>%
  layout(yaxis = list(title='Median Discharge (MGD)'), 
         xaxis = list(title='Water Sector'),
         title = "Median Discharge per Water Sector per Year",
         barmode='stack')

#-----Mean-------#

plot_ly(Discharge.by.Category.Year, x = ~Use_Type[1:2], y = ~Average[1:2], type='bar', name="2010", 
        text=c("2010","2010"),
        marker= list(color=colors_discharge[9]))%>%
  add_trace(y = ~Average[3:4], name= "2011", 
            text=c("2011","2011"),
            marker= list(color=colors_discharge[8]))%>%
  add_trace(y = ~Average[5:6], name= "2012", 
            text=c("2012","2012"),
            marker= list(color=colors_discharge[7]))%>%
  add_trace(y = ~Average[7:8], name= "2013",
            text=c("2013","2013"),
            marker= list(color=colors_discharge[6]))%>%
  add_trace(y = ~Average[9:10], name= "2014",
            text=c("2014","2014"),
            marker= list(color=colors_discharge[5]))%>%
  add_trace(y = ~Average[11:12], name= "2015",
            text=c("2015","2015"),
            marker= list(color=colors_discharge[4]))%>%
  add_trace(y = ~Average[13:14], name= "2016",
            text=c("2016","2016"),
            marker= list(color=colors_discharge[3]))%>%
  add_trace(y = ~Average[15:16], name= "2017",
            text=c("2017","2017"),
            marker= list(color=colors_discharge[2]))%>%
  layout(yaxis = list(title='Mean Discharge (MGD)'), 
         xaxis = list(title='Water Sector'),
         title = "Mean Discharge per Water Sector per Year",
         barmode='stack')

#-----Submissions------#
plot_ly(ECHO_Submissions_2010_2016, x = ~Sector, y = ~report_2010, type='bar', name="2010", 
          text=c("2010","2010"),
          marker= list(color=colors_discharge[9]))%>%
  add_trace(y = ~report_2011, name= "2011", 
            text=c("2011","2011"),
            marker= list(color=colors_discharge[8]))%>%
  add_trace(y = ~report_2012, name= "2012", 
            text=c("2012","2012"),
            marker= list(color=colors_discharge[7]))%>%
  add_trace(y = ~report_2013, name= "2013",
            text=c("2013","2013"),
            marker= list(color=colors_discharge[6]))%>%
  add_trace(y = ~report_2014, name= "2014",
            text=c("2014","2014"),
            marker= list(color=colors_discharge[5]))%>%
  add_trace(y = ~report_2015, name= "2015",
            text=c("2015","2015"),
            marker= list(color=colors_discharge[4]))%>%
  add_trace(y = ~report_2016, name= "2016",
            text=c("2016","2016"),
            marker= list(color=colors_discharge[3]))%>%
  layout(yaxis = list(title='Number of Submissions'), barmode='stack')


