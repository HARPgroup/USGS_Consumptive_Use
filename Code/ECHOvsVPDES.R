#############################################################################################################################
###########################################ECHO vs VPDES#####################################################################

#Author: Morgan McCarthy, M.S. Biological Systems Engineering, Virginia Tech

#############################################Purpose#########################################################################

#This script is intended to compare aggregated flows from CWA regulated facilities in the ECHO Database (ECHOInterface.R) to their respective 
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

#############################################################################################################################
##################################################Inputs#####################################################################

state<-"VA"
path<-"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated"
options(scipen=999) #Disable scientific notation

#############################################################################################################################
##########################################ECHO Discharge Data################################################################

#Aggregated ECHO Discharge Data
FlowFrame<-read.csv(paste0(path,"/FlowFrame_2012_present.csv"),stringsAsFactors = F)
FlowFrame_mon_mgd<-subset(FlowFrame, FlowFrame$Stat=="MK")
#Download statistical codes from ECHO to understand statistics used in FlowFrame
CodeKey<-read.csv("https://echo.epa.gov/system/files/REF_ICIS-NPDES_STATISTICAL_BASE.csv",stringsAsFactors = F,na.strings = 'BLANK')

#Time series ECHO DMR Data. Monthly, Daily, Weekly, Annual Averages. Rare to see averages other than monthyl average. 
ECHO_Discharge<-read.csv("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Documentation/Imports/ECHO_timeseries.csv")

#Query from CWA ECHO REST Services to obtain all discharging facilities within state of interest.
#Specific parameters can be specified in the query to obtain customized results: qcolumns

Req_URL<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&qcolumns=1,2,14,23,24,25,26,27,60,63,65,67,84,91,95,97,204,205,206,207,209,210&passthrough=Y&p_st=",state)
URL_Download<-getURL(Req_URL) #Download URL from above
URL_Parse<-xmlParse(URL_Download)#parses the downloaded XML of facilities and generates an R structure that represents the XML/HTML tree-main goal is to retrieve query ID or QID
QID<-xmlToList(URL_Parse)#Converts parsed query to a more R-like list and stores it as a variable
QID<-QID$QueryID
GET_Facilities<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qcolumns=1,2,14,23,24,25,26,27,60,63,65,67,84,91,95,97,204,205,206,207,209,210&passthrough=Y&qid=",QID)
ECHO_Facilities<-read.csv(GET_Facilities,stringsAsFactors = F)

rm(Req_URL,URL_Download,URL_Parse,GET_Facilities,QID,state)#Remove clutter

#############################################################################################################################
#############################################VPDES Permit Data###############################################################

#This section of code retrieves the coordinates and flow limits for each point source discharging facility with an individual VPDES permit. 
#General permits are written for a general class of discharges, while individual permits are for 
#municipal and industrial facilities with effluent limitations and monitoring requirements.
#Coordinates for each unique outfall is found in the VDEQ's VEGIS Database 
#("http://www.deq.virginia.gov/mapper_ext/GIS_Datasets/"). 
#Permitted flow limits are found on the VDEQ website under Permitting & Compliance 

#######Retrieve Coordinates of VPDES Outfalls#######
temp<-tempfile(fileext = ".zip")
#Locations and attribute data about active outfalls in the State
download.file("http://www.deq.virginia.gov/mapper_ext/GIS_Datasets/VPDES_Geodatabase.zip", destfile = temp)
unzip(temp, exdir = "path")
#Explore what is in VPDES_Geodatabase.gdb
ogrListLayers(paste0(path,"/VPDES_Geodatabase.gdb")) #Two layers: VPDES Outfalls and OpenFileGDB
VPDES_Outfalls<-as.data.frame(readOGR(paste0(path,"/VPDES_Geodatabase.gdb"),layer="VPDES_OUTFALLS"))
names(VPDES_Outfalls)[names(VPDES_Outfalls)=="OUTFALL_ID"]<-'VPDESID'
names(VPDES_Outfalls)[names(VPDES_Outfalls)=="VAP_PMT_NO"]<-'FacilityID'
VPDES_Outfalls<-VPDES_Outfalls[VPDES_Outfalls$VAP_TYPE=='VPDES_IP',] #Narrow down to Individual Permits: which specify effluent limitations for municipal and industrial facilities. 
names(ECHO_Facilities)[names(ECHO_Facilities)=="SourceID"]<-"FacilityID"#Need to rename to give a central columnn name for future joins

#######Retrieve VPDES Individual Permit Info from DEQ Website#######
#Website that contains VPDES Individual Permits: 
#http://www.deq.virginia.gov/Programs/Water/PermittingCompliance/PollutionDischargeElimination/PermitsFees.aspx#GGPs

#Individual Permits updated as of March 2018
#Warnings about unknown or uninitiliased columns: previous IP contact sheets named the columns differently. It doesn't hinder any processes though. 
GET('http://www.deq.virginia.gov/Portals/0/DEQ/Water/PollutionDischargeElimination/VPDES%20Spreadsheets/VPDES%20IP%20Contact%20Flow%20for%20WEB%20March%202018.xls?ver=2018-03-13-170732-267', 
    write_disk(temp <- tempfile(fileext = ".xls")))
VPDES_IP <- read_excel(temp, skip=5)
VPDES_IP<-VPDES_IP[!is.na(VPDES_IP$Facility),]
VPDES_IP$`Design Flow (MGD)`<-as.numeric(VPDES_IP$`Design Flow (MGD)`)
length(unique(VPDES_IP$`Permit Number`))#865 Unique IP's as of March 2018

#However we see we have a lot of duplicate entries for the same permit because of multiple contacts
length(VPDES_IP$Facility[duplicated(VPDES_IP$`Permit Number`)]) #377 duplicates
VPDES_IP<-VPDES_IP[!duplicated(VPDES_IP$`Permit Number`),] #getting rid of duplicates and looking at unique permits

#It's important to note that the individual permit information just includes design and total flow
#We are not sure yet what the effluent limits are.
#note that some facilities report a design flow of 0 MGD...let's mark these and investigate later.

#Summarize the information within the VPDES IP Spreadsheet
VPDES_IP%>%
  summarise(Sum_DesFlow=sum(`Design Flow (MGD)`,na.rm = T),Max_DesFlow=max(`Design Flow (MGD)`,na.rm = T),
            ZeroCount_DesFlow=count(VPDES_IP%>%filter(`Design Flow (MGD)`==0)),
            NACount_DesFlow=length(VPDES_IP$`Design Flow (MGD)`[is.na(VPDES_IP$`Design Flow (MGD)`)]),
            Sum_TotalFlow=sum(`Total Flow`,na.rm = T),Max_TotalFlow=max(`Total Flow`,na.rm = T),
            NACount_TotalFlow=length(VPDES_IP$`Total Flow`[is.na(VPDES_IP$`Total Flow`)]))
            
#23 facilities that report having a design flow of 0 MGD

#############################################################################################################################
###############################################Coordinates for Outfalls######################################################

#Set projections for VPDES Outfall coordinates#
VPDES_Coordinates<-VPDES_Outfalls[,c(15,16)]
VPDES_Coordinates <- proj4::project(VPDES_Coordinates, proj="+proj=merc +a=6378137 +b=6378137 +lat_ts=0.0 +lon_0=0.0 +x_0=0.0 +y_0=0 +k=1.0 +units=m +nadgrids=@null +wktext  +no_defs", inverse=TRUE)

#Replace coordinates in VPDES_Outfalls data frame
VPDES_Outfalls$Longitude<-VPDES_Coordinates$x
VPDES_Outfalls$Latitude<-VPDES_Coordinates$y

#Have seperate data frame with VPDESID and coordinates
VPDES_Coordinates<-subset(VPDES_Outfalls, select=c(1,10,17,18))
colnames(VPDES_Coordinates)<-c("OutfallID","FacilityID","Outfall_Longitude","Outfall_Latitude")

#Attach Outfall Coordinates to ECHO_Discharge 
ECHO_Discharge$OutfallID<-gsub("echo_","", as.character(ECHO_Discharge$OutfallID))
ECHO_Discharge<-subset(ECHO_Discharge,select=-c(1))
ECHO_Discharge<-merge.data.frame(ECHO_Discharge,VPDES_Coordinates,by="OutfallID",all.x=T) #Keeps all outfalls even if doesn't match in VPDES IP

#Attach Facility Coordinates to ECHO_Discharge
ECHO_Facilities_Coordinates<-subset(ECHO_Facilities, select = c(2,4,5))
ECHO_Discharge<-merge.data.frame(ECHO_Discharge,ECHO_Facilities_Coordinates,by="FacilityID",all.x=T)
sum(is.na(ECHO_Discharge$FacilityID))#4463 NA facility ID's because ECHO includes discharging facilities on outskirts of VA, while VPDES IP spreadsheet only contains those in VA

#Now we have a data frame containing time series data for each outfall located in ECHO with outfall and facility coordinates. 

#############################################################################################################################
###############################################QA/QC and Flagging############################################################

##################################################################
####################Design Flow = 0###############################

#Merge VPDES Design Flows with ECHO_Discharge based on permit number 
VPDES_DesignFlow<-subset(VPDES_IP,select=c(3,15))
colnames(VPDES_DesignFlow)<-c("FacilityID","DesignFlow_mgd")
VA_Discharge_DMR<-merge.data.frame(ECHO_Discharge,VPDES_DesignFlow,by="FacilityID",all.x=T)
VA_Discharge_DMR<-subset(VA_Discharge_DMR,select=c(1,3,15,14,2,12,13,16,5,6,7,8,9,10,11))

#Flag facilities that have a design flow of 0 MGD
VA_Discharge_DMR$VPDES_flag_desflow<-ifelse(VA_Discharge_DMR$DesignFlow_mgd==0,"VPDES_flag_desflow",NA)
Flagged_Zero_DesFlow<-subset(VA_Discharge_DMR,subset=VA_Discharge_DMR$DesignFlow_mgd==0,
                             select=-c(14,15))%>%arrange(desc(Measured_Effluent-DesignFlow_mgd))
#Summarize time series data by facility 
Zero_desflow<-Flagged_Zero_DesFlow%>%
  group_by(FacilityName)%>%
  summarise(Design_Flow=mean(DesignFlow_mgd,na.rm=T), ECHO_Limit=mean(Permitted_Limit,na.rm = T),Max_ME=max(Measured_Effluent,na.rm=T),
            Min_ME=min(Measured_Effluent,na.rm=T),Mean_ME=mean(Measured_Effluent,na.rm = T),
            Median_ME=median(Measured_Effluent,na.rm=T), STD_ME=sd(Measured_Effluent,na.rm=T))

#write.table(Flagged_Zero_DesFlow,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/Flagged_Zero_DesFlow.txt",sep="\t",row.names = F)
#write.csv(Zero_desflow,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/QA_QC/Zero_desflow.csv")

##################################################################
###########ECHO Measured Effluents > VPDES Design flow###########

#Flag facilities that report measured effluent greater than the design flow 
VA_Discharge_DMR$dmr_flag_desflow<-ifelse(VA_Discharge_DMR$Measured_Effluent>VA_Discharge_DMR$DesignFlow_mgd,"dmr_flag_desflow",NA)
length(which(VA_Discharge_DMR$dmr_flag_desflow=="dmr_flag_desflow")) #2456 flags

#Sort by largest difference between the measured effluent and the design flow
Flagged_Design_Flow<-subset(VA_Discharge_DMR,subset=VA_Discharge_DMR$dmr_flag_desflow=="dmr_flag_desflow",
                            select=-c(14,15,16))%>%arrange(desc(Measured_Effluent-DesignFlow_mgd)) 



#Summarize the time series data to see the overall facilties that are reporting larger discharge amounts than their design flow permit
#Sort by difference between the standard deviation of reported effluent and the supposed Design Flow. 
MEgreaterDF<-Flagged_Design_Flow%>%
  group_by(FacilityName)%>%
  summarise(Max_ME=max(Measured_Effluent, na.rm = T),Min_ME=min(Measured_Effluent,na.rm = T),
            Mean_ME=mean(Measured_Effluent, na.rm = T),Median_ME=median(Measured_Effluent, na.rm = T),
            STD_ME=sd(Measured_Effluent,na.rm = T),Design_Flow=median(DesignFlow_mgd),
            ECHO_Limit=median(Permitted_Limit,na.rm = T))%>%arrange(desc(STD_ME-Design_Flow))

#write.table(Flagged_Design_Flow,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/Flagged_Exceeds_DesFlow.txt",sep="\t",row.names = F)
#write.csv(MEgreaterDF,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/QA_QC/MEgreaterDF.csv")

##################################################################
######Measured Effluents > 100*Median Measured Effluent###########

#Summmarize measured effluent values from ECHO by OutfallID#
VA_Discharge_DMR_Summary<-VA_Discharge_DMR%>%
  group_by(OutfallID)%>%
  summarise(Mean_ME=mean(Measured_Effluent, na.rm = T),Median_ME=median(Measured_Effluent, na.rm = T),Std_ME=sd(Measured_Effluent, na.rm = T))

VA_Discharge_DMR<-merge.data.frame(VA_Discharge_DMR,VA_Discharge_DMR_Summary,by="OutfallID",all.x=T)

#Flag facilities that report measured effluents greater than 100 times the median value
#This may mean there is a potential unit conversion error 
VA_Discharge_DMR$dmr_flag_units<-ifelse(VA_Discharge_DMR$Measured_Effluent>100*VA_Discharge_DMR$Median_ME,"dmr_flag_units",NA)

Flagged_Units<-subset(VA_Discharge_DMR,subset=VA_Discharge_DMR$dmr_flag_units=="dmr_flag_units",
                      select=-c(14,15,16,17))%>%arrange(desc(Measured_Effluent-Median_ME))
Flagged_Units<-Flagged_Units[,c(2,3,4,5,1,6,7,8,9,11,14,15,16,10,12,13,17)]


Flagged_Units_Summarized<-Flagged_Units%>%
  group_by(FacilityName)%>%
  summarise(Max_ME=max(Measured_Effluent,na.rm=T),Min_ME=min(Measured_Effluent,na.rm = T),
            Mean_ME=mean(Measured_Effluent, na.rm = T),Median_ME=median(Measured_Effluent, na.rm = T),
            Std_ME=sd(Measured_Effluent, na.rm = T))%>%arrange(desc(Std_ME))

#write.table(Flagged_Units,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/Flagged_Exceeds_100Median.txt",sep="\t",row.names = F)
#write.csv(Flagged_Units_Summarized,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/QA_QC/Flagged_Units_Summarized.csv")

##################################################################
#######################ECHO Adminstered Flags#####################

#The Effluent Charts located on the ECHO RESt Services present effluent limits, releases,
#and violations over time for the CWA wastewater discharge permits issued by NPDES.

#There are codes that describe the type of alleged effluent or monitoring/reporting violation:
        #E90 - effluent violation
        #D90 - DMR overdue, with a numeric Limit
        #D80 - DMR Overdue, monitoring only required

Flagged_ECHO_Violations<-subset(VA_Discharge_DMR,subset = !(Violation_Code==""), 
                        select=c(2,3,4,5,1,6,7,8,9,11,10,12,13,14,15))

Flagged_ECHO_Violations$Violation_Code<-as.character(Flagged_ECHO_Violations$Violation_Code)
count(Flagged_ECHO_Violations,Violation_Code)

#The Water Facility Search ECHO REST Services also provides a myriad of attributes that act as flags. 
#This includes the facility's current compliance status under the Clean Water Act (CWP_STATUS),
#the number of instances where effluent limits have been exceeded in the past 3 years(CWP_E90_CNT), 
#number of inspections/compliance evaluations, under the corresponding statute, occurring at the facility within the last five years (CWP_INSPECTION_COUNT),
#and the date on which the most recent inspection of the facility took place (CWP_DATE_LAST_INSPECTION).

ECHO_Facility_Violations<-subset(ECHO_Facilities,select=c(2,13,14,15,16))

Flagged_ECHO_Violations<-merge.data.frame(Flagged_ECHO_Violations,ECHO_Facility_Violations,by='FacilityID',all.x=T)

ECHO_Violations_Summary<-Flagged_ECHO_Violations%>%
  group_by(FacilityName)%>%
  summarise(Violation_Count=n(),Violation_Code=first(Violation_Code),
            Violation_Severity=median(Violation_Severity),SumCWPE90Cnt=sum(CWPE90Cnt),
            SumCWPInspectionCount=sum(CWPInspectionCount), DateLastInspection=last(CWPDateLastInspection),
            LastCWPStatus=last(CWPStatus),ECHO_Limits=median(Permitted_Limit),
            Median_ME=median(Measured_Effluent, na.rm = T), Std_ME=sd(Measured_Effluent, na.rm = T))%>%arrange(desc(Violation_Count))


#write.table(Flagged_ECHO_Violations,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/Flagged_ECHO_Violations.txt",sep="\t",row.names = F)
#write.csv(ECHO_Violations_Summary,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/QA_QC/ECHO_Violations_Summary.csv")     

VA_Discharge_DMR<-VA_Discharge_DMR[,c(2,3,4,5,1,6,7,8,9,18,19,20,11,10,12,13,14,15,16,17,21)]

save.image("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/ECHOvsVPDES.RData")
