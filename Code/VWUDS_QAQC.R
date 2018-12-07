###########################################################################################################################################
###############################################Virginia Water Use Database System (VWUDS)##################################################

# This code serves to import and analyze withdrawal data from the Virginia Water Use Database System (VWUDS).
# VWUDS is maintained by Virginia's Department of Environmental Qualtiy (VDEQ) and is self reported by water users. 
# The DEQ only requires users to obtain a permit if 300,000 gallons or more are withdrawn per month from either surface or groundwater sources (White, 2016). 

# Since the water withdrawals are self reported, QA/QC measures need to be taken. The main source of error is centered on source coordinates.
#Sources are similar to outfalls; structures that withdraw water from either surface or groundwater.

# This script first identifies suspicious source coordinates (from 1982-2015) and merges manual corrections to those flagged data points.
# Coordinates that were deemed suspicious:
      #a. did not fit within the bounding box of VA, 
      #b. had switched negative signs/lacked negative signs for longitude
      #c. were missing and were found using google maps/earth
      #(Manual Corrections were performed during Fall of 2017)

# The second part of the script introduces monthly withdrawal data from 1982-2017 that was collected August of 2018.
# It investigates potential interal corrections by VDEQ staff and refromats for future consumptive use calculations in the future.

# http://deq1.bse.vt.edu/d.bet/admin/structure/views/view/vwuds_monthly_wateruse/edit

##################################################Library Initilization########################################################################
setwd('G:/My Drive/GRA/VWUDS Data/VAHydro VWUDS Anlaysis')
path<-('G:/My Drive/GRA/VWUDS Data/')

library(httr)
library(XML)
library(dplyr)
library(lubridate)
library(data.table)
library(geosphere)
library(daff)
library(plotly)
library(RColorBrewer)
library(shiny)
library(stringr)
library(tibble)
library(Hmisc)
library(tm)

options(scipen=999) #Disable scientific notation
options(digits=9) #Digits to 9 decimal points

###########################################################################################################################################
###################################################Data Dump VWUDS 1982-2015###############################################################

#############Pre-Manual Correction of Coordinates#############
VWUDS_1982_2015<-read.table(paste0(path,"VWUDS Monthly Withdrawals Uncorrected 1982_2015.txt"), header=T, sep=",")
VWUDS_1982_2015<-as.data.table(VWUDS_1982_2015) #Since we are dealing with a large dataset, processing time is large---change to data.table 

#Investigate Coordinates of Withdrawing Facilities#
#Bounding Box for Virginia 
#        min       max
#x -83.67539 -75.16643
#y  36.54076  39.46601
VWUDS_1982_2015$vwuds_flag_coord<-ifelse(VWUDS_1982_2015$Latitude>39.46601|
                                           VWUDS_1982_2015$Latitude<36.54076|
                                           VWUDS_1982_2015$Longitude<(-83.67539)|
                                           VWUDS_1982_2015$Longitude>(-75.16643)|
                                           is.na(VWUDS_1982_2015$Latitude)|
                                           is.na(VWUDS_1982_2015$Longitude),
                                         "vwuds_flag_coord","")
#Arrange by ascending DEQ Source ID
VWUDS_1982_2015<-arrange(VWUDS_1982_2015,DEQ.ID.of.Source)

#Group timeseries data by Source ID to summarise coordinates
VWUDS_1982_2015_Summary<-VWUDS_1982_2015%>%
  group_by(DEQ.ID.of.Source)%>%
  summarise(FacilityName=first(Facility),
            Latitude=mean(Latitude),Longitude=mean(Longitude),
            vwuds_flag_coord=first(vwuds_flag_coord))

#Now isolate those suspicious coordinates into a separate data table
VWUDS_Flag_Coord_82_15<-subset(VWUDS_1982_2015,subset= !(vwuds_flag_coord==""))
VWUDS_Flag_Coord_82_15<-
  VWUDS_Flag_Coord_82_15%>%
  group_by(DEQ.ID.of.Source)%>%
  summarise(FacilityName=first(Facility),FacilityID=mean(Facility.ID),
            Latitude=mean(Latitude),Longitude=mean(Longitude),
            flag=first(vwuds_flag_coord), Mean_Flow=mean(Million.Gallons.Month),
            Median_Flow=median(Million.Gallons.Month))%>%arrange(DEQ.ID.of.Source)

#Of the 6781 unique withdrawing sources, 1521 are flagged for suspicious coordinates

#############Post-Manual Correction of Coordinates#############

#Download list of source coordinates that contain manual corrections for 1524 sources
Corrected_Coordinates<-read.table(paste0(path,"VWUDS Monthly Withdrawals 1982_2015.txt"), header=T, sep=",")
Corrected_Coordinates<-as.data.table(Corrected_Coordinates)

#Group by source for future merge
Corrected_Coordinates_summary<-Corrected_Coordinates%>%
  subset(select=c(1,8,9))%>%arrange(DEQ_ID)
colnames(Corrected_Coordinates_summary)<-(c("DEQ.ID.of.Source","Corrected Latitude","Corrected Longitude"))

Corrected_Coordinates_summary<-Corrected_Coordinates_summary%>%
  group_by(DEQ.ID.of.Source)%>%
  summarise(Corrected_Latitude=mean(`Corrected Latitude`),Corrected_Longitude=mean(`Corrected Longitude`))%>%arrange(DEQ.ID.of.Source)

##############################################################
########Merge pre and post correction data tables#############

VWUDS_1982_2015<-merge(VWUDS_1982_2015,Corrected_Coordinates_summary,by="DEQ.ID.of.Source",all.x=T)
VWUDS_1982_2015_Summary<-merge(VWUDS_1982_2015_Summary,Corrected_Coordinates_summary,by="DEQ.ID.of.Source",all.x=T) #1521 flagged sources

#Now we have a data table including monthly withdrawals including pre and post corrected coordinates including a flag for VWUDS. 
rm(Corrected_Coordinates,VWUDS_1982_2015,VWUDS_1982_2015_Summary)

###########################################################################################################################################
###################################################Monthly Withdrawal Data 2010-2017#######################################################

#Here we are comparing withdrawal data from two different view from within VAHydro. We want to make sure that the data represented 
#is uniform across all platforms. 

######################################################################################
###############Download Data from VWUDS Monthly Water Use View########################

#Name of View: VWUDS Monthly Water Use (DH Feature) 

#Link to this View: http://deq1.bse.vt.edu/d.dh/vwuds-monthly-water-use?hydrocode=&hydroid=&name_op=%3D&name=&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=2010-01-01&tstime%5Bmax%5D=2017-12-31&bundle%5B%5D=well&bundle%5B%5D=intake&fstatus_1_op=not+in&fstatus_1%5B%5D=duplicate&name_1_op=%3D&name_1=&propcode_op=%3D&propcode=&ftype_op=%3D&ftype=
VWUDS_2010_2017<-read.csv("G:/My Drive/VWUDS Data/vwuds_monthly_wateruse.csv", sep=",", header=T)
VWUDS_2010_2017<-as.data.table(VWUDS_2010_2017)
names(VWUDS_2010_2017)[2]<-c("Facility.ID")
VWUDS_2010_2017<-subset(VWUDS_2010_2017,select=-c(3,4,15,16))

#---------------------------------------------#
#---------Set Data Type for Attributes--------#
VWUDS_2010_2017$Million.Gallons.Month<-as.numeric(gsub(",", "", as.character(VWUDS_2010_2017$Million.Gallons.Month)))
VWUDS_2010_2017$Date<-as.character(paste0("01-",VWUDS_2010_2017$Date))
VWUDS_2010_2017$Date<-as.Date(VWUDS_2010_2017$Date, format="%d-%b-%y")

VWUDS_2010_2017<-VWUDS_2010_2017[order(VWUDS_2010_2017[,1],VWUDS_2010_2017[,5],decreasing=F),]

#---------------------------------------------#
#----------Remove Duplicated Entries----------#

duplicate<-VWUDS_2010_2017[duplicated(VWUDS_2010_2017, by=c("DEQ.ID.of.Source","Date","Million.Gallons.Month")),]
VWUDS_2010_2017<-VWUDS_2010_2017[!duplicated(VWUDS_2010_2017, by=c("DEQ.ID.of.Source","Date","Million.Gallons.Month")),]


# There is still a presence of duplicate entries for as source in a single month (~1045 entries). 
# They are also reporting a value of 0.0.
# Therefore, apply sum of those values to reduce data. 
count(VWUDS_2010_2017[duplicated(VWUDS_2010_2017, by=c("DEQ.ID.of.Source","Date")),])

VWUDS_2010_2017<-VWUDS_2010_2017[, lapply(.SD,sum),  by=list(DEQ.ID.of.Source,Facility.ID,Facility,Source.Name,Date,Facility.Status,Use.Type,Locality,Latitude,Longitude,Source.Type),
                                 .SDcols=c(6)]

#---------------------------------------------#
#--------Summarise Dataframe------------------#

VWUDS_2010_2017%>%
  summarise(no_Facilities=n_distinct(Facility.ID),no_sources=n_distinct(DEQ.ID.of.Source),
            Sum_Withdrawals=sum(Million.Gallons.Month,na.rm=T)/12,Mean_Withdrawal=mean(Million.Gallons.Month,na.rm=T),
            Median_Withdrawal=median(Million.Gallons.Month,na.rm=T),Range=(max(Date)-min(Date)))

#---------------------------------------------#
#------------Convert MGM to MGD---------------#
VWUDS_2010_2017<-VWUDS_2010_2017%>%add_column(Withdrawals_MGD=NA, .after="Million.Gallons.Month")

# Divide by number of days in specific month to account for leap years in 2012 and 2016 #
VWUDS_2010_2017$Withdrawals_MGD<-
  ifelse(!(is.na(VWUDS_2010_2017$Million.Gallons.Month)),
         VWUDS_2010_2017$Million.Gallons.Month/monthDays(VWUDS_2010_2017$Date),NA)

#---------------------------------------------#
#---------Check Flagged Coordinates-----------#
VWUDS_2010_2017$vwuds_flag_coord<-ifelse(VWUDS_2010_2017$Latitude>39.46601|
                                           VWUDS_2010_2017$Latitude<36.54076|
                                            VWUDS_2010_2017$Longitude<(-83.67539)|
                                            VWUDS_2010_2017$Longitude>(-75.16643)|
                                            is.na(VWUDS_2010_2017$Latitude)|
                                            is.na(VWUDS_2010_2017$Longitude),
                                          "vwuds_flag_coord","")

#971 Flagged Sources with suspicious Coordinates
Flagged_VWUDS_2010_2017<-subset(VWUDS_2010_2017,subset= !(vwuds_flag_coord==""))
Flagged_VWUDS_2010_2017<-
  Flagged_VWUDS_2010_2017%>%
  group_by(DEQ.ID.of.Source)%>%
  summarise(FacilityName=first(Facility),
            Latitude=mean(Latitude),Longitude=mean(Longitude),
            flag=first(vwuds_flag_coord))%>%arrange(DEQ.ID.of.Source)

#---------------------------------------------#
#---Merge Manual Corrections of Coordinates---#

# Note: All of these coordinates were manually corrected with the use of google maps
corr_PSC<-read.csv(paste0(path,"Post_Suspicious_Coordinates_2.csv"), header=T)
corr_PSC<-subset(corr_PSC, select=c(1,5,6))
corr_PSC_2<-read.csv(paste0(path,"PSC_3.csv"), header=T)
corr_PSC_2<-subset(corr_PSC_2, select=c(2,6,7))
corr_PSC_3<-read.csv(paste0(path,"PSC4.csv"), header=T)
corr_PSC_3<-subset(corr_PSC_3, select=c(2,6,7))
corr_coordinates<-rbind(Corrected_Coordinates_summary,corr_PSC)
corr_coordinates<-rbind(corr_coordinates,corr_PSC_2)
corr_coordinates<-rbind(corr_coordinates,corr_PSC_3)
corr_coordinates<-corr_coordinates[!duplicated(corr_coordinates),]
corr_coordinates<-as.data.table(corr_coordinates)

#If all.x, extra rows are added--make sure to get rid of NA rows after this step
VWUDS_2010_2017<-merge(VWUDS_2010_2017,corr_coordinates,by="DEQ.ID.of.Source",all.x=T)

#Need to assign coordinates for the sources not included in 1982-2015 corrected lat and longs. If NA, use provided coordinates. If not, leave it alone. 
VWUDS_2010_2017$Corrected_Latitude<-ifelse(is.na(VWUDS_2010_2017$Corrected_Latitude),VWUDS_2010_2017$Latitude,VWUDS_2010_2017$Corrected_Latitude)
VWUDS_2010_2017$Corrected_Longitude<-ifelse(is.na(VWUDS_2010_2017$Corrected_Longitude),VWUDS_2010_2017$Longitude,VWUDS_2010_2017$Corrected_Longitude)

Post_Suspicious_Coordinates<-subset(VWUDS_2010_2017, subset=VWUDS_2010_2017$Corrected_Latitude>39.46601|
                                      VWUDS_2010_2017$Corrected_Latitude<36.54076|
                                      VWUDS_2010_2017$Corrected_Longitude<(-83.67539)|
                                      VWUDS_2010_2017$Corrected_Longitude>(-75.16643)|
                                      is.na(VWUDS_2010_2017$Corrected_Latitude)|
                                      is.na(VWUDS_2010_2017$Corrected_Longitude))

Post_Suspicious_Coordinates<-
  Post_Suspicious_Coordinates%>%
  group_by(DEQ.ID.of.Source)%>%
  summarise(Facility=first(Facility),Latitude=first(Latitude),Longitude=first(Longitude),
            Corrected_Latitude=first(Corrected_Latitude), Corrected_Longitude=first(Corrected_Longitude))


rm(Post_Suspicious_Coordinates,corr_PSC,corr_PSC_2,corr_PSC_3,corr_coordinates)
#---------------------------------------------#
#-----------Remove Hydropower-----------------#

#Why? regulations changed around 2010 and now they report to another source. They are considered less consumptive
VWUDS_2010_2017[VWUDS_2010_2017$Use.Type=="hydropower",]%>%
  summarise(no_Facilities=n_distinct(Facility),no_sources=n_distinct(DEQ.ID.of.Source),
            Sum_Withdrawals=sum(Million.Gallons.Month),Mean_Withdrawal=mean(Million.Gallons.Month),
            Median_Withdrawal=median((Million.Gallons.Month)),Range=(max(Date)-min(Date)))

VWUDS_2010_2017<-VWUDS_2010_2017[!VWUDS_2010_2017$Use.Type=="hydropower",]

#---------------------------------------------#
#------------Reclassify Use Type--------------#

#Reclassify water use types into:
#Agriculture/Irrigation
#Industrial
#Commercial
#Energy
#Municipal

VWUDS_2010_2017$Facility<-toupper(VWUDS_2010_2017$Facility)
Wateruse<-VWUDS_2010_2017%>%
  group_by(Facility.ID)%>%
  summarise(Facility_Name=first(as.character(Facility)),Facility_Type=first(as.character(Use.Type)))

#---Word Frequency----#

# Go through each sector to find frequent terms in each facility name
Sector<-subset(Wateruse,subset=Facility_Type=="industrial",select = c(2))
# Sector<-subset(Wateruse,subset=Facility_Type=="agriculture",select = c(2))
# Sector<-subset(Wateruse,subset=Facility_Type=="municipal",select = c(2))
# Sector<-subset(Wateruse,subset=Facility_Type=="fossilpower",select = c(2))
# Sector<-subset(Wateruse,subset=Facility_Type=="nuclearpower",select = c(2))
# Sector<-subset(Wateruse,subset=Facility_Type=="commercial",select = c(2))
# Sector<-subset(Wateruse,subset=Facility_Type=="mining",select = c(2))
# Sector<-subset(Wateruse,subset=Facility_Type=="manufacturing",select = c(2))

corpus <- Corpus(VectorSource(Sector))
skipWords <- function(x) removeWords(x, stopwords("english"))
funcs <- list(tolower, removePunctuation, removeNumbers, stripWhitespace, skipWords)
a <- tm_map(corpus, FUN = tm_reduce, tmFuns = funcs)
a.dtm1 <- TermDocumentMatrix(a, control = list(wordLengths = c(3,10)))
N<-20
findFreqTerms(a.dtm1, lowfreq=0, highfreq=Inf)
m <- as.matrix(a.dtm1)
v <- sort(rowSums(m), decreasing=TRUE)
head(v, N)

#----------------------#

Wateruse$Reclass_Use_Type<-NA
for (i in 1:length(Wateruse$Facility.ID)){
  if(length(grep('\\bPOWER\\b',Wateruse$Facility_Name[i]))>0|
     length(grep('\\bPOWER STATION\\b',Wateruse$Facility_Name[i]))>0|
     length(grep('\\bCOMBUSTION\\b',Wateruse$Facility_Name[i]))>0|
     length(grep('\\bCLINCH RIVER PLANT\\b',Wateruse$Facility_Name[i]))>0|
     length(grep('\\bENERGY\\b',Wateruse$Facility_Name[i]))>0){
    Wateruse$Reclass_Use_Type[i]<-'Energy'
  }else if (length(grep('\\bMUNICIPAL\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bSERVICE AREA\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bSERV AREA\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bREGIONAL WATER SYSTEM\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bWASTE WATER\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bWWTP\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bWWTF\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bWASTEWATE\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bWT PLANT\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bWATER TREATMENT PLANT\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bSEWAGE TREATMENT PLAN\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bSEWAGE TREATMENT\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bSEWAGE TREATMENT PLANT\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bSANITATION\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bWATER RECLAMATION\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bPUMPING\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bWTF\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bSTP\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bWATER WORKS\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bWATER UTILITIES\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bWWTREAT PLANT\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bTRICKLING FILTER\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bPARK WATER SYSTEM\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bWATER TREATMEN\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bWATER TREATMENT PL\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bPOLLUTION CONTROL\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bPOLLUTION CONTR\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bCENTRAL SYSTEM\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bCOMBINED SEW SYSTEM\\b',Wateruse$Facility_Name[i]))>0|
            length(grep('\\bCOMBINED SEWER SYSTEM\\b',Wateruse$Facility_Name[i]))>0|
            length(grep("\\bMS4\\b",Wateruse$Facility_Name[i]))>0| #Municipal Separate Strom Sewer System
            length(grep("\\bTRAILER\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bMOBILE HOME\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bTRACT\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCOMMUNITY\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bHOMES\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bAPARTMENTS\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bSUBDIVISION\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bESTATES\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bESTATE\\b",Wateruse$Facility_Name[i]))>0|
            length(grep('\\bAUTHORITY\\b',Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCITY OF\\b",Wateruse$Facility_Name[i]))>0){
    Wateruse$Reclass_Use_Type[i]<-'Municipal'
  }else if (length(grep('\\bAIRPORT\\b',Wateruse$Facility_Name[i]))>0|
            length(grep("\\bGOLF COURSE\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCOUNTRY CLUB\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCOUNTRY CLB\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCLUB\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bGOLF\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCOURSE\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCHURCH\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCOMPLEX\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bSCHOOL\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bSCHOOLS\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bTRAINING CENTER\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bLEARNING CENTER\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bELEMENTARY\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bINSTITUTE\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCOURTHOUSE\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bSPACE FLIGHT CENTER\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bEDUCATIONAL\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCEMETERY\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bREST AREA\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bRENTALS\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bINN\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bMUSEUM\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bUNIVERSITY\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bHOSPITAL\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bRESTAURANT\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bBUILDING\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCORRECTION CENTER\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bDETENTION CENTER\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCORRECTIONAL\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bREHABILITATION\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCAMPGROUND\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCORRECTION UNIT\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bSTATE PARK\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bDEPARTMENT OF LABOR\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bRESORT\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bYMCA\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bBUSCH GARDENS\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bRETREAT\\b",Wateruse$Facility_Name[i]))>0|
            length(grep("\\bCAR WASH\\b",Wateruse$Facility_Name[i]))>0){
    Wateruse$Reclass_Use_Type[i]<-'Commercial'
    } else if (length(grep('\\bFARM\\b',Wateruse$Facility_Name[i]))>0|
        length(grep('\\bORNAMENTALS\\b',Wateruse$Facility_Name[i]))>0|
        length(grep('\\bIRRIGATION\\b',Wateruse$Facility_Name[i]))>0|
        length(grep('\\bPRODUCE\\b',Wateruse$Facility_Name[i]))>0|
        length(grep('\\bLAWN\\b',Wateruse$Facility_Name[i]))>0|
        length(grep('\\bCENTER PIVOT\\b',Wateruse$Facility_Name[i]))>0|
        length(grep('\\bHOG\\b',Wateruse$Facility_Name[i]))>0|
        length(grep('\\bDAIRY\\b',Wateruse$Facility_Name[i]))>0|
        length(grep('\\bORCHARD\\b',Wateruse$Facility_Name[i]))>0|
        length(grep('\\bNURSERY\\b',Wateruse$Facility_Name[i]))>0|
        length(grep('\\bNURSERIES\\b',Wateruse$Facility_Name[i]))>0|
        length(grep('\\bVINEYARD\\b',Wateruse$Facility_Name[i]))>0|
        length(grep("\\bFISHERIES\\b",Wateruse$Facility_Name[i]))>0|
        length(grep("\\bFISH\\b",Wateruse$Facility_Name[i]))>0|
        length(grep("\\bHATCHERY\\b",Wateruse$Facility_Name[i]))>0|
        length(grep("\\bGREENHOUSE\\b",Wateruse$Facility_Name[i]))>0){
      Wateruse$Reclass_Use_Type[i]<-'Agriculture/Irrigation'
} else if (length(grep('\\bPLANT\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bPAPER\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bCONCRETE\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bSAND AND GRAVEL\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bAMMUNITION\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bFACILITY\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bTERMINALS\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bLUMBER\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bCONCENTRATOR\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bCONSTRUCTION\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bQUARRY\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bPLT\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bMOTORS\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bOVENS\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bPRODUCTS\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bTIMBER\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bCHEMICAL\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bINDUSTRIES\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bINDUSTRIAL\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bINDUSTRIAL PARK\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bDEVELOPMENT\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bWAREHOUSE\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bMANUFACTURING\\b',Wateruse$Facility_Name[i]))>0|
           length(grep("\\bLANDFILL\\b",Wateruse$Facility_Name[i]))>0|
           length(grep('\\bBREWERY\\b',Wateruse$Facility_Name[i]))>0|
           length(grep('\\bPURINA\\b',Wateruse$Facility_Name[i]))>0){
  Wateruse$Reclass_Use_Type[i]<-'Industrial'
}else{
  Wateruse$Reclass_Use_Type[i]<-NA
}
}

# Cluster Default Classifications into Five Sectors
for (i in 1:length(Wateruse$Facility.ID)){
  if (length(grep('agriculture',Wateruse$Facility_Type[i]))>0|
      length(grep('Agriculture',Wateruse$Facility_Type[i]))>0|
      length(grep('irrigation',Wateruse$Facility_Type[i]))>0|
      length(grep('Irrigation',Wateruse$Facility_Type[i]))>0){
    Wateruse$Facility_Type[i]<-'Agriculture/Irrigation'
  }
  else if (length(grep('industrial',Wateruse$Facility_Type[i]))>0|
           length(grep('manufacturing',Wateruse$Facility_Type[i]))>0|
           length(grep('mining',Wateruse$Facility_Type[i]))>0){
    Wateruse$Facility_Type[i]<-'Industrial'
  }
  else if (
    length(grep('commercial',Wateruse$Facility_Type[i]))>0){
    Wateruse$Facility_Type[i]<-'Commercial'
  }
  else if (length(grep('fossilpower',Wateruse$Facility_Type[i]))>0|
           length(grep('nuclearpower',Wateruse$Facility_Type[i]))>0){
    Wateruse$Facility_Type[i]<-'Energy'
  }
  else if (length(grep('municipal',Wateruse$Facility_Type[i]))>0|
           length(grep('public water supply',Wateruse$Facility_Type[i]))>0|
           length(grep('unknown',Wateruse$Facility_Type[i]))>0|
           length(grep('Public Water Supply',Wateruse$Facility_Type[i]))>0){
    Wateruse$Facility_Type[i]<-'Municipal'
  }
  print(paste("Facility ID ",i," of ", length(Wateruse$Facility.ID)))
}


Wateruse$Reclass_Use_Type<-ifelse(is.na(Wateruse$Reclass_Use_Type),Wateruse$Facility_Type,Wateruse$Reclass_Use_Type)


# This Section was Used to Test the Coverage and Accuracy of the Rules
#Wateruse<-Wateruse%>%group_by(Facility_Name)%>%summarise(Facility_Type=first(Facility_Type),Reclass=first(Reclass_Use_Type))
#Wateruse_pre<-subset(Wateruse,select = -c(4))
#Wateruse_post<-subset(Wateruse,select = -c(3))
#colnames(Wateruse_post)<-c("Facility_Name","Facility_Type")
# count(Wateruse,Reclass_Use_Type)
# count(Wateruse,Facility_Type)
# 
# Classify_diff<-diff_data(Wateruse_pre,Wateruse_post, show_unchanged=FALSE, show_unchanged_meta=FALSE,
#                              ignore_whitespace=T,unchanged_context=0)
# write_diff(Classify_diff, "Classify_diff.csv")
# summary(Classify_diff)
# render_diff(Classify_diff, title="Difference in Water Sector Classification VWUDS", view=interactive(),pretty=T)

#---------------------------------------------#
#----------Apply Reclassification-------------#
Wateruse_m<-subset(Wateruse,select = c(1,4))
VWUDS_2010_2017<-merge(VWUDS_2010_2017,Wateruse_m,by="Facility.ID",all.x=T)


# Save Cleaned and Classified Withdrawal Data for Spatial Analysis 
save(VWUDS_2010_2017,file="G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/VWUDS_2010_2017.RData")

# For Stat 5525
# VWUDS_2010_2017<-subset(VWUDS_2010_2017,subset=substr(VWUDS_2010_2017$Date,1,4)=="2010")
# save(VWUDS_2010_2017,file="G:/My Drive/VT Courses/Fall Semester Classes 2018/STAT 5525/McCarthy_Final_Code/RData for R Files/VWUDS_2010.RData")

#----------------------------------------------#
#------------Plot Use Type---------------------#
Water.Use_Colors <- colorspace::diverge_hcl(4)


# Before Classification
VWUDS_2010_2017%>%
  group_by(Facility.ID)%>%
  summarise(Use.Type=first(Use.Type))%>%
  count(Use.Type)%>%
  plot_ly(labels=~Use.Type,values=~n,
          textposition = 'inside',
          textinfo = 'text',
          text = ~paste(Use.Type,"\n",
                        n, ' Facilities'),
          insidetextfont = list(color = '#FFFFFF'),
          marker = list(colors=Water.Use_Colors,
                        line=list(color = '#FFFFFF', width =1)))%>%
  add_pie(hole = 0.6)%>%
  layout(title = 'Distrubution of Water Sectors in Raw Withdrawal Data',
         xaxis = list(showgrid=F, zeroline=F, showticklabels=F),
         yaxis = list(showgrid=F, zeroline=F, showticklabels=F))

# After Classification
VWUDS_2010_2017%>%
  group_by(Facility.ID)%>%
  summarise(Reclass_Use_Type=first(Reclass_Use_Type))%>%
  count(Reclass_Use_Type)%>%
  plot_ly(labels=~Reclass_Use_Type,values=~n,
          textposition = 'inside',
          textinfo = 'text',
          text= ~paste(Reclass_Use_Type,"\n",
                       n, ' Facilities'),
          insidetextfont = list(color = '#FFFFFF'),
          marker = list(colors=Water.Use_Colors,
                        line=list(color = '#FFFFFF', width =1)))%>%
  add_pie(hole = 0.6)%>%
  layout(title = 'Distrubution of Water Sectors in QA/QC Withdrawal Data',
         xaxis = list(showgrid=F, zeroline=F, showticklabels=F),
         yaxis = list(showgrid=F, zeroline=F, showticklabels=F))

