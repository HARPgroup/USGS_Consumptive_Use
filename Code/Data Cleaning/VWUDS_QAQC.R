###########################################################################################################################################
#--------------------------------------------------Virginia Water Use Database System (VWUDS) QA/QC---------------------------------------#

# Primary Author: Morgan McCarthy, M.S. Biological Systems Engineering, Virginia Tech

###########################################################################################################################################
#---------------------------------------------------------------Purpose-------------------------------------------------------------------#

# This code serves to import and analyze withdrawal data from the Virginia Water Use Database System (VWUDS).
# VWUDS is maintained by Virginia's Department of Environmental Qualtiy (VDEQ) and is self reported by water users.
# The DEQ only requires users to obtain a permit if 300,000 gallons or more are withdrawn per month from either surface or groundwater sources (White, 2016).

# Since the water withdrawals are self reported, QA/QC measures need to be taken. The main source of error is centered on source coordinates.
# Sources are similar to outfalls; structures that withdraw water from either surface or groundwater.

# This script first identifies suspicious source coordinates (from 1982-2015) and merges manual corrections to those flagged data points.
# Coordinates that were deemed suspicious:
      #a. did not fit within the bounding box of VA,
      #b. had switched negative signs/lacked negative signs for longitude
      #c. were missing and were found using google maps/earth
      #(Manual Corrections were performed during Fall of 2017)

# The second part of the script introduces monthly withdrawal data from 1982-2016 that was collected August of 2018.
# It investigates potential interal corrections by VDEQ staff and refromats for future consumptive use calculations in the future.

# http://deq1.bse.vt.edu/d.bet/admin/structure/views/view/vwuds_monthly_wateruse/edit

###########################################################################################################################################
#------------------------------------------------------Load Library and Options-----------------------------------------------------------#
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
###################################################Monthly Withdrawal Data 2010-2017#######################################################

######################################################################################
#-----------------Download Data from VWUDS Monthly Water Use View--------------------#

# This function downloads the monthly withdrawals from VAHydro. Some manipulation including setting data type for attributes, 
# removing duplicate entries, and converting MGM to MGD occur in this function as well.

VWUDS_monthly<- function(){
  # Name of View: VWUDS Monthly Water Use (DH Feature) 
  # Link to this View: http://deq1.bse.vt.edu/d.dh/vwuds-monthly-water-use?hydrocode=&hydroid=&name_op=%3D&name=&tstime_op=between&tstime%5Bvalue%5D=&tstime%5Bmin%5D=2010-01-01&tstime%5Bmax%5D=2017-12-31&bundle%5B%5D=well&bundle%5B%5D=intake&fstatus_1_op=not+in&fstatus_1%5B%5D=duplicate&name_1_op=%3D&name_1=&propcode_op=%3D&propcode=&ftype_op=%3D&ftype=
  VWUDS_2010_2017<-read.csv("G:/My Drive/VWUDS Data/vwuds_monthly_wateruse_2010_2016.csv", sep=",", header=T)
  VWUDS_2010_2017<-as.data.table(VWUDS_2010_2017) # setting type to data table makes manipulation easier
  names(VWUDS_2010_2017)[2]<-c("Facility.ID")
  names(VWUDS_2010_2017)[3]<-c("Hydrocode")
  
  #---------------------------------------------#
  #---------Set Data Type for Attributes--------#
  VWUDS_2010_2017$Million.Gallons.Month<-as.numeric(gsub(",", "", as.character(VWUDS_2010_2017$Million.Gallons.Month))) # get rid of commas in reported withdrawals. If you leave them in, converting to a numeric from character will force it into a factor
  VWUDS_2010_2017$Date<-as.character(paste0("0",VWUDS_2010_2017$Date))
  VWUDS_2010_2017$Date<-as.Date(VWUDS_2010_2017$Date, format="%d/%m/%Y")
  VWUDS_2010_2017<-VWUDS_2010_2017[order(VWUDS_2010_2017[,c("Facility.ID")],VWUDS_2010_2017[,c("Date")],decreasing=F),]
  
  #---------------------------------------------#
  #----------Remove Duplicated Entries----------#
  
  duplicate<-VWUDS_2010_2017[duplicated(VWUDS_2010_2017, by=c("DEQ.ID.of.Source","Date","Million.Gallons.Month")),] # make sure to look for human errors, like duplicate submissions of withdrawal for same month
  VWUDS_2010_2017<-VWUDS_2010_2017[!duplicated(VWUDS_2010_2017, by=c("DEQ.ID.of.Source","Date","Million.Gallons.Month")),] # this line removes them
  
  
  # There is still a presence of duplicate entries for as source in a single month (~1045 entries). 
  # They are also reporting a value of 0.0.
  # Therefore, apply sum of those values to reduce data. 
  count(VWUDS_2010_2017[duplicated(VWUDS_2010_2017, by=c("DEQ.ID.of.Source","Date")),])
  
  VWUDS_2010_2017<-VWUDS_2010_2017[, lapply(.SD,sum),  by=list(DEQ.ID.of.Source,Facility.ID,Hydrocode,Owner,Facility,Source.Name,Date,Facility.Status,Use.Type,Locality,Latitude,Longitude,Source.Type),
                                   .SDcols=c(8)]
  
  #---------------------------------------------#
  #------------Convert MGM to MGD---------------#
  VWUDS_2010_2017$Million.Gallons.Month[VWUDS_2010_2017$Facility.ID=="67334"&substr(VWUDS_2010_2017$Date,1,4)=="2016"]<-0.0216
  
  VWUDS_2010_2017<-VWUDS_2010_2017%>%add_column(Withdrawals_MGD=NA, .after="Million.Gallons.Month")
  
  # Divide by number of days in specific month to account for leap years in 2012 and 2016 #
  VWUDS_2010_2017$Withdrawals_MGD<-
    ifelse(!(is.na(VWUDS_2010_2017$Million.Gallons.Month)),
           VWUDS_2010_2017$Million.Gallons.Month/monthDays(VWUDS_2010_2017$Date),NA)
  
  assign("VWUDS_2010_2017",VWUDS_2010_2017,envir=.GlobalEnv)
  
}
VWUDS_monthly()

# load("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/VWUDS_2010_2017.RData")
VWUDS_2010_2017<-VWUDS_2010_2017[VWUDS_2010_2017$Date %within% interval("2010-01-01","2016-12-31"),]

######################################################################################
#------------------------Flag Anamolous Coordinates----------------------------------#

VWUDS_flag<- function(){
  #---------------------------------------------#
  #---------Check Flagged Coordinates-----------#
  VWUDS_2010_2017$vwuds_flag_coord<-ifelse(VWUDS_2010_2017$Latitude>39.46601|
                                             VWUDS_2010_2017$Latitude<36.54076|
                                             VWUDS_2010_2017$Longitude<(-83.67539)|
                                             VWUDS_2010_2017$Longitude>(-75.16643)|
                                             is.na(VWUDS_2010_2017$Latitude)|
                                             is.na(VWUDS_2010_2017$Longitude),
                                           "vwuds_flag_coord","")
  
  #956 Flagged Sources with suspicious Coordinates
  Flagged_VWUDS_2010_2017<-subset(VWUDS_2010_2017,subset= !(vwuds_flag_coord==""))
  Flagged_VWUDS_2010_2017<-
    Flagged_VWUDS_2010_2017%>%
    group_by(DEQ.ID.of.Source)%>%
    summarise(FacilityName=first(Facility),
              Latitude=mean(Latitude),Longitude=mean(Longitude),
              flag=first(vwuds_flag_coord))%>%arrange(DEQ.ID.of.Source)
  
  # unique(Flagged_VWUDS_2010_2017$Latitude)
  # table(Flagged_VWUDS_2010_2017$Latitude)
  # 
  # unique(Flagged_VWUDS_2010_2017$Longitude)
  # table(Flagged_VWUDS_2010_2017$Longitude)
  # 
  # length(which(Flagged_VWUDS_2010_2017$Latitude==-99))
  # length(which(Flagged_VWUDS_2010_2017$Latitude>39.46601))
  # length(which(Flagged_VWUDS_2010_2017$Latitude<36.54076))
  # length(which(is.na(Flagged_VWUDS_2010_2017$Latitude)))
  # 
  # length(which(Flagged_VWUDS_2010_2017$Longitude==-99))
  # length(which(Flagged_VWUDS_2010_2017$Longitude>(-75.16643)))
  # length(which(Flagged_VWUDS_2010_2017$Longitude<(-83.67539)))
  # length(which(is.na(Flagged_VWUDS_2010_2017$Longitude)))
  
  
  #---------------------------------------------#
  #---Merge Manual Corrections of Coordinates---#
  
  # Note: All of these coordinates were manually corrected with the use of google maps
  #Download list of source coordinates that contain manual corrections for 1524 sources
  Corrected_Coordinates<-read.csv(paste0(path,"Corrected_coordinates.csv"), header=T, sep=",")
  
  
  #If all.x, extra rows are added--make sure to get rid of NA rows after this step
  VWUDS_2010_2017<-merge(VWUDS_2010_2017,Corrected_Coordinates,by="DEQ.ID.of.Source",all.x=T)
  
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
  

  assign("VWUDS_2010_2017",VWUDS_2010_2017,envir = .GlobalEnv) 
  assign("Flagged_VWUDS_2010_2017",Flagged_VWUDS_2010_2017,envir = .GlobalEnv) 
}
VWUDS_flag()

#---------------------------------------------#
#--------Summarize Dataframe------------------#

VWUDS_2010_2017%>%
  dplyr::group_by(Year = substr(Date,1,4))%>%
  dplyr::summarise(no_Facilities=n_distinct(Facility.ID),no_sources=n_distinct(DEQ.ID.of.Source), entries=n(),
                   Sum_Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T)/12,Mean_Withdrawal_MGD=mean(Withdrawals_MGD,na.rm=T),
                   Median_Withdrawal_MGD=median(Withdrawals_MGD,na.rm=T),Range=(max(Date)-min(Date)))

VWUDS_2010_2017%>%
  dplyr::summarise(no_Facilities=n_distinct(Facility.ID),no_sources=n_distinct(DEQ.ID.of.Source), entries=n(),
                   Sum_Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T)/12,Mean_Withdrawal_MGD=mean(Withdrawals_MGD,na.rm=T),
                   Median_Withdrawal_MGD=median(Withdrawals_MGD,na.rm=T),Range=(max(Date)-min(Date)))


######################################################################################
#-----------------------Classification of Water Use Type-----------------------------#

#---------------------------------------------#
#-----------Remove Hydropower-----------------#

#Why? regulations changed around 2010 and now they report to another source. They are considered less consumptive
VWUDS_2010_2017[VWUDS_2010_2017$Use.Type=="hydropower",]%>%
  dplyr::group_by(Year= substr(Date,1,4))%>%
  dplyr::summarise(no_Facilities=n_distinct(Facility),no_sources=n_distinct(DEQ.ID.of.Source), entries=n(),
            Sum_Withdrawals=sum(Withdrawals_MGD)/12,Mean_Withdrawal=mean(Withdrawals_MGD),
            Median_Withdrawal=median((Withdrawals_MGD)),Range=(max(Date)-min(Date)))

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
  dplyr::group_by(Facility.ID)%>%
  dplyr::summarise(Facility_Name=first(as.character(Facility)),Facility_Type=first(as.character(Use.Type)))

#---Word Frequency----#

# This function looks at the facility names in each delcared water use sectors and finds the most frequent terms

common_corpus<- function(Wateruse,sector){
  Sector<-subset(Wateruse,subset=Facility_Type==sector,select = c(2))
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
}
common_corpus(Wateruse,"industrial")
common_corpus(Wateruse,"agriculture")
common_corpus(Wateruse,"irrigation")
common_corpus(Wateruse,"municipal")
common_corpus(Wateruse,"fossilpower")
common_corpus(Wateruse,"nuclearpower")
common_corpus(Wateruse,"commercial")
common_corpus(Wateruse,"mining")
common_corpus(Wateruse,"manufacturing")

count(Wateruse,Facility_Type)

#----------------------#

unique(Wateruse$Facility_Type)

reclass<- function(Wateruse){
  
  
  Wateruse$Reclass_Use_Type<-NA

  for (i in 1:length(Wateruse$Facility.ID)){
    if(length(grep('\\bPOWER\\b',Wateruse$Facility_Name[i]))>0|
       length(grep('\\bPOWER STATION\\b',Wateruse$Facility_Name[i]))>0|
       length(grep('\\bCOMBUSTION\\b',Wateruse$Facility_Name[i]))>0|
       length(grep('\\bCLINCH RIVER PLANT\\b',Wateruse$Facility_Name[i]))>0|
       length(grep('\\bENERGY\\b',Wateruse$Facility_Name[i]))>0){
      Wateruse$Reclass_Use_Type[i]<-'Energy'
    }else if (length(grep('\\bMUNICIPAL\\b',Wateruse$Facility_Name[i]))>0|
              length(grep('\\bPUBLIC WATER SYSTEM\\b',Wateruse$Facility_Name[i]))>0|
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
              length(grep("\\bBASE\\b",Wateruse$Facility_Name[i]))>0|
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
               length(grep("\\bGREENHOUSE\\b",Wateruse$Facility_Name[i]))>0){
      Wateruse$Reclass_Use_Type[i]<-'Agriculture/Irrigation'
    } else if (length(grep("\\bFISHERIES\\b",Wateruse$Facility_Name[i]))>0|
               length(grep("\\bTROUT\\b",Wateruse$Facility_Name[i]))>0|
               length(grep("\\bCULTURAL\\b",Wateruse$Facility_Name[i]))>0|
               length(grep("\\bFISH\\b",Wateruse$Facility_Name[i]))>0|
               length(grep("\\bHATCHERY\\b",Wateruse$Facility_Name[i]))>0){
      Wateruse$Reclass_Use_Type[i]<-'Aquaculture'
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
  
  assign("VWUDS_2010_2017",VWUDS_2010_2017,envir = .GlobalEnv)
  assign("Wateruse",Wateruse,envir = .GlobalEnv)
}
reclass(Wateruse)

#---------------------------------------------#
#-------Manual Change of Water Use Type-------#

change_use<- function(VWUDSID,new_sector){
  VWUDS_2010_2017$Reclass_Use_Type[VWUDS_2010_2017$Facility.ID==VWUDSID]<-new_sector
  assign('VWUDS_2010_2017',VWUDS_2010_2017,envir=.GlobalEnv)
}
change_use("67334","Commercial")
change_use("73898","Aquaculture")
change_use("73058","Aquaculture")
change_use("74032","Commercial")
change_use("73140","Commercial")
change_use("74059","Commercial")
change_use("74241","Commercial")
change_use("90421","Commercial")
change_use("90584","Commercial")
change_use("74156","Commercial")
change_use("66899","Commercial")
change_use("72125","Commercial")
change_use("72223","Commercial")
change_use("71756","Commercial")
change_use("74084","Commercial")
change_use("71821","Commercial")
change_use("72534","Commercial")
change_use("67255","Commercial")
change_use("72791","Commercial")
change_use("73171","Industrial")
change_use("73152","Aquaculture")
change_use("73163","Aquaculture")

#--Number of Reporting Months for each Source per Year--#
W_Months<-
   VWUDS_2010_2017%>%
   dplyr::group_by(DEQ.ID.of.Source,Year=substring(Date,1,4))%>%
   dplyr::count(Month=substring(Date,6,7))%>%
   dplyr::summarise(Mon_Reported=sum(n))
 
VWUDS_2010_2017<-
   VWUDS_2010_2017%>%add_column(Year=substring(VWUDS_2010_2017$Date,1,4), .after="Date")
 
VWUDS_2010_2017<-merge(VWUDS_2010_2017,W_Months,by=c("DEQ.ID.of.Source","Year"),all.x=T)


#---------------------------------------------#
#--------Summarize Dataframe------------------#

Source_Withdrawals<-VWUDS_2010_2017%>%
  dplyr::group_by(DEQ.ID.of.Source,Year=substr(Date,1,4))%>%
  dplyr::summarise(Facility_Name=first(Facility),
                   Facility.ID=first(Facility.ID),
                   Sources=n_distinct(DEQ.ID.of.Source),
                   entries=n(),
                   Ave_Mon_Reported=mean(Mon_Reported),
                   Source.Type=first(Source.Type),
                   Withdrawals_MGal=sum(Million.Gallons.Month,na.rm=T),
                   Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T)/first(Mon_Reported),
                   Sector=first(Reclass_Use_Type))%>%arrange(desc(Withdrawals_MGD))


Facility_Withdrawals<-Source_Withdrawals%>%
  dplyr::group_by(Facility.ID,Year)%>%
  dplyr::summarise(Facility_Name=first(Facility_Name),
                   Sources=n_distinct(DEQ.ID.of.Source),
                   entries=sum(entries),
                   Source.Type=first(Source.Type),
                   Withdrawals_MGal=sum(Withdrawals_MGal,na.rm=T),
                   Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T),
                   Sector=first(Sector))%>%arrange(desc(Withdrawals_MGD))

energy<-Facility_Withdrawals[Facility_Withdrawals$Sector=="Energy",]%>%
  dplyr::group_by(Year)%>%
                dplyr::summarise(no_Facilities=n_distinct(Facility.ID),no_sources=sum(Sources), 
                entries=sum(entries),
                 Sum_Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T),Mean_Withdrawal_MGD=mean(Withdrawals_MGD,na.rm=T),
                 Median_Withdrawal_MGD=median(Withdrawals_MGD,na.rm=T))

energy<-energy%>%
  mutate(pct_change=round((Sum_Withdrawals_MGD/lag(Sum_Withdrawals_MGD) -1 )*100,digits=1))
energy%>%summarise(mean(no_Facilities),mean(no_sources),mean(entries),mean(Sum_Withdrawals_MGD),mean(pct_change,na.rm=T))



nonenergy<-Facility_Withdrawals[!Facility_Withdrawals$Sector=="Energy",]%>%
  dplyr::group_by(Year)%>%
  dplyr::summarise(no_Facilities=n_distinct(Facility.ID),no_sources=sum(Sources), 
                   entries=sum(entries),
                   Sum_Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T),Mean_Withdrawal_MGD=mean(Withdrawals_MGD,na.rm=T),
                   Median_Withdrawal_MGD=median(Withdrawals_MGD,na.rm=T))

nonenergy<-nonenergy%>%
  mutate(pct_change=round((Sum_Withdrawals_MGD/lag(Sum_Withdrawals_MGD) -1 )*100,digits=1))
nonenergy%>%summarise(mean(no_Facilities),mean(no_sources),mean(entries),mean(Sum_Withdrawals_MGD),mean(pct_change,na.rm=T))


# Save Cleaned and Classified Withdrawal Data for Spatial Analysis 

save(VWUDS_2010_2017,file="G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/VWUDS_2010_2017.RData")

#----------------------------------------------#
#-----------------GW vs. SW--------------------#

GW_v_SW<- function(){
  with_colors<-colorspace::heat_hcl(4,c=c(80,30),l=c(30,90),power=c(1/5,1.5))
  
  GW_vs_SW<-VWUDS_2010_2017%>%
    dplyr::group_by(DEQ.ID.of.Source,Year=substr(Date,1,4))%>%
    dplyr::summarise(Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T)/first(Mon_Reported),Source.Type=first(Source.Type))%>%
    dplyr::group_by(Year,Source.Type)%>%
    dplyr::summarise(Withdrawals_MGD=sum(Withdrawals_MGD,na.rm=T))
  
  GW_vs_SW<-GW_vs_SW%>%
    dplyr::group_by(Source.Type)%>%
    dplyr::summarise(Withdrawals_MGD=median(Withdrawals_MGD,na.rm=T))
  GW_vs_SW<-mutate_if(GW_vs_SW,is.factor,as.character)
  
  m<-list(l=50,r=50,b=50,t=50)
  a<-list(
    x=0.5,
    y=0.5,
    text=paste0('<b>',round(sum(GW_vs_SW$Withdrawals_MGD),0)," MGD",'<b>'),
    showarrow=FALSE,
    font = list(color = '#252525', size = 30),
    ax=0,
    ay=0
  )
  plot1<-plot_ly()%>%
    add_pie(data=GW_vs_SW,labels=~Source.Type,values=~Withdrawals_MGD, name="Ground Water vs Surface Water",
            hole=0.6,
            textposition='outside',
            text=~paste0('<b>',Source.Type,'<b>',"\n", "~", round((Withdrawals_MGD/sum(Withdrawals_MGD))*100, 0), "%",'</b>'),
            textinfo='text',
            insidetextfont = list(color = '#FFFFFF', size=14),
            outsidetextfont = list(color='#252525',size=21),
            marker = list(colors=with_colors,line=list(color = '#FFFFFF', width =3)))%>%
    layout(showlegend=FALSE,
           xaxis = list(showgrid=F, zeroline=F, showticklabels=F),
           yaxis = list(showgrid=F, zeroline=F, showticklabels=F),
           autosize=T,margin=m,annotations=a)
  
  path="G:/My Drive/USGS_ConsumptiveUse/Spring, 2019/Statewide Analysis/Withdrawals/"
  file_name <- paste(path,"longterm_GW_vs_SW", ".pdf", sep="")
  plotly_IMAGE(x=plot1,width=700,height=416,format="pdf",out_file=file_name)
  
  return(list(plot1))
  
}
GW_v_SW()
#----------------------------------------------#
#------------Plot Use Type---------------------#

Use_Type<- function(Use_Type,label){
  Water.Use_Colors <- colorspace::diverge_hcl(4)
  
  
  # Before Classification
  VWUDS_2010_2017%>%
    dplyr::group_by(Facility.ID)%>%
    dplyr::summarise(Use.Type=first(Use_Type))%>%
    dplyr::count(Use.Type)%>%
    plot_ly(labels=~Use.Type,values=~n,
            textposition = 'inside',
            textinfo = 'text',
            text = ~paste(Use.Type,"\n",
                          n, ' Facilities'),
            insidetextfont = list(color = '#FFFFFF'),
            marker = list(colors=Water.Use_Colors,
                          line=list(color = '#FFFFFF', width =1)))%>%
    add_pie(hole = 0.6)%>%
    layout(title = paste0('Distribution of Water Sectors in ',label),
           xaxis = list(showgrid=F, zeroline=F, showticklabels=F),
           yaxis = list(showgrid=F, zeroline=F, showticklabels=F))
  
  
}
Use_Type(VWUDS_2010_2017$Use.Type,"Raw Data")
Use_Type(VWUDS_2010_2017$Reclass_Use_Type,"QA/QC Withdrawal Data")
