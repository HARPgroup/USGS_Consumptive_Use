##################################################################################################################################
#-------------------------------------------------ECHO_Timeseries.R Script-------------------------------------------------------#

# Primary Author: Morgan McCarthy, M.S. Biological Systems Engineering, Virginia Tech

##################################################################################################################################
#--------------------------------------------------------Purpose-----------------------------------------------------------------#

# Utilize the EPA's Enforcement and Compliance Online History (ECHO) Representation State Transfer (REST) Services
# to extract data on facilities regulated under the Clean Water Act (CWA) and managed under the National Pollutant 
# Discharge Elimination System (NPDES) Program. 

# Ultimately this statewide discharge data will be combined with withdrawal data to refine State Water Budget estimates.
# Accurate representations of water supply will fuel more informed policies and management practices.

##################################################################################################################################
#------------------------------------------------Links to ECHO Resources---------------------------------------------------------#

#The following links include inforamtion about the REST services used to query data associated with discharging facilities. 

#ECHO CWA REST Services: Facility Search - Water 
#https://echo.epa.gov/tools/web-services/facility-search-water

#ECHO EFF REST Services: Effluent Charts-Discharge Monitoring Reports
#https://echo.epa.gov/tools/web-services/effluent-charts

#ECHO DFR REST Services: Detailed Facility Report
#https://echo.epa.gov/tools/web-services/detailed-facility-report

##################################################################################################################################
#------------------------------------------------Load Library and Options--------------------------------------------------------#

rm(list=ls()) #Start with clear environment

library(foreign) #allows for easy manipulation of *.dbf file types
library(rgdal) #extract files from file geodatabases-like in ArcMap
library(dplyr) #data manipulation package that speeds up grouping, summarizing, ordering. 
library(XML) #downloads and reads XML file types-used to access ECHORest Services to download facilities
library(RCurl) #downloads and interacts with web-based content
library(readxl) #reads excel files
library(jsonlite)
library(lubridate)
library(httr)
library(daff)#helps calculate the differences between two dataframes
library(data.table)

options(scipen=999) #Disable scientific notation
options(digits = 9)

##################################################################################################################################
#-------------------------------------------Clean Water Act Facility Download----------------------------------------------------#

# This section of the code generate a query in the CWA Water Facility Search REST Services in ECHO.
# It pulls every discharging facility regulated by the CWA with a NPDES permit. 
# Queries are created using a generated URL with the base address of: https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?

# This particular query is created in two-steps. An XML document of all CWA facilities in VA is first downloaded (uri_query and ECHO_xml).
# Then the XML is parsed (ECHO_query) to generate a query ID (QID) that can be used to access summary data for each facility (uri_summary).

VA_Facility_Pull<- function(state){
  Req_URL<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&qcolumns=1,2,3,4,5,10,14,15,21,22,23,24,25,26,27,60,61,63,65,67,84,91,95,97,204,205,206,207,209,210,224&passthrough=Y&p_st=",state)
  URL_Download<-getURL(Req_URL) #Download URL from above
  URL_Parse<-xmlParse(URL_Download)#parses the downloaded XML of facilities and generates an R structure that represents the XML/HTML tree-main goal is to retrieve query ID or QID
  QID<-xmlToList(URL_Parse)#Converts parsed query to a more R-like list and stores it as a variable
  QID<-QID$QueryID
  GET_Facilities<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qcolumns=1,2,3,4,5,10,14,15,21,22,23,24,25,26,27,60,61,63,65,67,84,91,95,97,204,205,206,207,209,210,224&passthrough=Y&qid=",QID)
  ECHO_Facilities<-read.csv(GET_Facilities,stringsAsFactors = F) #Important to note this returns all facilities, active or not
  ECHO_Facilities$CWPName<-toupper(ECHO_Facilities$CWPName)
  
  
  #Narrow to NPDES Permits only
  ECHO_Facilities<-subset(ECHO_Facilities, ECHO_Facilities$CWPPermitTypeDesc=="NPDES Individual Permit")
  
  assign("ECHO_Facilities",ECHO_Facilities,envir = .GlobalEnv)
  
}
VA_Facility_Pull("VA")

##################################################################################################################################
#----------------------------------------------Discharge Monitoring Report Download----------------------------------------------#

# Uses ECHO EFF REST Services: Effluent Charts-Discharge Monitoring Reports

ts_ECHO_pull<- function(iteration,startDate,endDate){
  
  #Create Variables for Desired Information
  
  Facility_Name<-character() #Name of the CWA regulated discharging facility
  FacilityID<-character() #Facility ID 
  VPDESID<-character() #Unique ID used in Virginia for a facility's outfall: concatonated facility ID with 3 digit outfall ID
  eff_limit<-numeric() #numerical limit for flow 
  eff_limit_units<-character() #units of measure applicable to effluent quantity limit
  dmr_value<-numeric() #measured effluent through outfall 
  dmr_units<-character() #units for measured effluent
  statistic<-character() #indicates the statistic analysis used for the measured effluent-we are interested in averages
  mp_begin<-character() #beginning date of monitoring period (mp)
  mp_end<-character() #end data of monitoring period (mp)
  mon_in_mp<-numeric() #number of months included in monitoring period--need to multiply measured effluent by this 
  nodi<-character() #if the DMR value is NA, the no data indicator code describes why that is the case
  violation<-character() #Code identifying if a Violation has occurred  (e.g., D80 = Required Monitoring DMR Value Non-Receipt, E90 = Effluent Violation, C20 = Schedule Event Achieved Late).
  violation_severity<-numeric() #Severity of any alleged violation caused by the reported value: 5 = significant noncompliance; 3 = reportable noncompliance; 2 = effluent violation, i.e., discharge in excess of permitted limit; 1 = monitoring or reporting violation; 0 = no violation.
  outfall_type<-character()
  
  for (i in iteration:length(ECHO_Facilities$SourceID)){
    sourceID<-ECHO_Facilities$SourceID[i]
    print(paste("Processing Facility ID: ", sourceID, "(",i," of ",length(ECHO_Facilities$SourceID),")", sep=""))
    DMR_data<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",sourceID,"&parameter_code=50050&start_date=",startDate,"&end_date=",endDate) #CWA Effluent Chart ECHO REST Service for a single facility for a given timeframe, parameter code 50050 is for flow thru or in conduit
    DMR_data<-read.csv(DMR_data,stringsAsFactors = F)#reads downloaded CWA Effluent Chart that contains discharge monitoring report (DMR) for a single facility
    DMR_data$dmr_value_nmbr[DMR_data$nodi_code %in% c('C','7')]<-0 #nodi_code is the unique code indicating the reason why an expected DMR value was not submitted, C and 7 means no discharge and influent, respectively. So set to 0.
    data_length<-length(unique(DMR_data$monitoring_period_end_date))#sees if there is any reported data worth extracting and examining
    if(data_length>0){ #if the value is NOT NA, enter loop
      outfall_nmbr<-as.character(unique(DMR_data$perm_feature_nmbr)) #Stores Outfalls which are called permanent features in the DMR
      outfall_ID<-unique(DMR_data$perm_feature_nmbr) #perm_feature_nmbr is a three-character code in ICIS-NPDES that identifies the point of discharge
      for(j in 1:length(outfall_ID)){ #If the code is less than three characters in the .CSV, append zeros to the beginning of the number (e.g., 1 is equivalent to 001)
        if(!is.na(as.character(outfall_ID[j]))){
          leadingzeros<-paste(rep(0,3-nchar(outfall_ID[j])),collapse= '')
          outfall_ID[j]<-paste0(leadingzeros,as.character(outfall_ID[j]))
        }else{
          outfall_ID[j]<-as.character(outfall_ID[j]) #if the outfall number is already three digits, no reformatting needed
        }
      }
      for(k in 1:length(outfall_ID)){ #Now we go through the DMR attached to each unique individual outfall and extract the information we would like
        outfall<-as.character(outfall_ID[k])
        outfall_DMR<-DMR_data[DMR_data$perm_feature_nmbr==outfall_nmbr[k],] #specifies that we want to go through each unique outfall
        eff_limit_i<-numeric() #Create variables that will store DMR data for each outfall
        eff_limit_units_i<-character()
        dmr_value_i<-numeric(length(outfall_DMR$perm_feature_nmbr))
        dmr_units_i<-character()
        statistic_i<-character(length(outfall_DMR$perm_feature_nmbr))
        mp_end_i<-character()
        mon_in_mp_i<-numeric(length(outfall_DMR$perm_feature_nmbr))
        mp_begin_i<-character()
        violation_i<-character()
        violation_severity_i<-numeric()
        nodi_i<-character()
        outfall_type_i<-character()
        
        for(l in 1:length(outfall_DMR$perm_feature_nmbr)){ #extracts discharge quantity from each outfall by examining the statistical code associated with it. In this case, we want an average.
          if(!is.na(outfall_DMR$statistical_base_code[l]=="MK")){ #ideally, we want a monthly average, which is indicated by the code "MK"
            statistic_i[l]<-"mon_ave"
            dmr_value_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="MK"])[l] 
            dmr_units_i[l]<-outfall_DMR$standard_unit_desc[outfall_DMR$statistical_base_code=="MK"][l]
            eff_limit_i[l]<-as.numeric(outfall_DMR$limit_value_nmbr[outfall_DMR$statistical_base_code=="MK"])[l]
            eff_limit_units_i<-outfall_DMR$limit_unit_desc[outfall_DMR$statistical_base_code=="MK"][l]
            mp_end_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="MK"][l] 
            mon_in_mp_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="MK"])[l]
            mp_begin_i[l]<-as.character(round_date(mdy(mp_end_i[l]) %m-% months(mon_in_mp_i[l]),unit="month"))
            violation_i[l]<-outfall_DMR$violation_code[outfall_DMR$statistical_base_code=="MK"][l]
            violation_severity_i[l]<-outfall_DMR$violation_severity[outfall_DMR$statistical_base_code=="MK"][l]
            nodi_i[l]<-outfall_DMR$nodi_desc[outfall_DMR$statistical_base_code=="MK"][l] 
            outfall_type_i[l]<-outfall_DMR$perm_feature_type_code[outfall_DMR$statistical_base_code=="MK"][l] 
          }
        }
        #Now we store the values we get from each outfall in each facility[i] in a larger matrix
        #We do this so that results are not over written after each iteration
        Facility_Name<-c(Facility_Name,rep(paste0(ECHO_Facilities$CWPName[ECHO_Facilities$SourceID==sourceID]),length(dmr_value_i)))
        statistic<-c(statistic,statistic_i)
        VPDESID<-c(VPDESID,paste0(sourceID,rep(outfall,length(dmr_value_i))))
        FacilityID<-c(FacilityID,paste0(rep(sourceID, length(dmr_value_i))))
        dmr_value<-c(dmr_value,dmr_value_i)
        dmr_units<-c(dmr_units,dmr_units_i)
        eff_limit<-c(eff_limit,eff_limit_i)
        eff_limit_units<-c(eff_limit_units,eff_limit_units_i)
        mp_end<-c(mp_end,mp_end_i)
        mon_in_mp<-c(mon_in_mp,mon_in_mp_i)
        mp_begin<-c(mp_begin,mp_begin_i)
        violation<-c(violation,violation_i)
        violation_severity<-c(violation_severity,violation_severity_i)
        nodi<-c(nodi,nodi_i)
        outfall_type<-c(outfall_type,outfall_type_i)
      }
    }else{ #if the DMR contains no data, set variables to NA
      Facility_Name<-c(Facility_Name,NA)
      statistic<-c(statistic,NA)
      VPDESID<-c(VPDESID,NA)
      FacilityID<-c(FacilityID,NA)
      dmr_value<-c(dmr_value,NA)
      dmr_units<-c(dmr_units,NA)
      eff_limit<-c(eff_limit,NA)
      eff_limit_units<-c(eff_limit_units,NA)
      mp_end<-c(mp_end,NA)
      mon_in_mp<-c(mon_in_mp,NA)
      mp_begin<-c(mp_begin,NA)
      violation<-c(violation,NA)
      violation_severity<-c(violation_severity,NA)
      nodi<-c(nodi,NA)
      outfall_type<-c(outfall_type,NA)
    }
  }
  
  # See all data extracted from ECHO
  ECHO_timeseries_wNAs<-data.frame(FacilityName=Facility_Name,Facility.ID=FacilityID,OutfallID=VPDESID,Statistic=statistic,
                                   Outfall_Type=outfall_type,
                                   Measured_Effluent=dmr_value,Units=dmr_units,Permitted_Limit=eff_limit,
                                   MP_Begin_Date=mp_begin,MP_End_Date=mp_end,Mon_in_MP=mon_in_mp,Violation_Code=violation,
                                   Violation_Severity=violation_severity,NODI=nodi)
  
  ECHO_timeseries<-ECHO_timeseries_wNAs[!(is.na(ECHO_timeseries_wNAs$MP_End_Date)),] #remove if a monitoring period end date is missing 
  
  ECHO_timeseries$MP_Begin_Date<-as.Date(ECHO_timeseries$MP_Begin_Date, format="%Y-%m-%d") # Dates are not treated as dates when pulled from ECHO, therefore you need to change the data type
  ECHO_timeseries$MP_End_Date<-as.Date(ECHO_timeseries$MP_End_Date, format="%m/%d/%Y")
  
  duplicated<-ECHO_timeseries[duplicated(ECHO_timeseries, by=c("OutfallID","MP_End_Date")),] # remove deuplicated entries from dataframe - you can see how many are duplicated with this line
  ECHO_timeseries<-ECHO_timeseries[!duplicated(ECHO_timeseries, by=c("OutfallID","MP_End_Date")),] # this line actually removes the duplicated entries
  
  assign("ECHO_timeseries",timeseries,envir=.GlobalEnv)
  
}
ts_ECHO_pull(1,"01/01/2010",format(as.Date(Sys.Date()), "%m/%d/%Y"))

##################################################################################################################################
#---------------------------------Convert Annual, Semi-Annual, and Quarterly Entries into Monthly Entries------------------------#

nmbr_submissions_conversion<- function(nmbr_submissions,label){
  
  Submissions<-subset(ECHO_timeseries,ECHO_timeseries$Mon_in_MP==nmbr_submissions)
  
  summary<-Submissions%>%
    dplyr::summarise(Facilities=n_distinct(Submissions$Facility.ID),Outfalls=n_distinct(Submissions$OutfallID),
                     entries=n())
  
  FacilityName<-character()
  Facility.ID<-character()
  OutfallID<-character()
  Statistic<-character()
  Measured_Effluent<-numeric()
  Units<-character()
  Permitted_Limit<-numeric()
  MP_Begin_Date<-character()
  MP_End_Date<-character()
  Mon_in_MP<-numeric()
  Violation_Code<-character()
  Violation_Severity<-character()
  NODI<-character()
  Outfall_Type<-character()
  
  for (i in 1:length(Submissions$Facility.ID)){
    FacilityName_i<-character()
    Facility.ID_i<-character()
    OutfallID_i<-character()
    Statistic_i<-character()
    Measured_Effluent_i<-numeric()
    Units_i<-character()
    Permitted_Limit_i<-numeric()
    MP_Begin_Date_i<-character()
    MP_End_Date_i<-character()
    Mon_in_MP_i<-numeric()
    Violation_Code_i<-character()
    Violation_Severity_i<-character()
    NODI_i<-character()
    Outfall_Type_i<-character()
    
    FacilityName_i<-rep(as.character(Submissions$FacilityName[i]), as.numeric(Submissions$Mon_in_MP[i]))
    Facility.ID_i<-rep(as.character(Submissions$Facility.ID[i]), as.numeric(Submissions$Mon_in_MP[i]))
    OutfallID_i<-rep(as.character(Submissions$OutfallID[i]), as.numeric(Submissions$Mon_in_MP[i]))
    Statistic_i<-rep(as.character(Submissions$Statistic[i]), as.numeric(Submissions$Mon_in_MP[i]))
    Measured_Effluent_i<-rep(as.character(Submissions$Measured_Effluent[i]), as.numeric(Submissions$Mon_in_MP[i]))
    Units_i<-rep(as.character(Submissions$Units[i]), as.numeric(Submissions$Mon_in_MP[i]))
    Permitted_Limit_i<-rep(as.character(Submissions$Permitted_Limit[i]), as.numeric(Submissions$Mon_in_MP[i]))
    MP_Begin_Date_i<-seq(as.Date(Submissions$MP_Begin_Date[i]),length=nmbr_submissions,by="1 month")
    MP_End_Date_i<-ceiling_date(MP_Begin_Date_i,"month")-days(1)
    Mon_in_MP<-rep(12, as.numeric(Submissions$Mon_in_MP[i]))
    Violation_Code_i<-rep(as.character(Submissions$Violation_Code[i]),as.numeric(Submissions$Mon_in_MP[i]))
    Violation_Severity_i<-rep(as.character(Submissions$Violation_Severity[i]),as.numeric(Submissions$Mon_in_MP[i]))
    NODI_i<-rep(as.character(Submissions$NODI[i]),as.numeric(Submissions$Mon_in_MP[i]))
    Outfall_Type_i<-rep(as.character(Submissions$Outfall_Type[i]),as.numeric(Submissions$Mon_in_MP[i]))
    
    FacilityName<-c(FacilityName,FacilityName_i)
    Facility.ID<-c(Facility.ID,Facility.ID_i)
    OutfallID<-c(OutfallID,OutfallID_i)
    Statistic<-c(Statistic,Statistic_i)
    Outfall_Type<-c(Outfall_Type,Outfall_Type_i)
    Measured_Effluent<-c(Measured_Effluent,Measured_Effluent_i)
    Units<-c(Units,Units_i)
    Permitted_Limit<-c(Permitted_Limit,Permitted_Limit_i)
    MP_Begin_Date<-c(MP_Begin_Date,as.character(MP_Begin_Date_i))
    MP_End_Date<-c(MP_End_Date,as.character(MP_End_Date_i))
    Mon_in_MP<-c(Mon_in_MP,Mon_in_MP_i)
    Violation_Code<-c(Violation_Code,Violation_Code_i)
    Violation_Severity<-c(Violation_Severity,Violation_Severity_i)
    NODI<-c(NODI,NODI_i)
    
  }
  
  nmbr_to_Monthly<-data.frame(FacilityName=FacilityName,
                                Facility.ID=Facility.ID,
                                OutfallID=OutfallID,
                                Statistic=Statistic,
                                Outfall_Type=Outfall_Type,
                                Measured_Effluent=Measured_Effluent,
                                Units=Units,
                                Permitted_Limit=Permitted_Limit,
                                MP_Begin_Date=MP_Begin_Date,
                                MP_End_Date=MP_End_Date,
                                Mon_in_MP=Mon_in_MP,
                                Violation_Code=Violation_Code,
                                Violation_Severity=Violation_Severity,
                                NODI=NODI)
  
  assign(label,nmbr_to_Monthly,envir = .GlobalEnv)
  
  return(list(summary))
  
  
}
nmbr_submissions_conversion(12,"Annual_to_Monthly")
nmbr_submissions_conversion(6,"SemiAnnual_to_Monthly")
nmbr_submissions_conversion(3,"Quarterly_to_Monthly")

#--------------------------------------------------------------------------------------------------------------------------------#
#------Remove Annual, Semi-Annual, and Quarterly Entries from ECHO_Timeseries and Replace with Converted Monthly Entries---------#

ECHO_timeseries<-subset(ECHO_timeseries,ECHO_timeseries$Mon_in_MP==1)
ECHO_timeseries%>%dplyr::summarise(Facilities=n_distinct(Facility.ID),Outfalls=n_distinct(OutfallID),entries=n())

ECHO_timeseries<-rbind(ECHO_timeseries,Annual_to_Monthly,SemiAnnual_to_Monthly,Quarterly_to_Monthly)
ECHO_timeseries<-as.data.table(ECHO_timeseries)

duplicated<-ECHO_timeseries[duplicated(ECHO_timeseries, by=c("OutfallID","MP_End_Date")),]
ECHO_timeseries<-ECHO_timeseries[!duplicated(ECHO_timeseries, by=c("OutfallID","MP_End_Date")),]

#################################################################################################################################
#----------------------------Save Timeseries as table for QA/QC in other scripts------------------------------------------------#

# Trim dataframe to specified range
ECHO_timeseries<-ECHO_timeseries[ECHO_timeseries$MP_Begin_Date %within% interval("2010-01-01","2016-12-31"),]

save.image("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/ECHO_Timeseries.RData") #Save the global environment for future reference

#In personal file
write.table(ECHO_timeseries,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Documentation/Imports/ECHO_timeseries_3_28.txt",sep="\t",row.names = F)

##################################################################################################################################
###################################################Analysis#######################################################################

#Track the changes between weekly runs here
# ECHO_timeseries_pre<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Documentation/Imports/ECHO_timeseries_8_13.txt",header=T)
# ECHO_timeseries_pre%>%
#   summarise(Outfalls=n_distinct(OutfallID),Facilities=n_distinct(FacilityName),
#             sum=sum(Measured_Effluent,na.rm=T),median=median(Measured_Effluent,na.rm=T),mean=
#               mean(Measured_Effluent,na.rm=T))
# 
# ECHO_timeseries_post<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Documentation/Imports/ECHO_timeseries_8_23.txt",header=T)
# ECHO_timeseries_post%>%
#   summarise(Outfalls=n_distinct(OutfallID),Facilities=n_distinct(FacilityName),
#             sum=sum(Measured_Effluent,na.rm=T),median=median(Measured_Effluent,na.rm=T),mean=
#               mean(Measured_Effluent,na.rm=T))
# 
# ECHO_difference<-diff_data(ECHO_timeseries_pre,ECHO_timeseries_post)
# write_diff(ECHO_difference, "ECHO_difference.csv")
# summary(ECHO_difference)
# render_diff(ECHO_difference, title="Difference in DMR Data", view=interactive(),pretty=T)
