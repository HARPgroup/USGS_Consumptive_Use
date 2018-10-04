##################################################################################################################################
###########################################ECHO Time Series ######################################################################

#Primary Author: Morgan McCarthy, M.S. Biological Systems Engineering, Virginia Tech

#################################################Purpose##########################################################################

#Utilize the EPA's Enforcement and Compliance Online History (ECHO) Representation State Transfer (REST) Services
#to extract data on facilities regulated under the Clean Water Act (CWA) and managed under the National Pollutant 
#Discharge Elimination System (NPDES) Program. 

#Ultimately this statewide discharge data will be combined with withdrawal data to refine State Water Budget estimates.
#Accurate representations of water supply will fuel more informed policies and management practices.

#################################################Resources########################################################################

#The following links include inforamtion about the REST services used to query data associated with discharging facilities. 

#ECHO CWA REST Services: Facility Search - Water 
#https://echo.epa.gov/tools/web-services/facility-search-water

#ECHO EFF REST Services: Effluent Charts-Discharge Monitoring Reports
#https://echo.epa.gov/tools/web-services/effluent-charts

#ECHO DFR REST Services: Detailed Facility Report
#https://echo.epa.gov/tools/web-services/detailed-facility-report

##################################################################################################################################
#############################################Initilisation########################################################################

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

state<-"VA"
startDate<-"01/01/2010" #mm/dd/yyyy: data on ECHO is limited to 2012 for most sites or 2009 for a few
endDate<-Sys.Date()
#endDate<-"12/31/2016"
endDate<-format(as.Date(endDate), "%m/%d/%Y")
options(scipen=999) #Disable scientific notation
options(digits = 9)

##################################################################################################################################
###########################################CWA Facility Download##################################################################

#This section of the code generate a query in the CWA Water Facility Search REST Services in ECHO.
#It pulls every discharging facility regulated by the CWA with a NPDES permit. 
#Queries are created using a generated URL with the base address of: https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?

#This particular query is created in two-steps. An XML document of all CWA facilities in VA is first downloaded (uri_query and ECHO_xml).
#Then the XML is parsed (ECHO_query) to generate a query ID (QID) that can be used to access summary data for each facility (uri_summary).

Req_URL<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&qcolumns=1,2,3,4,5,10,14,15,21,22,23,24,25,26,27,60,61,63,65,67,84,91,95,97,204,205,206,207,209,210&passthrough=Y&p_st=",state)
URL_Download<-getURL(Req_URL) #Download URL from above
URL_Parse<-xmlParse(URL_Download)#parses the downloaded XML of facilities and generates an R structure that represents the XML/HTML tree-main goal is to retrieve query ID or QID
QID<-xmlToList(URL_Parse)#Converts parsed query to a more R-like list and stores it as a variable
QID<-QID$QueryID
GET_Facilities<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qcolumns=1,2,3,4,5,10,14,15,21,22,23,24,25,26,27,60,61,63,65,67,84,91,95,97,204,205,206,207,209,210&passthrough=Y&qid=",QID)
ECHO_Facilities<-read.csv(GET_Facilities,stringsAsFactors = F) #Important to note this returns all facilities, active or not
ECHO_Facilities$CWPName<-toupper(ECHO_Facilities$CWPName)
#ECHO_Facilities_IP<-subset(ECHO_Facilities,subset = ECHO_Facilities$CWPPermitTypeDesc=="NPDES Individual Permit")
#ECHO_Facilities_activeIP<-subset(ECHO_Facilities,subset = ECHO_Facilities$CWPPermitTypeDesc=="NPDES Individual Permit" &
                                   #ECHO_Facilities$CWPPermitStatusDesc=="Effective")
rm(Req_URL,URL_Download,URL_Parse,GET_Facilities,QID) #Clear unnecessary variables

##################################################################################################################################
#########################################Extract Time Series Data#################################################################

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
mon_in_mp<-numeric() #number of months included in monitoring period
nodi<-character() #if the DMR value is NA, the no data indicator code describes why that is the case
violation<-character() #Code identifying if a Violation has occurred  (e.g., D80 = Required Monitoring DMR Value Non-Receipt, E90 = Effluent Violation, C20 = Schedule Event Achieved Late).
violation_severity<-numeric() #Severity of any alleged violation caused by the reported value: 5 = significant noncompliance; 3 = reportable noncompliance; 2 = effluent violation, i.e., discharge in excess of permitted limit; 1 = monitoring or reporting violation; 0 = no violation.

for (i in 1:length(ECHO_Facilities$SourceID)){
    sourceID<-ECHO_Facilities$SourceID[i]
    print(paste("Processing Facility ID: ", sourceID, "(",i," of ",length(ECHO_Facilities$SourceID),")", sep=""))
    DMR_data<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",sourceID,"&start_date=",startDate,"&end_date=",endDate) #CWA Effluent Chart ECHO REST Service for a single facility for a given timeframe
    DMR_data<-read.csv(DMR_data,stringsAsFactors = F)#reads downloaded CWA Effluent Chart that contains discharge monitoring report (DMR) for a single facility
    DMR_data<-DMR_data[DMR_data$parameter_code==50050,]#only looks at Flow, in conduit ot thru treatment plant - there are 347 parameter codes defined in ECHO
    DMR_data$dmr_value_nmbr[DMR_data$nodi_code %in% c('C','7')]<-0 #nodi_code is the unique code indicating the reason why an expected DMR value was not submitted, C and 7 means no influent. So set to 0.
    data_length<-length(unique(DMR_data$monitoring_period_end_date))#sees if there is any reported data worth extracting and examining
        if(data_length>0){ #if the value is NOT NA, enter loop
          outfall_nmbr<-as.character(unique(DMR_data$perm_feature_nmbr)) #Stores Outfalls which are called permanent features in the DMR
          outfall_ID<-unique(DMR_data$perm_feature_nmbr) #perm_feature_nmbr is a three-character code in ICIS-NPDES that identifies the point of discharge
          for(j in 1:length(outfall_ID)){ #If the code is less than three characters in the .CSV, append zeros to the beginning of the number (e.g., 1 is equivalent to 001)
            if(!is.na(as.numeric(outfall_ID[j]))){
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
                
              }else if(!is.na(outfall_DMR$statistical_base_code[l]=="DB")){ #if it is missing a monthly average, look at daily average in MGD
                statistic_i[l]<-"day_ave"
                dmr_value_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="DB"])[l] 
                dmr_units_i[l]<-outfall_DMR$standard_unit_desc[outfall_DMR$statistical_base_code=="DB"][l]
                eff_limit_i[l]<-as.numeric(outfall_DMR$limit_value_nmbr[outfall_DMR$statistical_base_code=="DB"])[l]
                eff_limit_units_i<-outfall_DMR$limit_unit_desc[outfall_DMR$statistical_base_code=="DB"][l]
                mp_end_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="DB"][l] 
                mon_in_mp_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="DB"])[l]
                mp_begin_i[l]<-as.character(round_date(mdy(mp_end_i[l]) %m-% months(mon_in_mp_i[l]),unit="month")) 
                violation_i[l]<-outfall_DMR$violation_code[outfall_DMR$statistical_base_code=="DB"][l]
                violation_severity_i[l]<-outfall_DMR$violation_severity[outfall_DMR$statistical_base_code=="DB"][l]
                nodi_i[l]<-outfall_DMR$nodi_desc[outfall_DMR$statistical_base_code=="DB"][l] 
                
              }else if(!is.na(outfall_DMR$statistical_base_code[l]=="WA")){ #if it is also missing a daily average, look at weekly average in MGD
                statistic_i[l]<-"wk_ave"
                dmr_value_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="WA"])[l] 
                dmr_units_i[l]<-outfall_DMR$standard_unit_desc[outfall_DMR$statistical_base_code=="WA"][l]
                eff_limit_i[l]<-as.numeric(outfall_DMR$limit_value_nmbr[outfall_DMR$statistical_base_code=="WA"])[l]
                eff_limit_units_i<-outfall_DMR$limit_unit_desc[outfall_DMR$statistical_base_code=="WA"][l]
                mp_end_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="WA"][l] 
                mon_in_mp_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="WA"])[l]
                mp_begin_i[l]<-as.character(round_date(mdy(mp_end_i[l]) %m-% months(mon_in_mp_i[l]),unit="month"))
                violation_i[l]<-outfall_DMR$violation_code[outfall_DMR$statistical_base_code=="WA"][l]
                violation_severity_i[l]<-outfall_DMR$violation_severity[outfall_DMR$statistical_base_code=="WA"][l]
                nodi_i[l]<-outfall_DMR$nodi_desc[outfall_DMR$statistical_base_code=="WA"][l] 
                
              }else if(!is.na(outfall_DMR$statistical_base_code[l]=="AB")){ #if it is also missing this, look at annual average in MGD
                statistic_i[l]<-"yr_ave"
                dmr_value_i[l]<-as.numeric(outfall_DMR$dmr_value_nmbr[outfall_DMR$statistical_base_code=="AB"])[l] 
                dmr_units_i[l]<-outfall_DMR$standard_unit_desc[outfall_DMR$statistical_base_code=="AB"][l]
                eff_limit_i[l]<-as.numeric(outfall_DMR$limit_value_nmbr[outfall_DMR$statistical_base_code=="AB"])[l]
                eff_limit_units_i<-outfall_DMR$limit_unit_desc[outfall_DMR$statistical_base_code=="AB"][l]
                mp_end_i[l]<-outfall_DMR$monitoring_period_end_date[outfall_DMR$statistical_base_code=="AB"][l] 
                mon_in_mp_i[l]<-as.numeric(outfall_DMR$nmbr_of_submission[outfall_DMR$statistical_base_code=="AB"])[l]
                mp_begin_i[l]<-as.character(round_date(mdy(mp_end_i[l]) %m-% months(mon_in_mp_i[l]),unit="month")) 
                violation_i[l]<-outfall_DMR$violation_code[outfall_DMR$statistical_base_code=="AB"][l]
                violation_severity_i[l]<-outfall_DMR$violation_severity[outfall_DMR$statistical_base_code=="AB"][l]
                nodi_i[l]<-outfall_DMR$nodi_desc[outfall_DMR$statistical_base_code=="AB"][l] 
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
          
        }
}

##################################################################################################################################
################################################Compile Data######################################################################

        ECHO_timeseries_wNAs<-data.frame(FacilityName=Facility_Name,Facility.ID=FacilityID,OutfallID=VPDESID,Statistic=statistic,Measured_Effluent=dmr_value,Units=dmr_units,Permitted_Limit=eff_limit,
                               MP_Begin_Date=mp_begin,MP_End_Date=mp_end,Violation_Code=violation,Violation_Severity=violation_severity,NODI=nodi)
        ECHO_timeseries<-ECHO_timeseries_wNAs[!(is.na(ECHO_timeseries_wNAs$MP_End_Date)),] #remove if a monitoring period end date is missing 
        ECHO_timeseries$MP_Begin_Date<-as.Date(ECHO_timeseries$MP_Begin_Date, format="%Y-%m-%d")
        ECHO_timeseries$MP_End_Date<-as.Date(ECHO_timeseries$MP_End_Date, format="%m/%d/%Y")
        
        
        NODI<-subset(ECHO_timeseries,subset=!(ECHO_timeseries$NODI==""))
        NODI<-data.frame(hydrocode=paste0("echo_",NODI$OutfallID), varkey="NODI",entity_type="dh_timeseries",
                         propname="NODI", propvalue="", proptext=as.character(NODI$NODI), propcode="",
                         startdate=NODI$MP_Begin_Date,enddate=NODI$MP_End_Date)
        
        NULLDMR<-subset(ECHO_timeseries,is.na(ECHO_timeseries$Measured_Effluent))
        
        write.csv(NULLDMR,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/QA_QC/NULLDMR.csv")
        save.image("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Code/R Workspaces/ECHO_Timeseries.RData") #Save the global environment for future reference
        
        #In personal file
        write.table(ECHO_timeseries,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Documentation/Imports/ECHO_timeseries_8_23.txt",sep="\t",row.names = F)
       
        #For repository
        write.table(ECHO_timeseries,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/ECHO_timeseries.txt",sep="\t",row.names = F)
        write.table(NODI,"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_mccartma/Documentation/QAQC/NODI.txt",sep="\t",row.names = F)      
##################################################################################################################################
###################################################Analysis#######################################################################
        
        #Track the changes between weekly runs here
        ECHO_timeseries_pre<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Documentation/Imports/ECHO_timeseries_8_13.txt",header=T)
        ECHO_timeseries_pre%>%
          summarise(Outfalls=n_distinct(OutfallID),Facilities=n_distinct(FacilityName),
                    sum=sum(Measured_Effluent,na.rm=T),median=median(Measured_Effluent,na.rm=T),mean=
                    mean(Measured_Effluent,na.rm=T))
        
        ECHO_timeseries_post<-read.table("G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated/Documentation/Imports/ECHO_timeseries_8_23.txt",header=T)
        ECHO_timeseries_post%>%
          summarise(Outfalls=n_distinct(OutfallID),Facilities=n_distinct(FacilityName),
                    sum=sum(Measured_Effluent,na.rm=T),median=median(Measured_Effluent,na.rm=T),mean=
                      mean(Measured_Effluent,na.rm=T))
        
        ECHO_difference<-diff_data(ECHO_timeseries_pre,ECHO_timeseries_post)
        write_diff(ECHO_difference, "ECHO_difference.csv")
        summary(ECHO_difference)
        render_diff(ECHO_difference, title="Difference in Flagged Coordinates", view=interactive(),pretty=T)
