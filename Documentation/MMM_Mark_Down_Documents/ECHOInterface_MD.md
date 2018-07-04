ECHOInterface.R
================

Purpose
-------

The purpose of this script is to utilize the EPA's Enforcement and Compliance Online History (ECHO) database to extract measured effluent and permitted limits from facilities regulated under the Clean Water Act (CWA) and managed under the National Pollutant Discharge Elimination System (NPDES) Program. The EPA provides a myriad of REST (Representational State Transfer) services that utlize the internet's HTTP to query this data from simple URL links. Services include a Water Facility Search, Detailed Facility Reports, and Effluent Charts for discharging facilities in the United States.

The following code uses these services to:

1.  Generate a list of CWA managed facilities (*Water Facility Search*).
2.  Use Discharge Monitoring Reports (DMR's from *Effluent Chart Service*) to indicate active outfalls for each facility that track flow, in conduit or thru treatment plant.
3.  Aggregate discharge quantities and effluent limits for each active outfall over a defined time range.

The script ultimately produces a dataframe labeled *FlowFrame* that contains:

-   Facility IDs for each CWA regulated facility (*ECHOID*) located in the state of Interest.
-   Points of discharge attached to each Facility (*outfall\_num*) that report flow in conduit or through the plant.
-   A unique ID for each point of discharge, concatenated with the Facility ID (*VPDESID*).
-   Aggregated discharges (Sum: *FlowSum* and Median: *FlowMed*) and permit limits (Median: *Limit*) that fall within three possible statistical categories.
    -   Maximum
    -   Average
    -   Total +Two character abbreviation for reported statistic (*Stat*) +Description of abbreviated statistic (*Stat\_Description*)

Ultimately this statewide discharge data will be combined with withdrawal data to refine State Water Budget estimates. Accurate representations of water supply will fuel more informed policies and management practices.

Resources
---------

The following links include inforamtion about the REST services used to query data associated with discharging facilities.

ECHO CWA REST Services: [Facility Search - Water](https://echo.epa.gov/tools/web-services/facility-search-water)

ECHO EFF REST Services: [Effluent Charts-Discharge Monitoring Reports](https://echo.epa.gov/tools/web-services/effluent-charts)

ECHO DFR REST Services: [Detailed Facility Report](https://echo.epa.gov/tools/web-services/detailed-facility-report)

Application Programming Interface (API) [Descriptions](https://www.any-api.com/epa_gov/cwa/docs/API_Description)

Data Dictionary
---------------

The data dictionary for this script can be found [here](G:\My%20Drive\USGS_ConsumptiveUse\Summer,%202018\Morgan\Data%20Dictionaries\Codebook-for-ECHOInterface)

Initilization
-------------

To start off, a clean workspace is required to avoid clutter and potential errors:

``` r
rm(list=ls())
```

The *XML*, *RCurl*, and *lubridate* functions are then loaded. These packages deal with XML downloads, interact with web-based material, and manipulate dates.

``` r
library(XML) #downloads and reads XML file types-used to access ECHORest Services to download facilities
library(RCurl) #downloads and interacts with web-based content
library(lubridate) #parses and manipulates dates
```

In order to use the ECHO REST Services, primary inputs are required. Several query type parameters can be defined to build a highly customized search. JSONP, JSON, and CSV formats are the main output options offered by ECHO, however, XML and CSV downloads are demonstrated in this script. In this initilization section, the **state** of interest is defined, along with a **start and end dates** for the date of interest. If these dates are not specified, the service will return the last three years of data by default.

``` r
state<-"VA" #Input for State of Interest. Must be Inputted as Abreviation 
startDate<-"01/01/2012" #mm/dd/yyyy: data on ECHO is limited to 2012 for most sites or 2009 for a few
endDate<-"12/31/2017" #mm/dd/yyyy
path<-"G:/My Drive/ECHO NPDES/USGS_Consumptive_Use_Updated"
```

It is important to note that ECHO presents data as-reported from facilities in each state, meaning data quality may variable depending on location. Typically, major facilities tend to have more complete data while smaller facilities can vary widely. QA/QC checks with state regulated permit databases are recommended to investigate data completeness and quality.

CWA Facility Download
---------------------

This section of the code generate a query in the CWA Water Facility Search REST Services in ECHO.It pulls every discharging facility regulated by the CWA with a NPDES permit. The base URL for this REST service is: (<https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities>?)

This particular query is created in two-steps. An XML document of all CWA facilities in the state of interest is first downloaded (*uri\_query* and *ECHO\_xml*). Then the XML is parsed (*ECHO\_query*) to generate a query ID (*QID*) that can be used to download summary data for each facility (*uri\_summary*). This list of discharging facilities and attribute data is then read in table format and created into a data frame named *ECHO\_Facilities*.

``` r
uri_query<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_facilities?output=XML&p_st=",state)
ECHO_xml<-getURL(uri_query) #this function downloads the URL from above, which creates an XML of facilities with CWA permit
ECHO_query<-xmlParse(ECHO_xml) #parses the downloaded XML of facilities and generates an R structure that represents the XML/HTML tree-main goal is to see query ID or QID

QID<-xmlToList(ECHO_query) #Converts parsed query to a more R-like list and stores it as a variable
QID<-QID$QueryID #Extracts Query ID 
uri_summary<-paste0("https://ofmpub.epa.gov/echo/cwa_rest_services.get_download?output=CSV&qid=",QID) #Uses QID to extract facility summary data
ECHO_Facilities<-read.csv(uri_summary,stringsAsFactors = F)
rm(uri_summary,uri_query,ECHO_query,ECHO_xml,QID) #Clear unnecessary variables
```

    ## Observations: 1,947
    ## Variables: 16
    ## $ CWPName                  <chr> " SHORE HEALTHCARE GROUP LLC", "ABB I...
    ## $ SourceID                 <chr> "VA0063606", "VA0086355", "VAL026531"...
    ## $ CWPStreet                <chr> "26181 PARKSLEY RD", "171 INDUSTRY DR...
    ## $ CWPCity                  <chr> "PARKSLEY", "BLAND", "ABINGDON", "MAP...
    ## $ CWPState                 <chr> "VA", "VA", "VA", "VA", "VA", "VA", "...
    ## $ CWPStateDistrict         <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ CWPEPARegion             <int> 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3...
    ## $ FacDerivedHuc            <int> 2080109, 5050002, 6010102, 2080109, 2...
    ## $ FacIndianSpatialFlg      <chr> "N", "N", "N", "N", "N", "N", "N", "N...
    ## $ CWPTotalDesignFlowNmbr   <dbl> 0.020, 0.010, NA, NA, NA, NA, NA, NA,...
    ## $ CWPActualAverageFlowNmbr <dbl> NA, NA, NA, 0.009, 0.009, 0.020, 0.24...
    ## $ ControlMeasure           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ ControlMeasureSchedule   <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...
    ## $ Over80CountUs            <int> 7, 0, 0, 6, 2, 0, 0, 0, NA, 0, 0, 0, ...
    ## $ PctilePctpre1960Us       <dbl> 86.7, 27.1, 47.4, 83.3, 81.1, 18.6, 4...
    ## $ Subsector                <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, N...

Manipulation of Summary Facility Data
-------------------------------------

This section of the code then loops through each CWA Facility in *ECHO\_Facilities* and obtains its Discharge Monitoring Report (DMR) for their attached points of discharge. The REST service used for extracting DMR data uses the following base URL: (<https://ofmpub.epa.gov/echo/eff_rest_services.get_effluent_chart>?)

Information including number of outfalls, outfall ID's, flow values, and statistical codes for those flows are collected. Each facility reports different statistics for their measured effluent, ranging from averages, maximums, and totals. A list of the statistical codes used in ECHO and their descriptions can be downloaded [here](https://echo.epa.gov/system/files/REF_ICIS-NPDES_STATISTICAL_BASE.csv). Flow is thus aggregated (by sum and median) for each reported statistic over the monitoring periods in the time range specified in the initilization section of this code.

Before entering the for loop, variables for desired information are defined for future population, as well as a data frame containing the days and months in a calendar year.

``` r
FlowSum<-numeric() #Summed Flow 
FlowMed<-numeric() #Median Flow
Units<-character() #Units reported for Flow
Limit<-numeric() #Limit/permitted amount of discharge for a single facility-not regulary updated in VA
VPDESID<-character() #Unique ID used in Virginia for a facility's outfall: concatonated facility ID with 3 digit outfall ID
ECHOID<-character() #Facility ID indicated in ECHO System
outfall_num<-character() #Outfall ID
Stat<-character() #Statistical Code Abrievation associated with flow value. Includes averages, maximums, minimums, etc. 
Stat_Description<-character() #Description of Code
mon_in_year<-data.frame(month=1:12,days=c(31,29,31,30,31,30,31,31,30,31,30,31)) #Months of the year and their days to be used to generate summed discharge over monitoring periods
```

The following for loop then goes through each facility located in the *ECHO\_Facilities* data frame (by *SourceID*) to extract associated effluent charts. The Effluent Chart REST Services are used to query this information (*uri\_effluent* and *DMR\_Data*). An effluent chart for a single facility contains detailed discharge limits, Discharge Monitoring Report (DMR) and NPDES Violation information for a given date range, effluent parameter, or outfall. ECHO has 347 defined effluent parameters including major pollutants. Since discharge quantity is the parameter of interest, *DMR\_data* is filtered to solely represent parameter code 50050 (flow in conduit or thru treatment plant). The data frame is further filtered to search for outfalls with no discharge or influent and subsequently set their DMR value to 0 rather than NA. This is indicated by a No Data Indicator Code (*nodi\_code*) of C (No Discharge) or 7 (No Influent).

``` r
for (i in 1:length(ECHO_Facilities$SourceID)) {
  sourceID<-ECHO_Facilities$SourceID[i]
  print(paste("Processing SourceID: ",sourceID," (",i," of ",length(ECHO_Facilities$SourceID),")", sep=""))
  uri_effluent<-paste0("https://ofmpub.epa.gov/echo/eff_rest_services.download_effluent_chart?p_id=",sourceID,"&start_date=",startDate,"&end_date=",endDate) #CWA Effluent Chart ECHO REST Service for a single facility for a given timeframe
  DMR_Data<-read.csv(uri_effluent,stringsAsFactors = F)#reads downloaded CWA Effluent Chart that contains DMR for a single facility
  DMR_Data<-DMR_Data[DMR_Data$parameter_code==50050,]#only looks at Flow, in conduit or thru treatment plant - there are 347 parameter codes defined in ECHO
  DMR_Data$dmr_value_nmbr[DMR_Data$nodi_code %in% c('C','7')]<-0#nodi_code is the unique code indicating the reason why an expected DMR value was not submitted, C (no Discharge) and 7 (No influent). So set to 0.
  DMR_Data$monitoring_period_end_date<-as.Date(as.POSIXct(DMR_Data$monitoring_period_end_date,"EST",format='%m/%d/%Y'))#Convert dates to Date class through POSIXct forcing
```

Next, the length of data within a single DMR is evaluated (*data\_length*. This checks for the number of submissions/monitoring periods within the designated date range. If the facility is not active during the date range, all reported variables are set to *NA*. Otherwise, if the *data\_length* is greater than 0, it will enter the rest of the for loop.

``` r
data_length<-length(unique(DMR_Data$monitoring_period_end_date))#sees if there is any reported data worth extracting and examining
```

Once it's determined that the facility's DMR contains sufficient data, the asscoiated points of discharge are indicated. Points of discharge are labeled as permanent features in ECHO and will be referred to as outfalls (*outfall\_nmbr* and *outfall\_ID*). The *outfall\_ID* is a three-character code in NPDES that identifies the point of discharge for each facility (e.g., 1 is equivalent to 001). Therefore, zeros need to be appended to outfall ID's less than three characters.

``` r
  if (data_length>0){#if the value is NOT NA, enter loop
    outfall_nmbr<-unique(DMR_Data$perm_feature_nmbr)#Stores Outfalls which are called permanent features in the DMR
    outfall_ID<-unique(DMR_Data$perm_feature_nmbr)#perm_feature_nmbr is a three-character code in ICIS-NPDES that identifies the point of discharge
    for (j in 1:length(outfall_ID)){#If the code is less than three characters in the .CSV, append zeros to the beginning of the number (e.g., 1 is equivalent to 001)
      if(!is.na(as.numeric(outfall_ID[j]))){
        addedzeroes<-paste(rep(0,3-nchar(outfall_ID[j])),collapse = '')#this section assigns leading zeros in front of the outfall numbers if they are under 3 characters
        outfall_ID[j]<-paste0(addedzeroes,as.character(outfall_ID[j]))
      } else{
        outfall_ID[j]<-as.character(outfall_ID[j])#if the outfall number is already three digits, no reformatting needed
      }
    }
```

Once each outfall is identified for a single facility, the code loops through the DMR for each unique outfall (*outfall\_DMR*).The number of unqiue statistics reported for each outfall (*stats*) is extracted and intermediate variables are created to store aggregated flows (*FlowSumi* and *FlowMedi*), units of flow (*Unitsi*), permitted effluent limits (*Limiti*), and the description of the reported statistics (*Stat\_Descriptioni*) for each unique outfall associated with the single facility at hand.

``` r
    for (k in 1:length(outfall_ID)){ #Now we go through the DMR attached to each unique individual outfall and extract the information we would like
      outfall<-as.character(outfall_ID[k])
      outfall_DMR<-DMR_Data[DMR_Data$perm_feature_nmbr==outfall_nmbr[k],]#specifies that we want to go through each unique outfall
      stats<-unique(outfall_DMR$statistical_base_code)#Determine how many statistics are reported for this outfall
      FlowSumi<-numeric(length(stats))#Create areas to store extracted flows, units, and limits
      FlowMedi<-numeric(length(stats))
      Unitsi<-numeric(length(stats))
      Limiti<-numeric(length(stats))
      Stat_Descriptioni<-unique(outfall_DMR$statistical_base_code)
```

The next section of code focuses on the statistics reported for a single outfall (i.e. Average, Maximum, Total). This ensures that flows are being aggregated seperately for each statistic reported for each outfall in a single facility. In order to aggregate flow by summation, the number of days within the monitoring periods needs to be defined (*nodays*).

``` r
  for (j in 1:length(stats)){ #For Each Statistical Code Reported for the Outfall, store the DMR 
        outfall_stat<-outfall_DMR[outfall_DMR$statistical_base_code==stats[j],]
        nodays<-numeric()
        nmbr<-numeric()
```

The number of submissions within a DMR correlates with the number of months within the monitoring period. Therefore, the date in which the monitoring period ends (*mo*) and the number of submissions (*nmbr*) can be used to calculate the number of days within that period (*nodays*).

``` r
    for (l in 1:length(outfall_stat$nmbr_of_submission)){ #The number of submissions in a DMR correlates with the number of months within the monitoring period
          mo<-month(outfall_stat$monitoring_period_end_date[l]) 
          nmbr[l]<-outfall_stat$nmbr_of_submission[l]
          nodays[l]<-sum(mon_in_year$days[mon_in_year$month%in%seq(mo-nmbr[l]+1,mo)]) #Knowing the number of months and days within those months, we can get days in each monitoring period
        }
```

Once the number of days is calculated, the sum of discharge for each reported statistic for each outfall within a facility can be aggregated. The median of all discharge records is also stored, along with associated units and the permitted effluent limit amount.

``` r
FlowSumi[j]<-sum(outfall_stat$dmr_value_nmbr*nodays,na.rm=T)/sum(nodays)#Store the aggregated sum of all discharge for this outfall. This sheds light on seasonal trends but may be affected by outliers/typos
        FlowMedi[j]<-median(outfall_stat$dmr_value_nmbr,na.rm=T)#Store the median of all discharge records for this outfall. The median helps eliminate the need to spot quarterly vs. annual. monthly data
        Unitsi[j]<-unique(outfall_stat$standard_unit_desc)#Find the units being associated with this particular outfall
        LimitswNA<-unique(outfall_stat$limit_value_nmbr)#Store limits and eliminate if NA or take median if multiple
```

It should be noted that the effluent limits stored in ECHO are variable and scarse for some states. If this is the case, NPDES permit values should be crossed checked with a state maintained database. If more than one limit is reported for the reported statistic, the median limit is stored. The description of the statistic evaluated in the nested for loop is finally stored.

``` r
 if(length(LimitswNA)>1){#Occasionally limits report as NA which can alter this code
          if(length(LimitswNA[!is.na(LimitswNA)])>1){ #Remember limits are the permitted amounts of discharge the facility is allowed to release into surface water
            warning("More than one real limit found, only using median")
            Limiti[j]<-median(outfall_stat$limit_value_nmbr[outfall_stat$limit_end_date==max(outfall_stat$limit_end_date,na.rm=T)],na.rm=T)
          }else{
            Limiti[j]<-LimitswNA[!is.na(LimitswNA)] 
          }
        }else{
          Limiti[j]<-LimitswNA
        }
  Stat_Descriptioni[j]<-unique(outfall_stat$statistical_base_short_desc)#Store the statistic used in overall for loop starting at 
      }
```

Once each reported statistic for each unique outfall in a single facility is evaluated, the flows, units, limits, and statistical codes for the iteration are stored in the large data frames that were defined prior to entering the entire for loop. The unique facility ID (*SourceID*) assigned by the EPA is stored as *ECHOID*, while a unique ID for each outfall in the state is created by concatonating the facility ID with the three digit *outfall\_ID* (*VPDESID*).

``` r
      FlowSum<-c(FlowSum,FlowSumi)#Store flow, units, limits, and stat codes as needed
      FlowMed<-c(FlowMed,FlowMedi)
      Units<-c(Units,Unitsi)
      Limit<-c(Limit,Limiti)
      Stat<-c(Stat,stats)
      Stat_Description<-c(Stat_Description,Stat_Descriptioni)
      outfall_num<-c(outfall_num,rep(outfall,length(stats)))
      VPDESID<-c(VPDESID,paste0(sourceID,rep(outfall,length(stats))))
      ECHOID<-c(ECHOID,rep(sourceID,length(stats)))
    }
```

If the CWA facility did not contain any data, the associated flows, units, limits, and statistical codes for the facility are stored as *NA*

``` r
  }else{
    FlowSum<-c(FlowSum,NA)
    FlowMed<-c(FlowMed,NA)
    Units<-c(Units,NA)
    Limit<-c(Limit,NA)
    Stat<-c(Stat,NA)
    Stat_Description<-c(Stat_Description,NA)
    outfall_num<-c(outfall_num,NA)
    VPDESID<-c(VPDESID,NA)
    ECHOID<-c(ECHOID,sourceID)
  }
}
```

Export Data
-----------

Once the code has looped through each facility and collected their facility ID, each outfall ID, and summed and median flow for each reported statistic, they are collectively stored in a data frame labeled *FlowFrame*. Only completed cases are kept, removing facilities missing relevant DMR data.

``` r
FlowFrame<-data.frame(ECHOID,VPDESID,outfall_num,FlowSum,FlowMed,Units,Limit,Stat,Stat_Description)
FlowFrame<-FlowFrame[!is.na(FlowFrame$VPDESID),] #Keep only completed cases
```

Conclusion
----------

Ultimately, the **ECHOInterface.R** script uses ECHO's REST Services to query CWA regulated facilities within a state of interest and summarize their measured effluents (by reported statistic) for a specified data range. These summarized discharges will be compared to their NPDES Permits maintained by the Virginia Department of Environmental Quality and analyzed for trends, and suspicious/erroneous data in **AnalysisCode.R** as a QA/QC measure. The discharges will then be combined with state maintained withdrawal data and transfer data to calculate consumptive water use over several spatial scales (**VA\_HUC\_8\_Consumptive\_Use.R**,**VA\_HUC\_10\_Consumptive\_Use.R**, & **VA\_HUC\_12\_Consumptive\_Use.R**). Accurate estimates of consumptive water use in Virginia will aid the DEQ's capacity to improve permitting, plan for water scarce scenarios, and model stream flows and water availability. Indicating hot spots of consumtpive use over spatial scales will also fuel more informed water supply management plans.
