
# Data Dictionary for ECHOvsVPDES.R

## Variables in Script

Variable | Description
--- | --------------------------------------------------------------------------------------------------------
**ECHO_Discharge** | DMR time series data for facilities and their outfalls regulated under the CWA. Includes Facility Name, OutfallID, Statistic of Reported Effluent, Measured Effluent, Units of Effluent, Permitted Limit of Effluent, Monitoring Period Begin and End Date, Violation Code, and Violation Severity. It is important to note that not all monitoring periods are the same length
**ECHO_Facilities** | List of facilities and attributes regulated under the Clean Water Act (CWA) and managed uner the National Pollutant Discharge Elimination System (NPDES) Program. Parameters included in this data frame are described below (**ECHO_Facilities** Parameters)
**VPDES_Outfalls** | Data frame of Outfalls located in the [VPDES Geodatabase](http://www.deq.virginia.gov/mapper_ext/GIS_Datasets/VPDES_Geodatabase.zip). Provides coordinates for each outfall
**VPDES_IP** | Data frame including contact information for each [VPDES Individual Permit](http://www.deq.virginia.gov/Portals/0/DEQ/Water/PollutionDischargeElimination/VPDES%20Spreadsheets/VPDES%20IP%20Contact%20Flow%20for%20WEB%20March%202018.xls?ver=2018-03-13-170732-267). Individual permits are issued by the DEQ for municipal and industrial facilities. Permit specificiations including efflunet limits and monitoring requirements are determined ona  site specific basis. This data frame Provides Design Flow (MGD) for each discharging facility with an individual permit. 
**VA_Discharge_DMR** | Data Frame that includes DMR time series data for CWA regulated outfalls with coordinates and flags for suspicious data. 
**Flagged_Zero_DesFlow** | Data frame including DMR time series data for facilities and their outfalls reporting a design flow of zero MGD.
**Flagged_Design_Flow** | Data frame including DMR time series data for outfalls with measured effluents greater than its design flow (MGD).
**Flagged_Units** | Data frame including DMR time series data for outfalls with measured effluents exceeding 100 times the median flow for the entire outfall. 
**Flagged_ECHO_Violations** | Data frame including DMR time series data for outfalls with a violation code provided by ECHO
      E90: Effluent Violation
      
      D90: DMR Overdue, with a numeric limit
      
      D80: DMR Overdue, monitoring only required

## ECHO_Facilities Parameters 

####(Descriptions from cwa_rest_services.metadata)

Parameter | Description | qcolumn ID
--- | -----------------------------------------------------------------------------| ---
**CWP_Name** | Facility or permit holder name as found in NPDES | 1
**SourceID** | Unique Identifier assigned by EPA | 2
**FAC_Derived_HUC** | The 8-digit Hydrologic Unit Code (HUC) of the watershed in which the facility resides. A HUC number is assigned to every watershed in the nation and uniquely identifies the watershed | 14
**FAC_LAT** | The latitude of the facility in decimal degrees expressed using the NAD83 horizontal datum. The coordinate comes from the FRS EPA Locational Reference Tables (LRT) file which represents the most accurate value for the facility based on the available spatial metadata | 23
**FAC_LONG** | The longitude of the facility in decimal degrees expressed using the NAD83 horizontal datum. The coordinate comes from the FRS EPA Locational Reference Tables (LRT) file which represents the most accurate value for the facility based on the available spatial metadata | 24
**CWPTotalDesignFlowNmbr** | The amount of wastewater flow in MGD that the facility is designed for | 25
**CWP_ACTUAL_AVERAGE_FLOW_NMBR** | The actual amount of the facility's wastewater flow measured in MGD | 26
**CWP_FACILITY_TYPE_INDICATOR** | Each NPDES permit is defined by the program office as a Major or non-major discharger. This field also indicates the permit type. POTW: Publicly Owned Treatment Works | 27
**CWP_PERMIT_STATUS_DESC** | The current stage/status in the NPDES permit life cycle | 60
**CWP_PERMIT_TYPE_DESC** | NPDES facility permit classification:- NPDES Individual Permit- General Permit Covered Facility- NPDES Master General Permit- Associated Permit Record- Individual Industrial User Permit- Individual State Issued Permit- State Issued Master General Permit- Unpermitted Facility | 63
**CWP_EFFECTIVE_DATE** | Date in which NPDES permit effectively started | 65
**CWP_EXPIRATION_DATE** | Date in which NPDES permit expires | 67
**CWP_STATUS** | An indication of the facility's current compliance status under the Clean Water Act:- Significant Violation- Noncompliance- No Violation- Unknown | 84
**CWP_E90_CNT** | Displays the number of instances where effluent limits have been exceeded in the past 3 years (E90 compliance code), based on monthly Discharge Monitoring Reports (DMRs) submitted by facilities | 91
**CWP_INSPECTION_COUNT** | The number of inspections/compliance evaluations, under the corresponding statute, occurring at the facility within the last five years | 95
**CWP_DATE_LAST_INSPECTION** | The date on which the most recent inspection of the facility took place (qcolumn=97).
**CWP_CSO_FLAG** | The discharge from a Combined Sewer System at a point prior to the treatment plant. CSOs are point sources subject to NPDES permit requirements including both technology-based and water quality-based requirements of the Clean Water Act | 204
**CWP_CSO_OUTFALLS** | The number of discharge outfalls at points prior to the treatment plant | 205
**CWP_STATE_WATER_BODY_CODE** | Code from the Assessment TMDL Tracking & Implementation System (ATTAINS) database, assigned by the US Geological Survey, used to classify watersheds in the United States and the Caribbean. The code consists of twelve digits which correspond to six levels of classification:- Region (first-level, 2-digit HUC)- Subregion (second-level, 4-digit HUC)- Accounting unit (third-level, 6-digit HUC)- Cataloguing unit (fourth-level, 8-digit HUC)- Watershed (fifth-level, 10-digit HUC)- Subwatershed (sixth-level, 12-digit HUC) | 206
**CWP_STATE_WATER_BODY_NAME** | The name of the watershed from the ATTAINS database in which the facility resides | 207
**RAD_GNIS_NAME** | The name of the waterbody from the Geographic Names Information System (GNIS) database in which the facility is permitted to discharge directly (RAD: Reach Address Database) | 209
**RAD_REACHCODE** | A nationally unique and permanent identifier for the waterbody, assigned by the US Geological Survey | 210



