# Data Dictionary for ECHOInterface.R

## Variables in Script

Variable | Description
--- | --------------------------------------------------------------------------------------------------------------------
**ECHO_Facilities** | List of facilities regulated under the Clean Water Act (CWA) and managed uner the National Pollutant Discharge Elimination System (NPDES) Program
**DMR_Data** | Data frame that contains effluent chart with Discharge Monitoring Report for each point of discharge (outfall)
**ECHOID** | CWA Facility ID Indicated in ECHO System
**VPDESID** | Unique ID used in Virginia to indicate a CWA Facility's Outfall: Concatonates Facility ID with outfall ID.
**outfall_num** | three-character code in ICIS-NPDES that identifies the point of discharge (e.g., 1 is equivalent to 001)
**FlowSum** | Summed Flow over aggregrated monitoring periods
**FlowMed** | Median Flow over aggregated monitoring periods
**Unit** | Associated units for reported flow (usually MGD)
**Limit** | Permitted amount of flow in conduit or thru treatment plant (parameter code 50050) allowed for a CWA regulated facility
**Stat** | Statistical code abbrieviation associated with measured flow value 
**Stat_Description** | Description of Code abbrieviation
**mon_in_year** | Data frame containing months of the year and their days to generate summed discharge over monitoring periods
**FlowFrame** | Data frame containing an ID for each CWA regulated facility, their outfall ID's, outfall number, summed flow over time range, median flow over time range, units for measured effluent, permitted effluent limit, statistic used for measured effluent (i.e average, maximum, total), and  description of the statistic used

## Parameters in ECHO_Facilities Data Frame

Parameter | Description
--- | --------------------------------------------------------------------------------------------------------------------
**CWPName** | Facility or permit holder name as found in NPDES 
**SourceID** | Unique Identifier assigned by EPA 
**CWPStreet** | Street Address 
**CWPCity** | City in which facility is located 
**CWPState** | State in which facility is located, two-digit abbreviation 
**CWPStateDistrict** | State Congressional District in which facility is located 
**CWPEPARegion** | EPA Region in which facility is located. EPA has 10 regions. 
**FacDerivedHuc** | HUC-8 Code Watershed in which facility is located in
**FacInidanSpatialFlg** | Flag if facility is located in Indian Country (*Logical*)
**CWPTotalDesignFlowNmbr** | Design Flow in MGD (Numeric)
**CWPActualAverageFlowNmbr** | Actual Measured Flow in MGD (Numeric)
**ControlMeasure** | Description of control measures employed to comply with effluent limits
**ControlMeasureSchedule** | Description of schedule for control measures
**OverCount80US** | Number of primary environemntal justice indexes exceeding the 80th of higher national percentile for the Census block group that the facility is located in
**PctilePctpre1960US** | National percentile of the census bloack group for the EJScreen Lead paint indicator
**Subsector** | Multi-Sector General Purpose Permit Subsector Individual Identifier