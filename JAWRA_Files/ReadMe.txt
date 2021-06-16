# Estimating water consumption of commercial, industrial, municipal, and thermoelectric users from monthly withdrawal and discharge 
## Supporting Code 

## Description
This repository is intended to hold code associated with the manuscript "Estimating water consumption of commercial, industrial, municipal, and thermoelectric users from monthly withdrawal and discharge" by Morgan McCarthy, Connor Brogan, Julie Shortridge, Robert Burgholzer, Joseph Kleiner, and Durelle Scott. This manuscript is currently under review at Journal of the American Water Resources Association. 

## Sponsorship and Disclaimer
This material is based upon work supported by the U.S. Geological Survey under Cooperative Agreement No. G17AC00322. The views and conclusions contained in this document are those of the authors and should not be interpreted as representing the opinions or policies of the U.S. Geological Survey. Mention of trade names or commercial products does not constitute their endorsement by the U.S. Geological Survey.

## Navigation and Description
This repository includes several R scripts that can be modified to replicate the data analysis described in the manuscript. A brief description of each is provided below: 

00_ECHO_Timeseries.R: Query and download discharge records from the EPA Enforcement Compliance History Online (ECHO) database. Output is a dataframe with time series of DMR entries. 

01_ECHO_QAQC.R: Classify unknown facility water use types, identify external outfalls, flag anomalous data. 

02_VWUDS_QAQC.R: Process monthly withdrawal data and classify unknown facility water use types.

03_Facility_Matching.R: Identify facilities present in both withdrawal and discharge datasets based on geographic coordinates and name similarity. 

04_Facility_Analysis.R: Calculate water consumption across facilities in the matched dataset generated in 03_Facility_Matching.R

05_VA_HUC10_Consumptive_Use.R: Calculates spatially aggregated consumptive use estimates for 2016 at the HUC-10 watershed scale.

05_VA_HUC6_Consumptive_Use.R: Same as above, but for HUC-6

05_VA_HUC8_Consumptive_Use.R: Same as above, but for HUC-8

05_VA_Statewide_Consumptive_Use.R: Calculates monthly spatially aggregated consumptive use estimates for the state of Virginia through time. 

Last updated: 6/16/2021

## Contact
For questions about the code contained in this repository, please contact Julie Shortridge (jshortridge at vt.edu)
