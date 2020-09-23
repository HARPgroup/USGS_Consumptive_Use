# USGS Consumptive Use Data Transfer, Export, and Analysis

## Description
This repository is intended to hold code, documentation, and tutorials for the USGS Consumptive Use Project run through Dr. Julie Shortridge and Dr. Durelle Scott of Virginia Tech as well as Robert Burgholzer and Joseph Kleiner of the Virginia Department of Environmental Quality.

## Sponsorship and Disclaimer

This material is based upon work supported by the U.S. Geological Survey under Cooperative Agreement No. G17AC00322. The views and conclusions contained in this document are those of the authors and should not be interpreted as representing the opinions or policies of the U.S. Geological Survey. Mention of trade names or commercial products does not constitute their endorsement by the U.S. Geological Survey.

Assembled: 10/10/2017

## Objectives
This project has two objectives. The first is to develop a set of data retrieval and export tools to supply updated NPDES DMR data to the VAHydro data system, and to then export withdrawl, discharge, and consumptive use data from VAHydro in a machine-readable format consistent with NWIS requirements. The second objective is to leverage this DMR data to estimate consumptive use across different user categories, assess trends in consumptive use through time, and develop statistical methods for estimating non-reported consumptive use.  

## Navigation and Directions
This project includes multiple scripts that can be used to extract NPDES point-source discharge data from the US EPA Enforcement Compliance History Online (ECHO) database and combine these records with withdrawal data to estimate water consumption. The figure below (visual mapping of scripts) presents a flow diagram describing the different R scripts contained in this repository and how they relate to each other to generate consumption calculations. 

## Contributors

##### Robert Burgholzer

Surface Water Modeler | Office of Water Supply | Virginia Department of Environmental Quality

<Robert.Burgholzer@deq.virginia.gov>
    
##### Joseph Kleiner

Senior Surface Water Modeler | Virginia Department of Environmental Quality

B.S. Biological Systems Engineering | Virginia Tech | 2016

<joseph.kleiner@deq.virginia.gov>

##### Dr. Julie Shortridge

Assistant Profesor & Extension Specialist | Virginia Tech | Department of Biological Systems Engineering

Ph.D Geography and Environmental Engineering | Johns Hopkins University | 2016

<jshortridge@vt.edu>
    
##### Dr. Durelle Scott
Assistant Professor | Virginia Tech | Department of Biological Systems Engineering

Ph.D Civil Engineering | University of Colorado at Boulder | 2001

<dscott@vt.edu>

##### Connor Brogan
B.S./M.S. Biological Systems Engineering | Virginia Tech | 2018

<connorb5@vt.edu>

##### Morgan McCarthy
M.S Biological Systems Engineering | Virginia Tech | 2019

<mccartma@vt.edu>

## Visual Mapping of Scripts

![](https://github.com/mccartma/USGS_Consumptive_Use/blob/master/Script_Mapping/Morgan%20Code%20Flow%20Chart.jpg)

### R scripts
ECHOInterface.R 

ECHO_Timeseries.R

ECHO_QAQC.R

VWUDS_QAQC.R

Facility_Matching.R

VA_Statewide_Consumptive_Use.R

VA_County_Consumptive_Use.R

VA_HUC6_Consumptive_Use.R

VA_HUC8_Consumptive_Use.R

VA_HUC10_Consumptive_Use.R

VA_HUC12_Consumptive_Use.R



 

