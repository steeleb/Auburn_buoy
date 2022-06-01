# Auburn_buoy repository

This repository contains code to collate, clean, and flag the data from the Lake Auburn buoy, located in Auburn, ME, USA. The buoy is operated by H. Ewing of Bates College (hewing@bates.edu) in conjunction with the Auburn Water District/Lewiston Water Division. The QAQC'd data that result from these scripts are archived at the Environmental Data Initiative Data Portal. Underwater sensors are stored in data package edi.XXX and above-water meteorological sensor data are stored in data package edi.XXX. 

These scripts and repository are stewarded by B. Steele (steeleb@caryinstitute, bsteele@bates.edu). Questions about the buoy and the data produced by the buoy should be directed to H. Ewing (hewing@bates.edu).

## Scripts 
All scripts are intended to be run within the RProject file 'Auburn_buoy_data_cleaning.Rproj'.

Processing scripts:

* libraries_lists_functions.R - as the name suggests, this script contains the libraries, lists, and functions referred to in each of the annual processing scripts.
* buoy_YYYY.R - these scripts do the heavy lifting of visualizing and cleaning the buoy data. Each year (YYYY) of buoy data are cleaned in a separate script. 

Preparation for data publication:

* buoy_collation.R - this script applies ODM2 controlled vocabulary to all column headers.
* EML SCRIPT - TBD

## Other documents

A file that contains flag definitions (AubBuoy_FlagCodeDefinitions.xlsx) is also contained in this repository. While the code definitions are included in the EDI metadata, this is an easy way to see definitions. 