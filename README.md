# Micro-scale Audit of Pedestrian Streetscapes for Safe Routes to School (MAPS-SRTS) Scoring
## Overview
This project contains scoring protocol for the MAPS-SRTS direct observation tool. The development, scoring schema, and reliability measures can be found [here](add in link to article).

## About
- The input for the "maps_srts_scoring.R" file should be the raw MAPS-SRTS data in CSV form with one observation (school) per row and should be placed in the data/raw_data folder and the filename should be "maps_srts_raw.csv"
- Variable names for the raw data should match the variables in the data dictionary, located in the data/metadata folder.
- This code is set up to handle up to 4 school access segments, 14 segments, and 10 crossings.
- If your data set has less than these limits, please delete or add to the scoring code blocks for school access segment scoring, segment scoring, and crossing scoring to match the upper limit of these sections in your data.
 
Questions about this code can be directed to [Dr. Leigh Ann Ganzar](mailto:leigh.a.ganzar@uth.tmc.edu). 
