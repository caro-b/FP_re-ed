# FP_re-ed
# **Final Project on Re-education camps in Xinjiang**

## **Background**
The purpose of this script is to analyze re-education camps in the Xinjiang region in which the Chinese government is interning Moslem minorities.
Until now the detection of these camps is mainly done manually by journalists reviewing various articles, press releases etc. which is quite a time-consuming task.
Therefore this script explores methods to detect these camps more automatically by combining features from multiple data sources. 

## **Analysis **
This analysis focuses on the biggest known re-education camp in Dabancheng, but may be enhanced and applied as a basis for further analyses and bigger areas of interest.
Scenes from two years 2017 and 2020 are used as during this timeframe the most camps were built and mostly fully constructed.
It must be mentioned that this analysis is only applicable to newly constructed camps, not to those for which the building had already existed but was only transferred to a camp from a former usage type.

Data sources:
Natural Earth, OSM, Sentinel2 (ESA), SRTM (NASA), VIIRS (NASA)

![Re-education Camp Change 2017 - 2021](figures/concruction.gif)


## **Outlook**
For further analysis it might be interesting to look into additional features of the re-education camps besides their size, as e.g. their minimum distance to urban structures and road infrastructure. 
Also an Object-Based Image Analysis using segmentation might be interesting to use the full potential of the 15 meter spatial resolutions of the Sentinel-2 data.
