# FP_re-ed
# **Final Project on Re-education camps in Xinjiang**

## **Background**
The purpose of this script is to analyze re-education camps in the Xinjiang region in which the Chinese government is interning Moslem minorities.
Until now the detection of these camps is mainly done manually by journalists reviewing various articles, press releases etc. which is quite a time-consuming task.
Therefore this script explores methods to detect these camps more automatically by combining features from multiple data sources. 

![Re-education Camps in Xianjing, China](data/figures/camps_xinjiang.png)


## **Analysis**
This analysis focuses on the biggest known re-education camp in Dabancheng, but may be enhanced and applied as a basis for further analyses and bigger areas of interest.

![Sentinel-2 Scene (2020)](data/figures/plot_aoi.png)

Sentinel-2 scenes from two years 2017 and 2020 are used as during this timeframe the most camps were built and mostly fully constructed.

![Re-education Camp Change 2017 - 2021](data/figures/construction.gif)

It must be mentioned that this analysis is only applicable to newly constructed camps, not to those for which the building had already existed but was only transferred to a camp from a former usage type.

Data sources:
Natural Earth, OSM, Sentinel2 (ESA), SRTM (NASA), The Xinjiang Data Project, VIIRS (NASA)


A hierarchical classification scheme was applied using masking to filter out areas unlikely to inhabit a camp.
The following metrics were used to idenrify potential camp areas:
1. nightlights data (VIIRS) to identify built-up areas which are lit up during the night
2. digital elevation model (SRTM) to filter out areas with a high area and slope
3. SAVI (Soil Adjusted Vegetation Index) to filter out vegetated areas (camps are assumed) 
4. CVA (Change Vector Analysis)
5. Texture

The assumptions are based on the information provided by The Xinjiang Data Project:
https://xjdp.aspi.org.au/data/?tab=datasets#



## **Outlook**
For further analysis it might be interesting to look into additional features of the re-education camps besides their size, as e.g. their minimum distance to urban structures and road infrastructure. 
Also an Object-Based Image Analysis using segmentation might be interesting to use the full potential of the 15 meter spatial resolutions of the Sentinel-2 data.
