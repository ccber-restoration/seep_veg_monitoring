# Overview

This repository is used for summarizing Harder Seep/East Storke Wetland vegetation monitoring data for annual reports.

## Data

Data is collected annually in fall/winter at 8 transects. Each transect is 30 meters long. Surveyors use a 1 square meter quadrat to record percent cover of species, bare ground, thatch, and standing water every 2 meters along the transect, alternating between the left and right side of the transect tape. There have been 2 project phases.

- Phase 1
  - Wetland Transects
    - WL1W
    - WL2W
    - WL1E
  - Upland Transects
    - U1E
    - U2E
    - U1W
- Phase 2
  - Wetland Transects
    - WL3W
    - WL4W
   
A map of the transects can been viewed on AGOL [here](https://ucsb.maps.arcgis.com/home/item.html?id=368afe3e4aa9463d919311adb3044834#overview).

The data used in this repository is collected through Survey123 "[CCBER VegMonitoring QuadratTransects](https://survey123.arcgis.com/surveys/6b7a083bf6124cb1b0b880cbf700f5e9/overview)". Download the data as CSV and use the [ncos-vegetation-monitoring](https://github.com/ccber-restoration/ncos-vegetation-monitoring) repo to clean it before running the code below.

## Code

After running 00_general_quadrat_cleaning.R in the ncos-vegetation-monitoring repo, add the cleaned CSV to this repository in the processed_data folder and use it as an input for other scripts.

- 01_Quadrat_report_Figures.R: Generates bar charts of absolute & relative percent cover for each phase/habitat.
- 02_Vegetation_Appendix.R: Creates species list figures for natives and non-natives in the upland and wetland habitats, showing their average percent cover per monitoring year.


