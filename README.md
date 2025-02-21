# README

## OAG RAD Data Analyst Code Sample Repository

### Repository Contents
*  _Main code sample_: __`MSP_enrollment_plot.R`__
*  _Other project files_: 
    * `MSP_data_cleaning.R` combines the primary data sources from the Center for Medicare & Medicaid Services, MassGIS, and the US Census Bureau into a usable format for generating plots, maps, and Shiny tools, as well as a [link to the processed dataset](https://drive.google.com/file/d/1C4G0NUO2CfAASyI3VD6w92veP3LbY5EO/view?usp=drive_link).
    * `plot_data.RData` is a smaller subset of the data produced in `MSP_data_cleaning.R` for the purpose of the code sample.
    * `MSP_enrollment_change_map.png` is the output figure generated by the code sample, showing change in Medicare Savings Programs enrollment after Massachusetts expanded eligilibility for the programs.

### Project Overview
This code sample is from a project I completed under the supervision of Dr. Maggie Shi at the University of Chicago. The goal was to assess the impact of a policy campaign I worked on as an organizer with Massachusetts Senior Action Council to expand access to Medicare Savings Programs.

Medicare Savings Programs (MSP) lower health care costs paid by low-income seniors. By enrolling in a Medicare Savings Program, eligible seniors immediately have their Medicare Part B premiums paid for them (~ $2,000 per year) and receive the Social Security Administration's Part D Extra Help benefit for prescription costs (~ $5,300 value per year).

States can set their own MSP eligibility criteria beyond federal baselines. In Massachusetts, the income eligibility threshold and asset limit for MSP were significantly raised in 2020 and again in 2023. 

My project depicted where Massachusetts' expanded eligibility led to greater enrollment in MSP. This information would be valuable to policymakers, advocates, and seniors themselves who are organizing to improve health care assistance. The project created plots, maps, and Shiny app tools to illuminate descriptive relationships between demographics, enrollment, and policy change. Data sources for these outputs are Center for Medicare and Medicaid Services (CMS) enrollment data, US Census demographic data, and Massachusetts county-level shapefiles.

### Reproduction Instructions
* The __`MSP_enrollment_plot.R`__ is my code sample, which can be run by following these steps:
    * In line 5, assign to `directory` the file path containing `plot_data.RData` which can be downloaded from this repo.
    * Running the `ggsave` function in lines 33-37 will save a map to your local directory. To simply view the map in RStudio, run the `print` command in line 39.
