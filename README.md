# Patterns and Severity of Non-Motorist Crashes in North Carolina

## Dataset Information
The dataset I chose comes from North Carolina's Department of Transportation (NCDOT), containing police-reported crashes in NC that involved a non-motorist (i.e., pedestrian, bicyclist). The 58328 crashes in this dataset are from January 2007 to December 2025. Each row represents a crash, and contains spatial coordinates, city/county, crash classifications, and person-level variables. This repository includes all the code I used to generate the figures and the corresponding report.

## How to use this repository

This is a Dockerized Project. You can start this project on a Mac or Linux machine.

```bash
git clone https://github.com/hannalee809/BIOS_611_Project.git
cd BIOS_611_Project
chmod +x start.sh
./start.sh
```

Then go to http://localhost:8787. The username is rstudio and the password is dogsandcats.

## Running the analysis

After starting the Docker container and opening RStudio, use the **Console** tab to run

```r
setwd("~/work")
```

Then go over to the **Terminal** tab to run

```bash
make
```

The Makefile runs the analysis in dependency order: it cleans the data, generates the figures, and renders `report.html`. Open `report.html` in a web browser to view the figures and accompanying analysis.

To generate an individual output, specify its target name. For example:

```bash
make Crashes_by_Time_and_Day.png
```

## Project organization

| File | Purpose |
|---|---|
| `NCDOT_NonMotoristCrashes.csv` | Original NCDOT dataset |
| `Non-Motorist_Crash_Data_Dictionary.pdf` | Definitions of the dataset variables |
| `clean_data.R` | Cleans the data and creates weekday labels |
| `vis1_crashes_by_time_and_day.R` | Plots crash counts by hour and day of the week |
| `vis2_ped_bicyclist_crash_types.R` | Generates ranked pedestrian and bicyclist crash-type plots |
| `vis3_crash_severity.R` | Plots severity proportions by month, time of day, and day of the week |
| `vis4_logistic_reg.R` | Fits a logistic regression model and plots adjusted odds ratios for severe injury |
| `report.Rmd` | Source document for the written report |
| `Makefile` | Defines the analysis steps and their dependencies |
| `Dockerfile` | Defines the container environment and installs R packages |
| `start.sh` | Builds the Docker image and starts the RStudio container |

## Makefile targets

A target is an output file or named action that Make can build or run.

| Target | Output or action |
|---|---|
| `all` (default) | Builds `report.html` and all required data files and figures |
| `clean_data.csv` | Creates the cleaned dataset |
| `Crashes_by_Time_and_Day.png` | Shows crash counts by hour and weekday |
| `Pedestrian_Ranked_Crash_Type.png` | Shows ranked pedestrian crash types |
| `Bicyclist_Ranked_Crash_Type.png` | Shows ranked bicyclist crash types |
| `Crash_Severity_Prop_by_Month.png` | Shows severity proportions by month |
| `Crash_Severity_Prop_by_Time_of_Day.png` | Shows severity proportions by time of day |
| `Crash_Severity_Prop_by_Day_of_Week.png` | Shows severity proportions by weekday |
| `injury_df.csv` | Creates the dataset used for the severity regression |
| `Logit_Reg_for_Crash_Severity.png` | Shows adjusted odds ratios and 95% confidence intervals |
| `report.html` | Renders the report with all seven figures |
| `clean` | Deletes generated datasets, figures, and the HTML report |

To remove generated outputs and rebuild the full analysis:

```bash
make clean
make
```
