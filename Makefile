.PHONY: all clean

FIGURES = Crashes_by_Time_and_Day.png \
          Pedestrian_Ranked_Crash_Type.png \
          Bicyclist_Ranked_Crash_Type.png \
          Crash_Severity_Prop_by_Month.png \
          Crash_Severity_Prop_by_Time_of_Day.png \
          Crash_Severity_Prop_by_Day_of_Week.png \
          Logit_Reg_for_Crash_Severity.png

all: report.html

clean_data.csv: clean_data.R NCDOT_NonMotoristCrashes.csv
	Rscript clean_data.R

Crashes_by_Time_and_Day.png: vis1_crashes_by_time_and_day.R clean_data.csv
	Rscript vis1_crashes_by_time_and_day.R

Pedestrian_Ranked_Crash_Type.png Bicyclist_Ranked_Crash_Type.png: vis2_ped_bicyclist_crash_types.R clean_data.csv
	Rscript vis2_ped_bicyclist_crash_types.R
	
Crash_Severity_Prop_by_Month.png Crash_Severity_Prop_by_Time_of_Day.png Crash_Severity_Prop_by_Day_of_Week.png injury_df.csv: vis3_crash_severity.R clean_data.csv
	Rscript vis3_crash_severity.R

Logit_Reg_for_Crash_Severity.png: vis4_logistic_reg.R injury_df.csv
	Rscript vis4_logistic_reg.R

report.html: report.Rmd $(FIGURES)
	Rscript -e "rmarkdown::render('report.Rmd', output_file = 'report.html')"
	
clean:
	rm -f clean_data.csv injury_df.csv $(FIGURES) report.html