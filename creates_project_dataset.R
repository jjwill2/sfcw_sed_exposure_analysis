#############################################################################################
# Creates Projecat Dataset 
# Daily ave flow and SSC data from 3 SF Clearwater sites
# Jason Williams
# last update: 11/9/2020
############################################################################################

# libraries
library(dplyr)



# reads in data----------------------------------------------------------------------------

# daily average flow from USGS gage sites
flows <-read.csv("./R_inputs/USGS_daily_ave_flow.csv", header = TRUE, stringsAsFactors = FALSE)

str(flows)

# SSC data
wq <-read.csv("./R_inputs/SFCW_data_from_LROSW.csv", header = TRUE, stringsAsFactors = FALSE,
              na.strings = "NULL")

str(wq)


# formats & combines--------------------------------------------------------------------------

# predict harpster daily ave flow based on Harpster-Stites regression
harpster_dailyave <-
  flows %>%
  filter(site == "USGS-13338500") %>%
  mutate(flow_cfs = (flow..cfs. * 0.9962) - 31.947) %>% # Harpster flow based on regression between stites & harpster
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  select(site, Date, flow_cfs)
harpster_dailyave$site <-"USGS-13338100"

str(harpster_dailyave)

# combine & formate flow data
flow_formatted <-
  flows %>%
  select(site, Date, flow..cfs.) %>%
  rename(flow_cfs = flow..cfs.) %>%
  mutate(Date = as.Date(Date, format = "%Y-%m-%d")) %>%
  filter(site != "USGS-13338100") %>%
  rbind(harpster_dailyave) %>%
  mutate(site.id = ifelse(site == "USGS-13338100", "Harpster",
                          ifelse(site == "USGS-13338500", "Stites", "Elk City"))) %>%
  filter(Date >= "2005-10-01") %>%
  filter(Date <= "2011-09-30") %>%
  select(site.id, Date, flow_cfs)
str(flow_formatted)


# format SSC
wq_formatted <-
  wq %>%
  filter(Characteristic.Name == "Suspended Sediment Concentration (SSC)") %>%
  mutate(ssc_mgl = as.numeric(Result.Value),
         ssc_org = ifelse(Project.ID == "lp3", "USFS", "USGS"),
         site.id = ifelse(Monitoring.Location.ID %in% c("lp1-USGS-13338500", "lp3-Stites"), "Stites",
                          ifelse(Monitoring.Location.ID %in% c("lp1-USGS-13338100", "lp3-Wimer"), "Harpster",
                                                               "Elk City")),
         Date = as.Date(Activity.Start.Date, format = "%m/%d/%Y")) %>%
  select(site.id, Date, ssc_mgl, ssc_org)
str(wq_formatted)


# combine daily average flow & SSC measurements
project_data <-
  merge(flow_formatted, wq_formatted, by = c("Date", "site.id"), all.x = TRUE) 

str(project_data)

write.csv(project_data, "./R_inputs/project_data.csv")
