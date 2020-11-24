#########################################################################
# Applies SEV score functions to Stites and Harpster data
# Jason Williams
# last update: 11/23/2020
########################################################################

source("ssc_prediction_and_performance.R") # get data
source("sev_score_fun.R") # define finctions

library(lubridate)

# SEV Analysis using all data-------------------------------------------------------------------------

alldata <-
  rbind(harpster_all_predictions, stites_all_predictions) %>%
  select(site.id, Date, flow_cfs, ssc_mgl, ssc_org, gam_ssc) %>%
  rename(siteid = site.id, date = Date, ssc = gam_ssc,
         measured_ssc = ssc_mgl)

alldata$siteid <-as.character(alldata$siteid)

str(alldata)

# df with siteid (chr), date (Date), ssc (numeric)
# assumes daily data (rather than shorter inverval)


# check format
check_format(alldata)

# calculate max daily z score
harpster_z <-max_daily_z(alldata, "Harpster")
stites_z <-max_daily_z(alldata, "Stites")


# plot max daily z time series
plot_max_daily_z(harpster_z)
plot_max_daily_z(stites_z)

# plot max z, threshold, and duration
plotall(harpster_z)
plotall(stites_z)


# SEV analysis for spring scenario-----------------------------------------------------------


# Feb 1 - June migration period
springdata <-
  alldata %>%
  mutate(daynum = yday(date),
         year = year(date)) %>%
  filter(daynum >= 32 & daynum <= 182)

str(springdata)

# calculate max daily z
stites_06 <-max_daily_z(subset(springdata, year == 2006), "Stites")
stites_07 <-max_daily_z(subset(springdata, year == 2007), "Stites")
stites_08 <-max_daily_z(subset(springdata, year == 2008), "Stites")
stites_09 <-max_daily_z(subset(springdata, year == 2009), "Stites")
stites_10 <-max_daily_z(subset(springdata, year == 2010), "Stites")
stites_11 <-max_daily_z(subset(springdata, year == 2011), "Stites")
stites_spring <-rbind(stites_06, stites_07, stites_08, stites_09,
                      stites_10, stites_11)

harpster_06 <-max_daily_z(subset(springdata, year == 2006), "Harpster")
harpster_07 <-max_daily_z(subset(springdata, year == 2007), "Harpster")
harpster_08 <-max_daily_z(subset(springdata, year == 2008), "Harpster")
harpster_09 <-max_daily_z(subset(springdata, year == 2009), "Harpster")
harpster_10 <-max_daily_z(subset(springdata, year == 2010), "Harpster")
harpster_11 <-max_daily_z(subset(springdata, year == 2011), "Harpster")
harpster_spring <-rbind(harpster_06, harpster_07, harpster_08, harpster_09,
                      harpster_10, harpster_11)


# plot max z
rbind(harpster_spring, stites_spring) %>%
  ggplot(aes(x = daynum, y = maxz_value, 
             color = siteid)) +
    geom_line() +
    facet_wrap(~year) +
    theme_few() +
    ylim(0, 14) +
    scale_color_manual(values = c("black", "darkgrey")) +
    geom_hline(yintercept = 4, linetype = "dotted") +
    geom_hline(yintercept = 9, linetype = "dashed") +
    labs(x = "day of year", y = "max daily z score")


# plot all variables

fig4 <-
rbind(harpster_spring, stites_spring) %>%
  mutate(dummy_date = as.Date(paste0("1901", "-", month(date),"-", day(date)))) %>%
  select(siteid, daynum, date, dummy_date, year, ssc, consec_days, threshold_value, maxz_value) %>%
  melt(id.vars = c("siteid", "daynum", "date", "dummy_date", "year"), variable.name = "parameter",
       value.name = "value") %>%
  ggplot(aes(x = dummy_date, y = value, 
             color = siteid)) +
  geom_line() +
  facet_grid(parameter~year, scales = "free_y") +
  theme_bw() +
  scale_color_manual(values = c("black", "dodgerblue")) +
  scale_x_date(breaks = "month", date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "top", legend.title = element_blank()) +
  labs(x = "Date")
fig4

ggsave("./R_outputs/fig4.png", fig4, width = 8.5, height = 6, units = "in", dpi = 800)

# plot predicted vs observed ssc
rbind(harpster_spring, stites_spring) %>%
  ggplot() +
    geom_line(aes(x = daynum, y = ssc)) +
    geom_point(aes(x = daynum, y = measured_ssc)) +
    facet_grid(siteid ~ year) +
    theme_bw() +
    labs(x = "day of year", y = "ssc (mg/l)")


# plot max z vs days
rbind(harpster_spring, stites_spring) %>%
  ggplot() +
  geom_point(aes(x = maxz_value, y =consec_days)) +
  facet_grid(siteid ~ year) +
  theme_bw()

# plot threshold ssc vs days
rbind(harpster_spring, stites_spring) %>%
  ggplot() +
  geom_point(aes(x = threshold_value, y =consec_days)) +
  facet_grid(siteid ~ year) +
  theme_bw()
  
# summary
summary(harpster_spring)
summary(stites_spring)


# SEV analysis for year-round scenario--------------------------------------------------------

alldata2 <-
  alldata %>%
  mutate(daynum = yday(date),
         year = year(date))

# calculate max daily z
stites_yr_06 <-max_daily_z(subset(alldata2, year == 2006), "Stites")
stites_yr_07 <-max_daily_z(subset(alldata2, year == 2007), "Stites")
stites_yr_08 <-max_daily_z(subset(alldata2, year == 2008), "Stites")
stites_yr_09 <-max_daily_z(subset(alldata2, year == 2009), "Stites")
stites_yr_10 <-max_daily_z(subset(alldata2, year == 2010), "Stites")
stites_yr_11 <-max_daily_z(subset(alldata2, year == 2011), "Stites")
stites_yr <-rbind(stites_yr_06, stites_yr_07, stites_yr_08, stites_yr_09,
                      stites_yr_10, stites_yr_11)

harpster_yr_06 <-max_daily_z(subset(alldata2, year == 2006), "Harpster")
harpster_yr_07 <-max_daily_z(subset(alldata2, year == 2007), "Harpster")
harpster_yr_08 <-max_daily_z(subset(alldata2, year == 2008), "Harpster")
harpster_yr_09 <-max_daily_z(subset(alldata2, year == 2009), "Harpster")
harpster_yr_10 <-max_daily_z(subset(alldata2, year == 2010), "Harpster")
harpster_yr_11 <-max_daily_z(subset(alldata2, year == 2011), "Harpster")
harpster_yr <-rbind(harpster_yr_06, harpster_yr_07, harpster_yr_08, 
                        harpster_yr_09, harpster_yr_10, harpster_yr_11)

# plot max z
rbind(harpster_yr, stites_yr) %>%
  ggplot(aes(x = daynum, y = maxz_value, 
             color = siteid)) +
  geom_line() +
  facet_wrap(~year) +
  theme_few() +
  ylim(0, 14) +
  scale_color_manual(values = c("black", "darkgrey")) +
  geom_hline(yintercept = 4, linetype = "dotted") +
  geom_hline(yintercept = 9, linetype = "dashed") +
  labs(x = "day of year", y = "max daily z score")


# plot all variables
fig5 <-
rbind(harpster_yr, stites_yr) %>%
  mutate(dummy_date = as.Date(paste0("1901", "-", month(date),"-", day(date)))) %>%
  select(siteid, daynum, date, dummy_date, year, ssc, consec_days, threshold_value, maxz_value) %>%
  melt(id.vars = c("siteid", "daynum", "date", "dummy_date", "year"), variable.name = "parameter",
       value.name = "value") %>%
  ggplot(aes(x = dummy_date, y = value, 
             color = siteid)) +
  geom_line() +
  facet_grid(parameter~year, scales = "free_y") +
  theme_bw() +
  scale_color_manual(values = c("black", "dodgerblue")) +
  scale_x_date(breaks = "month", date_labels = "%b") +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "top", legend.title = element_blank()) +
  labs(x = "Date")
fig5 

ggsave("./R_outputs/fig5.png", fig5, width = 8.5, height = 6, units = "in", dpi = 800)

# summary
summary(harpster_yr)
summary(stites_yr)

# monthly average concentration
monthly_ave <-
  rbind(harpster_yr, stites_yr) %>%
  mutate(month = month(date)) %>%
  group_by(siteid, year, month) %>%
  summarize(min_ssc = min(ssc),
            max_ssc = max(ssc),
            mean_ssc = mean(ssc))

# plot monthly average
fig6 <-
monthly_ave %>%
  ggplot(aes(x = month, y = mean_ssc,
             color = as.factor(year), shape = as.factor(year))) + 
  geom_point() +
  geom_line() +
  facet_wrap(~siteid) +
  geom_hline(yintercept = 50, linetype = "dashed") +
  theme_few() +
  theme(legend.title = element_blank()) +
  scale_x_continuous(breaks = c(1, 3, 5, 7, 9, 11)) +
  labs(y = "monthly mean GAM-predicted SSC (mg/l)")
fig6
ggsave("./R_outputs/fig6.png", fig6, width = 6.0, height = 4.0, units = "in", dpi = 800)
