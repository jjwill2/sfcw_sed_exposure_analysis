#############################################################################################
# Predicts daily ave SSC & evaluates prediction performance
# Jason Williams
# last update: 11-23-2020
############################################################################################

# libraries
library(ggplot2)
library(ggthemes)
library(reshape2)
library(gridExtra)
library(mgcv)

# read in data--------------------------------------------------------------------------------

source("creates_training_and_test_data.R")

str(stites_for_reg)

str(harpster_for_reg)

####### Stites ###############################################################################

# stites regressions---------------------------------------------------------------------------


# model 1 - linear
stites_linear <-lm(ssc_mgl ~  0 + flow_cfs, 
                 data = subset(stites_for_reg, type == "training" ))

  # summary
  summary(stites_linear)

  # diagnostic plots
  par(mfrow = c(2,2))
  plot(stites_linear)

# model 2 - linear with log10 transformation
stites_loglinear <-lm(log10(ssc_mgl) ~ 0 + log10(flow_cfs), 
                 data = subset(stites_for_reg, type == "training"))

  # summary 
  summary(stites_loglinear)
  
  # diagnostic plots
  par(mfrow = c(2,2))
  plot(stites_loglinear)
  
# model 3 - generalized additive model (GAM)
stites_gam <-gam(ssc_mgl ~ s(flow_cfs), 
                  data = subset(stites_for_reg, type == "training"),
                  method = "REML")

  # summary
  summary(stites_gam)
  
  # diagnostic plots
  par(mfrow = c(2,2))
  gam.check(stites_gam)

  
# stites predictions for test data-------------------------------------------------------
  
predict.stites_linear <-stites_linear %>% predict(subset(stites_for_reg, type == "test"))
predict.stites_loglinear <-stites_loglinear %>% predict(subset(stites_for_reg, type == "test")) # log10
predict.stites_gam <-stites_gam %>% predict(subset(stites_for_reg, type == "test"))

# df with predictions
stites_predictions <-
  stites_for_reg %>%
  filter(type == "test") %>%
  cbind(predict.stites_linear, predict.stites_loglinear, predict.stites_gam)
  

# calculate Duan 91983) bias correction factor for stites_reg2 predictions
n <-as.numeric(nrow(stites_for_reg %>% filter(type == "test")))  
n

logdiff <-
  stites_predictions %>%
  mutate(log_ssc = log10(ssc_mgl)) %>%
  mutate(diff = 10^(log_ssc - predict.stites_loglinear))

numerator <-sum(logdiff$diff)
numerator

BCF <-numerator/n
BCF

# df with BCF-adjusted prediction
stites_predictions2 <-
  stites_predictions %>%
  mutate(predict.stites.loglinear_adj = 10^(predict.stites_loglinear) * BCF)


# stites test data performance statistics------------------------------------------------

stites_performance <-
  stites_predictions2 %>%
  select(Date, site.id, flow_cfs, ssc_mgl, ssc_org, predict.stites_linear,
         predict.stites.loglinear_adj, predict.stites_gam) %>%
  melt(id.vars = c("Date", "site.id", "flow_cfs", "ssc_mgl", "ssc_org"),
       variable.name = "model", value.name = "predicted_ssc") %>%
  mutate(bias = predicted_ssc - ssc_mgl,
         error = abs(predicted_ssc - ssc_mgl),
         PD = 100*((predicted_ssc - ssc_mgl) / ssc_mgl),
         RPD = 100*((predicted_ssc - ssc_mgl) / ((predicted_ssc + ssc_mgl)/2)) )


# bias
tapply(stites_performance$bias, stites_performance$model, summary)

# error
tapply(stites_performance$error, stites_performance$model, summary)

# PD
tapply(stites_performance$PD, stites_performance$model, summary)

# RPD
tapply(stites_performance$RPD, stites_performance$model, summary)

# function to calculate r2 of predicted vs observed by model
r2_fun <-function(dataframe) {
  
  for(i in levels(unique(dataframe$model))) {
    
    
    lm <-lm(ssc_mgl~predicted_ssc, data = subset(dataframe, model == i))
    
    print(i)
    
    print(summary(lm))
  
} }

# r2
r2_fun(stites_performance)

# RMSE
RMSE_fun <- function(dataframe) {
  
  for(i in levels(unique(dataframe$model))) { 
  
  subset <-
    dataframe %>% filter(model == i)  
    
  print (i)  
  print(sqrt(mean((subset$predicted_ssc-subset$ssc_mgl)^2)))
  
} }

RMSE_fun(stites_performance)

# scatter of predicted vs observed
stites_performance %>%
  ggplot(aes(x = predicted_ssc, y = ssc_mgl)) +
    geom_point() +
    facet_wrap(~model, nrow = 1) +
    geom_smooth(method = "lm", se = FALSE) +
    geom_abline(slope = 1, linetype == "dashed") +
    theme_few() +
    labs(x = "predicted SSC (mg/l)", y = "measured SSC (mg/l)")

# time series scatter of predicted vs observed
stites_performance %>%
  ggplot() +
  geom_point(aes(x = Date, y = predicted_ssc, color = model, shape = model)) +
  facet_wrap(~model) +
  geom_point(aes(x = Date, y = ssc_mgl, fill = "measured"), shape = 1) +
  theme_few() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs(x = "Date", y = "SSC (mg/l)")


# stites predictions - all days wy 2006-2011------------------------------------------------

stites_all <-
  project_data %>%
  filter(site.id == "Stites") 


stites_all_predictions <-
  stites_all %>%
  mutate(linear_ssc = predict(stites_linear, stites_all),
         loglinear_ssc = (10^(predict(stites_loglinear, stites_all)) * BCF),
          gam_ssc = predict(stites_gam, stites_all))


# plot gam-predicted vs observed
stites_all_predictions %>%
  ggplot() +
    geom_line(aes(x = Date, y = gam_ssc), color = "darkgrey") +
    geom_point(aes(x = Date, y = ssc_mgl), shape = 1) +
    theme_few() +
    labs(y = "SSC (mg/l)") +
    theme(axis.title.x = element_blank()) +
    ggtitle("Stites")

###### Harpster#########################################################################

# harpster  regressions------------------------------------------------------------------

# model 1 - linear
harpster_linear <-lm(ssc_mgl ~ 0 + flow_cfs, 
                   data = subset(harpster_for_reg, type == "training"))
  
  # summary
  summary(harpster_linear)
  
  # diagnostic plots
  par(mfrow = c(2,2))
  plot(harpster_linear)


# model 2 - linear with log10 transformation
harpster_loglinear <-lm(log10(ssc_mgl) ~ 0 + log10(flow_cfs), 
                   data = subset(harpster_for_reg, type == "training"))
  
  # summary 
  summary(harpster_loglinear)
  
  # diagnostic plots
  par(mfrow = c(2,2))
  plot(harpster_loglinear)  
  
# model 3 - generalized additive model (GAM)
harpster_gam <-gam(ssc_mgl ~ s(flow_cfs), 
                   data = subset(harpster_for_reg, type == "training"),
                   method = "REML")
  
  # summary
  summary(harpster_gam)
  
  # diagnostic plots
  par(mfrow = c(2,2))
  gam.check(harpster_gam)
  
  
# harpster predictions for test data-------------------------------------------------------
  
predict.harpster_linear <-harpster_linear %>% predict(subset(harpster_for_reg, type == "test"))
predict.harpster_loglinear <-harpster_loglinear %>% predict(subset(harpster_for_reg, type == "test")) # log10
predict.harpster_gam <-harpster_gam %>% predict(subset(harpster_for_reg, type == "test"))

# df with predictions
harpster_predictions <-
  harpster_for_reg %>%
  filter(type == "test") %>%
  cbind(predict.harpster_linear, predict.harpster_loglinear, predict.harpster_gam)
  
  
# calculate Duan 91983) bias correction factor for stites_reg2 predictions
n_harp <-as.numeric(nrow(harpster_for_reg %>% filter(type == "test")))  
n_harp
  
logdiff_harp <-
  harpster_predictions %>%
  mutate(log_ssc = log10(ssc_mgl)) %>%
  mutate(diff = 10^(log_ssc - predict.harpster_loglinear))
  
numerator_harp <-sum(logdiff_harp$diff)
numerator_harp
  
BCF_harp <-numerator_harp/n_harp
BCF_harp
  
# df with BCF-adjusted prediction
harpster_predictions2 <-
  harpster_predictions %>%
  mutate(predict.harpster.loglinear_adj = 10^(predict.harpster_loglinear) * BCF_harp)  
  
  
  
# stites test data performance statistics------------------------------------------------

harpster_performance <-
  harpster_predictions2 %>%
  select(Date, site.id, flow_cfs, ssc_mgl, ssc_org, predict.harpster_linear,
         predict.harpster.loglinear_adj, predict.harpster_gam) %>%
  melt(id.vars = c("Date", "site.id", "flow_cfs", "ssc_mgl", "ssc_org"),
       variable.name = "model", value.name = "predicted_ssc") %>%
  mutate(bias = predicted_ssc - ssc_mgl,
         error = abs(predicted_ssc - ssc_mgl),
         PD = 100*((predicted_ssc - ssc_mgl) / ssc_mgl),
         RPD = 100*((predicted_ssc - ssc_mgl) / ((predicted_ssc + ssc_mgl)/2)) )


# bias
tapply(harpster_performance$bias, harpster_performance$model, summary)

# error
tapply(harpster_performance$error, harpster_performance$model, summary)

# PD
tapply(harpster_performance$PD, harpster_performance$model, summary)

# RPD
tapply(harpster_performance$RPD, harpster_performance$model, summary)

# r2
r2_fun(harpster_performance)

# RMSE
RMSE_fun(harpster_performance)

# scatter of predicted vs observed
harpster_performance %>%
  ggplot(aes(x = predicted_ssc, y = ssc_mgl)) +
  geom_point() +
  facet_wrap(~model, nrow = 1) +
  geom_smooth(method = "lm", se = FALSE) +
  geom_abline(slope = 1, linetype == "dashed") +
  theme_few() +
  labs(x = "predicted SSC (mg/l)", y = "measured SSC (mg/l)")

# time series scatter of predicted vs observed
harpster_performance %>%
  ggplot() +
  geom_point(aes(x = Date, y = predicted_ssc, color = model, shape = model)) +
  facet_wrap(~model) +
  geom_point(aes(x = Date, y = ssc_mgl, fill = "measured"), shape = 1) +
  theme_few() +
  theme(legend.title = element_blank(), legend.position = "bottom") +
  labs(x = "Date", y = "SSC (mg/l)")  
  
  
# harpster predictions - all days wy 2006-2011------------------------------------------------

harpster_all <-
  project_data %>%
  filter(site.id == "Harpster") 


harpster_all_predictions <-
  harpster_all %>%
  mutate(linear_ssc = predict(harpster_linear, harpster_all),
         loglinear_ssc = (10^(predict(harpster_loglinear, harpster_all)) * BCF_harp),
         gam_ssc = predict(harpster_gam, harpster_all))


# plot gam-predicted vs observed
harpster_all_predictions %>%
  ggplot() +
  geom_line(aes(x = Date, y = gam_ssc), color = "darkgrey") +
  geom_point(aes(x = Date, y = ssc_mgl), shape = 1) +
  theme_few() +
  labs(y = "SSC (mg/l)") +
  theme(axis.title.x = element_blank()) +
  ggtitle("Harpster")

  
  

# plot gam predicted vs observed time series for both sites together---------------------------

ssc_timeseries <-
  rbind(harpster_all_predictions, stites_all_predictions) %>%
  ggplot() + 
  facet_wrap(~site.id, ncol = 2) +
  geom_line(aes(x = Date, y = gam_ssc, color = "GAM-predicted")) +
  geom_point(aes(x = Date, y = ssc_mgl, fill = "observed"), shape = 1, size = 0.5) +
  scale_fill_manual(name = "", values = c("observed" = "grey")) +
  scale_color_manual(name = "", values = c("GAM-predicted" = "darkgrey")) +
  theme_few() +
  labs(y = "SSC (mg/l)") +
  theme(axis.title.x = element_blank(), legend.position = c(0.25, 0.9),
        legend.background = element_blank(), legend.box = "horizontal",
        axis.text = element_text(size = 8), axis.title = element_text(size = 8),
        legend.text = element_text(size = 8))
ssc_timeseries

ssc_scatter <-
  rbind(harpster_all_predictions, stites_all_predictions) %>%
  filter(!is.na(ssc_mgl)) %>%
  ggplot(aes(x = gam_ssc, y = ssc_mgl)) +
    geom_abline(slope = 1, linetype = "dashed") +
    geom_point(alpha = 0.5, size = 0.75) +
    facet_wrap(~site.id, ncol = 2) +
    theme_few() +
    scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500)) +
    labs(x = "GAM-predicted SSC (mg/l)", y = "measured SSC (mg/l)") +
    theme(axis.text = element_text(size = 8), axis.title = element_text(size = 8),
          legend.text = element_text(size = 8))
ssc_scatter


# combined plot
performance_plot <-grid.arrange(ssc_timeseries, ssc_scatter, ncol = 1)

ggsave("./R_outputs/Fig3.png", performance_plot, width = 6, height = 4, units = "in", dpi = 800)


