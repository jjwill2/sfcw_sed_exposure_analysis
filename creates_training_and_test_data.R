#############################################################################################
# Creates training & test data for model development and preformance analysis, respctively
# Jason Williams
# 11-23-2020
###########################################################################################

library(dplyr)
library(ggplot2)

# read in data--------------------------------------------------------------------------------

project_data <-read.csv("./R_inputs/project_data.csv", header = TRUE)

project_data$Date <-as.Date(project_data$Date, format = "%Y-%m-%d")

str(project_data)

# create Stites training and test data--------------------------------------------------------

stites_data <-
  project_data %>%
  filter(site.id == "Stites") %>%
  filter(!is.na(ssc_mgl)) %>%
  arrange(Date)

stites_data$id <-1:nrow(stites_data)

# set seed for reproducible random sampling
set.seed(10)

stites_training <-
  stites_data %>%
  sample_frac(0.5) %>%
  mutate(type = "training")

stites_test <-
  stites_data %>%
  filter(!id %in% stites_training$id) %>%
  mutate(type = "test")

stites_for_reg <-
  rbind(stites_training, stites_test) %>%
  select(-X)

# plot flow vs ssc for each type
stites_for_reg %>%
  ggplot(aes(x = flow_cfs, y = ssc_mgl, color = type)) +
    geom_point()


# create Harpster training and test data--------------------------------------------------------

harpster_data <-
  project_data %>%
  filter(site.id == "Harpster") %>%
  filter(!is.na(ssc_mgl)) %>%
  arrange(Date)

harpster_data$id <-1:nrow(harpster_data)

# set seed for reproducible random sampling
set.seed(10)

harpster_training <-
  harpster_data %>%
  sample_frac(0.5) %>%
  mutate(type = "training")

harpster_test <-
  harpster_data %>%
  filter(!id %in% harpster_training$id) %>%
  mutate(type = "test")

harpster_for_reg <-
  rbind(harpster_training, harpster_test) %>%
  select(-X)

# plot flow vs ssc for each type
harpster_for_reg %>%
  ggplot(aes(x = flow_cfs, y = ssc_mgl, color = type)) +
  geom_point()
