#####################################################################################
# Plots Flow and SSC patterns 
# Jason Williams
# last update: 11/23/2020
#####################################################################################

# libraries
library(ggplot2)
library(ggthemes)
library(gridExtra)


# read in data--------------------------------------------------------------------------------

project_data <-read.csv("./R_inputs/project_data.csv", header = TRUE)

project_data$Date <-as.Date(project_data$Date, format = "%Y-%m-%d")

str(project_data)



# flow plot----------------------------------------------------------------------------------
flow_plot <-
project_data %>%
  filter(site.id != "Elk City") %>%
    ggplot(aes(x = Date, y = flow_cfs)) +
      geom_line(color = "grey40") +
      facet_wrap(~site.id) +
      labs(y = "daily average flow (cfs)") +
      theme_few() +
      theme(axis.title.x = element_blank(), axis.text = element_text(size = 8),
            axis.title = element_text(size = 8)) 
flow_plot
      

# ssc plot
ssc_plot <-
project_data %>%
  filter(site.id != "Elk City") %>%
  ggplot(aes(x = Date, y = ssc_mgl, shape = ssc_org, color= ssc_org)) +
  geom_point() +
  scale_color_discrete(na.translate = F) +
  scale_shape_discrete(na.translate = F) +
  facet_wrap(~site.id) +
  labs(y = "SSC (mg/l)") +
  theme_few() +
  theme(axis.title.x = element_blank(),legend.position = "bottom",
        legend.title = element_blank(), strip.text = element_blank(), 
        strip.background = element_blank(),
        axis.text = element_text(size = 8), axis.title = element_text(size = 8),
        legend.text = element_text(size = 8))
ssc_plot

# combined plot
flow_ssc_plot <-grid.arrange(flow_plot, ssc_plot, ncol = 1)

# save png
ggsave(".R_outputs/fig2.png", flow_ssc_plot, width = 6, height = 4, units = "in", dpi = 800)

