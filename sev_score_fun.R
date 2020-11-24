



# function to run checks on data ----------------------------------------------------------------------
# write separate functions for each check, then combine into one master check function


# check1 - dataframe column names

check1 <-function(dataframe) {
  
  df_colnames <-colnames(dataframe)
  
  required_colnames <-colnames(data.frame(c("siteid", "date", "ssc")))
  
  ifelse(grepl(paste(required_colnames, collapse = "-.*"), paste(df_colnames, collapse = "-")), 
         "Check 1 (field names): requirements met", 
         "Check 1 (field names): warning! column names siteid, date, and ssc are required")
  
}


# check 2 - data frame column format

check2 <-function(dataframe) {
  
  df_class <-sapply(dataframe, class)
  
  required_class <-c("character", "Date", "numeric")
  names(required_class) <-c("siteid", "date", "ssc")
  
  print("Check 2: field formats")
  
  print(ifelse(df_class == required_class, 
               "field format ok",
               "check field format"))
  
}


# check 3 - missing dates

check3 <-function(dataframe) {
  
  # full date range
  FullSeq <- seq.Date(from = min(dataframe$date), to = max(dataframe$date), by = 1)
  
  # missing dates
  Missing <- FullSeq[!FullSeq %in% dataframe$date]

  print(ifelse(length(Missing) !=0,
         "Check 3 (dates): warning! there are missing days, which may cause inaccurate z scores", 
         "Check 3 (dates): requirments met; no missing days"))
  
  if(length(Missing) !=0) {print(Missing)}
  

  
}


# check 4 - multiple ssc results for a date

check4 <-function(dataframe) {
  
  multiples <-
    dataframe %>%
      group_by(date, siteid) %>%
      summarise(count = n()) %>%
      filter(count > 1)
  
  print(ifelse(max(multiples$count) > 1, 
               "Check 4: warning! some days have multiple results, the daily maximum will be used to calculte z",
               "Check 4: requirements met; 1 result per day"))
  
  if(max(multiples$count) > 1) {print(multiples)}
  
}


# combine all into master format check function

check_format <-function(dataframe) {
  
  print(check1(dataframe))
  
  check2(dataframe)

  check3(dataframe)
  
  check4(dataframe)
  
}


# function to calculate max daily z score for specified site-------------------------------------------


library(data.table) # for group by & mutate

max_daily_z <-function(dataframe, site) {
  
  
  # filter for site
  filtered <-
    dataframe %>%
    filter(siteid == site)
  
  # min and max ssc in dataset
  min_ssc <-ifelse(floor(min(filtered$ssc)) == 0, 1, floor(min(filtered$ssc))) # min possible = 1
  max_ssc <-floor(max(filtered$ssc))
  
  # for each ssc value in range, create df with # consec days > threshold
  
  dflist = list()
  
  for(i in min_ssc:max_ssc) {
    
    df <-
      filtered %>% 
      arrange(date) %>%
      group_by(ID = data.table::rleid(ssc > i)) %>%
      mutate(threshold_value = i,
             consec_days = if_else(ssc > i, row_number(), 0L)) %>%
      mutate(z = 1.0642 + (0.6068 * log(consec_days * 24)) + (0.7384 * log(i)))
    
    df$z[df$z == -Inf] <-0 # if consec_days = 0, set z to 0
    
    dflist[[i]] <- df # add df name to list
    
  }
  
  # combine all dfs together
  combined <-do.call(rbind, dflist)
  
  # get max z by date
  maxz <-
    combined %>%
    group_by(date) %>%
    mutate(maxz_value = max(z)) %>%
    ungroup() %>%
    filter(z == maxz_value) %>%
    arrange(date) %>%
    group_by(date) %>%
    mutate(min_threshold = min(threshold_value)) %>%
    ungroup() %>%
    filter(threshold_value == min_threshold) %>% # keep only one row when consec_days = 0
    arrange(date) %>%
    select(-min_threshold)

}


# function to plot max daily z time series----------------------------------------------------------------
# requires df created by max_daily_z as input

plot_max_daily_z <-function(df) {

zplot <-
  df %>%
    ggplot(aes(x = date, y = maxz_value)) +
      geom_line() +
      theme_few() +
      geom_hline(yintercept = 4, linetype = "dashed") +
      geom_hline(yintercept = 9, linetype = "dashed") +
      ylim(0, 14) +
      labs(y = "maximum daily z score") +
      ggtitle(df$siteid)

  zplot
}


# function to plot time series for all (threshold, duration, max z)----------------------------------------

library(reshape2) # for melt

plotall <-function(df) {
  
  allplot <-
    df %>%
    select(date, ssc, threshold_value, consec_days, maxz_value) %>%
    rename(`duration (days)` = consec_days, `ssc threshold (mg/l)` = threshold_value, 
           `max z score` = maxz_value, `ssc (mg/l)` = ssc) %>%
    melt(id.vars = "date", variable.name = "parameter", value.name = "value") %>%
    ggplot(aes(x = date, y = value)) +
    geom_line() +
    facet_wrap(~parameter, scales = "free_y", ncol = 1) +
    theme_few() +
    ggtitle(df$siteid)
  
  allplot
  
}


  
  

