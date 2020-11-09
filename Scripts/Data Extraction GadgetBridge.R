
# load the packages we'll need
library(DBI)
library(lubridate)
library(plyr); library(dplyr)

# define the directory that contains the SQLite database from Gadgetbridge
in_dir <- 'D:\\Directory\\'

# function to read in the data
read_gadgetbridge_data <- function(in_dir_f, db_name_f){
  # connect to the sqlite data base
  con = dbConnect(RSQLite::SQLite(), dbname=paste0(in_dir_f, db_name_f))
  # load the table with the Mi-Fit walking info
  # (MI_BAND_ACTIVITY_SAMPLE)
  # the others contain other information not relevant for this exercise
  # select on HEART_RATE and RAW_INTENSITY to get non-missing observations
  # otherwise, size of data is huge b/c it records 1 line / second
  raw_data_f = dbGetQuery(con,'select * from MI_BAND_ACTIVITY_SAMPLE where 
                          HEART_RATE != -1 and RAW_INTENSITY != -1' )  
  # close the sql connection
  dbDisconnect(con)
  # Convert unix timestamp to proper R date object
  # make sure to set the timezone to your location!
  raw_data_f$TIMESTAMP_CLEAN <- lubridate::as_datetime(raw_data_f$TIMESTAMP, tz = "Europe/Paris") 
  # format the date for later aggregation
  raw_data_f$hour <- lubridate::hour(raw_data_f$TIMESTAMP_CLEAN)
  year_f <- lubridate::year(raw_data_f$TIMESTAMP_CLEAN)
  month_f <- lubridate::month(raw_data_f$TIMESTAMP_CLEAN)
  day_f <- lubridate::day(raw_data_f$TIMESTAMP_CLEAN)
  raw_data_f$date <- paste(year_f, month_f, day_f, sep = '-')
  return(raw_data_f)
}

# load the raw data with the function
raw_data_df <- read_gadgetbridge_data(in_dir, 'Gadgetbridge')


head(raw_data_df,10)


# make the aggregation per day / hour
make_hour_aggregation <- function(input_data_f){
  # set values of greater than 250 to NA
  input_data_f$HEART_RATE[input_data_f$HEART_RATE > 250] <- NA

  # aggregate to day / hour
  day_hour_agg_f <- input_data_f %>% 
    group_by(date, hour) %>% 
    summarize(hourly_steps = sum(STEPS, na.rm = T),
              mean_heart_rate = mean(HEART_RATE, na.rm = T),
              sd_heart_rate = sd(HEART_RATE, na.rm = T)) %>%
    # create column for cumulative sum
    mutate(cumulative_daily_steps = cumsum(hourly_steps)) %>% 
    # create column for daily total
    group_by(date) %>% mutate(daily_total = sum(hourly_steps)) 
  
  # add day of the week
  day_hour_agg_f$dow <- wday(day_hour_agg_f$date, label = TRUE)
  
  # add a weekday/weekend variable 
  day_hour_agg_f$week_weekend <- NA
  day_hour_agg_f$week_weekend[day_hour_agg_f$dow == 'Sun'] <- 'Weekend'
  day_hour_agg_f$week_weekend[day_hour_agg_f$dow == 'Sat'] <- 'Weekend'
  day_hour_agg_f$week_weekend[day_hour_agg_f$dow == 'Mon'] <- 'Weekday'
  day_hour_agg_f$week_weekend[day_hour_agg_f$dow == 'Tue'] <- 'Weekday'
  day_hour_agg_f$week_weekend[day_hour_agg_f$dow == 'Wed'] <- 'Weekday'
  day_hour_agg_f$week_weekend[day_hour_agg_f$dow == 'Thu'] <- 'Weekday'
  day_hour_agg_f$week_weekend[day_hour_agg_f$dow == 'Fri'] <- 'Weekday'
  
  # put the columns in the right order
  day_hour_agg_f <- day_hour_agg_f %>% select(date, daily_total,  hour,  
                                              hourly_steps,  cumulative_daily_steps, 
                                              dow, week_weekend, mean_heart_rate, sd_heart_rate)
  
  # add column meta-data for device
  day_hour_agg_f$device <- 'MiBand'
  
  return(day_hour_agg_f)
  
}

omnibus_mi_band <- make_hour_aggregation(raw_data_df)

# we should have 18 observations / day
# except for day in progress when data are extracted
table(omnibus_mi_band$date)
table(table(omnibus_mi_band$date))

# plot - same from first accupedo blog post

# load the ggplot2 package  
library(ggplot2)  

# make the plot
ggplot(data = omnibus_mi_band, aes(x = hour, y = cumulative_daily_steps, color = week_weekend)) +   
  geom_point() +  
  coord_cartesian(ylim = c(0, 20000)) +  
  geom_smooth(method="loess", fill=NA) +  
  theme(legend.title=element_blank()) +  
  labs(x = "Hour of Day", y = "Cumulative Step Count", 
       title = 'Cumulative Step Count Across the Day: Weekdays vs. Weekends' )

# different lines per day of week  
# (not shown on the blog)
ggplot(data = omnibus_mi_band, aes(x = hour, y = cumulative_daily_steps, color = dow)) +   
  geom_point() +  
  coord_cartesian(ylim = c(0, 20000)) +  
  geom_smooth(method="loess", fill=NA) +  
  labs(x = "Hour of Day", y = "Cumulative Step Count", 
       title = 'Cumulative Step Count Per Hour By Day of the Week', 
       color = 'Day of Week')

