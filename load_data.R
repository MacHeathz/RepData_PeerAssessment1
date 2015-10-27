library(readr)
library(dplyr)
library(lubridate)

# If dataframe already exists, do nothing
if (!exists("activity_monitoring_data")) {
        # Define some strings for the download url, and the names of the zipfile and
        # the datafile.
        url <- "https://d396qusza40orc.cloudfront.net/repdata%2Fdata%2Factivity.zip"
        zip <- "repdata-data-activity.zip"
        file <- "activity.csv"
        
        # Download / unzip the data file if it's not yet in the workdirectory
        if (!file.exists(file)) {
                print("Data file not found.")
                if (!file.exists(zip)) {
                        # use downloader package for platform-independent https handling
                        suppressPackageStartupMessages(library(downloader))
                        print("Downloading data.")
                        download(url, zip)
                }
                print("Unzipping data zipfile.")
                unzip(zip)
                file.remove(zip)
        }

        activity_monitoring_data <- read_csv('activity.csv', col_types = "ici") %>%
                mutate(date = ymd(date))
}

# total <- activity_monitoring_data %>%
#                 group_by(date) %>%
#                 summarise(total.steps = sum(steps))
# 
# hist(total$total.steps)
# rug(total$total.steps)
total.steps <- tapply(activity_monitoring_data$steps, activity_monitoring_data$date, FUN = sum, na.rm = TRUE)
hist(total.steps)
rug(total.steps)
mean(total.steps, na.rm = TRUE)
median(total.steps, na.rm = TRUE)

Intervals <- unique(activity_monitoring_data$interval)
Activity <- tapply(activity_monitoring_data$steps, activity_monitoring_data$interval, FUN = mean, na.rm = TRUE)
bla <- data.frame(Intervals = Intervals, Activity = Activity)
plot(bla, type = "l")
bla2 <- bla[order(-Activity),]

library(impute)
impute_matrix <- as.matrix(data.frame(activity_monitoring_data$steps, activity_monitoring_data$interval))
imputed_result <- impute.knn(impute_matrix)
imp_activity_monitoring_data <- data.frame(steps = imputed_result$data[,1],
                                           date = activity_monitoring_data$date,
                                           interval = activity_monitoring_data$interval)
summary(imp_activity_monitoring_data)

