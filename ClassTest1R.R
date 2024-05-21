#Question 1
Retest_data <- read.csv("retest.csv")
str(Retest_data)
head(Retest_data, 15)
num_rows <- nrow(Retest_data)
num_rows

#Question 2
Retest_data$dateandtime <- as.Date(Retest_data$dateandtime, format = "%m/%d/%Y")
str(Retest_data)

#Question 3
original_names <- names(Retest_data)
original_names

names(Retest_data)[names(Retest_data) == "dateandtime"] <- "DateTime"
names(Retest_data)[names(Retest_data) == "duration..hours.min."] <- "TotalDuration"
names(Retest_data)[names(Retest_data) == "duration..seconds."] <- "DurationSeconds"
names(Retest_data)[names(Retest_data) == "value1"] <- "MeanValue_1"
names(Retest_data)[names(Retest_data) == "value2"] <- "MeanValue_2"

updated_names <- names(Retest_data)
updated_names

#Question 4
Retest_data$MeanValue_2 <- as.numeric(Retest_data$MeanValue_2)
str(Retest_data)

#Question 5
install.packages("mice")
library(mice)
install.packages("VIM")
library(VIM)

complete_records <- sum(complete.cases(Retest_data))
cat("Number of records with no missing data:", complete_records, "\n")

datetime_missing <- sum(is.na(Retest_data$DateTime))
cat("Number of datetime records missing:", datetime_missing, "\n")

missing_counts <- colSums(is.na(Retest_data))
variable_with_most_missing <- names(which.max(missing_counts))
cat("Varialbe with the largest amount of missing data points:", variable_with_most_missing, "\n")

total_cells <- prod(dim(Retest_data))
available_data <- sum(!is.na(Retest_data))
percent_available_data <- (available_data/total_cells) * 100
cat("Percentage of data available without missing data points:", percent_available_data, "\n")

#Question 6
initial_rows <- nrow(Retest_data)
Retest_data_clean <- na.omit(Retest_data)
final_rows <- nrow(Retest_data_clean)
deleted_records <- initial_rows - final_rows
cat("Number of records deleted due to missing data points:", deleted_records, "\n")
str(Retest_data_clean)
#All of the data is deleted as there are no records with no missing data, there is ~15% of the data missing

Retest_data_clean <- Retest_data[!complete.cases(Retest_data),]

#Question 7
windows(16,10)
par(mfrow = c(1, 2))

hist(Retest_data$MeanValue_1,
     breaks = 12, # no of bins on chart
     col = "red",
     xlab = "MeanValue_1",
     main = "Mean Value 1 Values")
hist(Retest_data$MeanValue_2,
     breaks = 12, # no of bins on chart
     col = "blue",
     xlab = "MeanValue_2",
     main = "Mean Value 2 Values")


#Question 8
install.packages("dplyr")
library(dplyr)
Retest_data_sorted <- Retest_data  %>% arrange(shape, city)
sorted_Retest_data <- Retest_data_sorted %>% select(DateTime, city, country, shape)
head(sorted_Retest_data, 15)
str(sorted_Retest_data)

#Question 9
Retest_data_gb_disk <- subset(Retest_data, country == "gb" & shape == "disk")
Retest_sub <- nrow(Retest_data_gb_disk)
cat("Number of records in Retest_data_gb_disk:", Retest_sub, "\n")

#Question 10
write.csv(Retest_data, "modified_Retest.csv", row.names = FALSE)
write.csv(Retest_sub, "Retest_sub.csv", row.names = FALSE)
write.csv(sorted_Retest_data, "sorted_Retest.csv", row.names = FALSE)
