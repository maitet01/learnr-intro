##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##
## Script Details ----
# Name of file - exercises.R
# Original author - Russell McCreath
# Orginal date - August 2020
#
# Version of R - 3.6.1
#
# Description - An accompanying R script to work alongside the
# Introduction to R training. This provides possible solutions to the 
# exercises from the slides. The questions are included and answers can be 
# hidden so this can be used during training rather than displaying the
# presentation. All blocks for each example are self contained, this means 
# that packages are loaded repeatedly.
##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##--##


### Explore ----

## Exercise 1 ----
# 1. Read in “Borders.csv” (giving the data frame an appropriate name)
# 2. What are the mean, median, and max values from the LengthOfStay variable? 
#     Can you do this in one step?
# 3. Produce a frequency table to check the sex variable.
#     Save this as an object with an appropriate name.
# 4. Export the frequency table as a csv file.


## Possible Solution ----
# 1. Read in “Borders.csv” (giving the data frame an appropriate name).
library(readr)
borders <- read_csv("data/Borders.csv")

# 2. What are the mean, median, and max values from the LengthOfStay variable? 
#     Can you do this in one step?
library(dplyr)
# Mean
mean(borders$LengthOfStay)
# Median
median(borders$LengthOfStay)
# Max
max(borders$LengthOfStay)
# Summary
summary(borders$LengthOfStay)

# 3. Produce a frequency table to check the sex variable.
#     Save this as an object with an appropriate name.
borders_sex_freq <- addmargins(table(borders$Sex))

# 4. Export the frequency table as a csv file.
write_csv(as.data.frame(borders_sex_freq), "borders_sex_freq.csv")


### Wrangle ----

## Exercise 2 ----
# 1. Read in “Borders.csv” (giving the data frame an appropriate name)
# 2. What patients had a LengthOfStay of between 2 and 10 days? 
# 3. Which of these patients were under Specialty E12 or C8?
# 4. Remove all columns other than URI, Specialty, and LengthOfStay
# 5. Store this data in an appropriately named data frame,
#     ordered by LengthOfStay

## Possible Solution ----
# 1. Read in “Borders.csv” (giving the data frame an appropriate name)
library(readr)
borders <- read_csv("data/Borders.csv")

# 2. What patients had a LengthOfStay of between 2 and 10 days?
library(dplyr)
borders %>%
  filter(LengthOfStay >= 2 & LengthOfStay <= 10)

# 3. Which of these patients were under Specialty E12 or C8?
borders %>%
  filter(LengthOfStay >= 2 & LengthOfStay <= 10) %>%
  filter(Specialty == "E12" | Specialty == "C8")

# 4. Remove all columns other than URI, Specialty, and LengthOfStay
borders %>%
  filter(LengthOfStay >= 2 & LengthOfStay <= 10) %>%
  filter(Specialty == "E12" | Specialty == "C8") %>%
  select(URI, Specialty, LengthOfStay)

# 5. Store this data in an appropriately named data frame,
#     ordered by Main_Condition and LengthOfStay
exercise_2_solution <- borders %>%
  filter(LengthOfStay >= 2 & LengthOfStay <= 10) %>%
  filter(Specialty == "E12" | Specialty == "C8") %>%
  select(URI, Specialty, LengthOfStay) %>%
  arrange(LengthOfStay)


## Exercise 3 ----
# 1. Read in “Borders.csv” (giving the data frame an appropriate name)
# 2. What is the earliest admission date by specialty?
# 3. What is the latest discharge date by specialty?
# 4. What are the number of admissions per hospital, per specialty?

## Possible Solution ----
# 1. Read in “Borders.csv” (giving the data frame an appropriate name)
library(readr)
borders <- read_csv("data/Borders.csv")

# 2. What is the earliest admission date?
library(dplyr)
borders %>%
  group_by(Specialty) %>%
  summarise(min_adm_date = min(DateofAdmission))

# 3. What is the latest discharge date by specialty?
borders %>%
  group_by(Specialty) %>%
  summarise(max_dis_date = max(DateofDischarge))

# 4. What are the number of admissions per hospital, per specialty?
borders %>%
  group_by(HospitalCode, Specialty) %>%
  count()


## Exercise 4 ----
# 1. Select the URI, Specialty, and Dateofbirth columns from the borders data 
#     and save to a new data frame.
# 2. Arrange this new data in ascending order by Specialty and check the results.
# 3. Extract the records with a missing Dateofbirth.
# 4. Finally, recode Specialty “A1” to be “General Medicine”

## Possible Solution
# 1. Select the URI, Specialty, and Dateofbirth columns from the borders data 
#     and save to a new data frame.
library(readr)
library(dplyr)
borders <- read_csv("data/Borders.csv")
subset_borders <- borders %>%
  select(URI, Specialty, Dateofbirth)

# 2. Arrange this new data in ascending order by Specialty and check the results.
library(magrittr)
subset_borders %<>%
  arrange(Specialty)
subset_borders

# 3. Extract the records with a missing Dateofbirth.
missing_dob_borders <- subset_borders %>%
  filter(is.na(Dateofbirth) == TRUE)

# 4. Finally, recode Specialty “A1” to be “General Medicine”
subset_borders %<>%
  mutate(Specialty = recode(Specialty, "A1" = "General Medicine"))


### Visualise ----

## Exercise 5 ----
# 1. Read in “Borders.csv” (giving the data frame an appropriate name)
# 2. Filter data to show only records from HospitalCode “B109H”
# 3. Create a histogram for patient’s LengthOfStay, check the output
# 4. Add axis labels and a title to your histogram
# 5. Save the plot (as a PNG) to the plots folder in your working directory

## Possible Solution ----
# 1. Read in “Borders.csv” (giving the data frame an appropriate name)
library(readr)
borders <- read_csv("data/Borders.csv")

# 2. Filter data to show only records from HospitalCode “B109H”
library(dplyr)
borders_b109h <- borders %>%
  filter(HospitalCode == "B109H")

# 3. Create a histogram for patient’s LengthOfStay, check the output
library(ggplot2)
b109H_los_hist <- ggplot(borders_b109h) +
  geom_histogram(aes(LengthOfStay))
b109H_los_hist

# 4. Add axis labels and a title to your histogram
b109H_los_hist <- ggplot(borders_b109h) +
  geom_histogram(aes(LengthOfStay)) +
  ggtitle("Length Of Stay Histogram") +
  xlab("Length Of Stay") +
  ylab("Count")
b109H_los_hist

# 5. Save the plot (as a PNG) to the plots folder in your working directory
ggsave(filename = "plots/b109H_los_hist.png", plot = b109H_los_hist)


### END OF SCRIPT ###
