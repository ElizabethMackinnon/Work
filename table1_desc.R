# Run this one time for each of measure=c(1,2,3)

library(nlme)
library(openxlsx)
library(tableone)
library(ggplot2)
library(ddpcr)
library(sjPlot)

# Read the CSV file
data <- read.csv("C:/Users/Lizma/Downloads/REDCap_Export_Cleaned.csv")

# seperate bd and cn
vars <- "Dx"
dx <- CreateTableOne(vars = vars, strata = "Dx", data = data)
print(dx)

# finding age mean, sdev for bd then cn
data_bd <- subset(data, Dx == "BD")
vars <- "Age"
age_bd <- CreateTableOne(vars = vars, data = data_bd)
print(age_bd)


data_bd <- subset(data, Dx == "CN")
vars <- "Age"
age_cn <- CreateTableOne(vars = vars, data = data_bd)
print(age_cn)


# calculate sex for bd then cn
data_bd_sex <- subset(data, Dx == "BD")
vars <- "Sex"
thy_bd <- CreateTableOne(vars = vars, data = data_bd_sex)
print(thy_bd)

data_cn_sex <- subset(data, Dx == "CN")
vars <- "Sex"
sex_cn <- CreateTableOne(vars = vars, data = data_cn_sex)
print(sex_cn)

#mean and sdev bmi bd then cn
data_bd_bmi <- subset(data, Dx == "BD")
vars <- "BMI"
bmi_bd <- CreateTableOne(vars = vars, data = data_bd_bmi)
print(bmi_bd)

data_cn_bmi<- subset(data, Dx == "CN")
vars <- "BMI"
bmi_cn<-CreateTableOne(vars = vars, data = data_cn_bmi)
print(bmi_cn)

# calculating metabolic disease score bd then cn
data_bd <- subset(data, Dx == "BD")
vars <- c("Obesity", "CholesterolDx", "DiabetesDX", "CardiovascularDx", "HypertensionDx")
data_bd[vars] <- lapply(data_bd[vars], function(x) as.numeric(!is.na(x)))
data_bd$average_values <- rowMeans(data_bd[vars], na.rm = TRUE)
group_average <- mean(data_bd$average_values, na.rm = TRUE)
std_dev <- sd(data_bd$average_values, na.rm = TRUE)
cat("Average number of these variables per person with BD = true:", group_average, "\n")
cat("Standard deviation of average values for BD = true:", std_dev, "\n")

data_cn<- subset(data, Dx == "CN")
vars <- c("Obesity", "CholesterolDx", "DiabetesDX", "CardiovascularDx", "HypertensionDx")
data_cn[vars] <- lapply(data_cn[vars], function(x) as.numeric(!is.na(x)))
data_cn$average_values <- rowMeans(data_cn[vars], na.rm = TRUE)
group_average <- mean(data_cn$average_values, na.rm = TRUE)
std_dev <- sd(data_cn$average_values, na.rm = TRUE)
cat("Average number of these variables per person with cn = true:", group_average, "\n")
cat("Standard deviation of average values for cn = true:", std_dev, "\n")

# thyroid
data_bd_thy <- subset(data, Dx == "BD")
vars <- "ThyroidDx"
thy_bd<-CreateTableOne(vars = vars, data = data_bd_th)
print(thy_bd)

data_cn_thy<-subset(data, Dx == "CN")
vars <- "ThyroidDx"
thy_cn <- CreateTableOne(vars = vars, data = data_cn_thy)
print(thy_cn)

#psych medications

data_bd <- subset(data, Dx == "BD")
vars <- c("Lithium", "AntiEpileptic", "AntiPsychotic", "AntiDepressant", "Benzo")
data_bd[vars] <- lapply(data_bd[vars], function(x) as.numeric(!is.na(x)))
data_bd$average_values <- rowMeans(data_bd[vars], na.rm = TRUE)
group_average <- mean(data_bd$average_values, na.rm = TRUE)
std_dev <- sd(data_bd$average_values, na.rm = TRUE)
cat("Average number of these variables per person with BD = true:", group_average, "\n")
cat("Standard deviation of average values for BD = true:", std_dev, "\n")

# SES details
# Load the dataset
data <- read.csv("REDCap_Export_Cleaned.csv")

# Filter rows where BD = true
data_bd <- subset(data, Dx == "BD")

# Check if the 'SES' column has data for BD = true
cat("Number of non-missing values in SES column for BD = true:", sum(!is.na(data_bd$SES)), "\n")

# Count occurrences in the 'SES' column
if (sum(!is.na(data_bd$SES)) > 0) {
  SES_counts_bd <- table(data_bd$SES)
  cat("Counts of unique values in SES column for BD = true:\n")
  print(SES_counts_bd)
} else {
  cat("The 'SES' column has no non-missing values for individuals with BD = true.\n")
}
# Load the dataset
data <- read.csv("REDCap_Export_Cleaned.csv")

# Filter rows where BD = true
data_bd <- subset(data, Dx == "BD")

# Count occurrences in the 'SES' column for BD = true
SES_counts_bd <- table(data_bd$SES)

# Print the results
print(SES_counts_bd)