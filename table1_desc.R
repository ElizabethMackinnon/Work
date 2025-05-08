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

# bmi class

data_bd <- subset(data, Dx == "BD")
cat("Number of non-missing values in BMICat column for BD = true:", sum(!is.na(data_bd$BMICat)), "\n")
if (sum(!is.na(data_bd$BMICat)) > 0) {
  BMICat_counts_bd <- table(data_bd$BMICat)
  cat("Counts of unique values in BMICat column for BD = true:\n")
  print(BMICat_counts_bd)
} else {
  cat("The 'BMICat' column has no non-missing values for individuals with BD = true.\n")
}

data_cn <- subset(data, Dx == "CN")
cat("Number of non-missing values in BMICat column for CN = true:", sum(!is.na(data_cn$BMICat)), "\n")
if (sum(!is.na(data_cn$BMICat)) > 0) {
  BMICat_counts_cn <- table(data_cn$BMICat)
  cat("Counts of unique values in BMICat column for cn = true:\n")
  print(BMICat_counts_cn)
} else {
  cat("The 'BMICat' column has no non-missing values for individuals with cn = true.\n")
}

# body weight
bw_data_bd <- subset(data, Dx == "BD")
vars <- "Weight.kg"
bw_bd <- CreateTableOne(vars = vars, data = bw_data_bd)
print(bw_bd)

bw_data_cn <- subset(data, Dx == "CN")
vars <- "Weight.kg"
bw_cn <- CreateTableOne(vars = vars, data = bw_data_cn)
print(bw_cn)

# calculating metabolic disease score bd then cn
data_bd <- subset(data, Dx == "BD")
vars <- c("Obesity", "CholesterolDx", "DiabetesDX", "CardiovascularDx", "HypertensionDx")
data_bd[vars] <- lapply(data_bd[vars], function(x) as.numeric(!is.na(x)))
data_bd$average_values <- rowMeans(data_bd[vars], na.rm = TRUE)
group_average <- mean(data_bd$average_values, na.rm = TRUE)
std_dev <- sd(data_bd$average_values, na.rm = TRUE)
cat("Average number of these variables per person with BD = true:", group_average, "\n")
cat("Standard deviation of average values for BD = true:", std_dev, "\n")

total_counts <- colSums(data_bd[vars], na.rm = TRUE)
print(total_counts)

data_cn<- subset(data, Dx == "CN")
vars <- c("Obesity", "CholesterolDx", "DiabetesDX", "CardiovascularDx", "HypertensionDx")
data_cn[vars] <- lapply(data_cn[vars], function(x) as.numeric(!is.na(x)))
data_cn$average_values <- rowMeans(data_cn[vars], na.rm = TRUE)
group_average <- mean(data_cn$average_values, na.rm = TRUE)
std_dev <- sd(data_cn$average_values, na.rm = TRUE)
cat("Average number of these variables per person with cn = true:", group_average, "\n")
cat("Standard deviation of average values for cn = true:", std_dev, "\n")

total_counts <- colSums(data_cn[vars], na.rm = TRUE)
print(total_counts)

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

lapply(vars, function(var) table(data_bd[[var]]))

# SES details
data_bd <- subset(data, Dx == "BD")
cat("Number of non-missing values in SES column for BD = true:", sum(!is.na(data_bd$SES)), "\n")
if (sum(!is.na(data_bd$SES)) > 0) {
  SES_counts_bd <- table(data_bd$SES)
  cat("Counts of unique values in SES column for BD = true:\n")
  print(SES_counts_bd)
} else {
  cat("The 'SES' column has no non-missing values for individuals with BD = true.\n")
}

data_cn <- subset(data, Dx == "CN")
cat("Number of non-missing values in SES column for CN = true:", sum(!is.na(data_cn$SES)), "\n")
if (sum(!is.na(data_cn$SES)) > 0) {
  SES_counts_cn <- table(data_cn$SES)
  cat("Counts of unique values in SES column for cn = true:\n")
  print(SES_counts_cn)
} else {
  cat("The 'SES' column has no non-missing values for individuals with cn = true.\n")
}


# WHR
data_bd_whr <- subset(data, Dx == "BD")
vars <- "WHR"
bmi_bd_whr <- CreateTableOne(vars = vars, data = data_bd_whr)
print(bmi_bd_whr)

data_cn_whr <- subset(data, Dx == "CN")
vars <- "WHR"
bmi_cn_whr <- CreateTableOne(vars = vars, data = data_cn_whr)
print(bmi_cn_whr)

# Chi square
data_bd$age_group <- cut(data_bd$Age, breaks = c(0, 30, 50, 70, Inf), 
                         labels = c("0-30", "31-50", "51-70", "71+"))
age_contingency <- table(data_bd$age_group, data_bd$Dx)
chi_squared_result <- chisq.test(age_contingency)
print(chi_squared_result)

sex_contingency <- table(data_bd$Sex, data_bd$Dx)
chi_squared_sex <- chisq.test(sex_contingency)
print(chi_squared_sex)

data_bd$BMI_group <- cut(data_bd$BMI, 
                         breaks = c(0, 16.5, 18.5, 25, 30, 35, 40, Inf), 
                         labels = c("Severely Underweight", "Underweight", "Normal", 
                                    "Overweight", "Obese I", "Obese II", "Obese III"))
bmi_contingency <- table(data_bd$BMI_group, data_bd$Dx)
chi_squared_bmi <- chisq.test(bmi_contingency)
print(chi_squared_bmi)

data_bd$WHR_group <- cut(data_bd$WHR, 
                         breaks = c(0, 0.8, 0.9, 1.0, Inf), 
                         labels = c("Low Risk", "Moderate Risk", "High Risk", "Very High Risk"))
whr_contingency <- table(data_bd$WHR_group, data_bd$Dx)
chi_squared_whr <- chisq.test(whr_contingency)
print(chi_squared_whr)
