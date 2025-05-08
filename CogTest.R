library(nlme)
library(openxlsx)
library(tableone)
library(ggplot2)
library(ddpcr)
library(sjPlot)

data <- read.csv("C:/Users/Lizma/Downloads/REDCap_Export_Cleaned.csv")

FINDRISC_data_bd <- subset(data, Dx == "BD")
vars <- "FINDRISC"
FINDRISC_bd <- CreateTableOne(vars = vars, data = data_bd)
print(FINDRISC_bd)

FINDRISC_data_cn <- subset(data, Dx == "CN")
vars <- "FINDRISC"
FINDRISC_cn <- CreateTableOne(vars = vars, data = data_cn)
print(FINDRISC_cn)

YMRS_data_bd <- subset(data, Dx == "BD")
vars <- "YMRS"
YMRS_bd <- CreateTableOne(vars = vars, data = data_bd)
print(YMRS_bd)

YMRS_data_cn <- subset(data, Dx == "CN")
vars <- "YMRS"
YMRS_cn <- CreateTableOne(vars = vars, data = data_cn)
print(YMRS_cn)

HAM.D_data_bd <- subset(data, Dx == "BD")
vars <- "HAM.D"
HAM.D_bd <- CreateTableOne(vars = vars, data = data_bd)
print(HAM.D_bd)

HAM.D_data_cn <- subset(data, Dx == "CN")
vars <- "HAM.D"
HAM.D_cn <- CreateTableOne(vars = vars, data = data_cn)
print(HAM.D_cn)

Clinical.Frailty.Scale_data_bd <- subset(data, Dx == "BD")
vars <- "Clinical.Frailty.Scale"
Clinical.Frailty.Scale_bd <- CreateTableOne(vars = vars, data = data_bd)
print(Clinical.Frailty.Scale_bd)

Clinical.Frailty.Scale_data_cn <- subset(data, Dx == "CN")
vars <- "Clinical.Frailty.Scale"
Clinical.Frailty.Scale_cn <- CreateTableOne(vars = vars, data = data_cn)
print(Clinical.Frailty.Scale_cn)

Cognitive.failures_data_bd <- subset(data, Dx == "BD")
vars <- "Cognitive.failures"
Cognitive.failures_bd <- CreateTableOne(vars = vars, data = data_bd)
print(Cognitive.failures_bd)

Cognitive.failures_data_cn <- subset(data, Dx == "CN")
vars <- "Cognitive.failures"
Cognitive.failures_cn <- CreateTableOne(vars = vars, data = data_cn)
print(Cognitive.failures_cn)

Total.CIRSG.score_data_bd <- subset(data, Dx == "BD")
vars <- "Total.CIRSG.score"
Total.CIRSG.score_bd <- CreateTableOne(vars = vars, data = data_bd)
print(Total.CIRSG.score_bd)

Total.CIRSG.score_data_cn <- subset(data, Dx == "CN")
vars <- "Total.CIRSG.score"
Total.CIRSG.score_cn <- CreateTableOne(vars = vars, data = data_cn)
print(Total.CIRSG.score_cn)
