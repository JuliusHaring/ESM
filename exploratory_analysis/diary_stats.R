library(sqldf)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

raw <- read.csv("../data/raw.csv")
raw2 <- sqldf("select * from raw group by REGISTRATION_ID, DAY")
