library(sqldf)
library(splines)
library(brms)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

raw <- read.csv("../data/raw.csv")
raw$ESM_ABDPAIN2 <- raw$ESM_ABDPAIN + 1

raw <- sqldf("select * FROM raw group by REGISTRATION_ID, DAY")

raw <- raw[ , which(names(raw) %in% c("REGISTRATION_ID","ESM_ABDPAIN2","Diary_discomfort", "Diary_belching", "Diary_bloating", "Diary_flatulence", "Diary_diarrhea", "Diary_constipation", "Diary_urge"))]

patient_grp <- raw[raw$REGISTRATION_ID < 2,]
%patient_grp <- patient_grp[ , -which(names(raw) %in% c("REGISTRATION_ID"))]
control_grp <- raw[raw$REGISTRATION_ID > 2,]
%control_grp <- control_grp[ , -which(names(raw) %in% c("REGISTRATION_ID"))]

columns <- names(patient_grp)
to_test <- control_grp

for(marker in columns){
  if(marker == "REGISTRATION_ID" || marker == "ESM_ABDPAIN2"){
    next
  }
  try({
    formula <- paste("ESM_ABDPAIN2~","s(",marker,",by=REGISTRATION_ID, bs='fs')")
    
    formula <- as.formula(formula)
    print(formula)
    print(paste("NA: ", sum(is.na(to_test[marker]))))
    print(paste("Available:", sum(!is.na(to_test[marker]))))
    print("")

    fit <- brm(formula, data=to_test, iter=2000, chains=1)
    summary(fit)
    
    fname <- paste("../images/",marker,".jpeg", sep="")
    jpeg(filename = fname,width=3.25,height=3.25,units="in",res=300)
    print(plot(marginal_effects(fit), ask=FALSE, plot=FALSE)[[1]] + ggplot2::xlim(0,10) + ggplot2::ylim(1,11) +scale_y_continuous(name="ESM_ABDPAIN", limits=c(0, 10)))
    dev.off()
    dev.off()
  })
}