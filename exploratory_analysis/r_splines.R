library(splines)
library(brms)
library(ggplot2)
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

test_mode <- FALSE

filtered <- read.csv("../data/filtered.csv")

patient_grp <- filtered[filtered$REGISTRATION_ID < 2,]
patient_grp$ESM_ABDPAIN2 <- patient_grp$ESM_ABDPAIN + 1
control_grp <- filtered[filtered$REGISTRATION_ID > 2,]
control_grp$ESM_ABDPAIN2 <- control_grp$ESM_ABDPAIN + 1

columns <- names(control_grp)
columns <- columns[grepl("ESM_", columns)]
columns <- columns[!columns %in% c("ESM_ABDPAIN","ESM_ABDPAIN2","ESM_START","ESM_END")] # Removes ESM_ABDPAIN (target)

if(test_mode){
  patient_grp <- control_grp[0:100,]
  columns <- c("ESM_GAS2","ESM_GAS1")
}

to_test <- patient_grp

for(marker in rev(unique(columns))){
  try({
    formula <- paste("ESM_ABDPAIN2~","s(",marker,",by=REGISTRATION_ID, bs='fs')")
    if(test_mode){
      formula <- paste("ESM_ABDPAIN2~","",marker)
    }
    formula <- as.formula(formula)
    print(formula)
    print(paste("NA: ", sum(is.na(to_test[marker]))))
    print(paste("Available:", sum(!is.na(to_test[marker]))))
    print("")

    fit <- brm(formula, data=to_test, iter=1000, chains=1, family=cumulative("logit"))
    summary(fit)
    
    fname <- paste("../images/",marker,".jpeg", sep="")
    jpeg(filename = fname,width=3.25,height=3.25,units="in",res=300)
    print(plot(marginal_effects(fit), ask=FALSE, plot=FALSE)[[1]] + ggplot2::xlim(0,10) + ggplot2::ylim(1,11) +scale_y_continuous(name="ESM_ABDPAIN", limits=c(0, 10)))
    dev.off()
    dev.off()
  })
}