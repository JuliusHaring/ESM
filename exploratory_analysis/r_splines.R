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

to_test <- control_grp
eval_types <- c('bars', 'bars_grouped', 'boxplot', 'data', 'dens', 'dens_overlay', 'ecdf_overlay', 'error_binned', 'error_hist', 'error_hist_grouped', 'error_scatter', 'error_scatter_avg', 'error_scatter_avg_vs_x', 'freqpoly', 'freqpoly_grouped', 'hist', 'intervals', 'intervals_data', 'intervals_grouped', 'loo_intervals', 'loo_pit', 'loo_pit_overlay', 'loo_pit_qq', 'loo_ribbon', 'ribbon', 'ribbon_data', 'ribbon_grouped', 'rootogram', 'scatter', 'scatter_avg', 'scatter_avg_grouped', 'stat', 'stat_2d', 'stat_freqpoly_grouped', 'stat_grouped', 'violin_grouped')

for(marker in columns){
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
    
    for(eval_type in eval_types){
      try({
      fname <- paste("../images/evaluations/",marker,"_",eval_type,".jpeg", sep="")
      jpeg(filename = fname,width=3.25,height=3.25,units="in",res=300)
      print(pp_check(fit,paste(eval_type)))
      dev.off()
      dev.off()
      })
    }
  })
}