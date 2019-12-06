
library(magrittr)
library(tidyverse)
library(haven)
library(gam)

ftr  = read_rds("results/model_predictors.RDS")
sds  = read_rds('Results/standard_deviations.RDS')
tars = c("nhtn","nd_sbp","nd_dbp")

data = list(
  complete = list(
    train=drop_na(read_rds("datasets/derivation_data.RDS"), !!!ftr),
    test=drop_na(read_rds("datasets/validation_data.RDS"), !!!ftr)
  ),
  imputed = map(read_rds("Results/imputed_data.RDS"), as_tibble)
)


for(f in names(sds)) {
  data$complete$train[[f]] %<>% divide_by(sds[f])
  data$complete$test[[f]] %<>% divide_by(sds[f])
  data$imputed$train[[f]] %<>% divide_by(sds[f])
  data$imputed$test[[f]] %<>% divide_by(sds[f])
}

saveRDS(data,"Results/model_data.RDS")

gam_scope <- purrr::set_names(ftr) %>% 
  map(
    .f = function(f){
      if(!is.factor(data$complete$train[[f]])){
        c("1",f,paste0("s(",f,", df=",c(2,3),")"))
      } else {
        c("1",f)
      }
    }
  )

# can't use map due to scope issues with gam function
GAM <- list()

for(i in seq_along(data)){
  
  model_data = data[[i]]
  
  GAM[[names(data)[i]]] <- purrr::set_names(tars) %>% 
    map(.f=function(tar){
      
      gam.obj <- suppressWarnings(
        gam(
          as.formula(paste(tar,'~ 1')),
          data=model_data$train, 
          family=binomial()
        )
      )
      
      mdl <- suppressWarnings(
        step.Gam(
          gam.obj,
          scope=gam_scope, 
          trace=TRUE,
          steps=ifelse(tar=='nhtn',14,14))
      )
      
      prd = list(
        train = predict(mdl,newdata=model_data$train,type='response'),
        test = predict(mdl,newdata=model_data$test,type='response')
      )
      
      list(mdl=mdl,prd=prd)
      
    }) 
  
}

write_rds(GAM, "Results/fitted_GAM.rds")
