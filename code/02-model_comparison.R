
rm(list=ls())

library(data.table)
library(tidyverse)
library(magrittr)
library(survival)
library(obliqueRSF)
library(party)
library(randomForestSRC)
library(xgboost)
library(rms)
library(glmnet)
library(obliqueRF)
library(pROC)
library(party)
library(mlr)
library(pec)
library(DescTools)
library(arm)
library(mice)
library(gam)
library(missForest)
library(ResourceSelection)


rmse<-function(prediction,outcome){
  x<-prediction-outcome
  sqrt(mean(x^2, na.rm = TRUE))
}

analysis = file.path('Datasets', 'derivation_data.RDS') %>% 
  readRDS() %>% 
  dplyr::select(-dataset) %>% 
  as.data.frame()

tars = c('nhtn', 'ndbp', 'slp_sbp', 'slp_dbp', 'pct_dip_sbp')

tar="ndbp"
take_out=setdiff(tars,tar)

if(tar %in% c('slp_sbp','slp_dbp','pct_dip_sbp')){
  fam='gaussian'
  obj='reg:linear'
  metrics=list('rmse')
} else {
  fam='binomial'
  obj='binary:logistic'
  metrics=list('auc')
}

analysis[,take_out]=NULL

ftr=setdiff(names(analysis),tar)

mat=model.frame(~., data=analysis[,ftr], na.action='na.pass')
mat=model.matrix(~.,data=mat)[,-1L]
dmat=xgb.DMatrix(data=mat,label=analysis[[tar]])
nsub=nrow(analysis)

nrun=250
run=1

res=vector(mode='list',length = nrun)

maxit=5
verbose=F

set.seed(329)

while(run<nrun){
  
  print(run)
  trn=sample(nsub, round(nsub*0.80))
  # Data pre-processing -----------------------------------------------------
  
  std=list(train=analysis[trn,])
  std$test=analysis[-trn,names(std$train)]
  
  ## imputation
  ## mean/mode imputation for 
  standard = list(
    train=list(mlr::impute(
      std$train,classes=list(numeric = imputeMean(),
                             integer = imputeMode(),
                             factor = imputeMode())) %>%
        use_series("data")),
    test=list(mlr::impute(
      std$test,classes=list(numeric = imputeMean(),
                            integer = imputeMode(),
                            factor = imputeMode())) %>%
        use_series("data"))
  )
  
  missF = list(
    
    train = list(
    missForest(
      as.data.frame(std$train),
      verbose = FALSE,
      variablewise = FALSE,
      maxiter = 10,
      nodesize = c(5, 15)
    )$ximp
  ),
    test = list(
      missForest(
        as.data.frame(std$test),
        verbose = FALSE,
        variablewise = FALSE,
        maxiter = 10,
        nodesize = c(5, 15)
      )$ximp
    )
    
  )
  
  ## using mice with RF
  
  imp = list(
    standard=standard,
    missF=missF
  )
  
  
  # Extreme Gradient Boosting -----------------------------------------------
  
  ## data
  dmat=list(
    trn=xgb.DMatrix(data=mat[trn,], label=analysis[trn,tar]),
    tst=xgb.DMatrix(data=mat[-trn,], label=analysis[-trn,tar])
  )
  
  ## tuning grid
  tune_params<-expand.grid(
    objective=obj,
    eta= 0.02, #if(obj=='reg:logistic') 0.02 else 0.04,
    gamma=c(6),
    colsample_bylevel=c(3/4),
    subsample=c(3/4),
    #num_parallel_tree=c(3),
    max_depth=c(3)) %>% 
    apply(1, as.list)
  
  nrounds=1000
  early_stopping_rounds=15
  print_every_n = 50
  
  folds=split(x=1:nrow(dmat$trn),f=sample(1:10,nrow(dmat$trn),replace=T))
  
  cv.res=map(tune_params, .f=function(params){
    xgb.cv(data=dmat$trn,
           params=params,
           nrounds=nrounds,
           folds=folds,
           metrics=metrics,
           print_every_n = print_every_n,
           early_stopping_rounds = early_stopping_rounds)
  })
  
  cv.dat = map2(cv.res,1:length(tune_params), function(xcv,lab){
    xcv$evaluation_log %>% 
      dplyr::select(iter,starts_with("test_"),ends_with("_mean")) %>% 
      dplyr::mutate(model=lab)}) %>% 
    reduce(rbind) 
  
  met_name=paste0('test_',metrics[[1]],'_mean')
  
  if(obj=='reg:linear'){
    cv.prm = cv.dat[which.min(cv.dat[[met_name]]),]
  } else {
    cv.prm = cv.dat[which.max(cv.dat[[met_name]]),]
  }
  
  bst = xgboost(data=dmat$trn,
                params=tune_params[[cv.prm$model]],
                nrounds=cv.prm$iter,
                metrics=metrics,
                verbose=F) 
  xgb=predict(bst,newdata=dmat$tst)
  
  ## peak at performance
  # rmse(xgb, analysis[-trn,tar])
  # roc(analysis[-trn,tar],xgb)
  
  # Logistic Regression -----------------------------------------------------
  
  ## data
  
  scope=list(
    lower=as.formula(paste(tar,'~ 1')),
    upper=as.formula(paste(tar,'~',paste(
      setdiff(names(imp[[1]]$train[[1]]),tar),collapse='+'))))
  
  step.fw.aic=vector(mode='list',length=length(imp))%>%
    set_names(names(imp))
  
  for(i in 1:length(imp)){
    
    prd=vector(mode='list',length=length(imp[[i]]$train))
    
    for(j in 1:length(imp[[i]]$train)){
      
      mdl=MASS::stepAIC(
        glm(scope$lower,data=imp[[i]]$train[[j]], family=fam),
        scope = scope, direction='both',trace=verbose,steps=15)
      
      prd[[j]]=map(1:length(imp[[i]]$test),.f=function(jj){
        predict(mdl,
                newdata=imp[[i]]$test[[jj]],
                type='response')
      }) %>% 
        reduce(function(x,y) x+y) %>% divide_by(length(imp[[i]]$test))
      
    }
    
    step.fw.aic[[i]]=reduce(prd,function(x,y)x+y)%>%
      divide_by(length(imp[[i]]$train))
    
  }
  
  step.bw.aic=vector(mode='list',length=length(imp))%>%
    set_names(names(imp))
  
  for(i in 1:length(imp)){
    
    prd=vector(mode='list',length=length(imp[[i]]$train))
    
    for(j in 1:length(imp[[i]]$train)){
      
      mdl=MASS::stepAIC(
        glm(scope$upper,data=imp[[i]]$train[[j]], family=fam),
        scope = scope, direction='both',trace=verbose,steps=15)
      
      prd[[j]]=map(1:length(imp[[i]]$test),.f=function(jj){
        predict(mdl,
                newdata=imp[[i]]$test[[jj]],
                type='response')
      }) %>% 
        reduce(function(x,y) x+y) %>% divide_by(length(imp[[i]]$test))
      
    }
    
    step.bw.aic[[i]]=reduce(prd,function(x,y)x+y)%>%
      divide_by(length(imp[[i]]$train))
    
  }
  
  #map(step.bw.aic, ~roc(analysis[-trn,tar],.))
  #map(step.bw.aic, ~rmse(.,analysis[-trn,tar]))
  
  # Generalized Additive Models ---------------------------------------------
  
  gam.scp <- list()
  for(f in setdiff(names(imp$standard$train[[1]]),tar)){
    if(!is.factor(imp$standard$train[[1]][[f]])){
      gam.scp[[f]]=c("1",f,paste0("s(",f,", df=",c(2,3),")"))
    } else {
      gam.scp[[f]]=c("1",f)
    }
  }
  
  gam=vector(mode='list',length=length(imp))%>%
    set_names(names(imp))
  
  for(i in 1:length(imp)){
    
    prd=vector(mode='list',length=length(imp[[i]]$train))
    
    for(j in 1:length(imp[[i]]$train)){
      
      tmp=imp[[i]]$train[[j]]
      gam.obj <- gam(scope$lower,data=tmp, family=fam)
      gam.mdl <- step.Gam(gam.obj,scope=gam.scp, trace=verbose,steps=16)
      
      prd[[j]]=map(1:length(imp[[i]]$test),.f=function(jj){
        predict(gam.mdl,
                newdata=imp[[i]]$test[[jj]],
                type='response')
      }) %>% 
        reduce(function(x,y) x+y) %>% divide_by(length(imp[[i]]$test))
      
    }
    
    gam[[i]]=reduce(prd,function(x,y)x+y)%>%divide_by(length(imp[[i]]$train))
    
  }
  
  # map(gam, ~roc(analysis[-trn,tar],.))
  
  # Penalized Logistic Regression -------------------------------------------
  
  
  pen.lso=pen.rdg=vector(mode='list',length=length(imp))%>%
    set_names(names(imp))
  
  for(i in 1:length(imp)){
    
    prd.lso=prd.rdg=vector(mode='list',length=length(imp[[i]]$train))
    
    for(j in 1:length(imp[[i]]$train)){
      f=setdiff(names(imp[[i]]$train[[j]]),tar)
      xtrn=model.matrix(~.,data=imp[[i]]$train[[j]][,f])[,-1]
      mdl.lso=cv.glmnet(x=xtrn,y=analysis[trn,tar],alpha=0.99,family=fam)
      mdl.rdg=cv.glmnet(x=xtrn,y=analysis[trn,tar],alpha=0.01,family=fam)
      
      prd.lso[[j]]=map(1:length(imp[[i]]$test),.f=function(jj){
        predict(mdl.lso,
                newx=model.matrix(~.,data=imp[[i]]$test[[jj]][,f])[,-1L],
                lambda='lambda.min',
                type='response')
      }) %>% 
        reduce(function(x,y) x+y) %>% divide_by(length(imp[[i]]$test))
      
      prd.rdg[[j]]=map(1:length(imp[[i]]$test),.f=function(jj){
        predict(mdl.rdg,
                newx=model.matrix(~.,data=imp[[i]]$test[[jj]][,f])[,-1L],
                lambda='lambda.min',
                type='response')
      }) %>% 
        reduce(function(x,y) x+y) %>% divide_by(length(imp[[i]]$test))
      
    }
    
    pen.lso[[i]]=reduce(prd.lso,function(x,y)x+y)%>%
      divide_by(length(imp[[i]]$train))
    pen.rdg[[i]]=reduce(prd.rdg,function(x,y)x+y)%>%
      divide_by(length(imp[[i]]$train))
    
  }
  
  # Random Forest -----------------------------------------------------------
  
  rf_data=analysis
  
  if(obj=='reg:logistic') rf_data[[tar]]%<>%factor()
  
  ## model
  frm=as.formula(paste(tar,'~ .'))
  rf.mdl=rfsrc(frm,data=rf_data[trn,],ntree=1000,nodesize=5,
                na.action='na.impute', nimpute=maxit)
  
  rf=predict(rf.mdl,
             newdata=rf_data[-trn,ftr],
             na.action='na.impute',
             nimpute=2)$predicted
  
  if(obj=='reg:logistic') rf=rf[,2]
  
  # Model Evaluation --------------------------------------------------------
  
  mdl=list('Boosting'=xgb,
           'RForest'=rf,
           'Backward STD'=step.bw.aic$standard,
           'Forward STD'=step.fw.aic$standard,
           'Backward missRF'=step.bw.aic$missF,
           'Forward missRF'=step.fw.aic$missF,
           'Ridge STD'=pen.rdg$standard,
           'Ridge missRF'=pen.rdg$missF,
           'Lasso STD'=pen.lso$standard,
           'Lasso missRF'=pen.lso$missF,
           'GAM STD'=gam$standard,
           'GAM missRF'=gam$missF)
  
  if(obj=='binary:logistic'){
    
    res[[run]]=map(mdl,.f=function(m){
      c(as.numeric(hoslem.test(analysis[-trn,tar],m)$statistic),
        as.numeric(roc(analysis[-trn,tar],c(m))$auc),
        BrierScore(resp=analysis[-trn,tar], pred=m, scaled=T))%>%
        set_names(c("htchi","auc","sbri"))
    }) %>% reduce(rbind) %>%
      data.frame() %>% mutate(mdl=names(mdl))
    
    print(res %>% reduce(rbind) %>%
            dplyr::group_by(mdl) %>%
            dplyr::summarise(auc=mean(auc),
                             htchi=mean(htchi),
                             sbri=mean(sbri))%>%
            dplyr::arrange(desc(auc)))
    
  } else {
    
    res[[run]]=map(mdl,.f=~rmse(., analysis[-trn,tar]))%>%
      reduce(rbind)%>%
      data.frame()%>%
      set_names("rmse") %>%
      mutate(mdl=names(mdl))
    
    print(res %>% reduce(rbind) %>%
            dplyr::group_by(mdl) %>%
            dplyr::summarise(rmse=mean(rmse))%>%
            dplyr::arrange(rmse))
    
  }
  
  run=run+1
  
  tar %>%
    paste0('model_comparison_', ., '.RDS') %>%
    file.path('Results', .) %>%
    saveRDS(
      discard(res, is.null),
      file = .
    )
  
}



