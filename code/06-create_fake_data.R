
lwr<-function(lab){round(quantile(trn[[lab]],probs=0.05)*sds[lab])}
upr<-function(lab){round(quantile(trn[[lab]],probs=0.95)*sds[lab])}

sds=readRDS("Results/standard_deviations.RDS")
DAT=readRDS("Results/GAM_data.RDS")
trn=DAT$train%>%droplevels()
tst=DAT$test%>%droplevels()


bounds=map(names(sds),.f=function(x){
  list(lower=lwr(x),upper=upr(x),middle=round(median(trn[[x]],na.rm=T)))
})%>%set_names(names(sds))

saveRDS(bounds,"Results/shiny_variable_boundaries.RDS")

tst%>%dplyr::select(nhtn,ndbp)%>%saveRDS("Results/shiny_outcomes_only.RDS")

nhtn_df = readRDS("datasets/validation_data.RDS") %>%
  dplyr::select(
    age,
    race,
    height_cm,
    neck_cm,
    cln_sbp,
    smoke,
    cln_dbp,
    hdl,
    acr_nmr
    #glucose,
    ) %>% na.omit()

set.seed(329)

for(i in names(nhtn_df)){
  nhtn_df[[i]]=sample(nhtn_df[[i]])
  if(i!='race' & i!='smoke'){
    nhtn_df[[i]]%<>%add(rnorm(length(.),mean = 0, sd = 1))
  }
}

nhtn_df$race<-sample(levels(nhtn_df$race),nrow(nhtn_df),replace=TRUE)
nhtn_df$smoke<-sample(levels(nhtn_df$smoke),nrow(nhtn_df),replace=TRUE)


ndbp_df = readRDS("datasets/validation_data.RDS") %>%
  dplyr::select(
    age,
    sex,
    race,
    alcohol,
    height_cm,
    waist_cm,
    hdl,
    acr_nmr
    #glucose,
  ) %>% na.omit()

set.seed(329)

for(i in names(ndbp_df)){
  ndbp_df[[i]]=sample(ndbp_df[[i]])
  if(i!='race' & i!='sex' & i != 'alcohol'){
    ndbp_df[[i]]%<>%add(rnorm(length(.),mean = 0, sd = 1))
  }
}

ndbp_df$race<-sample(levels(ndbp_df$race),nrow(ndbp_df),replace=TRUE)
ndbp_df$sex<-sample(levels(ndbp_df$sex),nrow(ndbp_df),replace=TRUE)
ndbp_df$alcohol<-sample(levels(ndbp_df$alcohol),nrow(ndbp_df),replace=TRUE)





write.csv(nhtn_df,'DPE4NHTN_WebApp/Fake_Data_NHTN.csv',row.names = FALSE)
write.csv(ndbp_df,'DPE4NHTN_WebApp/Fake_Data_NDBP.csv',row.names = FALSE)

