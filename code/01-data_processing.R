rm(list=ls())

# To do: add non dipping dbp to IDH and MHT

# Set up ------------------------------------------------------------------

library(tidyverse)
library(magrittr)
library(haven)

# CARDIA ------------------------------------------------------------------

data = cardia = list()

# location to read in data
cardia$slp = read_sas(
  file.path(
    'O:',
    'REGARDS',
    'CARDIA',
    'Data',
    'Population',
    'ABPM Ancillary Study Freeze',
    '03-19-2019',
    'data',
    'source',
    'summary_ABPM_y30.sas7bdat'
  )
) 

nobs=labs=list()

nobs[[1]]=5115
labs[[1]]="CARDIA participants"

nobs[[2]]=nrow(cardia$slp)
labs[[2]]="Participants who underwent \n24-hour ABPM"

cardia$slp %<>% 
  dplyr::filter(
    GE_5_Valid_sleep==1,
    GE_10_Valid_awake==1
  ) 

nobs[[3]]=nrow(cardia$slp)
labs[[3]]="Participants with \u2265 10 awake \nand \u2265 5 asleep ABPM readings"

cardia$slp %<>%
  dplyr::select(
    id,
    slp_sbp=mn_sysbp_SL,
    slp_dbp=mn_diabp_SL,
    awk_sbp=mn_sysbp_AW,
    awk_dbp=mn_diabp_AW,
    awk_hrt=mn_HR_AW,
    pct_dip_sbp=Pct_sysbp_dip,
    pct_dip_dbp=Pct_diabp_dip,
    slp_dur=sleep_duration
  ) %>%
  dplyr::mutate(
    nhtn=ifelse(slp_dbp>=70|slp_sbp>=120,1,0),
    nd_sbp=ifelse(pct_dip_sbp<10, 1,0),
    nd_dbp=ifelse(pct_dip_dbp<10, 1,0),
    slp_dur=as.numeric(slp_dur)/60^2
  )

exclusion_cascade=list(cardia=list(labs=labs,nobs=nobs))

#ggplot(cardia$slp,aes(x=slp_dur,y=nhtn))+geom_smooth(se=F)
# sum(cardia$slp$slp_dur>10)

cardia$abpm_forms <- 
  read_sas(
    file.path(
      'O:','REGARDS','JHS','Jaeger',
      'Miscellaneous','Swati',
      'cardia_abpm_781_updated.sas7bdat'
    )
  ) %>% 
  set_names(tolower(names(.))) %>%
  dplyr::select(
    id = short_id, 
    neck_cm,
    htn_meds
  )

cardia$data_forms <- 
  read_sas(
    file.path("O:","REGARDS","CARDIA","Data","Population",
      "A1808 REQ 4 11 2018 (Byron Jaeger)",
      "a1808req4_11_2018.sas7bdat"
    )
  ) %>%
  magrittr::set_names(tolower(names(.))) %>%
  dplyr::mutate(
    race = factor(ifelse(a01race1 == 5, 'White', 'Black')),
    sex = factor(ifelse(a01sex == 1, 'Male', 'Female')),
    educ = case_when(
      i03ed >= 12 ~ "Yes",
      i03ed < 12 ~ "No"
    ),
    alcohol = factor(ifelse(i07drink == 1, 'None', 'Some')),
    smoke = factor(
      i10smoke,
      levels = c(0, 1, 2),
      labels = c("Never", "Former", "Current")
    ),
    diabetes = ifelse(
      i08diab == 1, "No", ifelse(
        i08diab == 2, "Yes", NA)
    ),
    acr=ifelse(il1ratio>30, 1, 0),
    il1ualb=il1ualb*1000,
    age_exam1 = a01age1+30
  ) %>%
  dplyr::select(
    id=short_id,
    age = age_exam1,
    sex=sex,
    race=race,
    educ,
    alcohol,
    smoke,
    height_cm=i20hgt,
    weight_lbs=i20wgt,
    waist_cm=i20wst,
    cln_sbp=i02savg,
    cln_dbp=i02davg,
    ur_cre=il1ucreat,
    ur_alb=il1ualb,
    acr=acr,
    egfr=il7gfr,
    hba1c=i0yhba1c,
    ldl=il1ldl,
    hdl=il1hdl,
    chol=il1chol,
    glucose=il7glu,
    diabetes
  ) 

cardia$abpm_forms$neck_cm[cardia$abpm_forms$neck_cm<20]=NA
cardia$abpm_forms$neck_cm[cardia$abpm_forms$neck_cm>400]=NA

write_rds(cardia$data_forms,'Datasets/cardia_full_cohort.RDS')

cardia$data_forms %<>% dplyr::filter(id %in% cardia$slp$id)

cardia_for_analysis <- reduce(cardia, left_join,by='id')

write_rds(cardia_for_analysis$id,'Datasets/cardia_included_ids.RDS')

data$cardia = cardia_for_analysis %>%
  dplyr::rename(ID = id)%>%
  mutate(
    hispanic=0
  ) %>% 
  mutate_if(
    is.factor,
    as.character
  )

# JHS ---------------------------------------------------------------------

jhs=list()

nobs=labs=list()

nobs[[1]]=5306
labs[[1]]="JHS participants"

jhs_abpm = readr::read_csv(
  "O:/REGARDS/JHS/Derived datasets/07-01-2019/data/output/abpm/abpm_srp.csv"
) %>% 
  dplyr::mutate(
    cmp_abpm = case_when(
      slp_diary_valid==1 ~ cmp_idaco_awk_slp,
      slp_diary_valid==0 ~ cmp_idaco_dtm_ntm
    ),
    slp_sbp = case_when(
      !is.na(slp_sbp) ~ slp_sbp,
      is.na(slp_sbp) ~ ntm_sbp
    ),
    slp_dbp = case_when(
      !is.na(slp_dbp) ~ slp_dbp,
      is.na(slp_dbp) ~ ntm_dbp
    ),
    awk_sbp = case_when(
      !is.na(awk_sbp) ~ awk_sbp,
      is.na(awk_sbp) ~ dtm_sbp
    ),
    awk_dbp = case_when(
      !is.na(awk_dbp) ~ awk_dbp,
      is.na(awk_dbp) ~ dtm_dbp
    )
  )

nobs[[2]]=nrow(jhs_abpm)
labs[[2]]="Participants who underwent \n24-hour ABPM"

jhs_abpm %<>% dplyr::filter(cmp_abpm==1)

nobs[[3]]=nrow(jhs_abpm)
labs[[3]]="Participants with \u2265 10 awake \nand \u2265 5 asleep ABPM readings*"

jhs$slp <- jhs_abpm %>% 
    dplyr::select(
      id=subjid,
      slp_sbp,
      awk_sbp,
      slp_dbp,
      awk_dbp,
      awk_hrt,
      slp_dur=slp_duration
    ) %>% 
  mutate(
    pct_dip_sbp=100*(1-slp_sbp/awk_sbp),
    pct_dip_dbp=100*(1-slp_dbp/awk_dbp),
    nhtn=ifelse(slp_dbp>=70 | slp_sbp >= 120, 1, 0),
    nd_sbp=ifelse(pct_dip_sbp < 10, 1, 0),
    nd_dbp=ifelse(pct_dip_dbp < 10, 1, 0)
  )

exclusion_cascade$jhs=list(nobs=nobs, labs=labs)

jhs$anly <- file.path(
  '..','..','Datasets','JHS_analysis','Processed data','jhs_visit1.csv'
) %>% 
  read_csv() %>% 
  dplyr::mutate(
    race='Black',
    weight_lbs=weight*2.20462,
    hispanic=0,
    htn_meds=case_when(
      bpmeds=='Yes'~1,
      bpmeds=='No'~0
    ),
    alcohol=case_when(
      alc=='Yes'~"Some",
      alc=='No'~"None",
      TRUE ~ NA_character_
    ),
    smoke=case_when(
      eversmoker=="Yes" ~ "Former",
      TRUE ~ NA_character_
    ),
    smoke=case_when(
      currentsmoker=="Yes" ~ "Current",
      TRUE ~ smoke
    ),
    smoke=case_when(
      eversmoker=="No" & currentsmoker=="No" ~ "Never",
      TRUE ~ smoke
    ),
    albumin=albumin*100,
    creatinine=creatinine*100
  ) %>% 
  dplyr::select(
    id=subjid,
    age,
    sex,
    race,
    hispanic,
    smoke,
    alcohol,
    diabetes,
    neck_cm=neck,
    height_cm=height,
    weight_lbs,
    waist_cm=waist,
    educ=hsgrad,
    htn_meds,
    cln_sbp=sbp,
    cln_dbp=dbp,
    hdl,
    glucose=fpg,
    ur_alb=albumin,
    ur_cre=creatinine,
    egfr=egfrckdepi,
    hba1c,
    ldl,
    chol=totchol
  ) 

write_rds(jhs$anly, 'Datasets/jhs_full_cohort.RDS')

jhs_for_analysis <- reduce(jhs, left_join, by='id')

write_rds(jhs_for_analysis$id,'Datasets/jhs_included_ids.RDS')

data$jhs <- jhs_for_analysis %>%
  dplyr::rename(ID = id) %>%
  mutate(
    acr=ifelse(ur_alb/ur_cre > 30, 1, 0)
  ) %>% 
  mutate_if(
    is.factor,
    as.character
  )

# IDH ---------------------------------------------------------------------

nobs=labs=list()

nobs[[1]]=408
labs[[1]]="IDH participants"

nobs[[2]]=400
labs[[2]]="Participants who underwent \n24-hour ABPM"

data$idh = file.path(
  "..","..","Datasets","IDH and MHT","idh_v2.sas7bdat"
) %>% 
  read_sas() %>%
  mutate(
    educ = case_when(
      educ >= 12 ~ "Yes",
      educ < 12 ~ "No"
    ),
    diabetes = case_when(
      diabetes == 1 ~ "Yes",
      diabetes == 0 ~ "No"
    )
  ) %>% 
  dplyr::select(
    ID,
    slp_sbp,
    awk_sbp = aw_sbp,
    awk_dbp = aw_dbp,
    slp_dbp,
    slp_dur,
    awk_hrt = aw_HR,
    age,
    neck_cm,
    htn_meds,
    sex,
    race,
    hispanic,
    educ,
    alcohol,
    smoke,
    height_cm,
    #drinks_per_week
    weight_lbs,
    waist_cm,
    cln_sbp,
    cln_dbp,
    ur_cre,
    ur_alb,
    egfr,
    hba1c,
    ldl,
    hdl,
    chol,
    glucose,
    diabetes
  ) %>%
  mutate(
    race=factor(
      race,
      levels=1:5,
      labels=c(
        'White','Black','Other','Asian','Other'
      )
    ),
    pct_dip_sbp=100*(1-slp_sbp/awk_sbp),
    pct_dip_dbp=100*(1-slp_dbp/awk_dbp),
    smoke=factor(smoke,levels=0:2,labels=c("Never","Former","Current")),
    sex=factor(sex,levels=0:1,labels=c("Female","Male")),
    nhtn=ifelse(slp_dbp>=70|slp_sbp>=120,1,0),
    nd_sbp=ifelse(pct_dip_sbp<10, 1,0),
    nd_dbp=ifelse(pct_dip_dbp<10, 1,0),
    alcohol=factor(alcohol,labels=c("None","Some")),
    ur_alb=1000*ur_alb,
    acr=ifelse(ur_alb/ur_cre>30,1,0)
  )

nobs[[3]]=nrow(data$idh)
labs[[3]]="Participants with \u2265 10 awake \nand \u2265 5 asleep ABPM readings"

exclusion_cascade$idh=list(nobs=nobs,labs=labs)

# MHT ---------------------------------------------------------------------

nobs=labs=list()

nobs[[1]]=1011
labs[[1]]="MHT participants"

nobs[[2]]=893
labs[[2]]="Participants who underwent \n24-hour ABPM"

data$mht = read_sas(file.path(
  "..","..","Datasets","IDH and MHT","mht_v2.sas7bdat"
)) %>%
  mutate(
    educ = case_when(
      educ >= 12 ~ "Yes",
      educ < 12 ~ "No"
    ),
    diabetes = case_when(
      diabetes == 1 ~ "Yes",
      diabetes == 0 ~ "No"
    )
  ) %>% 
  dplyr::select(
    ID,
    slp_sbp,
    awk_sbp,
    pct_dip_sbp,
    slp_dbp,
    awk_dbp,
    awk_hrt = awk_hr,
    slp_dur,
    age,
    neck_cm,
    htn_meds,
    sex,
    race = race2,
    hispanic,
    educ,
    alcohol,
    smoke,
    height_cm,
    #drinks_per_week
    weight_lbs,
    waist_cm,
    cln_sbp,
    cln_dbp,
    ur_cre,
    ur_alb,
    egfr,
    hba1c,
    ldl,
    hdl,
    chol,
    glucose,
    diabetes
  ) %>%
  mutate(
    pct_dip_dbp=100*(1-slp_dbp/awk_dbp),
    race = factor(race),
    smoke = factor(
      smoke,
      levels = 0:2,
      labels = c("Never", "Former", "Current")
    ),
    sex = factor(sex, levels = 0:1, labels = c("Female", "Male")),
    nhtn = ifelse(slp_dbp >= 70 | slp_sbp >= 120, 1, 0),
    nd_sbp = ifelse(pct_dip_sbp < 10, 1, 0),
    nd_dbp=ifelse(pct_dip_dbp < 10, 1,0),
    alcohol = factor(alcohol, labels = c("None", "Some")),
    ur_alb = 1000 * ur_alb,
    acr = ifelse(ur_alb / ur_cre > 30, 1, 0)
  )

levels(data$mht$race)<-list(
  `White`=c('White (hispanic)','White (non-hispanic)'),
  `Black`=c('Black (hispanic)','Black (non-hispanic)'),
  `Asian`='Asian/Pacific Islander',
  `Other`=c('Unknown','Other','Other (hispanic)','Declined to State',
    'Asian/Indian','Native American'))

data$mht$race%<>%as.character()
data$idh$race%<>%as.character()


nobs[[3]]=nrow(data$mht)
labs[[3]]="Participants with \u2265 10 awake \nand \u2265 5 asleep ABPM readings"

exclusion_cascade$mht=list(nobs=nobs,labs=labs)

# par(mfrow=c(1,2))
# map(exclusion_cascade, ~plot_exclusion_cascade(nobs=.$nobs,labs=.$labs))

# map(data,dim)
# setdiff(names(data$cardia),names(data$jhs))

data %<>% map(
  ~mutate_if(.,is.factor,as.character)
)

analysis <- data %>% 
  map(~mutate(.x, ID = as.character(ID))) %>% 
  bind_rows(.id = 'study') %>%  
  mutate_if(is.character,as.factor) %>%
  mutate(
    bmi=(weight_lbs/2.20462)/(height_cm/100)^2,
    acr_nmr = log(1 + ur_alb / ur_cre),
    gfr=factor(
      ifelse(egfr < 60, 1, 0),
      levels = c(0,1),
      labels = c("less_than_60", "greater_than_60")
    ),
    acr=factor(
      acr,
      levels = c(0,1),
      labels = c("less_than_30", "greater_than_30")
    ),
    hispanic=factor(
      hispanic, 
      levels=c(0,1),
      labels=c("No","Yes")
    ),
    htn_meds=factor(
      htn_meds,
      levels=c(0,1),
      labels=c("No","Yes")
    ),
    race=factor(
      race,
      levels=c("White","Black","Asian","Other")
    ),
    study=factor(
      study, 
      levels=c("cardia","jhs","idh","mht")
    )
  ) %>% 
  dplyr::select(
    study, 
    ID, 
    everything(),
    -c(
      hba1c,
      ur_alb,
      ur_cre
    )
  )


# Original code to stratify based on study
# nval_bygrp=floor(prop.table(table(analysis$study))*502)
# grp_indx=map(c("cardia","jhs","idh","mht"),~which(analysis$study==.))
# 
# val = map2(nval_bygrp, grp_indx, .f=function(nval,indx){
#   sample(indx, nval)
# }) %>% reduce(c)
# This wont reproduce anymore due to 3.6 breaking random seeds
# however, I saved derivation IDs to datasets/derivation_ids.rds

derivation_ids <- read_rds("Datasets/derivation_ids.rds")

drv = which(analysis$ID %in% derivation_ids)

analysis %<>% 
  mutate(
    dataset = if_else(
      ID %in% derivation_ids, "Derivation", "Validation"
    )
  )

derivation=analysis[drv,]
validation=analysis[-drv,]

prop.table(table(derivation$study))
prop.table(table(validation$study))


outcomes <- c(
  "slp_sbp",
  "slp_dbp",
  "pct_dip_sbp",
  "pct_dip_dbp",
  "nhtn",
  "nd_sbp",
  "nd_dbp",
  "awk_sbp",
  "awk_dbp"
)

features <- c(
  "age",
  "neck_cm",
  "htn_meds",
  "sex",
  "race",
  "educ",
  "alcohol",
  "smoke",
  "height_cm",
  "weight_lbs",
  "waist_cm",
  "bmi",
  "cln_sbp",
  "cln_dbp",
  "egfr",
  "gfr",
  "ldl",
  "hdl",
  "chol",
  "glucose",
  "diabetes",
  "acr_nmr"
)


write_rds(features, "results/model_predictors.RDS")
write_rds(outcomes,  "results/model_outcomes.RDS")
write_rds(analysis,  'Datasets/analysis_data.RDS')
write_rds(derivation,'Datasets/derivation_data.RDS')
write_rds(validation,'Datasets/validation_data.RDS')
write_rds(exclusion_cascade,"Results/exclusion_casc.RDS")
