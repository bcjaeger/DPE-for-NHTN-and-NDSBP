

library(tidyverse)
library(magrittr)
library(fmsb)
library(pROC)

analysis <- readRDS("datasets/analysis_data.RDS") %>% 
  filter(dataset == 'Derivation') %>% 
  drop_na(sex, height_cm, neck_cm) %>% 
  mutate(sex = as.numeric(sex=='Male'))

GAM = read_rds('Results/fitted_GAM.RDS')

df <- read_rds("Results/model_data.RDS")



library(gam)
p_cmp <-
  predict(
    object = GAM$complete$nhtn$mdl,
    newdata = df$complete$test,
    type = 'response'
  )
p_imp <-
  predict(
    object = GAM$imputed$nhtn$mdl,
    newdata = df$complete$test,
    type = 'response'
  )

cor(p_cmp, p_imp)

mean(abs(p_cmp - p_imp))

mean(abs(p_cmp - p_imp)>0.05)

p_cmp <-
  predict(
    object = GAM$complete$nd_sbp$mdl,
    newdata = df$complete$test,
    type = 'response'
  )
p_imp <-
  predict(
    object = GAM$imputed$nd_sbp$mdl,
    newdata = df$complete$test,
    type = 'response'
  )

cor(p_cmp, p_imp)

mean(abs(p_cmp - p_imp))

mean(abs(p_cmp - p_imp)>0.05)

# Reviewer 1, comment no. 4 ----

cmp_roc <- function(model, data, ...){
  
  response = all.vars(model$formula)[1]
  
  pROC::roc(
    response = data[[response]], 
    predictor = predict(model, newdata = data), 
    ...
  )
  
}

fmt_roc <- function(roc_ci){
  
  chr_vec <- roc_ci %>% 
    as.numeric() %>% 
    round(2) %>% 
    format(nsmall = 2)
  
  paste0( chr_vec[2], ' (', chr_vec[1], ', ', chr_vec[3], ')' )
  
}

mdls <- list(
  one = glm(sex ~ neck_cm, data = analysis, family = binomial),
  two = glm(nhtn ~ neck_cm, data = analysis, family = binomial),
  three = glm(nhtn ~ sex, data = analysis, family = binomial)
)

cstat = mdls %>% 
  map(
    ~ cmp_roc(.x, data = analysis, ci = TRUE) %>% 
      use_series("ci")  %>% 
      fmt_roc()
  )
  
cstat


lm(slp_sbp ~ sex, data = analysis) %>% 
  summary() %>% 
  use_series('r.squared')


model_data <- readRDS("datasets/analysis_data.RDS") %>% 
  filter(dataset == 'Derivation') %>% 
  mutate(race = if_else(race == 'White', "White","Other"))

library(stats)

new_mdl <- gam(
  nhtn ~ age + smoke + s(cln_sbp, df = 3) + s(cln_dbp, df = 2) + 
    hdl + acr_nmr + sex * race,
  family = 'binomial',
  data = model_data
)

summary.glm(new_mdl)

new_mdl <- GAM$imputed$nhtn$mdl %>% 
  update(~. + race*sex, data = model_data) %>% 
  summary.glm()

new_mdl <- GAM$imputed$nhtn$mdl %>% 
  update(~. + race*sex - neck_cm, data = model_data) %>% 
  summary.glm()

trt_mdl_nhtn <- gam(
  nhtn ~ (age + smoke + s(cln_sbp, df = 3) + s(cln_dbp, df = 2) + hdl + acr_nmr + sex + race)*htn_meds,
  family = 'binomial',
  data = model_data
)

trt_mdl_nd_sbp <- gam(
  formula = nd_sbp ~ (s(age, df = 3) + sex + race + alcohol + 
    s(height_cm, df = 2) + waist_cm + hdl + s(acr_nmr, df = 3))*htn_meds, 
  family = binomial(), 
  data = model_data
)

summary.glm(trt_mdl_nd_sbp)

# Reviewer 1, comment no. 2 (minor) ----

# the age (range/ mean{plus minus}SD) and women (%) of the four studies

mean_sd <- function(x, dig){
  
  mn_x <- mean(x, na.rm=T)
  sd_x <- sd(x, na.rm=T)
  
  vals <- list(
    mean = mn_x, sd = sd_x
  ) %>% 
    map(.f = ~ format(round(.x,dig),nsmall=dig))
  
  glue::glue("{vals$mean} +/- {vals$sd}")
  
}

range_x <- function(x, dig){
  
  rng = format(round(range(x, na.rm=TRUE),dig),nsmall=dig)
  
  glue::glue("{rng[1]} - {rng[2]}")
  
}

analysis <- readRDS("datasets/analysis_data.RDS") %>% 
  mutate(sex = as.numeric(sex=='Male'))


analysis %>% 
  group_by(study) %>% 
  nest() %>% 
  mutate(
    nobs = map_int(data, nrow),
    age_meansd = map_chr(data, ~mean_sd(.x$age, dig=1)),
    age_range = map_chr(data, ~range_x(.x$age, dig=1)),
    prc_women = map_chr(data, ~format(round(100*mean(.x$sex==0),1),nsmall=1))
  ) %>% 
  select(-data) %>% 
  mutate(study = toupper(study)) %>% 
  flextable::flextable() %>% 
  flextable::set_header_labels(
    values = c(
      study = "Study",
      age_meansd = "Mean +/- SD",
      age_range = "Range",
      prc_women = "% Women"
    )
  ) %>% 
  flextable::add_header(
    age_meansd = 'Age, years',
    age_range = "Age, years"
  ) %>% 
  flextable::merge_h(part = 'header') %>% 
  flextable::merge_v(j=1) %>% 
  flextable::theme_box()

kp = Kappa.test(x = analysis$nhtn, y = analysis$nd_sbp)

est <- round(kp$Result$estimate,2)

ci <- round(kp$Result$conf.int,2)

mean(analysis$nhtn == analysis$nd_sbp)

glue::glue("{ est } ({ ci[1] },{ ci[2] })")


# Reviewer 3, comment no. 4 ----

analysis %>% 
  group_by(study) %>% 
  summarise(
    prev_nhtn = mean(nhtn),
    prev_nd_sbp = mean(nd_sbp)
  ) %>% 
  mutate_if(
    is.numeric,
    ~ format(round(100 * .x, 1), nsmall = 1)
  ) %>% 
  .[,-1] %>% 
  knitr::kable() %>% 
  kableExtra::kable_styling()




# Reviewer 3, comment no. 5 ----

analysis <- readRDS("datasets/analysis_data.RDS")

analysis %>% 
  group_by(nd_sbp, nhtn) %>% 
  summarize(
    slp_sbp = mean(slp_sbp),
    awk_sbp = mean(awk_sbp)
  ) %>% 
  mutate(all_sbp = (8*slp_sbp + 16*awk_sbp) / 24 )

analysis %>% 
  group_by(nd_sbp, nhtn) %>% 
  summarize(
    slp_sbp = mean(slp_sbp),
    awk_sbp = mean(awk_sbp)
  ) %>% 
  ungroup() %>% 
  mutate(all_sbp = (8*slp_sbp + 16*awk_sbp) / 24 ) %>% 
  mutate_at(vars(nd_sbp, nhtn), ~factor(.x, labels = c("No","Yes"))) %>% 
  select(nd_sbp, nhtn, all_sbp) %>% 
  spread(nhtn, all_sbp)
