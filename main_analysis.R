
# Setup -------------------------------------------------------------------

library(officer)
library(magrittr)
library(tidyverse)
library(labelled)
library(ResourceSelection)
library(gam)
library(pROC)
library(PredictABEL)
library(epiR)
library(ggthemes)
library(nricens)
library(gridExtra)
library(tibbleOne)
library(flextable)

ftr  = read_rds("results/model_predictors.RDS")

complete_cases = TRUE

analysis <- readRDS("datasets/analysis_data.RDS") %>% 
  {if(complete_cases) drop_na(.,!!!ftr) else .} %>% 
  mutate(
    study = fct_recode(
      study, 
      CARDIA = 'cardia',
      JHS = 'jhs',
      IDH = 'idh',
      MHT = 'mht'
    ),
    alcohol = fct_recode(
      alcohol,
      No = 'None',
      Yes = 'Some'
    ),
    nhtn = factor(nhtn, labels = c("No","Yes")),
    nd_sbp = factor(nd_sbp, labels = c("No","Yes")),
    acr = fct_recode(
      acr, 
      Yes = 'greater_than_30',
      No = 'less_than_30'
    ),
    #acr_nmr = exp(acr_nmr)-1,
    gfr = fct_recode(
      gfr, 
      Yes = 'greater_than_60',
      No = 'less_than_60'
    ),
    htn_jnc = if_else(
      cln_sbp >= 140 | cln_dbp >= 140 | htn_meds == "Yes", "Yes", "No"
    ),
    dataset = factor(dataset),
    htn_jnc = factor(htn_jnc)
  ) %>% 
  select_labelled(
    study = 'Study cohort',
    age = 'Age',
    sex = 'Sex',
    dataset = 'Data set',
    race = "Race/ethnicity",
    educ = 'High school graduate',
    smoke = 'Smoking status',
    alcohol = 'Alcohol use',
    slp_dur = 'Sleep duration',
    neck_cm = 'Neck circumference',
    waist_cm = 'Waist circumference',
    weight_lbs = 'Weight',
    height_cm = 'Height',
    bmi = 'Body mass index',
    acr_nmr = 'Albumin-to-creatinine ratio',
    acr = 'Albuminuria',
    gfr = 'eGFR < 60 ml/min/1.73 m2',
    glucose = 'Blood glucose',
    diabetes = 'Diabetes',
    hdl = 'High density lipoprotein-cholesterol',
    ldl = 'Low density lipoprotein-cholesterol',
    chol = "Total cholesterol",
    awk_hrt = "Heart rate while awake",
    htn_meds = 'Antihypertensive medication use',
    cln_sbp = "Clinical setting",
    slp_sbp = "During sleep",
    cln_dbp = "Clinical setting",
    slp_dbp = "During sleep",
    htn_jnc = 'Conventional hypertension',
    nhtn = 'Nocturnal hypertension',
    nd_sbp = 'Non-dipping systolic blood pressure'
  ) %>% 
  set_variable_units(
    age = 'years',
    slp_dur = 'hours',
    height_cm = 'cm',
    weight_lbs = 'lbs',
    waist_cm = 'cm',
    neck_cm = 'cm',
    bmi = 'kg/m2',
    acr_nmr = 'mg/g',
    glucose = 'mg/dL',
    awk_hrt = 'beats/min',
    hdl = 'mg/dL',
    ldl = 'mg/dL',
    chol = 'mg/dL'
  ) %>% 
  set_variable_groups(
    "Systolic blood pressure, mm Hg" = c("cln_sbp", "slp_sbp"),
    "Diastolic blood pressure, mm Hg" = c("cln_dbp", "slp_dbp")
  ) %>% 
  set_variable_abbrs(
    study = c(
      CARDIA = "Coronary Artery Risk Development in Young Adults",
      JHS = "Jackson Heart Study",
      IDH = "Improving Detection of Hypertension",
      MHT = "Masked Hypertension"
    ),
    gfr = c(eGFR = 'estimated glomerular filtration rate')
  ) %>% 
  set_variable_notes(
    acr = "Albuminuria was defined as a urinary albumin to urinary creatinine ratio \u2265 30 mg/g",
    nhtn = "Nocturnal hypertension was defined as mean systolic blood pressure \u2265 120 mm Hg or mean diastolic blood pressure \u2265 70 mm Hg while asleep",
    nd_sbp = "Non-dipping systolic blood pressure was defined as decline in mean systolic blood pressure from wakefulness to asleep < 10%",
    htn_jnc = "Systolic blood pressure \u2265 140 mm Hg or diastolic blood pressure \u2265 90 mm Hg or currently taking antihypertensive medication."
  )

# Reproducibility
set.seed(32989)

function_files <- list.files(
  path='code/functions',
  all.files=TRUE, 
  full.names=TRUE,
  pattern='.R'
)

for(f in function_files) source(f)

exclusion_cascade=readRDS("Results/exclusion_casc.RDS")

tiff(
  filename=file.path(
    "Manuscript",
    "Main",
    "figures",
    "figure_s1.tiff"),
  width=480*2.2,
  height=480
)

par(mfrow=c(1,4))
map(exclusion_cascade, ~plot_exclusion_cascade(.$nobs,.$labs))

dev.off()

# Define targets and data -------------------------------------------------

# tars = c(
#   nd_dbp = "nd_dbp"
# )

tars = c(
  nhtn = "nhtn",
  nd_sbp = "nd_sbp"
)

# tar_labs = c(
#   nd_dbp = "Non-dipping Diastolic Blood Pressure"
# )

tar_labs = c(
  nhtn = "Nocturnal Hypertension",
  nd_sbp = "Non-dipping Systolic Blood Pressure"
)

tar_recoder <- names(tar_labs) %>% set_names(tar_labs)

sds <- read_rds("Results/standard_deviations.rds")

analysis_type <- if(complete_cases) "complete" else "imputed"


if(complete_cases){
  
  data <- list(
    derivation = drop_na(read_rds("Datasets/derivation_data.RDS"), !!!ftr),
    validation = drop_na(read_rds("Datasets/validation_data.RDS"), !!!ftr)
  )
  
} else {
  
  data <- read_rds("Results/imputed_data.RDS") %>% 
    purrr::set_names("derivation", "validation")
  
}

fctr_to_01 <- function(x){
  
  if(!is.factor(x)){
    return(x)
  } 
  
  as.numeric(x) - 1
  
}

data$derivation %<>% mutate_at(tars, fctr_to_01) 
data$validation %<>% mutate_at(tars, fctr_to_01) 

font_size = 11

tbl_counter = 1
tbl_objs <- list()
tbl_caps <- list()

fig_counter = 1
fig_objs <- list()
fig_caps <- list()
fig_legs <- list()
fig_width <- list()
fig_height <- list()

# Characteristics table ---------------------------------------------------

### Guide for footnotes: *, †, ‡, §, ||, ¶, #, **

tbl_objs[[tbl_counter]] <- analysis %>% 
  tibble_one(formula = ~ . | dataset) %>% 
  to_word() %>% 
  flextable::width(width=1) %>% 
  flextable::width(j=1, width = 2.5) %>% 
  flextable::fontsize(size = font_size, part = 'all')

tbl_caps[[tbl_counter]] <- paste0(
  "Table ",
  tbl_counter,
  ": Participant characteristics stratified by assignment into the derivation or validation data set."
)

tbl_counter %<>% add(1)


# Cardia participant characteristics (included versus excluded) -----------

tbl_objs[[tbl_counter]] = read_rds('Datasets/cardia_full_cohort.RDS') %>% 
  dplyr::mutate(
    included = factor(
      id %in% read_rds('Datasets/cardia_included_ids.RDS'), 
      levels = c(FALSE, TRUE), 
      labels = c("No","Yes")
    ),
    acr_nmr = ur_alb / ur_cre,
    acr = factor(acr, levels = c(0,1), labels = c("No","Yes")),
    gfr = factor(egfr < 60, levels = c(FALSE, TRUE), labels = c("No","Yes")),
    diabetes = factor(diabetes, levels = c("No","Yes")),
  ) %>% 
  select_labelled(
    included = 'Included in current analysis',
    age = "Age",
    sex = 'Sex',
    smoke = 'Smoking Habits',
    waist_cm = 'Waist circumference',
    weight_lbs = 'Weight',
    height_cm = 'Height',
    acr_nmr = 'Albumin-to-creatinine ratio',
    acr = 'Albuminuria',
    gfr = "eGFR < 60 ml/min/1.73 m2",
    glucose = "Blood glucose",
    diabetes = "Diabetes",
    hdl = "High density lipoprotein-cholesterol",
    ldl = "Low density lipoprotein-cholesterol",
    chol = "Total cholesterol",
    cln_sbp = "Clinic systolic",
    cln_dbp = "Clinic diastolic"
  ) %>% 
  set_variable_units(
    age = 'years',
    waist_cm = 'cm',
    weight_lbs = 'lbs',
    height_cm = 'cm',
    acr_nmr = 'mg/g',
    glucose = 'mg/dL',
    hdl = 'mg/dL',
    ldl = 'mg/dL',
    chol = 'mg/dL'
  ) %>% 
  set_variable_groups(
    "Blood pressure, mm Hg" = c('cln_sbp','cln_dbp')
  ) %>% 
  tibble_one(formula = ~ . | included) %>% 
  to_word() %>% 
  flextable::width(width=1) %>% 
  flextable::width(j=1, width = 2.5) %>% 
  flextable::fontsize(size = font_size, part = 'all')

tbl_caps[[tbl_counter]] <- paste0(
  "Table ",
  tbl_counter,
  ": Characteristics of participants in the Coronary Artery Risk Development In young Adults (CARDIA) study stratified by inclusion in the current analysis."
)

tbl_counter %<>% add(1)


# JHS participant characteristics (included versus excluded) --------------

tbl_objs[[tbl_counter]] = read_rds('Datasets/jhs_full_cohort.RDS') %>% 
  dplyr::mutate(
    included = factor(
      id %in% read_rds("Datasets/jhs_included_ids.RDS"), 
      levels = c(FALSE, TRUE), 
      labels = c("No","Yes")
    ),
    sex = factor(sex, levels = c("Female","Male")),
    smoke = factor(smoke, levels = c("Never","Former","Current")),
    acr_nmr = ur_alb / ur_cre,
    acr = factor(
      acr_nmr > 30, 
      levels = c(FALSE,TRUE), 
      labels = c("No","Yes")
    ),
    gfr = factor(egfr < 60, levels = c(FALSE, TRUE), labels = c("No","Yes")),
    diabetes = factor(diabetes, levels = c("No","Yes")),
  ) %>% 
  select_labelled(
    included = 'Included in current analysis',
    age = "Age",
    sex = 'Sex',
    smoke = 'Smoking Habits',
    waist_cm = 'Waist circumference',
    weight_lbs = 'Weight',
    height_cm = 'Height',
    acr_nmr = 'Albumin-to-creatinine ratio',
    acr = 'Albuminuria',
    gfr = "eGFR < 60 ml/min/1.73 m2",
    glucose = "Blood glucose",
    diabetes = "Diabetes",
    hdl = "High density lipoprotein-cholesterol",
    ldl = "Low density lipoprotein-cholesterol",
    chol = "Total cholesterol",
    cln_sbp = "Clinic systolic",
    cln_dbp = "Clinic diastolic"
  ) %>% 
  set_variable_units(
    age = 'years',
    waist_cm = 'cm',
    weight_lbs = 'lbs',
    height_cm = 'cm',
    acr_nmr = 'mg/g',
    glucose = 'mg/dL',
    hdl = 'mg/dL',
    ldl = 'mg/dL',
    chol = 'mg/dL'
  ) %>% 
  set_variable_groups(
    "Blood pressure, mm Hg" = c('cln_sbp','cln_dbp')
  ) %>%
  tibble_one(formula = ~ . | included) %>% 
  to_word() %>% 
  flextable::width(width=1) %>% 
  flextable::width(j=1, width = 2.5) %>% 
  flextable::fontsize(size = font_size, part = 'all')

tbl_caps[[tbl_counter]] <- paste0(
  "Table ",
  tbl_counter,
  ": Characteristics of participants in the Jackson Heart Study (JHS) stratified by inclusion in the current analysis."
)

tbl_counter %<>% add(1)

# Model summary table -----------------------------------------------------

GAM = read_rds('Results/fitted_GAM.RDS')[[analysis_type]][tars]

fctrs <- map_lgl(data$derivation, is.factor) %>% 
  which() %>% 
  names()

ref_labs <- fctrs %>% 
  purrr::set_names() %>% 
  map_chr(~levels(data$derivation[[.x]])[1]) %>% 
  enframe() %>% 
  mutate(var = paste0(name, value))

mtab <- tars %>% 
  map_dfr(
    .f=function(tar){
      
      mdl <- GAM[[tar]]$mdl
      dta <- mdl$data
      
      merge_in <- ref_labs %>% 
        filter(name %in% all.vars(mdl$formula))
      
      model_df <- mdl %>%
        summary.glm() %>%
        coefficients() %>%
        as_tibble(rownames = 'var') %>%
        dplyr::rename(ste=`Std. Error`) %>% 
        dplyr::mutate(
          non_linear = case_when(
            substr(var, 0,2)=='s(' ~ 1,
            TRUE ~ 0
          ),
          est = case_when(
            non_linear==0 ~ Estimate
          ),
          ste = case_when(
            non_linear==0 ~ ste
          ),
          var = gsub("s(", "", var, fixed = TRUE),
          var = gsub(", df = 2)", "", var, fixed = TRUE),
          var = gsub(", df = 3)", "", var, fixed = TRUE)
        ) %>%
        dplyr::filter(var !='(Intercept)') %>% 
        dplyr::mutate(upr=est+1.96*ste,lwr=est-1.96*ste,
          val=paste0(
            frexp(est),' (',
            frexp(lwr),', ', 
            frexp(upr),')')
        ) %>%
        dplyr::select(var,val,non_linear) 
      
      nonlinear_variables <- model_df %>%
        dplyr::filter(non_linear==1) %>% 
        pluck("var") %>% 
        set_names(paste(.)) %>% 
        map(
          ~{
            xval=c(
              mean(dta[[.]])*sds[.],
              mean(dta[[.]])*sds[.]+sds[.]
            )
            ds=map_dbl(1:100,
              .f=function(i){
                tmp=dta[sample(nrow(dta),replace=T),]
                mod=suppressWarnings(
                  gam(mdl$formula,data=tmp,family='binomial')
                )
                prd=map(xval,.f=function(xv){
                  tmp[[.]]=xv/sds[.]
                  predict(mod,newdata=tmp)
                })
                median(exp(prd[[2]]-prd[[1]]),na.rm=TRUE)
              })
            quants <- quantile(ds,probs=c(0.025, 0.500, 0.975)) %>% fr()
            paste0(quants[2],' (',quants[1],', ', quants[3],')')
          }
        ) %>% 
        bind_rows() %>% 
        gather()
      
      for(i in nonlinear_variables$key){
        model_df$val[model_df$var==i] <- 
          nonlinear_variables$value[nonlinear_variables$key==i]
      }
      
      model_df %>% 
        mutate(
          val = case_when(
            non_linear==1 ~ paste0(val,'*'),
            TRUE ~ val
          )
        ) %>% 
        dplyr::select(-non_linear)
      
    },
    .id = 'tar'
  ) %>% 
  tidyr::spread(tar,val) %>% 
  mutate(
    var = factor(
      var,
      levels = c(
        'age',
        'sexMale',
        'raceBlack',
        'raceAsian',
        'raceOther',
        'neck_cm',
        'height_cm',
        'waist_cm',
        'weight_lbs',
        'alcoholSome',
        'hdl',
        'cln_sbp',
        'cln_dbp',
        'acr_nmr',
        'diabetesYes',
        'smokeFormer',
        'smokeNever',
        'glucose'
      ),
      labels = c(
        paste0('Age, ', round(sds['age']), ' years'),
        'Male',
        'Black',
        'Asian',
        'Other race',
        paste0('Neck circumference, ', round(sds['neck_cm']), ' cm'),
        paste0('Height, ', round(sds['height_cm']), ' cm'),
        paste0('Waist circumference, ', round(sds['waist_cm']), ' cm'),
        paste0('Weight, ', round(sds['weight_lbs']), ' lbs'),
        'Alcohol use',
        paste0('HDL, ', round(sds['hdl']), ' mg/dL'),
        paste0('Clinic SBP, ', round(sds['cln_sbp']), ' mm Hg'),
        paste0('Clinic DBP, ', round(sds['cln_dbp']), ' mm Hg'),
        paste0('Log(1+ACR),', ' g/24hr'),
        "Diabetes",
        "Former smoker",
        "Never smoker",
        paste0('Blood glucose, ', round(sds['glucose']), ' mg/dL')
      )
    )
  ) %>% 
  dplyr::arrange(var) %>%
  dplyr::select(
    `Variable`=var, 
    !!!tar_recoder
  ) %>% 
  dplyr::mutate_all(as.character) %>% 
  dplyr::mutate_all(clean_na)

tbl_objs[[tbl_counter]] <- flextable(mtab) %>% 
  flextable::theme_box() %>% 
  flextable::fontsize(size = font_size, part = 'all') %>% 
  flextable::width(width = 2) %>% 
  flextable::align(align='center', part = 'all') %>% 
  flextable::align(j = 1, align = 'left', part = "all") %>% 
  flextable::footnote(
    i = 1, 
    j = 1, 
    ref_symbols = "",
    part = 'header',
    value = as_paragraph("Table values were computed using the derivation data.")
  ) %>%
  flextable::footnote(
    i = 1, 
    j = 1, 
    ref_symbols = "",
    part = 'header',
    value = as_paragraph("* This is a non-linear variable in the predictive equation. The odds ratio is presented using the mean as a reference value.")
  ) %>%
  flextable::footnote(
    i = 1, 
    j = 1, 
    ref_symbols = "",
    part = 'header',
    value = as_paragraph("--- indicates that a variable was not selected for inclusion in the corresponding equation.")
  ) %>%
  flextable::footnote(
    i = 1, 
    j = 1, 
    ref_symbols = "",
    part = 'header',
    value = as_paragraph("The odds ratios for continuous predictor variables (age, neck circumference, waist circumference, blood glucose, HDL cholesterol, log albumin-to-creatinine ratio, urinary creatinine, clinic systolic blood pressure and clinic diastolic blood pressure) are presented for a one standard deviation higher level of the exposure value.")
  ) %>%
  flextable::footnote(
    i = 1, 
    j = 1, 
    ref_symbols = "",
    part = 'header',
    value = as_paragraph("ACR = albumin-to-creatinine ratio; DBP = diastolic blood pressure; SBP = systolic blood pressure.")
  )

tbl_caps[[tbl_counter]] <- paste0(
    "Table ",
    tbl_counter,
    ": Odds ratios for variables selected for inclusion in the predictive equations for ",
  list_elements(tolower(tar_labs)), "."
  )

tbl_counter %<>% add(1)

# Model evaluation table -----------------------------------------------------

do_hosmer <- function(df, target, predictions, ...){
  hoslem.test(
    df[[target]],
    df[[predictions]],
    ...
  ) %>% 
    magrittr::use_series("p.value") %>% 
    round(digits = 3) %>% 
    format(nsmall = 3)
}

do_cstat <- function(df, target, predictions, ...){
  
  cstat = pROC::roc(
    response = df[[target]],
    predictor = df[[predictions]],
    auc = TRUE,
    ci = TRUE
  ) %>% 
    use_series("ci") %>% 
    as.numeric()
  
  paste0(
    fr(cstat[2]), " (",
    fr(cstat[1]), ", ",
    fr(cstat[3]), ")"
  )
  
}

mdl_eval_data <- data$validation %>% 
  mutate(
    race = fct_collapse(
      race,
      "Non-white" = c("Black","Asian","Other")
    ),
    overall = "Overall"
  ) %>% 
  as_tibble()

for(t in tars){
  mdl_eval_data[[paste0(t,'_pred')]] <- GAM[[t]]$prd$test
}

grp_vars <- c("race","sex","study","htn_meds","educ","overall")

cols_to_mutate <- tars %>% 
  map(
    ~ c(
      paste(.x),
      paste0(.x,'_pred')
    )
  )

cols_to_make <- tars %>% 
  map(
    ~c(
      paste0("prev_",.x),
      paste0("cal_",.x),
      paste0("cstat_",.x)
    )
  )

mdl_eval <- grp_vars %>% 
  purrr::set_names() %>% 
  map_dfr(
    .f = function(grp_var){
      
      grp_eval_data <- mdl_eval_data %>% 
        group_by_at(grp_var) %>%
        nest() %>% 
        purrr::set_names(
          nm = gsub(grp_var, "level", names(.))
        )
        
      for(i in seq_along(cols_to_mutate)){
        
        grp_eval_data[[cols_to_make[[i]][1]]] <- map_dbl(
          .x = grp_eval_data$data,
          .f = ~ mean(.x[[ cols_to_mutate[[i]][1] ]])
        )
        
        grp_eval_data[[cols_to_make[[i]][2]]] <- map_chr(
          .x = grp_eval_data$data,
          .f = do_hosmer,
          target = cols_to_mutate[[i]][1],
          predictions = cols_to_mutate[[i]][2],
          g = 10
        )
        
        grp_eval_data[[cols_to_make[[i]][3]]] <- map_chr(
          .x = grp_eval_data$data,
          .f = do_cstat,
          target = cols_to_mutate[[i]][1],
          predictions = cols_to_mutate[[i]][2]
        )
        
      }
        grp_eval_data %>%
          ungroup() %>% 
          mutate(nobs = map_int(data, nrow)) %>% 
          dplyr::select(-data) %>% 
          mutate_if(is.factor, as.character)
    },
    .id = "variable"
  )

flex_col_labels <- cols_to_make %>% 
  map(
    ~ .x %>% 
      purrr::set_names() %>% 
      map(
        .f = function(eval_name){
          eval_name = gsub(
            pattern = 'prev_',
            replacement = '',
            x = eval_name
          )
          eval_name = gsub(
            pattern = 'cal_',
            replacement = '',
            x = eval_name
          )
          eval_name = gsub(
            pattern = 'cstat_',
            replacement = '',
            x = eval_name
          )
          tar_labs[eval_name]
        })
  ) %>% 
  flatten()

flex_col_labels$level = ''

tbl_objs[[tbl_counter]] <- mdl_eval %>%
  mutate(
    variable = recode(
      variable, 
      race = 'Race',
      sex = 'Sex',
      educ = 'High school graduate',
      htn_meds = 'Antihypertensive medication use',
      overall = "All participants in validation data"
    ),
    level = recode(
      level,
      cardia = 'CARDIA',
      jhs = 'JHS',
      idh = 'IDH',
      mht = 'MHT'
    )
  ) %>% 
  mutate_at(
    vars(starts_with("prev")),
    ~format(round(.x*100,1),nsmall=1)
  ) %>% 
  group_by(variable) %>% 
  mutate(
    perc = format(round(100 * nobs / sum(nobs),1),nsmall=1),
    level = paste0(
      level, ', N = ',
      nobs, ' (', 
      perc, "%)"
    )
  ) %>% 
  select(
    variable, 
    level,
    starts_with("prev"),
    starts_with('cal'),
    starts_with('cstat')
  ) %>% 
  as_grouped_data(groups = 'variable') %>% 
  as_flextable() %>% 
  flextable::add_header_row(
    values = c(
      "",
      "Prevalence",
      "P-value from Hosmer and Lemeshow's goodness of fit test",
      "Concordance Statistic (95% Confidence Interval)"
    ),
    colwidths = c(1, rep(length(tars), length(cols_to_make[[1]])))
  ) %>% 
  theme_box() %>% 
  flextable::compose(
    i = ~ !is.na(variable),
    j = 1,
    value = flextable::as_paragraph(
      flextable::as_chunk(variable)
    )
  ) %>% 
  flextable::bg(
    i = ~ !is.na(variable),
    bg = 'grey80'
  ) %>% 
  flextable::align(
    align = "center",
    part = 'all'
  ) %>% 
  flextable::align(
    j = 1, 
    align = 'left',
    part = 'all'
  ) %>% 
  flextable::width(width = 1.25) %>% 
  flextable::width(j = 1, width = 2) %>% 
  flextable::fontsize(size = font_size, part = 'all') %>% 
  flextable::set_header_labels(
    values = flex_col_labels
  ) 

tbl_caps[[tbl_counter]] <- paste0(
  "Table ",
  tbl_counter,
  ": Calibration and discrimination of predictive equations for nocturnal hypertension and non-dipping systolic blood pressure overall and in sub-groups determined by race, sex, and antihypertensive medication use."
)

tbl_counter %<>% add(1)


# Model sensitivity table -------------------------------------------------

var_label(analysis$cln_sbp) <- "Clinic SBP"
var_label(analysis$cln_dbp) <- "Clinic DBP"
var_label(analysis$slp_sbp) <- "Asleep SBP"
var_label(analysis$slp_dbp) <- "Asleep DBP"

recoder <- map_chr(
  .x = analysis, 
  .f = ~attr(.x, 'label')
) %>% 
  c(
    egfr = 'eGFR',
    educ = "High school education",
    sex = 'Sex',
    hispanic = 'Hispanic'
  )

msens <- read_rds("Results/model_sensitivity.rds") %>%
  purrr::discard(is.null) %>% 
  bind_rows(.id = 'tar') %>% 
  filter(tar %in% tars) %>% 
  mutate(tar = recode(tar, !!!recoder)) %>% 
  arrange(tar) %>% 
  select(tar, variable, tbl_val) %>% 
  spread(tar, tbl_val) %>% 
  mutate(variable = recode(variable, !!!recoder)) %>% 
  mutate(variable = factor(variable)) %>% 
  arrange(variable) %>% 
  rename(Variable = variable)

tbl_objs[[tbl_counter]] <- flextable::flextable(msens) %>% 
  flextable::theme_box() %>%
  flextable::width(width = 2) %>% 
  flextable::width(j = 1, width = 2.5) %>% 
  flextable::align(align = 'center', part = 'all') %>% 
  flextable::align(j = 1, align = "left", part = 'all')

if(length(tars)==1){
  tbl_objs[[tbl_counter]] %<>% 
    flextable::set_header_labels(values = as.list(tar_labs))
}

tbl_caps[[tbl_counter]] <- paste0(
  "Table ",
  tbl_counter,
  ": Proportions of bootstrap replicates where candidate variables were selected for inclusion in predictive equations for ", 
  tolower(list_elements(tar_labs))
)

tbl_counter %<>% add(1)


# Cut-points --------------------------------------------------------------

cpnts <- tars %>% 
  map(
    .f=function(tar){
      
      G <- GAM[[tar]]
      
      pscr = tibble(
        prob = seq(
          quantile(GAM[[tar]]$prd$train, probs = 0.2), 
          quantile(GAM[[tar]]$prd$train, probs = 0.9), 
          length.out = 1000
        ),
        perc = 0,
        diff = 0,
        sens = 0,
        spec = 0,
        ppv = 0,
        npv = 0,
        youd = 0
      )
      
      # Get percent screened for all probs
      for(i in 1:length(pscr$prob)){
        pscr$perc[i] = mean(G$prd$train>=pscr$prob[i])
      }
      
      # Find percent that is closest to prevalance
      prev = mean(data$derivation[[tar]] == 1)
      pscr %<>% mutate(diff=abs(perc-prev))
      
      prevalance_cut_point = pscr$prob[which.min(pscr$diff)]
      
      for(i in 1:length(pscr$prob)){
        
        tmp = ifelse(G$prd$train>=pscr$prob[i],1,0)
        
        epivals <- table(tmp, data$derivation[[tar]]) %>%
          rev() %>% epiR::epi.tests() %>% 
          use_series('elements') 
        
        pscr$sens[i] = epivals$sensitivity$est
        pscr$spec[i] = epivals$specificity$est
        pscr$ppv[i] = epivals$ppv
        pscr$npv[i] = epivals$npv
        pscr$youd[i] = epivals$youden$est
        
      }
      
      tmp = pscr[pscr$sens>0.80,]
      sensitivity_cut_point = tmp$prob[nrow(tmp)]
      
      tmp=pscr[pscr$ppv>0.80,]
      if(nrow(tmp)==0){
        tmp=pscr[pscr$ppv>0.5,]
      }
      ppv_cut_point=tmp$prob[which.max(tmp$npv)]
      
      ### max youden
      youden_cut_point=pscr$prob[which.max(pscr$youd)]
      
      c(prev=prevalance_cut_point,
        sens=sensitivity_cut_point,
        ppv=ppv_cut_point,
        youden=youden_cut_point)
      
    }
  ) 

perc_above_bp_cutpoint <- tars %>% 
  map(
    .f = function(tar){
      map_dbl(
        .x = cpnts[[tar]], 
        .f = ~ mean(GAM[[tar]]$prd$train > .x)
      )
    }
  )

bp.classifiers <- list(
  "bp_120_70" = c(120,70),
  "bp_130_80" = c(130,80),
  "bp_140_90" = c(140,90)
) %>% 
  map(.f=function(bp){
    with(data$validation, ifelse(
      cln_sbp >= bp[1] | cln_dbp >= bp[2], 1, 0
    ))
  })

bp.classifiers$htn_meds<-as.numeric(data$validation$htn_meds)-1

# Reference models --------------------------------------------------------

std <- tars %>% 
  map(.f=function(tar){
    
    mdl <- glm(
      as.formula(paste(tar,'~ cln_sbp+cln_dbp+htn_meds')),
      family = binomial(),
      data = data$derivation
    )
    
    prd <- predict(
      object = mdl,
      newdata = data$validation,
      type='response'
    )
    
    list(mdl=mdl,prd=prd)
  })

roc_comparison = std %>% 
  enframe(value = 'ref') %>% 
  mutate(
    ref = map(ref, 'prd'),
    ref = map2(name, ref, 
      .f = ~ pROC::roc(
        response = data$validation[[.x]],
        predictor = .y
      )
    ),
    peq = map(GAM, ~.x$prd$test),
    peq = map2(name, peq, 
      .f = ~ pROC::roc(
        response = data$validation[[.x]],
        predictor = .y
      )
    ),
    test = map2(ref, peq,
      .f = ~ pROC::roc.test(.x, .y, method = 'bootstrap')
    )
  ) 




# Test characteristic table -----------------------------------------------

tchar = tars %>% 
  map(.f=function(tar){
    
    list(
      auc=list(
        pe=pROC::roc(
          response = data$validation[[tar]],
          predictor = GAM[[tar]]$prd$test,
          ci = TRUE
        ),
        bp = pROC::roc(
          response = data$validation[[tar]],
          predictor = std[[tar]]$prd,
          ci = TRUE
        )
      ) %>%
        map(.f=~paste0(fr(.$auc),' (',fr(.$ci[1]),', ',fr(.$ci[3]),')')),
      
      test_char = list(
        map(cpnts[[tar]],.f=function(cp){
          
          epivals <- 
            table(
              GAM[[tar]]$prd$test >= cp, 
              data$validation[[tar]]
            ) %>%
            rev() %>% 
            epi.tests()
          
          c(
            epivals$elements$aprev$est*100,
            epivals$elements$sensitivity$est,
            epivals$elements$specificity$est,
            epivals$elements$pv.positive$est,
            epivals$elements$pv.negative$est,
            epivals$elements$youden$est
          ) %>% 
            adapt_round()
          
        }) %>% reduce(cbind),
        
        map(bp.classifiers,.f=function(bpc){
          
          epivals <- table(bpc, data$validation[[tar]]) %>%
            rev() %>% 
            epi.tests()
          c(
            epivals$elements$aprev$est*100,
            epivals$elements$sensitivity$est,
            epivals$elements$specificity$est,
            epivals$elements$pv.positive$est,
            epivals$elements$pv.negative$est,
            epivals$elements$youden$est
          ) %>% 
            adapt_round()
          
        }) %>% 
          reduce(cbind)
        
      ) %>% 
        reduce(cbind) %>%
        set_colnames(
          c('1','2','3','4','120/70','130/80','140/90','meds')
        )%>%
        set_rownames(c("Percent screened","Sensitivity","Specificity",
          "Positive Predictive Value","Negative Predictive Value",
          "Youden\'s Index"))
    )
    
  }) %>% set_names(tars)

bind_on <- map(
  cpnts, ~ matrix(
    paste0(
      c(rep("\u2265",7),""),
      c(round(.x,2),"120/70", "130/80", "140/90", "Yes")
    ),
    nrow = 1
  ) %>% 
    set_rownames("Classification cut-point")
)
  
tbl_objs[[tbl_counter]] = map(tchar, ~.x$test_char) %>%
  map2(bind_on, ~ rbind(.y, .x)) %>% 
  map(as_tibble, rownames = ' ') %>% 
  bind_rows(.id = 'tar') %>% 
  set_names(c("tar"," ",paste(1:4),"I","II","III","IV")) %>% 
  mutate(tar = recode(tar, !!!tar_labs)) %>% 
  as_grouped_data(groups = 'tar') %>% 
  as_flextable()  %>% 
  flextable::add_header_row(
    values = c(
      "",
      "Predictive equation probability cut-points",
      "Systolic/Diastolic Blood pressure cut-points, mm Hg",	
      "Currently using anti-hypertensive medication"
    ),
    colwidths = c(1, 4, 3, 1)
  ) %>% 
  flextable::add_header_row(
    values = c(
      "",
      "Methods of identifying who should undergo 24-hour ambulatory blood pressure monitoring."
    ),
    colwidths = c(1, 8)
  ) %>% 
  theme_box() %>% 
  flextable::compose(
    i = ~ !is.na(tar),
    j = 1,
    value = flextable::as_paragraph(
      flextable::as_chunk(tar)
    )
  ) %>% 
  flextable::bg(
    i = ~ !is.na(tar),
    bg = 'grey80'
  ) %>% 
  flextable::align(
    align = "center",
    part = 'all'
  ) %>% 
  flextable::align(
    j = 1, 
    align = 'left',
    part = 'body'
  ) %>% 
  flextable::width(width = 3/4) %>% 
  flextable::width(j = 1, width = 1.75) %>% 
  flextable::width(j = 9, width = 1) %>% 
  flextable::fontsize(size = font_size, part = 'all')

tbl_caps[[tbl_counter]] <- paste0(
  "Table ",
  tbl_counter,
  ": Test characteristics of the predictive equations and alternative screening methods for identifying adults with a high probability of ", 
  list_elements(tolower(tar_labs)), "."
)

tbl_counter %<>% add(1)

if(all(c('nhtn','nd_sbp') %in% tars)){
  if(complete_cases){
    saveRDS(cpnts, "Results/cut_points_complete_cases.RDS")
  } else {
    saveRDS(cpnts, "Results/cut_points.RDS")
  }
}

# Net reclassification table ----------------------------------------------

nri_stats <- tars %>% 
  map_dfr(
    .f = function(tar){
      bp.classifiers %>% 
        map_dfr(
          ~ nribin(
            event = data$validation[[tar]],
            p.std = .x, 
            p.new = GAM[[tar]]$prd$test,
            cut = cpnts[[tar]]['youden'],
            niter = 1000
          ) %>% 
            use_series("nri") %>% 
            as_tibble(rownames = 'type') %>% 
            mutate(
              tbl_val = paste0(
                fr(Estimate),' (',
                fr(Lower),', ',
                fr(Upper),')'
              )
            ) %>% 
            filter(type %in% c("NRI", "NRI+", "NRI-")) %>% 
            select(type, tbl_val),
          .id = 'bp_classif'
        )
    },
    .id = 'tar'
  )

cntn_rc <- tars %>% 
  map_dfr(
    .f = function(tar){
      rc = reclassification(
        data = as.data.frame(data$validation),
        cOutcome = which(names(data$validation)==tar),
        predrisk1 = std[[tar]]$prd,
        predrisk2 = GAM[[tar]]$prd$test,
        cutoff = as.numeric(c(0,sort(cpnts[[tar]]),1))
      )
      
      tibble(
        NRI_c = with(
          rc$cntn_NRI, 
          paste0(
            fr(nri), " (",
            fr(nri - 1.96*se.nri), ", ",
            fr(nri + 1.96*se.nri), ")"
          )
        ),
        IDI_c = with(
          rc$cntn_NRI, 
          paste0(
            fr(idi), " (",
            fr(idi - 1.96*se.idi), ", ",
            fr(idi + 1.96*se.idi), ")"
          )
        )
      )
    },
    .id = "tar"
  ) %>% 
  gather(type, tbl_val, -tar) %>% 
  mutate(bp_classif = 'ref_mdl')

tbl_objs[[tbl_counter]] <- nri_stats %>% 
  bind_rows(cntn_rc) %>% 
  tidyr::spread(tar,tbl_val) %>%
  mutate(
    bp_classif=factor(
      bp_classif,
      levels=c(names(bp.classifiers), "ref_mdl"),
      labels=c(
        "Clinic SBP/DBP \u2265 120/70 mm Hg",
        "Clinic SBP/DBP \u2265 130/80 mm Hg",
        "Clinic SBP/DBP \u2265 140/90 mm Hg",
        "Antihypertensive medication use",
        "Models using SBP, DBP and antihypertensive medication use"
      )
    ),
    type = factor(
      type, 
      levels = c("NRI","NRI-", "NRI+","NRI_c","IDI_c"),
      labels = c(
        "Overall categorical net reclassification index",
        "Negative categorical net reclassification index",
        "Positive categorical net reclassification index",
        "Continuous net reclassification index",
        "Integrated discriminative improvement index"
      )
    )
  ) %>%
  arrange(type, bp_classif) %>% 
  set_names(
    c("Methods of identifying who should undergo 24-hour ambulatory blood pressure monitoring",
      'type', 
      tar_labs
    )
  ) %>%
  as_grouped_data(groups = 'type') %>% 
  flextable::as_flextable() %>% 
  flextable::add_header_row(
    values = c(
      "",
      "Reclassification improvement using predictive equations (95% confidence interval)"
    ),
    colwidths = c(1, length(tars))
  ) %>% 
  theme_box() %>% 
  flextable::compose(
    i = ~ !is.na(type),
    j = 1,
    value = flextable::as_paragraph(
      flextable::as_chunk(type)
    )
  ) %>% 
  flextable::bg(
    i = ~ !is.na(type),
    bg = 'grey80'
  ) %>% 
  flextable::align(
    align = "center",
    part = 'all'
  ) %>% 
  flextable::align(
    j = 1, 
    align = 'left',
    part = 'all'
  ) %>% 
  flextable::width(width = 2) %>% 
  flextable::width(j = 1, width = 2.5) %>% 
  flextable::footnote(
    i = 1, 
    j = 1,
    ref_symbols = "",
    value = as_paragraph(
      "Table values were computed using the validation data."
    )
  ) %>% 
  flextable::footnote(
    i = ~ type %in% c(
      "Overall categorical net reclassification index",
      "Negative categorical net reclassification index",
      "Positive categorical net reclassification index"
    ), 
    j = 1,
    ref_symbols = "*",
    value = as_paragraph(
      paste0(
      "For categorical net reclassification indices, the probability cut-points maximizing Youden’s index for the predictive equations (", 
      list_elements(map_chr(cpnts, ~fr(.x['youden']))),
      " for ",
      tolower(list_elements(tar_labs)),
      ", respectively) were used.",
      " These cut-points were chosen assuming that they provide better overall classification characteristics than the other three cut-points."
      )
    )
  ) %>% 
  flextable::footnote(
    i = ~ `Methods of identifying who should undergo 24-hour ambulatory blood pressure monitoring` == "Models using SBP, DBP and antihypertensive medication use", 
    j = 1,
    ref_symbols = "†",
    value = as_paragraph(
      paste0(
      "Predicted probabilities were obtained from equations formed for ",
      list_elements(tar_labs), " separately, ",
      "using logistic regression in the derivation data set with clinic systolic and diastolic blood pressure and antihypertensive medication use as independent variables."
      )
    )
  ) %>% 
  flextable::fontsize(size = font_size, part = 'all') %>% 
  flextable::merge_v(part = 'header')

  
tbl_caps[[tbl_counter]] <- paste0(
  "Table ",
  tbl_counter,
  ": Net reclassification improvement and integrated discriminative improvement using predictive equations from the current analysis versus screening methods based on clinic blood pressure and antihypertensive medication use."
)

tbl_counter %<>% add(1)



# Formula expression table ------------------------------------------------

library(gt)

formulas=map_chr(tars,.f=function(tar){
  
  mfrm=paste(GAM[[analysis_type]][[tar]]$mdl$formula)
  mfrm[3]=gsub("s(","poly(",mfrm[3],fixed=T)
  mfrm[3]=gsub("df =","degree =",mfrm[3],fixed=T)
  mfrm[3]=gsub(")",", raw = T)",mfrm[3],fixed=T)
  dta=GAM$complete[[tar]]$mdl$data
  
  for(i in names(dta)){
    if(i %in% names(sds)){
      dta[[i]]%<>%multiply_by(sds[i])
    }
  }
  
  mdl=glm(
    as.formula(paste(mfrm[2],mfrm[1],mfrm[3])),
    data=dta,
    family='binomial'
  )
  
  cfs=format(round(coef(mdl),6),nsmall=6)
  
  out=paste0(cfs,"*",names(cfs), collapse=' + ')
  out=gsub("poly(","",out,fixed=T)
  out=gsub(", degree = 2, raw = T)","",out,fixed=T)
  out=gsub(", degree = 3, raw = T)","",out,fixed=T)
  out=gsub("*(Intercept)","",out,fixed=T)
  out=gsub("  "," ",out,fixed=T)
  out=gsub(" + -"," - ",out,fixed=T)
  out=gsub("age","(age in years)",out,fixed=T)
  out=gsub("neck_cm","(neck circumference in cm)",out,fixed=T)
  out=gsub("sexMale","(1 if male, 0 otherwise)",out,fixed=T)
  out=gsub("diabetesYes","(1 if diabetic, 0 otherwise)",out,fixed=T)
  out=gsub("raceBlack","(1 if black, 0 otherwise)",out,fixed=T)
  out=gsub("raceAsian","(1 if asian, 0 otherwise)",out,fixed=T)
  out=gsub("raceOther","(1 if other race, 0 otherwise)",out,fixed=T)
  out=gsub("raceWhite","(1 if white, 0 otherwise)",out,fixed=T)
  out=gsub("smokeCurrent","(1 if current smoker, 0 otherwise)",out,fixed=T)
  out=gsub("smokeFormer","(1 if former smoker, 0 otherwise)",out,fixed=T)
  out=gsub("alcoholSome","(1 if drinks alcohol, 0 otherwise)",out,fixed=T)
  out=gsub("height_cm","(height in cm)",out,fixed=T)
  out=gsub("weight_lbs","(weight in lbs)",out,fixed=T)
  out=gsub("cln_sbp","(clinic SBP in mm Hg)",out,fixed=T)
  out=gsub("cln_dbp","(clinic DBP in mm Hg)",out,fixed=T)
  out=gsub("waist_cm","(waist circumference in cm)",out,fixed=T)
  out=gsub("ur_cre","(urinary creatinine in mg/dL)",out,fixed=T)
  out=gsub("hdl","(HDL in mg/dL)",out,fixed=T)
  out=gsub("glucose","(glucose in mg/dL)",out,fixed=T)
  out=gsub("acr_nmr","log(ACR + 1)",out,fixed=T)
  out=gsub(")1",")",out,fixed=T)
  out=gsub(")2",")^2",out,fixed=T)
  out=gsub(")3",")^3",out,fixed=T)
  
  out
  
}) %>% 
  set_names(
    c(
      "Nocturnal hypertension",
      "Non-dipping systolic blood pressure"
    )
  ) %>% 
  enframe(name='Equation', value='Formula') %>% 
  gt::gt() %>% 
  gt::tab_footnote(
    footnote="Predicted probability = exp(linear predictor) / (1 + exp(linear predictor))",
    locations=cells_column_labels(columns='Formula')
  ) %>% 
  gt::tab_footnote(
    footnote="The predictive equations shown here apply polynomials to model non-linear effects. These polynomials are approximately equal to the non-parametric smoothing functions used by the predictive equations developed in the current analysis.",
    locations=cells_column_labels(columns='Equation')
  ) %>% 
  gt::tab_footnote(
    footnote = "ACR = albumin-to-creatinine ratio; DBP = diastolic blood pressure; HDL = high density lipoproteins; SBP = systolic blood pressure.",
    locations = cells_column_labels(columns='Formula')
  ) %>% 
  gt::tab_options(
    footnote.glyph=c("*, †, ‡, §, ||, ¶, #, **")
  )

formulas

# Sens/Spec Figure --------------------------------------------------------

cpdat <- cpnts %>% 
  map_dfr(
    ~ as.data.frame(.x) %>% 
      mutate(cut_pnt = 1:nrow(.)),
    .id = 'rslt'
  ) %>% 
  rename(cp = .x) %>% 
  mutate(
    cp_txt = cp,
    rslt = recode(rslt, !!!tar_labs)
  )

c1 = cpdat$cut_pnt==2 & cpdat$rslt=='Nocturnal Hypertension' & !complete_cases
c2 = cpdat$cut_pnt==4 & cpdat$rslt=='Nocturnal Hypertension' & !complete_cases

cpdat$cp_txt[c1] %<>% add(0.025)
cpdat$cp_txt[c2] %<>% subtract(0.025)

c1 = cpdat$cut_pnt==1 & 
  cpdat$rslt=='Non-dipping Systolic Blood Pressure' & 
  !complete_cases
c2 = cpdat$cut_pnt==4 &
  cpdat$rslt=='Non-dipping Systolic Blood Pressure' &
  !complete_cases

cpdat$cp_txt[c1] %<>% add(0.05)
cpdat$cp_txt[c2] %<>% subtract(0.05)

c1 = cpdat$cut_pnt==2 & cpdat$rslt=='Non-dipping Diastolic Blood Pressure'
c2 = cpdat$cut_pnt==4 & cpdat$rslt=='Non-dipping Diastolic Blood Pressure'

cpdat$cp_txt[c1] %<>% add(0.025)
cpdat$cp_txt[c2] %<>% subtract(0.025)


roc_stats <-  map2_dfr(
  GAM, tars, 
  .f = function(gam_obj, tar){
    roc_obj = pROC::roc(
      data$derivation[[tar]], 
      gam_obj$prd$train
    ) 
    tibble(
      sens = roc_obj$sensitivities,
      spec = roc_obj$specificities,
      thrs = roc_obj$thresholds,
      rslt = tar
    )
  }) %>% 
  group_by(rslt) %>% 
  dplyr::filter(abs(thrs)<1) %>%
  dplyr::arrange(desc(sens)) %>% 
  mutate(youd = sens + spec) %>% 
  ungroup() %>%
  mutate(
    rslt=factor(
      rslt, 
      levels=tars,
      labels=tar_labs)
  ) %>%
  gather(variable, value, -thrs, -rslt) %>%
  mutate(
    variable = factor(
      variable,
      levels=c('youd','spec','sens'), 
      labels=c('Youden\'s \nIndex','Specificity','Sensitivity')
    )
  )

rows_1to3 = rows_lst3 <- roc_stats %>% 
  group_by(variable, rslt) %>% 
  slice(1)

rows_1to3$thrs = 0
rows_lst3$thrs = 1

rows_1to3$value = rep(c(1, 0, 1), each = length(tars))
rows_lst3$value = rep(c(1, 1, 0), each = length(tars))

ggdat <- list(
  rows_1to3,
  roc_stats,
  rows_lst3
) %>% 
  bind_rows()

p <- ggdat %>% 
  ggplot(
    aes(
      x=thrs,
      y=value,
      col=variable
    )
  )+
  geom_line()+
  facet_wrap(~rslt)+
  geom_text(
    data=cpdat,
    aes(x = cp_txt, y = -0.09, label = cut_pnt),
    inherit.aes=FALSE,
    size=3
  ) +
  geom_segment(
    data = cpdat,
    aes(
      x = cp,
      xend = cp,
      y = 0,
      yend = 2
    ),
    alpha = 0.4,
    size = 0.5,
    linetype = 2,
    inherit.aes = FALSE
  ) +
  geom_hline(
    aes(yintercept = 0.001), 
    size = 1
  ) +
  coord_cartesian(ylim = c(-0.1, 1.5)) +
  labs(
    x = '\nProbability cut-point',
    y = 'Test Characteristics',
    col = ''
  ) +
  theme(
    panel.spacing = unit(1, "lines"),
    legend.position = 'right',
    legend.direction = 'vertical'
  ) + 
  theme(text = element_text(size = 11))

fig_objs[[fig_counter]] <- p
fig_caps[[fig_counter]] <- paste0(
  "Figure ", 
  fig_counter, 
  ": Sensitivity, specificity and Youden’s index of the predictive equations for", 
  list_elements(tolower(tar_labs)),"."
)

fig_legs[[fig_counter]] <- c(
  "Results are based on the derivation data.",
  "Probability cut-points selected for validation (bottom of each panel):",
  "1: Closest number of predicted and observed cases with nocturnal hypertension and non-dipping systolic blood pressure.",
  
  "2: The maximum specificity with a sensitivity \u2265 0.80.",
  "3: The maximum negative predictive value with a positive predictive value \u2265 0.80.", 
  "4: The maximum sum of sensitivity and specificity."
)

fig_width[[fig_counter]] = 7.5
fig_height[[fig_counter]] = 4

fig_counter %<>% add(1)

# Calibration figure ------------------------------------------------------

ht = tars %>% 
  map(
    .f=function(tar){
      hoslem.test(
        data$validation[[tar]],
        GAM[[tar]]$prd$test,
        g=10
      )
    }
  )

library(devEMF)

filename = file.path(
  "Manuscript",
  "Main",
  "figures",
  "figure_2.emf"
)

emf(
  file=filename,
  width=6.5*2,
  height=3*2
)


cex = 1.25

par(
  mfrow=c(1,2),
  cex.lab = cex,
  cex.axis = cex,
  cex.main = cex
)


library(rms)

for(t in tars){
  
  p = val.prob(
    p = GAM[[t]]$prd$test,
    y = data$validation[[t]],
    statloc = F,
    smooth = T,
    legendloc = c(0.01, 0.99),
    main = tar_labs[t],
    cex = cex
  )
  
  text(
    x=0.75,y=0.4, 
    "Calibration tests:", 
    cex = cex
  )
  
  text(
    x=0.75,y=0.32, 
    paste0(
      "Unreliability: p = ", 
      format(round(p["U:p"],3),nsmall=3)
    ), 
    cex = cex
  )
  text(
    x=0.70,y=0.24, 
    paste0(
      "Hosmer Lemeshow: p = ", 
      format(round(ht[[t]]$p.value,3),nsmall=3)
    ), 
    cex = cex
  )
  
}

dev.off()

fig_objs[[fig_counter]] <- filename
fig_caps[[fig_counter]] <- paste0(
  "Figure ", 
  fig_counter, 
  ": Calibration slope plots for ",
  tolower(list_elements(tar_labs))
)

fig_legs[[fig_counter]] <- c(
  "Results are based on the validation data.",
  "The ideal calibration curve shows the slope of a perfectly calibrated model.",
  "The Logistic and nonparametric calibration slopes estimate the calibration of a predicted equation by fitting a logistic model and a locally estimated scatterplot smoothing model, with predicted probability and observed status playing the role of independent and dependent variables, respectively."
)

fig_width[[fig_counter]]= 6.5
fig_height[[fig_counter]] = 3

fig_counter %<>% add(1)

# Send output to word doc -------------------------------------------------

tbl_objs %<>% 
  map(
    .f = ~ .x %>% 
      flextable::font(fontname = 'Calibri', part = 'all') %>% 
      flextable::fontsize(size = font_size, part = 'all')
  )

my_doc <- read_docx('Manuscript/Main/documents/template.docx') 

ntbl <- length(tbl_objs)
nfig <- length(fig_objs)

if(ntbl > 0){
  for(i in seq(ntbl)){
   my_doc %<>% 
      body_add_par(tbl_caps[[i]],) %>% 
      body_add_flextable(tbl_objs[[i]]) %>% 
      body_add_break()
  }
}

library(devEMF)

if(nfig > 0){
  for(i in seq(nfig)){
    
    if(inherits(fig_objs[[i]], 'gg')){
      filename <- tempfile(fileext = ".emf")
      emf(file = filename, width = 7.5, height = 4)
      print(fig_objs[[i]])
      dev.off()
    } else {
      filename <- fig_objs[[i]]
    }
    
    my_doc %<>% 
      body_add_par(fig_caps[[i]]) %>% 
      body_add_img(
        filename, 
        width = fig_width[[i]],
        height = fig_height[[i]]
      )
    
    if(length(fig_legs[[i]])>0){
      for(j in seq_along(fig_legs[[i]])){
        my_doc %<>% 
          body_add_par(fig_legs[[i]][[j]])   
      }
    }
    
    my_doc %<>% body_add_break()
    
  }
}

my_doc %>% 
  print(
    file.path(
      "Manuscript",
      "Main",
      "documents",
      paste0(
        Sys.Date(),
        "_results_",
        paste(tars,collapse='_'),
        ifelse(complete_cases,'_sensitivity','_main'),
        ".docx"
      )
    )
  )

