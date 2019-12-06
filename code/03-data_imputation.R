
library(tidyverse)
library(magrittr)
library(missForest)

ftrs = read_rds("results/model_predictors.RDS")
tars = read_rds("results/model_outcomes.RDS")

vars <- c(ftrs, tars, "study", "slp_dur")

trn <- readRDS("datasets/derivation_data.RDS")[, vars]
tst <- readRDS("datasets/validation_data.RDS")[, vars]

sds <- ftrs %>%
  purrr::set_names() %>%
  map_chr( ~ class(trn[[.x]])) %>%
  enframe() %>%
  filter(value != 'factor') %>%
  mutate(
    value = map_dbl(name, ~ round(sd(trn[[.x]], na.rm = TRUE)))
  ) %>% 
  deframe()

write_rds(sds, "Results/standard_deviations.RDS")

set.seed(329)

imputed_data <- list(
  train = trn, 
  test = tst
) %>% 
  map(
    ~ mutate_at(.x, vars(nhtn, nd_sbp, nd_dbp), as.factor) %>% 
      as.data.frame() %>% 
      missForest(
        verbose = FALSE, 
        variablewise = FALSE,
        maxiter = 10,
        nodesize = c(5, 15)
      ) %>% 
      use_series("ximp")
  )

write_rds(imputed_data,"Results/imputed_data.RDS")
