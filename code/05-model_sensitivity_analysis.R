
library(magrittr)
library(tidyverse)
library(gam)

data <- read_rds("Results/model_data.RDS")

derivation <- data$imputed$train

ftr <- read_rds("Results/model_predictors.RDS")
tars <- c('nhtn', 'nd_sbp','nd_dbp')

gam_scope <- purrr::set_names(ftr) %>% 
  map(
    .f = function(f){
      if(!is.factor(derivation[[f]])){
        c("1",f,paste0("s(",f,", df=",c(2,3),")"))
      } else {
        c("1",f)
      }
    }
  )

nboots  <- 1000

output <- vector(mode = 'list', length = length(tars)) %>% 
  set_names(tars)

for(tar in tars){
  
  results <- vector(mode='list', length = nboots)
  
  set.seed(329)
  
  # GAM doesn't work with map - so use for-loops instead
  
  for(i in 1:nboots){
    
    print(i)
    
    bstrap <- derivation[sample(1:nrow(derivation), replace = TRUE), ]
    
    gam.obj <- suppressWarnings(
      gam(
        as.formula(paste(tar,'~ 1')),
        data = bstrap, 
        family = binomial()
      )
    )
    
    mdl <- suppressWarnings(
      step.Gam(
        gam.obj,
        scope = gam_scope, 
        trace = FALSE,
        steps = 15
      )
    )
    
    results[[i]] <- enframe(setdiff(all.vars(mdl$formula),tar))
    
  }
  
  results %<>% 
    bind_rows(.id = 'id') %>% 
    group_by(id) %>% 
    mutate(
      nvars = max(name),
      name = 1
    ) %>% 
    spread(value, name, fill = 0)
  
  output[[tar]] <- results %>% 
    ungroup() %>% 
    summarise_at(
      .vars = intersect(ftr, names(.)), ~mean(.x)
    ) %>% 
    gather(variable, perc_sel) %>% 
    arrange(desc(perc_sel)) %>% 
    mutate(tbl_val = format(round(100 * perc_sel, 1), nsmall = 1))
  
}

write_rds(output, "Results/model_sensitivity.rds")

# bind_rows(output, .id = 'tar') %>% 
#   spread(tar, perc_sel)



