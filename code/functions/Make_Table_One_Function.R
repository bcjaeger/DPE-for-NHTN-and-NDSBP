
build_meta <- function(data,main,control=NULL){
  
  structure(
    list(data=data,main=main,control=control),
    class='meta_data'
  )
}

build_cnt <- function(..., abrv=NULL,footnote=NULL,group=NULL,descr =NULL){
  
  args <- eval(substitute(alist(...)))
  args <- sapply(args, as.character)
  if(is.null(descr)) descr = args[1] %>% tolower()
  
  structure(
    list(label=args[1], 
         unit =args[2], 
         lvls=NULL,
         abbreviation=abrv,
         group=group,
         footnote=footnote,
         descr=descr),
    class = 'cnt_var'
  )

  
}

build_cat <- function(..., unit=NULL,abrv=NULL,footnote=NULL,group=NULL,descr=NULL){
  
  args <- eval(substitute(alist(...)))
  args <- sapply(args, as.character)
  if(length(args)==1) args = c(args, 'No','Yes')
  if(is.null(descr)) descr = args[1] %>% tolower() 
  
  structure(
    list(label = args[1], 
         unit = unit,
         lvls = args[2:length(args)],
         abbreviation=abrv,
         footnote=footnote,
         group=group,
         descr=descr),
    class = 'cat_var'
  )

  
}

build_cut <- function(..., unit=NULL, abrv=NULL, footnote=NULL, 
                      group=NULL, descr=NULL){
  
  args <- eval(substitute(alist(...)))
  args <- sapply(args, as.character)
  if(is.null(descr)){
    if(!is.null(abrv)){
      descr = abrv %>% tolower()
    } else {
      descr = args[1] %>% tolower()
    }
  } 
  
  structure(
    list(label = args[1], 
         unit = unit,
         lvls = args[2:length(args)],
         abbreviation=abrv,
         footnote=footnote,
         group=group,
         descr=descr),
    class = 'cut_var'
  )
  
}


build_out <- function(...,descr='Short Description',
                      unit=NULL,
                      cutpoint=NULL, 
                      abrv=NULL, 
                      caption_lvls=NULL){
  
  args <- eval(substitute(alist(...)))
  args <- sapply(args, as.character)
  
  if(length(args)==1){
    
    if(is.null(cutpoint)){
      lvls = c('No','Yes')
    } else {
      lvls = paste(c('<=','>'), cutpoint)
    }
   
  } else if (length(args) > 1){
    lvls = args[2:length(args)]
  } else{
    stop('Please specify levels')
  }
  
  structure(
    list(label = args[1],
         lvls = lvls,
         descr = descr,
         unit = unit,
         abbreviation=abrv,
         caption_lvls=caption_lvls),
    class = ifelse(is.null(unit),'cat_out','cut_out')
  )
}

gen_abrv <- function(meta_obj, sep = ' = '){
  purrr::map(meta_obj, .f= ~ if(!is.null(.$abbreviation)){
    if(grepl('=', .$abbreviation)){
      paste(.$abbreviation)
    } else {
      paste(.$label, .$abbreviation, sep = sep)
    }
  } 
    ) %>% 
    reduce(c) %>% unique() %>%
    paste(collapse='; ')
}



edit_pval <- function(pval){
  
  fr <- function(x) format(round(x,3),nsmall=3)
  require(purrr)
  
  map_chr(pval, function(x){
    if(x < 0.001){
      paste('< 0.001')
    } else if(x < 0.01){
      fr(x)
    } else if(x < 0.05){
      fr(x)
    } else {
      fr(x)
    }
  })
  

}

indent <- function(x) paste('   ', x)

gen_rowname.cat_var <- function(x){
  
  if(length(x$lvls)>2){
    paste(x$label)
  } else {
    paste(x$label)
  }
  
}

gen_rowname.cnt_var <- function(x){
  
  if(is.null(x$group)){
    paste(x$label,x$unit,sep=', ') 
  } else{
    indent(x$label)
  }
  

}

gen_rowname.cut_var <- function(x){
  
  if(length(x$lvls)>2){
    paste(x$label, x$unit, sep=', ')
  } else {
    paste(x$label, x$lvls[2], x$unit)
  }
  
}

gen_rowname.cut_out <- function(x){
  paste(x$label,x$unit,sep=', ')
}

gen_rowname.cat_out <- function(x){
  paste(x$label)
}

gen_rowname <- function(x){
  
  UseMethod('gen_rowname')
  
}

process_data <- function(meta_data){
  
  dat=meta_data$data
  meta=meta_data$main
  
  fctr_classes = c(
    paste(c('cut','cat'),'out',sep='_'),
    paste(c('cut','cat'),'var',sep='_'))
  
  fctrs <- map_lgl(meta, .f=~class(.)%in%fctr_classes)
  
  for(i in names(meta)){
    if(fctrs[i]) dat[[i]] %<>% factor(labels=meta[[i]]$lvls)
  } 
  
  dat
  
}


make_tbl_one <- function(meta_data, row_vars, col_vars,
                         cmp_pval=FALSE, include.freq=FALSE,
                         caption=NULL){
  
  require(tidyverse, quietly = TRUE)
  
  data = meta_data %>% process_data()
  X = meta_data$main[row_vars] %>% set_names(row_vars)
  Y = meta_data$main[col_vars] %>% set_names(col_vars)
  
  for(i in names(Y)){
    t = table(data[[i]])
    
    if(is.null(Y[[i]]$unit)){
      Y[[i]]$colnames = paste0(Y[[i]]$lvls, '<br/>(n = ', t, ')')  
    } else {
      Y[[i]]$colnames = paste0(Y[[i]]$lvls,' ', Y[[i]]$unit,
                               '<br/>(n = ', t, ')')
    }

  }
  
  tab_colperc <- function(var, cmp_pval=FALSE){
    
    var_meta = X[[var]]
    t = table(data[[var]], data[[outcome]])
    p = format(round(100*prop.table(t, margin = 2),1),nsmall=1) 
    
    
    if(nrow(t)==2){
      
      t2=t[2,] %>% trimws(); p2=p[2,] %>% trimws()
      
      if(include.freq){
        vals <- paste0(t2, ' (',p2, '%)')
      } else {
        vals <- paste0(p2,'%')
      }
      
      res = matrix(vals, ncol = length(t2)) %>% 
        set_colnames(outcome_meta$colnames) %>%
        set_rownames(gen_rowname(x=X[[var]]))
      
      if(cmp_pval){
        chi_pval = chisq.test(t)$p.value %>% edit_pval()
        chi_pvec = c(chi_pval)
      }
      
    } else {
      
      if(include.freq){
        vals <- paste0(t, ' (', p, '%)')
      } else {
        vals <- paste0(p,'%')
      }
      
      res = c("") %>% 
        rbind(matrix(vals, ncol = ncol(t))) %>%
        set_colnames(cnames) %>%
        set_rownames(c(gen_rowname(x=X[[var]]), 
                       indent(paste0(var_meta$lvls))))
      
      if(cmp_pval){
        chi_pval = chisq.test(t)$p.value %>% edit_pval()
        chi_pvec = c(chi_pval, rep("",nrow(t)))
      }
      
    }
    
    if(cmp_pval){
      cbind(res, 'P Value' = chi_pvec)
    } else {
      res
    }
    
  }
  
  tab_meansd <- function(var, cmp_pval){
    
    mean_sd <- function(var){
      
      mu = mean(var, na.rm=TRUE)
      x_abs <- abs(mu)
      
      if(x_abs < 10) {
        dig = 2
      } else if(x_abs < 1000){
        dig = 1
      } else if(x_abs > 1000){
        dig = 0
      } else {
        dig = 0
      }
      
      mu %<>% round(dig) %>% format(nsmall=dig)
      sd = sd(var, na.rm=TRUE)%>%
        round(dig) %>% format(nsmall=dig)
      
      res = paste0(mu, ' (', sd,')')
      
      res
      
    }
    
    var_meta = X[[var]]
    tab_vals <- tapply(data[[var]], data[[outcome]], mean_sd)
    
    if(cmp_pval){
      
      pval = t.test(data[[var]]~data[[outcome]])$p.value %>% edit_pval()
      
      cbind(t(tab_vals), p = pval) %>%
        set_rownames(gen_rowname(X[[var]])) %>%
        set_colnames(c(cnames, 'P Value'))
      
    } else {
      
      t(tab_vals)%>%
        set_rownames(gen_rowname(X[[var]])) %>%
        set_colnames(cnames)
      
    }
    
    
  }
  
  grp_vars= map(X,.f=~if(!is.null(.$group))paste(.$group))
  not_grp = map_lgl(grp_vars,~is.null(.)) 
  
  if(sum(not_grp)<length(X)){
    
    grp_vars = grp_vars[-which(not_grp)]
    tmp = names(grp_vars) %>% set_names(grp_vars)
    grp_vars = tmp
    
    is.first <- function(x){
      
      first=c(TRUE,rep(FALSE,length(x)-1))
      
      for(i in 2:length(x)){
        if(names(x)[i]!=names(x)[i-1]) first[i]=TRUE
      }
      
      first
      
    }
    
    grp_vars = grp_vars[is.first(grp_vars)]
    
  }
  
  
  cnt_vars <- sapply(data, is.numeric) %>% 
    which() %>% names() %>% 
    setdiff(names(Y)) %>%
    intersect(names(X))
  
  cat_vars <- sapply(data, function(x){
    is.factor(x) | is.character(x) }) %>% 
    which() %>% names() %>% 
    setdiff(names(Y)) %>%
    intersect(names(X))
  
  n_str <- ifelse(include.freq, 'count (%)', 'percentage')
  m_str <- 'mean (SD)'
  
  c1=length(cat_vars)>0
  c2=length(cnt_vars)>0
  
  if(c1&c2){
    notes=paste0('Table values are ', m_str, ' or ', n_str,'.')
  } else if(c1&!c2){
    notes = paste0('Table values are ', n_str,'.')
  } else if(!c1&c2){
    notes = paste0('Table values are ', m_str,'.')
  }
  
  notes %<>% c(map_chr(Y, ~ paste0(.$label,': ', .$descr)))
  
  output <- list()
  
  for(i in seq_along(Y)){
    
    outcome = names(Y)[i] 
    outcome_meta = Y[[outcome]]
    cnames = outcome_meta$colnames
    
    out <- list()
    
    for(j in seq_along(X)){
      
      x = names(X)[j]
      
      use.footnote <- !is.null(X[[j]]$footnote)
      
      if(x %in% setdiff(cnt_vars,grp_vars)){
        out[[j]]=tab_meansd(x, cmp_pval = cmp_pval)
      } else if(x %in% grp_vars) {
        ncol = ifelse(cmp_pval, length(cnames)+1, length(cnames))
        above = matrix("", nrow=1,ncol=ncol)
        rownames(above) = paste(X[[x]]$group,X[[x]]$unit,sep=', ')
        out[[j]] = rbind(above,tab_meansd(x, cmp_pval = cmp_pval))
        
      } else if(x %in% cat_vars){
        out[[j]] = tab_colperc(x, cmp_pval = cmp_pval)
        
      }
      
      if(use.footnote){
        rownames(out[[j]])[1] %<>% paste0('[note]')
      }
      
    }
    
    output[[i]] = do.call(rbind, out)
    
  }
  
  xnotes = map(X,~.$footnote) %>% reduce(c)
  
  if(!is.null(xnotes)){
    notes %<>% c(xnotes %>% paste(collapse='; '))
  }

  strat = map(Y, function(x){
    if(!is.null(x$abbreviation)){
      abrv = x$abbreviation %>% stringr::str_split('=') %>% 
        unlist() %>% trimws()
      gsub(pattern=abrv[1],replacement = abrv[2], x=x$label) %>% tolower()
    } else {
      x$label %>% tolower()
    }}) %>% reduce(c) %>% list_elements()
  
  if(is.null(caption)){
    cap = paste0('Participant characteristics[note] stratified by ', strat,'.')
  } else {
    cap = caption
  }

  structure(
    list(
      data = data,
      X=X, Y=Y,pval=cmp_pval,
      caption=cap,
      notes=notes,
      table = do.call(cbind, output)),
    class = 'Table_One'
  )
  
}

print.Table_One <- function(tbl){
  print(tbl$caption)
  print(tbl$table)
  print(tbl$notes)
}

#TODO place footnote symbol next to rownames of abbreviated variables

make_kbl_one <- function(tbl_one){
  
  require(kableExtra)

  X = tbl_one$X
  Y = tbl_one$Y
  dat = tbl_one$data
  notes=tbl_one$notes

  
  if(any(map_lgl(X, .f=~!is.null(.$abbreviation)))){
    notes %<>% c(paste0('Abbreviations: ',
                        gen_abrv(X),'; ',
                        gen_abrv(Y)))
  }
  
  headers = c(1, map_int(Y, ~length(.$lvls))) %>% 
    set_names(c(" ", map_chr(Y, ~ paste0(.$label, '[note]'))))
  if(tbl_one$pval) headers[2:length(headers)] %<>% add(1)
  
  indents = grepl("    ",rownames(tbl_one$table))
  
  suppressWarnings(
    knitr::kable(
      tbl_one$table, 
      format='html', 
      align=rep('c', ncol(tbl_one$table)), 
      caption = tbl_one$caption, 
      booktabs = TRUE,
      escape = FALSE)  %>%
        kable_styling(bootstrap_options = c("striped", "hover"),
                      full_width = TRUE)  %>% 
        column_spec(1, width = "6cm") %>% 
        add_indent(which(indents)) %>% 
        add_header_above(headers) %>%
        add_footnote(notes, notation = 'symbol', threeparttable = TRUE)
    )
  
}

