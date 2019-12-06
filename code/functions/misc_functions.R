

is.bin <- function(x) if(length(unique(na.omit(x)))==2) TRUE else FALSE

frexp = function(x) format(round(exp(x),2),nsmall=2)

fr = function(x) format(round(x,2),nsmall=2)

clean_na <- function(x) {x[is.na(x)]<-'--'; x}