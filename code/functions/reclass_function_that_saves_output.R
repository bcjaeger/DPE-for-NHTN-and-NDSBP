reclassification<-function (data, cOutcome, predrisk1, predrisk2, cutoff){
  c1 <- cut(predrisk1, breaks = cutoff, include.lowest = TRUE, 
    right = FALSE)
  c2 <- cut(predrisk2, breaks = cutoff, include.lowest = TRUE, 
    right = FALSE)
  tabReclas <- table(`Initial Model` = c1, `Updated Model` = c2)
  ta <- table(c1, c2, data[, cOutcome])
  TabAbs <- ta[, , 1]
  tab1 <- cbind(TabAbs, ` % reclassified` = round((rowSums(TabAbs) - 
      diag(TabAbs))/rowSums(TabAbs), 2) * 100)
  names(dimnames(tab1)) <- c("Initial Model", "Updated Model")
  TabPre <- ta[, , 2]
  tab2 <- cbind(TabPre, ` % reclassified` = round((rowSums(TabPre) - 
      diag(TabPre))/rowSums(TabPre), 2) * 100)
  names(dimnames(tab2)) <- c("Initial Model", "Updated Model")
  Tab <- tabReclas
  tab <- cbind(Tab, ` % reclassified` = round((rowSums(Tab) - 
      diag(Tab))/rowSums(Tab), 2) * 100)
  names(dimnames(tab)) <- c("Initial Model", "Updated Model")
  c11 <- factor(c1, levels = levels(c1), labels = c(1:length(levels(c1))))
  c22 <- factor(c2, levels = levels(c2), labels = c(1:length(levels(c2))))
  x <- improveProb(x1 = as.numeric(c11) * (1/(length(levels(c11)))), 
    x2 = as.numeric(c22) * (1/(length(levels(c22)))), y = data[, 
      cOutcome])
  y <- improveProb(x1 = predrisk1, x2 = predrisk2, y = data[, 
    cOutcome])
  return(list(catg_NRI=x,cntn_NRI=y))
}
