source("R/pkgTest.R")
pkgTest("tidyverse")

deltaCt <- function(df, controlTarget) {
  control.df <- df %>% filter(Target == controlTarget) %>% mutate(Ct.control = Ct)
  
  df %>% 
    inner_join(select(control.df, Biogroup, Sample, Ct.control),  by = c("Biogroup","Sample")) %>%
    mutate(deltaCt = Ct - Ct.control)
}

deltaDeltaCt <- function(df, controlBiogroup) {
  normFactor <- df %>% group_by(Target) %>% 
    filter(Biogroup == controlBiogroup) %>% 
    summarize(deltaCt.normFactor = mean(deltaCt)) 
  
  df %>% 
    inner_join(normFactor,  by = "Target") %>%
    mutate(deltaDeltaCt = deltaCt - deltaCt.normFactor)
}

calcRQ <- function(df) df %>% mutate(RQ = 2^-deltaDeltaCt)
