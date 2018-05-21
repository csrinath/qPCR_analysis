source("R/readExcel.R")
source("R/qPCR_calc.R")

# File 1 has Ct and RQ values (based on GAPDH, condition4)
file <- "data1.xls"
df <- file %>% makeFilePath() %>% 
  qPCR.file.reader() %>% 
  deltaCt("GAPDH") %>%
  deltaDeltaCt("Condition4") %>%
  calcRQ()

plot(df$RQ.flex, df$RQ)

df.mean <- summarizeData(df, RQ)
df.mean.flex <- summarizeData(df, RQ.flex)

