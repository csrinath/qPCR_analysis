source("R/readExcel.R")
source("R/qPCR_calc.R")

# File 1 has Ct and RQ values (based on GAPDH, condition4)
file <- "data1.xls"
df <- file %>% makeFilePath() %>% 
  qPCR.file.reader() %>% 
  missingVal(Ct,40) %>%
  deltaCt("GAPDH") %>%
  deltaDeltaCt("Condition4") %>%
  calcRQ()

# Check whether RQ and RQ.flex are the same
plot(log(df$RQ.flex), log(df$RQ))

df.mean <- summarizeData(df, RQ)
df.mean.flex <- df %>% normalizeData(RQ.flex, "Condition4") %>% summarizeData(RQ.flex.normalized)


