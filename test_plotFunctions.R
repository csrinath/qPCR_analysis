source("R/readExcel.R")
source("R/qPCR_calc.R")
source("R/plotFunctions.R")


# File 1 has Ct and RQ values (based on GAPDH, condition4)
#files <- c("data1.xls")
files <- c("2018-05-17 - AHCF Reprogramming Expt 7 - Plate 1.xls",
           "2018-05-17 - AHCF Reprogramming Expt 7 - Plate 2.xls",
           "2018-05-17 - AHCF Reprogramming Expt 7 - Plate 3.xls")

experiment <- "20180517_exp7"

dir.create(file.path("output", experiment), showWarnings = F)

df <- files %>% map(makeFilePath) %>% 
  map(qPCR.file.reader) %>% 
  map(missingVal) %>%
  bind_rows() %>%
  deltaCt("GAPDH") %>%
  deltaDeltaCt("GFP") %>%
  calcRQ()

df.mean <- summarizeData(df, RQ)

df.mean %>% 
  makeAllGenePlots(experiment) %>% 
  makeMultiGenePlot(experiment, nCol=4)
 