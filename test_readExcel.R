source("R/readExcel.R")

file1 = "data1.xls"

samples1 <- file1 %>% makeFilePath() %>% qPCR.sample.reader()
data1 <- file1 %>% makeFilePath() %>% qPCR.data.reader()
fullData1 <- file1 %>% makeFilePath() %>% qPCR.file.reader()

head(samples1)
head(data1)
head(fullData1)


