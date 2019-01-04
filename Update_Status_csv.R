#build data
library(tidyverse)
library(knitr)
library(rmarkdown)

today <- Sys.Date()
time_ago <- today-120

setwd('S:/Relationship Management/Command Center/GGN Reports/GGN Total Time By Status Report')

finf <- file.info(list.files(getwd(),pattern ="GGN_Time_By"), extNJ_cols = FALSE)
finf$date <- as.Date(finf$mtime, tz = "EST")
finf$filename <- row.names(finf)
finf$fileType <- str_sub(finf$filename, -3, -1)
finf1 <- finf %>%  group_by(date) %>% filter(fileType == "csv") %>% ungroup()
finf2 <- finf1 %>%  group_by(date) %>% filter(date >= time_ago & fileType == "csv" & mtime == max(mtime)) %>% ungroup()
finf2 <- as.data.frame(finf2)
row.names(finf2) <- finf2$filename
file_list <- row.names(finf2)
Filepath <- paste0('S:/Relationship Management/Command Center/GGN Reports/GGN Total Time By Status Report/',file_list)
filnum <-1

for(file in Filepath){
  today_status <- read.csv(file)
  today_status <- today_status
  today_status <- today_status %>% select(1,3,5,6)
  dts <- rep(as.Date(substr(file,87,94),'%m%d%Y')-1,length(today_status$User.ID))
  today_status$dt <- dts
  
  if(filnum==1){status_dataset <- today_status}
  if(filnum!=1){status_dataset <- rbind(status_dataset,today_status)}
  filnum<-filnum+1
}

setwd('U:/')

colnames(status_dataset)<-c('LOGIN_ID','time','STATUS','TIME_IN_STATUS','report_date')

write.csv(status_dataset,'status_e2e.csv',row.names=FALSE)
