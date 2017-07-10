library(ggplot2)
library(scales)

makeFolder <- function(fname){
  dir("C:\\Users\\Lily\\Documents\\GA\\R\\report\\2017\\bi-weekly\\trend")
  e <- dir()
  if(fname %in% e){
    print(paste("Folder", fname, "exits.", sep=" "))
  }else{
    # make directory
    mdir <- "mkdir C:\\Users\\Lily\\Documents\\GA\\R\\report\\2017\\bi-weekly\\trend\\"
    f_list <- unlist(lapply(mdir, paste, fname, sep=""))
    lapply(f_list, shell)
    print(paste("Folder", fname, "created.", sep=" "))
  }
}

#每月圖表 function final 適用於五月(含)之後的觀察
monthlyTrend <- function(cname, standardMonth, SDmonth, mon){
  setwd("C:/Users/Lily/Documents/GA/R/report/2017/bi-weekly/trend/")
# 
#   if(abs(as.numeric(total)) > 6){
#     standardMonth <- SDmonth
#   }else{
#     standardMonth <- standardMonth
#   }
  standard_data <- read.csv(paste("C:/Users/Lily/Documents/GA/R/report/2017/Monthly/", standardMonth, "/total_dat/", cname, '.csv', sep=""), header=T)
  standard_data$day <- format(as.Date(as.character(standard_data$date), format="%Y%m%d"), "%w")
  len <- length(standard_data$sessions)-6
  CMA=numeric(len)  
  for(i in 1:len)
  { 
    CMA[i]=(standard_data[i,3]+standard_data[i+1,3]+standard_data[i+2,3]+standard_data[i+3,3]+standard_data[i+4,3]+standard_data[i+5,3]+standard_data[i+6,3])/7
  }
  standard_data$CMA <- c(rep(NA,3), CMA, rep(NA,3))
  std <- round(sd(CMA),4)
  mean <- round(mean(CMA),2)
  upper <- round((mean + 2*std),2)
  lower <- round((mean - 2*std),2)
  M_data <- read.csv(paste("C:/Users/Lily/Documents/GA/R/report/2017/bi-weekly/", mon, "/total_dat/", cname, '.csv', sep=""), header=T)
  M_data$day <- format(as.Date(as.character(M_data$date), format="%Y%m%d"), "%w")
  # M_data$day <- match(M_data$day[], day_trans[,2])
  M_len <- length(M_data$sessions)-6
  M_CMA=numeric(M_len)  
  for(i in 1:M_len)
  { 
    M_CMA[i]=(M_data[i,3]+M_data[i+1,3]+M_data[i+2,3]+M_data[i+3,3]+M_data[i+4,3]+M_data[i+5,3]+M_data[i+6,3])/7
  }
  M_data$CMA <- c(rep(NA,3), M_CMA, rep(NA,3))
  M_data$date <- as.Date(as.character(M_data$date), format="%Y%m%d")
  p <- ggplot(M_data, aes(x1=sessions, x2=CMA))+#(M_data, aes(x=M_data$date, y=M_data$CMA))+#, linetype=country)) + 
    geom_line(aes(x=M_data$date, y=M_data$sessions, colour="sessions"), M_data)+
    geom_line(aes(x=M_data$date, y=M_data$CMA, colour="CMA"), M_data)+
    geom_hline(yintercept=mean, color="#0099FF")+
    geom_hline(yintercept=upper, color="#3399FF", linetype="dashed")+ #linetype="dashed"
    geom_hline(yintercept=lower, color="#3399FF", linetype="dashed")+
    scale_x_date(labels = date_format("%m%d"))+
    labs(title = paste(cname, "_", standardMonth)
         , x = "date"
         , y = "sessions")+
    scale_colour_manual("legend", values = c("#CC66FF", "#333333")) 
  ggsave(paste(mon, "/", cname, "_", standardMonth, ".png", sep=""),p)
  ###
  fromMonNumber <- format(M_data$date, "%m")[1]
  
  
  # from_mon <- M_data[format(M_data$date, "%m")==fromMonNumber,5]
  # # print(from_mon)
  M_data_CMA <- M_data$CMA[!is.na(M_data$CMA)]
  # # print(from_mon)
  U_count <- sum(M_data_CMA > upper)
  L_count <- -sum(M_data_CMA < lower)
  total <- U_count + L_count
  if(total > 3){
    U_alert <- "+"
    L_alert <- ""
  }else if(total < -3){
    U_alert <- ""
    L_alert <- "-"
  }else{
    U_alert <- ""
    L_alert <- ""
  }
  
  write.csv(M_data, paste(mon, "/", cname, ".csv", sep=""))
  return(c(mean, std, upper,lower, U_count, L_count, total, U_alert, L_alert, fromMonNumber, standardMonth))
}

# statOutput <- function(SDmonth, OBmonth){
#   setwd("C:/Users/Lily/Documents/GA/R/report/2017/bi-weekly/trend/")
#   clist <- list("Australia","Austria","Belgium","Canada","Czechia","Denmark","France","Germany","Greece","Hong Kong","Hungary","India","Iran","Israel","Italy","Japan","Mexico","Netherlands","Norway","Poland","Portugal","Romania","South Africa","South Korea","Spain","Sweden","Switzerland","Taiwan","Thailand","Turkey","United Kingdom","United States")
#   report <- read.csv(paste(SDmonth, "/", SDmonth, ".csv", sep=""), header=T)
#   
#   A <- apply(report[, c(1,12,8)], 1, function(x) monthlyTrend(x[1], x[2], x[3], SDmonth, OBmonth))
#   colnames(A) <- clist
#   B <- t(A)
#   colnames(B) <- c("mean", "std", "upper", "lower", "U_count", "L_count", "total", "U_alert", "L_alert", "lastMonNumber", "standardMonth")
#   C <- as.data.frame(B)
#   lan_list<- c("en-au", "", "", "", "cs-cz", "", "fr-fr", "de-de", "", "zh-hk", "", "en-in", "", "", "it-it", "ja-jp", "es-mx", "nl-nl", "", "pl-pl", "pt-pt", "", "", "ko-kr", "es-es", "sv-se", "", "zh-tw", "th-th", "", "en-uk", "en-us")
#   C$lan <- lan_list
#   as.data.frame(C)
#   write.csv(C, paste(OBmonth, "/", OBmonth, ".csv", sep=""))
#   return(C)
# }

# statOutput("May", "0529-0609")
biweekDate <- "0529-0609"
makeFolder(biweekDate)
clist <- list("Australia","Austria","Belgium","Canada","Czechia","Denmark","France","Germany","Greece","Hong Kong","Hungary","India","Iran","Israel","Italy","Japan","Mexico","Netherlands","Norway","Poland","Portugal","Romania","South Africa","South Korea","Spain","Sweden","Switzerland","Taiwan","Thailand","Turkey","United Kingdom","United States")
A <- as.data.frame(lapply(clist, monthlyTrend, "April", "April", biweekDate))
colnames(A) <- clist
B <- t(A)
colnames(B) <- c("mean", "std", "upper", "lower", "U_count", "L_count", "total", "U_alert", "L_alert", "fromMonNumber", "standardMonth")
C <- as.data.frame(B)
lan_list<- c("en-au", "", "", "", "cs-cz", "", "fr-fr", "de-de", "", "zh-hk", "", "en-in", "", "", "it-it", "ja-jp", "es-mx", "nl-nl", "", "pl-pl", "pt-pt", "", "", "ko-kr", "es-es", "sv-se", "", "zh-tw", "th-th", "", "en-uk", "en-us")
C$lan <- lan_list
as.data.frame(C)
write.csv(C, paste(biweekDate, "/", biweekDate, ".csv", sep=""))
# monthlyTrend <- function(cname, standardMonth, total, SDmonth, mon)