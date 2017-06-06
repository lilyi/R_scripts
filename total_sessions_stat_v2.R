setwd("C:/Users/Lily/Documents/GA/R/report/2017/Monthly/Q1/total_dat/")
cname <- "Australia"
clist <- list("Australia","Austria","Belgium","Canada","Czechia","Denmark","France","Germany","Greece","Hong Kong","Hungary","India","Iran","Israel","Italy","Japan","Mexico","Netherlands","Norway","Poland","Portugal","Romania","South Africa","South Korea","Spain","Sweden","Switzerland","Taiwan","Thailand","Turkey","United Kingdom","United States")
stat <- function(cname){
  Q1_data <- read.csv(paste(cname, '.csv', sep=""), header=T)
  std <- round(sd(Q1_data$sessions),4)
  mean <- round(mean(Q1_data$sessions),2)
  upper <- round((mean + 2*std),2)
  lower <- round((mean - 2*std),2)
  bi_data <- read.csv(paste("C:/Users/Lily/Documents/GA/R/report/2017/bi-weekly/0515-0526/total_dat/", cname, ".csv", sep=""), header=T)
  bi_std <- round(sd(bi_data$sessions),4)
  bi_mean <- round(mean(bi_data$sessions),2)
  bi_U <- round((bi_mean + 2*bi_std),2)
  bi_L <- round((bi_mean - 2*bi_std),2)
  
  if(bi_U > upper){
    U_alert <- "+"
    L_alert <- ""
    if(bi_L < lower){
      L_alert <- "-"
    }
  }else if(bi_L < lower){
    L_alert <- "-"
    U_alert <- ""
    if(bi_U > upper){
      U_alert <- "+"
    }
  }else{
    U_alert <- ""
    L_alert <- ""
  }
  U_dif <- abs(bi_U-upper)
  L_dif <- abs(bi_L-lower)
  # maxi_bi <- max(bi_data$sessions)
  # mini_bi <- min(bi_data$sessions)
  # if(maxi_bi>upper){
  #   U_alert <- "+"
  #   L_alert <- ""
  #   if(mini_bi < lower){
  #     L_alert <- "-"
  #   }
  # }else if(mini_bi < lower){
  #   L_alert <- "-"
  #   U_alert <- ""
  #   if(maxi_bi > upper){
  #     U_alert <- "+"
  #   }
  # }else{
  #   U_alert <- ""
  #   L_alert <- ""
  # }
  # U_dif <- abs(maxi_bi-upper)
  # L_dif <- abs(mini_bi-lower)
  return(c(mean, std, upper,lower, bi_mean, bi_std, bi_U, bi_L, U_dif, L_dif, U_alert, L_alert))
}
stat(cname)
A <- as.data.frame(lapply(clist, stat))
colnames(A) <- clist
B <- t(A)
colnames(B) <- c("mean", "std", "upper", "lower", "bi_mean", "bi_std", "bi_U", "bi_L", "U_dif", "L_dif", "U_alert", "L_alert")
C <- as.data.frame(B)
lan_list<- c("en-au", "", "", "", "cs-cz", "", "fr-fr", "de-de", "", "zh-hk", "", "en-in", "", "", "it-it", "ja-jp", "es-mx", "nl-nl", "", "pl-pl", "pt-pt", "", "", "ko-kr", "es-es", "sv-se", "", "zh-tw", "th-th", "", "en-uk", "en-us")
C$lan <- lan_list
as.data.frame(C)
write.csv(C, 'C:/Users/Lily/Documents/GA/R/report/2017/bi-weekly/0515-0526/stat_lan_v2.csv')
