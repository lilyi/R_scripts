require(devtools)
library(googleAuthR)
require(RGoogleAnalytics)
library(grid)
library(gridExtra)
library(optparse)
library(ggplot2)
options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/analytics"))
options("googleAuthR.client_id" = "908916142832-bf3o6rpn8phh344booolr1ovfla7ea9p.apps.googleusercontent.com")
options("googleAuthR.client_secret" = "MlS4oatMCIMqzI3bpvWMeH3W")
service_token <- gar_auth_service("C:/Users/Lily/Documents/GA/R/key_secrets.json", scope=getOption("googleAuthR.scopes.selected"))
client.id = "908916142832-bf3o6rpn8phh344booolr1ovfla7ea9p.apps.googleusercontent.com"
client.secret = "MlS4oatMCIMqzI3bpvWMeH3W"
token <- Auth(client.id,client.secret)
invisible(GetProfiles(token))

setwd('C:/Users/Lily/Documents/GA/R/report/2017/solutions')

option_list <- list(
  make_option(c("-s", "--stime"), type="character", default="2017-04-01", 
              help="start time as [default= %default]", metavar="character"),
  make_option(c("-e", "--etime"), type="character", default="2017-04-30", 
              help="end time as [default= %default]", metavar="character"),
  make_option(c("-t", "--tit"), type="character", default="April", 
              help="month as [default= %default]", metavar="character")
)

opt_parser <- OptionParser(option_list=option_list)
opt <- parse_args(opt_parser)
stime <- opt$stime
etime <- opt$etime
tit <- opt$tit
# stime <- "2017-04-01"
# etime <- "2017-04-30"

lan_list <- c("/en","/en-us","/en-uk","/en-au","/en-in","/de-de","/es-es","/es-mx","/fr-fr","/it-it","/nl-nl","/sv-se","/zh-tw","/zh-hk","/pt-pt","/pl-pl","/ja-jp","/ko-kr","/cs-cz","/th-th")
lan_list2 <- c("en","en_us","en_uk","en_au","en_in","de_de","es_es","es_mx","fr_fr","it_it","nl_nl","sv_se","zh_tw","zh_hk","pt_pt","pl_pl","ja_jp","ko_kr","cs_cz", "th_th")
lan_list3 <- c("/en/","/en-us/","/en-uk/","/en-au/","/en-in/","/de-de/","/es-es/","/es-mx/","/fr-fr/","/it-it/","/nl-nl/","/sv-se/","/zh-tw/","/zh-hk/","/pt-pt/","/pl-pl/","/ja-jp/","/ko-kr/","/cs-cz/","/th-th/")
lanList <- cbind(lan_list,lan_list3)
solution <- function(lan, lan2){
  query.list <- Init(table.id = "ga:3035421", start.date = stime,
                     end.date = etime,
                     metrics="ga:sessions",
                     dimensions = "ga:pagePath",
                     sort = "-ga:sessions",
                     # max.results = 10000,
                     filters = paste("ga:pagePath=~/solution", ";ga:pagePath=@", lan, sep=""))
  ga.query <- QueryBuilder(query.list)
  gaData <- GetReportData(ga.query, token)
  # SUM <- sum(gaData$sessions)
  pageName <- substr(gaData[,1],start=regexpr("/solution/", gaData[,1])+10,stop=regexpr(lan2, gaData[,1])-1) 
  
  return(c(gaData[1:5,], pageName[1:5]))
}
result <- as.data.frame(apply(lanList[, c(1,2)], 1, function(x) solution(x[1], x[2])))
# result <- as.data.frame(lapply(lan_list, solution))
cre_col1 <- lapply(lan_list2, paste, 'sessions', sep="_")
cre_col2 <- lapply(lan_list2, paste, 'page', sep="_")
colu <- mapply(append, cre_col2, cre_col1, SIMPLIFY=TRUE)
colnames(result) <- colu
tit <- "Q2_2017"
write.csv(result, paste("solution_", tit, ".csv", sep="")) 
