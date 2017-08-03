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
stime <- "2017-04-01"
etime <- "2017-07-31"

query.list <- Init(table.id = "ga:3035421", start.date = stime,
                   end.date = etime,
                   metrics="ga:sessions",
                   dimensions = "ga:pagePath",
                   sort = "-ga:sessions",
                   max.results = 10000,
                   filters = "ga:pagePath=~/solution/")
ga.query <- QueryBuilder(query.list)
gaData <- GetReportData(ga.query, token)
# B <- gaData[2,1]
lanFilt <- as.data.frame(gaData[grepl("/zh-tw/", gaData[,1]),])
getTitle <- function(data, lan){
  
  pageName <- substr(data,start=regexpr("/solution/", data)+10,stop=regexpr(lan, data)-1) 
  return(pageName)
}
# apply(lanFilt[,1], 1, function(x) getTitle(x[1], "zh-tw"))
titles <- lapply(lanFilt[,1], getTitle, "/zh-tw/")

######here


B <- lanFilt[2,1]
pageName <- substr(B,start=regexpr("/solution/", B)+10,stop=regexpr("/zh-tw/", B)-1) 
apply(lanFilt[, 1], 1, function(x) substr(x[1], x[2], x[3]))

lan_list <- c("/en/","/en-us/","/en-uk/","/en-au/","/en-in/","/de-de/","/es-es/","/es-mx/","/fr-fr/","/it-it/","/nl-nl/","/sv-se/","/zh-tw/","/zh-hk/","/pt-pt/","/pl-pl/","/ja-jp/","/ko-k/r","/cs-cz/","/th-th/")

# lan_list2 <- c("en","en_us","en_uk","en_au","en_in","de_de","es_es","es_mx","fr_fr","it_it","nl_nl","sv_se","zh_tw","zh_hk","pt_pt","pl_pl","ja_jp","ko_kr","cs_cz", "th_th")

# solution <- function(lan){
#   query.list <- Init(table.id = "ga:3035421", start.date = stime,
#                      end.date = etime,
#                      metrics="ga:sessions",
#                      dimensions = "ga:pagePath",
#                      sort = "-ga:sessions",
#                      # max.results = 10000,
#                      filters = "ga:pagePath=~/solution/")
#   ga.query <- QueryBuilder(query.list)
#   gaData <- GetReportData(ga.query, token)
#   # SUM <- sum(gaData$sessions)
#   return(gaData[1:5,])
# }

result <- as.data.frame(lapply(lan_list, solution))
cre_col1 <- lapply(lan_list2, paste, 'sessions', sep="_")
cre_col2 <- lapply(lan_list2, paste, 'page', sep="_")
colu <- mapply(append, cre_col2, cre_col1, SIMPLIFY=TRUE)
colnames(result) <- colu
write.csv(result, paste("solution_", tit, ".csv", sep="")) 

#####
data <- read.csv("C:/Users/Lily/Documents/GA/R/report/2017/solutions/solution_Q2_2017_title_only.csv", header = FALSE)
library(plyr)

uniqueData <- ddply(data,"V1",numcolwise(sum))
top10 <- uniqueData[with(uniqueData, order(-V2)),][1:10,]
write.csv(top10, paste("C:/Users/Lily/Documents/GA/R/report/2017/solutions/top10_solution_Q2.csv", sep="")) 

