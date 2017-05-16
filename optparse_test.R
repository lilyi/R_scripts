library(optparse)
option_list <- list(
  make_option(c("-s", "--stime"), type="character", default="2017-04-30",
              help="start time as [default= %default]", metavar="character", action = "store"),
  make_option(c("-e", "--etime"), type="character", default="2017-05-06",
              help="end time as [default= %default]", metavar="character", action = "store"),
  make_option(c("-t", "--tit"), type="character", default="0430-0506",
              help="month as [default= %default]", metavar="character", action = "store"),
  make_option(c("-a", "--astime"), type="character", default="2017-05-07",
              help="second start time as [default= %default]", metavar="character", action = "store"),
  make_option(c("-b", "--betime"), type="character", default="2017-05-13",
              help="second end time as [default= %default]", metavar="character", action = "store"),
  make_option(c("-c", "--ctit"), type="character", default="0507-0513",
              help="second month as [default= %default]", metavar="character", action = "store")
)
print("H1")
opt_parser <- OptionParser(option_list=option_list)

opt <- parse_args(opt_parser)
print("H2")

stime1 <- opt$stime1
etime1 <- opt$etime1
tit1 <- opt$tit1
stime2 <- opt$cstime
etime2 <- opt$cetime
tit2 <- opt$ctit
print(opt)