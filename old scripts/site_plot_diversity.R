# script to import subplot-species matrix using direct connection to MySQL
# to calculate diversity statistics at plot level
# by default takes all years of observation/experiment

library(vegan)
library(plyr)
require(MASS)
require(labdsv)
require(RMySQL)
getwd()
rm(list=ls())

#########################################################
#########################################################
# Must have established ssh connection to db to proceed #
#########################################################
#########################################################

# create connection to SQL
m <- dbDriver("MySQL") 	# tells R to use MySQL driver
con <- dbConnect(m)	# define connection
					# this references .my.cnf

# pass query & create dataframe
# pass query and hold results
#rs <- dbSendQuery(con, "select * from pi where PI_name like '%Harpole'")
#df <- fetch(rs)		# create a dataframe from query
#df				# results!

# pass query to MySQL db

rs <- dbSendQuery(con,"call plot_sp_matrix_site('cdcr.us')")
# create dataframe from results
plot.sp.mat <- fetch(rs, n=-1)
names(plot.sp.mat)[1:20]

dim(plot.sp.mat)

# separate identifier data from taxa columns
names(plot.sp.mat)[1:10]
# position 5 is first taxon
tax.first <- 5
tax.last <- ncol(plot.sp.mat)

# make a separate data frame to hold identifiers and summary variables
plot.div <- data.frame(site=plot.sp.mat$site_code,				year=plot.sp.mat$year,
				block=plot.sp.mat$block,
				plot=plot.sp.mat$plot)
# richness is just summary of >0 observations
plot.div$rich.vegan <- specnumber(plot.sp.mat[,tax.first:tax.last])
# shannon diversity is sum(p[i]*ln(p[i])) where p[i] is proportion of total abundance
plot.div$shan <- diversity(plot.sp.mat[,tax.first:tax.last])
plot.div$simpson <- diversity(plot.sp.mat[,tax.first:tax.last],index='invsimpson')
# Evenness metric is shannon diversity / log(S)
plot.div$even <- plot.div$shan/log(plot.div$rich)

which(is.na(plot.div$even))
is.na(plot.div$even) <- 0

# now have plot-year level metrics
#dim(subplot.div)
#head(subplot.div)
#setwd("/Users/eric/Documents/NutNet/data")
setwd("~/Dropbox/NutNet_data/data-requests/Harpole/")
write.csv(plot.div,'cdcr-plot_diversity_stats.csv')

# roll up to take means
# plot.mean.diversity <- ddply(subplot.div, .(site, block, plot, year), colwise(mean, .(rich.vegan, shan, even)))
block.mean.diversity <- ddply(plot.div, .(site, block, year), colwise(mean, .(rich.vegan, shan, even)))
site.mean.diversity <- ddply(plot.div, .(site, year), colwise(mean, .(rich.vegan, shan, even)))

# or merge with "comb_by_plot" to include in other analyses
head(plot.div)
plot.div.cdcr <- plot.div

## cdcr
con <- dbConnect(m)
rs <- dbSendQuery(con,"call plot_sp_matrix_site('cdcr.us')")
# create dataframe from results
plot.sp.mat <- fetch(rs, n=-1)
names(plot.sp.mat)[1:20]

dim(plot.sp.mat)

# separate identifier data from taxa columns
names(plot.sp.mat)[1:10]
# position 5 is first taxon
tax.first <- 5
tax.last <- ncol(plot.sp.mat)

# make a separate data frame to hold identifiers and summary variables
plot.div <- data.frame(site=plot.sp.mat$site_code,				year=plot.sp.mat$year,
				block=plot.sp.mat$block,
				plot=plot.sp.mat$plot)
# richness is just summary of >0 observations
plot.div$rich.vegan <- specnumber(plot.sp.mat[,tax.first:tax.last])
# shannon diversity is sum(p[i]*ln(p[i])) where p[i] is proportion of total abundance
plot.div$shan <- diversity(plot.sp.mat[,tax.first:tax.last])
plot.div$simpson <- diversity(plot.sp.mat[,tax.first:tax.last],index='invsimpson')
# Evenness metric is shannon diversity / log(S)
plot.div$even <- plot.div$shan/log(plot.div$rich)

which(is.na(plot.div$even))
is.na(plot.div$even) <- 0

# now have plot-year level metrics
#dim(subplot.div)
#head(subplot.div)
#setwd("/Users/eric/Documents/NutNet/data")
setwd("~/Dropbox/NutNet_data/data-requests/Harpole/")
write.csv(plot.div,'cdcr-plot_diversity_stats.csv')
plot.div.cdcr <- plot.div

plot.div <- rbind(plot.div.cbgb,plot.div.cdcr)
str(plot.div)

list.files()
comb <- read.csv('stan-other-trt-byplot-1Jul2013.csv')
str(comb)
comb <- comb[comb$site_code=='cdcr.us',]
comb <- merge(comb, plot.div[c("site","block","plot","year","rich.vegan","shan","simpson","even")], by.x=c("site_code","block","plot","cover_year"), by.y=c("site","block","plot","year"), all.x=TRUE)
dim(comb)	# should be same n rows, with three extra vars... 
#comb[which(is.na(comb$shan)),] # sgs messed up plots

# check richness values
#comb[comb$rich.x != comb$rich.y,c("site_name","block","plot","cover_year","INT_rich","NAT_rich","UNK_rich","rich","rich.vegan")]
#length(which(comb$rich.x != comb$rich.y))
#?Sys.time
date.string<-format(Sys.time(),"%d-%b-%Y")
write.table(comb, file = paste("cdcr-comb-by-plot-clim-soil-diversity",date.string,".csv", sep=""), sep=",", col.names=T,row.names=F,quote=F)
