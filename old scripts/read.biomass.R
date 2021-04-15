# This script open the most recent biomass data from the dropbox folder,
# confirm the labels correspond to the functional groups and other valid categories,
# estimate total mass, valid mass (mass in pre-defined categories), and,
# if cover data is available, estimate the difference between graminoids proportion 
# measured using cover and biomass. This helps to highlight potential data problems.

# bm0: contains the original data
# bm: contains the data transformed into wide format

# I recommend sourcing this file after sourcing read.cover.R
require(data.table)
if (!exists("dropboxdir")) dropboxdir <- "~/Dropbox/NutNet data/"

if (!exists("fileName_nutnet")) {
  fileName_nutnet <- function(filePattern, dir=".", extension=".csv") {
    fN <- dir(dir, pattern=paste0("^",filePattern,".*",extension,"$"))
    fN <- fN[which.max(as.POSIXct(gsub(paste0("(",filePattern,"|",extension,")"),"", fN), format="%d-%B-%Y"))]
    return(fN)
  }
}

## fileName_nutnet("full-biomass-", dir=dropboxdir)
bm0 <- read.csv(paste0(dropboxdir, fileName_nutnet("full-biomass-", dir=dropboxdir)),stringsAsFactors = FALSE)
setDT(bm0)
# Fixing Koffler subplot issue
bm0[site_code=="koffler.ca" & plot==19,subplot:="A"]

# Known issues of biomass categories
validLitterClasses <- c("STANDING DEAD","LITTER","DOWNED WOODY DEBRIS")
notValidBMClasses <- c("FUNGUS","LIVE","TOTAL",
                       "LICHEN","VASCULAR","NON-VASCULAR",
                       "ANNUAL","PERENNIAL")
message("Confirm valid alive classes:\n - ", 
        paste0( bm0[,.(category=sort(unique(category)))][
                    !(category %in% c(notValidBMClasses,validLitterClasses)),category],
                collapse="\n - "))
# After confirming the classes... go ahead!
bm <- bm0[,.(all_mass=.SD[,sum(mass,na.rm=TRUE)],
            live_mass=.SD[live=="1" & !(category %in% notValidBMClasses),
                          sum(mass,na.rm=TRUE)], 
            forb_mass=.SD[category %like% "FORB",sum(mass,na.rm=TRUE)],
            litter_mass=.SD[category %in% validLitterClasses,sum(mass,na.rm=TRUE)],
            gram_mass=.SD[category=="GRAMINOID",sum(mass,na.rm=TRUE)],
            legu_mass=.SD[category=="LEGUME",sum(mass,na.rm=TRUE)]), 
          by=.(site_code,block,plot,subplot,year,year_trt,trt)]

# Some sites in some years did not record forbs and graminoids, just total biomass.
# If live_mass == 0 gram_mass + forb_mass should be zero. Using the total of the site
# really unlikely that graminoids and forbs are zero in every single plot!
 
bm[bm[,by=.(site_code,year_trt),
      .(crit=xor(sum(forb_mass) + sum(gram_mass) == 0, sum(live_mass) == 0))][crit==TRUE], 
   on=c("site_code","year_trt"), 
        c("forb_mass", "gram_mass", "legu_mass") := NA]

# Estimate proportions
bm[,`:=`(valid_prop=(live_mass + litter_mass)/all_mass, 
         gram_prop=gram_mass/live_mass,
         legu_prop=legu_mass/live_mass,
         forb_prop=forb_mass/live_mass)]

if (exists("cover")) {
  # biomass classification issues: measured as the combination of site and year
  BM_Filter <- bm[,lapply(.SD,mean,na.rm=TRUE),by=.(site_code,year_trt),.SDcols=live_mass:gram_prop][
    cover[,.(coverRatio=sum(.SD[Family=="Poaceae" | Family=="Cyperaceae" | local_lifeform2=="GRAMINOID",
                                max_cover])/sum(.SD[,max_cover])),by=.(site_code,year_trt)],on=c("site_code","year_trt")][
                                  ,.(site_code,year_trt,gram_prop,coverRatio,DIFF=abs(gram_prop-coverRatio))]
  if (FALSE) { 
    View(BM_Filter)
    ggplot(data=BM_Filter[,.(site_code=ifelse(site_code %like% "marc.ar|pape.de",site_code,
                                              ifelse(gram_prop==0,"no grass bm","others")),DIFF)],
           aes(x=DIFF,color=site_code,fill=site_code)) + geom_histogram(alpha=0.6)
    ggsave("figures/biomass-filter.pdf",width=5,height=4)
  }
  # filtering suspicious cases: difference larger than 50% and graminoids biomass=0
  unique(bm[BM_Filter[DIFF >= 0.5 & gram_prop != 0,.(site_code,year_trt)],on=c("site_code","year_trt")][,.(site_code)])
}
