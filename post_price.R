
library(grid)
library(gridExtra)
library(ggplot2)
library(reshape2)
library(MCMCglmm)
library(tidyr)
library("yarrr")

library(priceTools)
library(readr)
library(raster)

library(tidyverse)
library(data.table)
library(foreach)

p <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

price.list<-list.files(path = "~/Desktop/Academic/R Code/NutNet/input_new/", pattern = ".rds$", recursive = TRUE, full.names = TRUE)
#price.list<-list.files(path = "~/Desktop/Academic/R Code/NutNet/input_new/", pattern = ".rds$")

View(price.list)


price.all <- foreach (file = price.list,.combine=rbind) %do% {
  price.file<-readRDS(file)
  price.file$data
}


write.csv(price.all,"~/Desktop/Academic/Data/NutNet/nutnet_price_combine_new.csv")

price.all <- read.csv("~/Desktop/Academic/Data/NutNet/nutnet_price_combine_new.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


View(price.all)
price.all2<-separate(price.all,trt_year,into=c("trt_year.x","trt_year.y"),sep = " ", remove=FALSE)
price.all3<-separate(price.all2,trt_year.y,into=c("trt.y","year.y"),sep = "_", remove=FALSE)
price.all4<-separate(price.all3,trt_year.x,into=c("trt.x","year.x"),sep = "_", remove=FALSE)
price.all5<-separate(price.all4,site.year.id,into=c("site.year.id.x","site.year.id.y"),sep = " ", remove=FALSE)

View(price.all5)
nrow(price.all5)
#88,982 rows

#price.reduced<-price.all5[price.all5$trt_year.x %in% c('Control_0','NPK_0'),]
nrow(price.reduced)
#46,686 rows
#don't do this ever you fucking moron
#replicates are between blocks, not within blocks for 90% of sites
#price.reduced2<-price.reduced[price.reduced$block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'),]
nrow(price.reduced2)
#12,450

View(price.reduced)
price.reduced$unique.id<-as.character(with(price.reduced, paste(site.year.id,trt_year,block,plot, sep=".")))

#remove duplicates
#price.reduced2$unique.id[duplicated(price.reduced2)]
price.r<-price.reduced %>% distinct(unique.id, .keep_all = TRUE)
nrow(price.r)
#8,646

View(price.r)

price.r$year.y<-as.numeric(price.r$year.y)
is.numeric(price.r$year.y)
is.numeric(price.r$SL)

write.csv(price.r,"~/Desktop/Academic/Data/NutNet/nutnet_price_all2.csv")

p.all <- read.csv("~/Desktop/Academic/Data/NutNet/nutnet_price_all.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.native <- read.csv("~/Desktop/Academic/Data/NutNet/nutnet_price_native.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.int <- read.csv("~/Desktop/Academic/Data/NutNet/nutnet_price_introduced.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
??priceTools

is.numeric(p.all$year.y)

summary(p.all)
View(p.all)
colnames(p.all)
levels(p.all$trt.x)

t.ctl<-p.all[p.all$trt_year %in% c('Control_0 NPK_0'),]

t.ctl2<-t.ctl[complete.cases(t.ctl), ]
View(t.ctl)

ctl<-p.all[p.all$trt.x %in% c('Control'),]
ctl2<-ctl[ctl$trt.y %in% c('Control'),]
ctl3<-ctl2[complete.cases(ctl2), ]
View(ctl3)
npk<-p.all[p.all$trt.x %in% c('NPK'),]
npk2<-npk[npk$trt.y %in% c('NPK'),]
nrow(npk2)

npk3<-npk2[complete.cases(npk2), ]
nrow(npk3)

theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
leap.zig(b.dat.all,type='cafe',xlim=c(0,15),ylim=c(0,700),standardize = FALSE,raw.points = FALSE)+ 
  annotate("text", x = mean(b.dat.all$x.rich), y = mean(b.dat.all$x.func), 
           label = "*",size=8)+ggtitle('Across Time')+theme_classic()


hist(npk3$SL)
hist(npk3$SG)
hist(npk3$CDE)
View(npk3)

b.dat.all<-bind_rows(npk3,t.ctl2)
is.numeric(b.dat.all$year.y)
View(b.dat.all)

#price models
#all
SL.m <- brm(SL ~  year.y + (year.y | site.year.id/block/plot/site.year.id.y), 
                   data = npk3, cores = 4, chains = 4)


SG.m <- brm(SG ~  year.y + (year.y | site.year.id/block/plot/site.year.id.y), 
            data = npk3, cores = 4, chains = 4)


CDE.m <- brm(CDE ~  year.y + (year.y | site.year.id/block/plot/site.year.id.y), 
            data = b.dat.all, cores = 4, chains = 4)


#native
SL.n.m <- brm(SL~  year.y + (year.y | site.year.id/block/plot/site.year.id.y), 
            data = p.native, cores = 4, chains = 4)


SG.n.m <- brm(SG~  year.y + (year.y | site.year.id/block/plot/site.year.id.y), 
            data = p.native, cores = 4, chains = 4)


CDE.n.m <- brm(CDE~  year.y + (year.y | site.year.id/block/plot/site.year.id.y), 
             data = p.native, cores = 4, chains = 4)


#introduced
SL.i.m <- brm(SL~  year.y + (year.y | site.year.id/block/plot/site.year.id.y), 
              data = p.introduced, cores = 4, chains = 4)


SG.i.m <- brm(SG~  year.y + (year.y | site.year.id/block/plot/site.year.id.y), 
              data = p.introduced, cores = 4, chains = 4)


CDE.i.m <- brm(CDE~  year.y + (year.y | site.year.id/block/plot/site.year.id.y), 
               data = p.introduced, cores = 4, chains = 4)

setwd('~/Dropbox/Projects/NutNet/Model_fits/')
save(SL.m,SG.m,CDE.m,file = 'price.all.Rdata')
#save(SL.n.m,SG.n.m,CDE.n.m,file = 'price.native.Rdata')
#save(SL.i.m,SG.i.m,CDE.i.m,file = 'price.introduced.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/price.all.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/price.native.Rdata')
load('~/Dropbox/Projects/NutNet/Model_fits/price.introduced.Rdata')



summary(CDE.m)
hist(m1$Estimate)


# inspection of chain diagnostic
plot(CDE.m)# looks ok


# predicted values vs observed: not great, but not too bad (there is a skew-normal distribution
# that is in the brms package - I will see if that improves this later)
pp_check(CDE.m)

#residuals
m1<-residuals(CDE.m)
m1<-as.data.frame(m1)
nrow(m1)
nrow(plot)
rr.plot<-cbind(plot,m1$Estimate)
View(rr.plot)

head(rr.plot)
par(mfrow=c(3,2))
with(rr.plot, plot(continent, m1$Estimate))
with(rr.plot, plot(habitat, m1$Estimate))
with(rr.plot, plot(site.year.id, m1$Estimate))
with(rr.plot, plot(block, m1$Estimate))
with(rr.plot, plot(plot, m1$Estimate))


# #------plot richness model all sp----------------
# fixed effects
sl.fitted <- cbind(CDE.m$data,
                          # get fitted values; setting re_formula=NA means we are getting 'fixed' effects
                          fitted(CDE.m, re_formula = NA)) %>% 
  as_tibble() 

# fixed effect coefficients (I want these for the coefficient plot)
sl_fixef <- fixef(CDE.m)
View(sl_fixef)
# coefficients for experiment-level (random) effects
sl_coef <- coef(CDE.m)
View(sl_coef)
sl_coef<-as.data.frame(sl_coef$site.year.id)
sl_coef2 <-  bind_cols(sl_coef$site.year.id[,,'Intercept'] %>% 
                                as_tibble() %>% 
                                mutate(Intercept = Estimate,
                                       Intercept_lower = Q2.5,
                                       Intercept_upper = Q97.5,
                                       site.year.id = rownames(sl_coef$site.year.id[,,'Intercept'])) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5),
                              sl_coef$site.year.id[,,'year.y'] %>% 
                                as_tibble() %>% 
                                mutate(Slope = Estimate,
                                       Slope_lower = Q2.5,
                                       Slope_upper = Q97.5) %>% 
                                select(-Estimate, -Est.Error, -Q2.5, -Q97.5)) %>% 
  # join with min and max of the x-values
  inner_join(p.all %>% 
               group_by(site.year.id) %>% 
               summarise(xmin = min(year.y),
                         xmax = max(year.y)),
             by = 'site.year.id')

View(sl_coef2)
View(plot)
View(sl.fitted)
colnames(dat)
dat<-distinct(p, site.year.id, continent,habitat)
View(dat)

sl_coef3<-full_join(sl_coef2,dat)
View(sl_coef3)
sl.fitted2<-full_join(sl.fitted,dat)
View(sl.fitted2)

theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
r1<-ggplot() +
  # data
  geom_point(data = sl.fitted2,
             aes(x = year.y , y = SL,
                 colour = continent, alpha=0.5),
             size = 1.2) +
  geom_jitter(data=sl.fitted2,
              aes(x = year.y, y = SL,
                  colour = continent), height=0.45,width = 0.45)+
  # experiment (random) effects
  geom_segment(data = sl_coef3,
               aes(x = xmin, 
                   xend = xmax,
                   y = Intercept + Slope * xmin,
                   yend = Intercept + Slope * xmax,
                   group = site.year.id,
                   colour = continent),
               size = .7) +
  # uncertainy in fixed effect
  geom_ribbon(data = sl.fitted,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.3) +
  # fixed effect
  geom_line(data = sl.fitted,
            aes(x = year.y, y = Estimate),
            size = 1.5) +
  labs(x = 'Years',
       y = 'Species richness', title= 'a) Plot Richness') +
  scale_colour_manual(values = c("#FA6B09FF", "#8F2F8BFF", "#F9B90AFF",  "#EE0011FF","#15983DFF", "#0C5BB0FF" ))+
  theme_bw()#+ theme(legend.position="bottom")
