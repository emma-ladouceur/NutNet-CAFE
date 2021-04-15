


library(patchwork)
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")




load('~/Dropbox/Projects/NutNet/Data/rich.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/bm.mod.dat.Rdata')



load('~/Dropbox/Projects/NutNet/Data/sgain_dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/sloss.n.mod.dat.Rdata')





plot <- read.csv("~/Dropbox/Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

plot <- plot %>% group_by(site_code) %>% filter(max.year >= 3) %>%
  ungroup()



plot.rich_fitted.npk

plot.rich_fitted.npk$Model<-"Species Richness"
plot.rich_fitted.ctl$Model<-"Species Richness"
plot.rich_fitted.npk <- plot.rich_fitted.npk %>% rename(Treatment = trt) 
plot.rich_fitted.ctl <- plot.rich_fitted.ctl %>% rename(Treatment = trt) 
fitted.rich<-bind_rows(plot.rich_fitted.npk,plot.rich_fitted.ctl)

fitted.rich$Treatment <- factor(fitted.rich$Treatment , levels=c("NPK","Control"))

plot.rich_coef3 <- plot.rich_coef3 %>% filter(!is.na(TESlope))


# GAINS

is.factor(sgain.trt_fitted.npk$starting.richness)
is.factor(sgain.trt_coef3$starting.richness)

sgain.trt_fitted.npk<-sgain.trt_fitted.npk[complete.cases(sgain.trt_fitted.npk$starting.richness), ]
sgain.trt_coef3<-sgain.trt_coef3[complete.cases(sgain.trt_coef3$starting.richness), ]

sgain.trt_fitted.npk$starting.richness <- factor(sgain.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sgain.trt_coef3$starting.richness <- factor(sgain.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

sgain.trt_coef3$xs<-1

sgain.trt_fitted.npk$Model<-"C) Species Gain"
sgain.trt_fitted.ctl$Model<-"C) Species Gain"
sgain.trt_fitted.npk <- sgain.trt_fitted.npk %>% rename(Treatment = trt.y) 
sgain.trt_fitted.ctl <- sgain.trt_fitted.ctl %>% rename(Treatment = trt.y) 
fitted.sgain<-bind_rows(sgain.trt_fitted.npk,sgain.trt_fitted.ctl)

fitted.sgain

fitted.sgain$Treatment <- factor(fitted.sgain$Treatment , levels=c("NPK","Control"))




# LOSSES





summary(sloss.s)


as.factor(as.character(sloss.trt_fitted.npk$starting.richness))
as.factor(as.character(sloss.trt_coef3$starting.richness))

sloss.trt_fitted.npk<-sloss.trt_fitted.npk[complete.cases(sloss.trt_fitted.npk$starting.richness), ]
sloss.trt_coef3<-sloss.trt_coef3[complete.cases(sloss.trt_coef3$starting.richness), ]

sloss.trt_fitted.npk$starting.richness <- factor(sloss.trt_fitted.npk$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sloss.trt_coef3$starting.richness <- factor(sloss.trt_coef3$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))


sloss.trt_coef3$xs<-1

sloss.trt_fitted.npk$Model<-"B) Species Loss"
sloss.trt_fitted.ctl$Model<-"B) Species Loss"
sloss.trt_fitted.npk <- sloss.trt_fitted.npk %>% rename(Treatment = trt.y) 
sloss.trt_fitted.ctl <- sloss.trt_fitted.ctl %>% rename(Treatment = trt.y) 
fitted.sloss<-bind_rows(sloss.trt_fitted.npk,sloss.trt_fitted.ctl)

fitted.sloss

fitted.sloss$Treatment <- factor(fitted.sloss$Treatment , levels=c("NPK","Control"))


View(fitted.sloss)

# 
# sloss.trt_fitted.npk$Q2.5<-abs(sloss.trt_fitted.npk$Q2.5)
# sloss.trt_fitted.npk$Q97.5<-abs(sloss.trt_fitted.npk$Q97.5)
# fitted.sloss$Estimate<-abs(fitted.sloss$Estimate)
# sloss.trt_fitted.ctl$Q2.5<-abs(sloss.trt_fitted.ctl$Q2.5)
# sloss.trt_fitted.ctl$Q97.5<-abs(sloss.trt_fitted.ctl$Q97.5)


# orange code "#F98400"
together<-ggplot() +
  geom_ribbon(data = sloss.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="#B40F20",alpha = 0.5) +
  # fixed effect
  geom_line(data = fitted.sloss,
            aes(x = year.y, y = Estimate,linetype=Treatment,color=Treatment),
            color="#B40F20",size = 1.5) +
  geom_ribbon(data = sloss.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="black",alpha = 0.5) +
  geom_ribbon(data = plot.rich_fitted.npk,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              fill="#0B775E",alpha = 0.5) +
  # fixed effect
  geom_line(data = fitted.rich,
            aes(x = year_trt, y = Estimate, linetype= Treatment,color=Treatment),
            color="#0B775E",size = 1.5) +
  geom_ribbon(data = plot.rich_fitted.ctl,
              aes(x = year_trt, ymin = Q2.5, ymax = Q97.5),
              alpha = 0.5) +
  geom_ribbon(data = sgain.trt_fitted.npk,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="#046C9A",alpha = 0.5) +
  # fixed effect
  geom_line(data = fitted.sgain,
            aes(x = year.y, y = Estimate, linetype=Treatment, color=Treatment),
            color="#046C9A",size = 1.5) +
  geom_ribbon(data = sgain.trt_fitted.ctl,
              aes(x = year.y, ymin = Q2.5, ymax = Q97.5),
              fill="black",alpha = 0.5) +
  scale_x_continuous(breaks=c(0,1,3,6,9,12)) +
  labs(x='Year',
       #x = 'Years',
       y = ' Species richness', title= 'A)') +
  theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="none",
                     plot.title = element_text(size=12),
                     plot.margin= margin(t = 0.1, r = 0.2, b = 0.2, l = 0.2, unit = "cm"),
                     axis.title.x = element_text(size=9),
                     axis.title.y = element_text(size=9),
                     axis.text=element_text(size=9))

together
