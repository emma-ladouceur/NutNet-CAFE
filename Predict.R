


# packages
library(tidyverse)
library(brms)
library(tidybayes)
library(ggplot2)
library(patchwork)
library(gridExtra)
library(grid)

# data
sp <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/biomass_sp_CAFE.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p.all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


p.all <- p.all %>% group_by(site_code) %>% #filter(max.year >= 3) 
  filter(year_max >= 3) 

p.all$site_code <- as.factor(p.all$site_code)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)
p.all$year.y<-as.numeric(p.all$year.y)


load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sg.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/cde.Rdata') # CDE.s

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sloss.Rdata') # s.loss.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sgain.Rdata') # s.gain.s

head(p.all)

summary(sloss.3_p)

s.loss.predict <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m =  max(year.y.m),
            year.y =  max(year.y)) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(predicted = map(data, ~predicted_draws(sloss.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 

head(s.loss.predict)

s.loss.predict.df  <- s.loss.predict %>% 
  unnest(cols = c(predicted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))
 
head(s.loss.predict.df)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.loss.predict.df, file = 'predict_s.loss.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/predict_s.loss.Rdata')

s.loss.predict <- s.loss.predict.df %>%
  select(-.draw) %>%
  group_by(Trt_group, year.y, year.y.m) %>%
  mutate( P_Estimate = mean(.prediction),
          P_Estimate_lower = quantile(.prediction, probs=0.025),
          P_Estimate_upper = quantile(.prediction, probs=0.975) ) %>% 
  select(-.prediction) %>% distinct()

head(s.loss.predict)


fig_2a <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = s.loss.n , x = trt.y, colour = 	"#C0C0C0"), 
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = s.loss.predict,
             aes(x = trt.y, y = P_Estimate, colour = trt.y), size = 3) +
  geom_errorbar(data = s.loss.predict,
                aes(x = trt.y, ymin = P_Estimate_lower, ymax = P_Estimate_upper, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0" ,"black", "#B40F20"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  #ylim(-10, 0)+
  labs(x='',
       y = 'Average species loss',
       title= 'a) Species loss (s.loss)') 


fig_2a



summary(sgain.3_p)

s.gain.predict <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m =  max(year.y.m),
            year.y =  max(year.y)) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(predicted = map(data, ~predicted_draws(sgain.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 

head(s.gain.predict)

s.gain.predict.df  <- s.gain.predict %>% 
  unnest(cols = c(predicted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(s.gain.predict.df)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.gain.predict.df, file = 'predict_s.gain.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/predict_s.gain.Rdata')

s.gain.predict <- s.gain.predict.df %>%
  select(-.draw) %>%
  group_by(Trt_group, year.y, year.y.m) %>%
  mutate( P_Estimate = mean(.prediction),
          P_Estimate_lower = quantile(.prediction, probs=0.025),
          P_Estimate_upper = quantile(.prediction, probs=0.975) ) %>% 
  select(-.prediction) %>% distinct()

head(s.gain.predict)


fig_2b <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = s.gain , x = trt.y, colour = 	"#C0C0C0"), 
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = s.gain.predict,
             aes(x = trt.y, y = P_Estimate, colour = trt.y), size = 3) +
  geom_errorbar(data = s.gain.predict,
                aes(x = trt.y, ymin = P_Estimate_lower, ymax = P_Estimate_upper, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0" ,"black", "#046C9A"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  #ylim(-10, 0)+
  labs(x='',
       y = 'Average species gain',
       title= 'a) Species gain (s.gain)') 


fig_2b




s.sl.predict <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m =  max(year.y.m),
            year.y =  max(year.y)) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(predicted = map(data, ~predicted_draws(sl.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 

head(s.sl.predict)

s.sl.predict.df  <- s.sl.predict %>% 
  unnest(cols = c(predicted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(s.sl.predict.df)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.sl.predict.df, file = 'predict_s.sl.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/predict_s.sl.Rdata')

s.sl.predict <- s.sl.predict.df %>%
  select(-.draw) %>%
  group_by(Trt_group, year.y, year.y.m) %>%
  mutate( P_Estimate = mean(.prediction),
          P_Estimate_lower = quantile(.prediction, probs=0.025),
          P_Estimate_upper = quantile(.prediction, probs=0.975) ) %>% 
  select(-.prediction) %>% distinct()

head(s.sl.predict)


fig_2c <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = SL, x = trt.y, colour = 	"#C0C0C0"), 
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = s.sl.predict,
             aes(x = trt.y, y = P_Estimate, colour = trt.y), size = 3) +
  geom_errorbar(data = s.sl.predict,
                aes(x = trt.y, ymin = P_Estimate_lower, ymax = P_Estimate_upper, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0" ,"black", "#B40F20"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim(-200, 300)+
  labs(x='',
       y = 'Average species sl',
       title= 'a) Species sl (s.sl)') 


fig_2c



summary(sg.3_p)

s.sg.predict <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m =  max(year.y.m),
            year.y =  max(year.y)) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(predicted = map(data, ~predicted_draws(sg.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 

head(s.sg.predict)

s.sg.predict.df  <- s.sg.predict %>% 
  unnest(cols = c(predicted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(s.sg.predict.df)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.sg.predict.df, file = 'predict_s.sg.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/predict_s.sg.Rdata')

s.sg.predict <- s.sg.predict.df %>%
  select(-.draw) %>%
  group_by(Trt_group, year.y, year.y.m) %>%
  mutate( P_Estimate = mean(.prediction),
          P_Estimate_lower = quantile(.prediction, probs=0.025),
          P_Estimate_upper = quantile(.prediction, probs=0.975) ) %>% 
  select(-.prediction) %>% distinct()

head(s.sg.predict)


fig_2d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = SG , x = trt.y, colour = 	"#C0C0C0"), 
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = s.sg.predict,
             aes(x = trt.y, y = P_Estimate, colour = trt.y), size = 3) +
  geom_errorbar(data = s.sg.predict,
                aes(x = trt.y, ymin = P_Estimate_lower, ymax = P_Estimate_upper, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0" ,"black", "#046C9A"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim(-200, 300)+
  labs(x='',
       y = 'Average species sg',
       title= 'a) Species sg (s.sg)') 


fig_2d





summary(cde.3_p)

s.cde.predict <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m =  max(year.y.m),
            year.y =  max(year.y)) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(predicted = map(data, ~predicted_draws(cde.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 

head(s.cde.predict)

s.cde.predict.df  <- s.cde.predict %>% 
  unnest(cols = c(predicted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(s.cde.predict.df)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.cde.predict.df, file = 'predict_s.cde.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/predict_s.cde.Rdata')

s.cde.predict <- s.cde.predict.df %>%
  select(-.draw) %>%
  group_by(Trt_group, year.y, year.y.m) %>%
  filter(.prediction > quantile(.prediction, probs=0.025),
         .prediction < quantile(.prediction, probs=0.975)) %>% sample_n(1000)  %>%
  mutate( P_Estimate = mean(.prediction),
          P_Estimate_lower = quantile(.prediction, probs=0.025),
          P_Estimate_upper = quantile(.prediction, probs=0.975) ) %>%
  select(-.prediction) %>% distinct()

head(s.cde.predict)


fig_2e <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = CDE , x = trt.y, colour = 	"#C0C0C0"), 
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = s.cde.predict,
             aes(x = trt.y, y = P_Estimate, colour = trt.y), size = 3) +
  geom_errorbar(data = s.cde.predict,
                aes(x = trt.y, ymin = P_Estimate_lower, ymax = P_Estimate_upper, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0" ,"black", "#F98400"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim(-400, 450)+
  labs(x='',
       y = 'Average species cde',
       title= 'a) Species cde (s.cde)') 


fig_2e


# 9X14 LANDSCAPE
(fig_2a | fig_2b)/ (fig_2c | fig_2d | fig_2e)


