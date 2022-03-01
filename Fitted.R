

# packages
library(tidyverse)
library(brms)
library(tidybayes)
library(ggplot2)
library(patchwork)
library(gridExtra)
library(grid)

p.all <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


colnames(p.all)

p.all <- p.all %>% group_by(site_code) %>% #filter(max.year >= 3) 
  filter(year_max >= 3) %>% mutate(year.y == max(year.y))

View(p.all)

p.all$site_code <- as.factor(p.all$site_code)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)
p.all$year.y<-as.numeric(p.all$year.y)

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sg.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/cde.Rdata') # CDE.s

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sloss.Rdata') # s.loss.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sgain.Rdata') # s.gain.s

s.loss.fitted <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m =  max(year.y.m),
            year.y =  max(year.y)) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(fitted = map(data, ~epred_draws(sloss.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 


head(s.loss.fitted)


s.loss.fitted.df  <- s.loss.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(s.loss.fitted.df)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.loss.fitted.df, file = 'fitted_s.loss.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.loss.Rdata')

s.loss.fitted <- s.loss.fitted.df %>%
  select(-.draw) %>%
  group_by(Trt_group, year.y, year.y.m) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>% 
  select(-.epred) %>% distinct()

head(s.loss.fitted)


fig_2a <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = s.loss.n , x = trt.y, colour = 	"#C0C0C0"), 
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = s.loss.fitted,
             aes(x = trt.y, y = P_Estimate, colour = trt.y), size = 3) +
  geom_errorbar(data = s.loss.fitted,
                aes(x = trt.y, ymin = P_Estimate_lower, ymax = P_Estimate_upper, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0" ,"black", "#B40F20"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim(-20, 0)+
  labs(x='',
       y = 'Average species loss',
       title= 'a) Species loss (s.loss)') 


fig_2a

#sgain


summary(sgain.3_p)

s.gain.fitted <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m =  max(year.y.m),
            year.y =  max(year.y)) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(fitted = map(data, ~epred_draws(sgain.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 

head(s.gain.fitted)

s.gain.fitted.df  <- s.gain.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(s.gain.fitted.df)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.gain.fitted.df, file = 'fitted_s.gain.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.gain.Rdata')

s.gain.fitted <- s.gain.fitted.df %>%
  select(-.draw) %>%
  group_by(Trt_group, year.y, year.y.m) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>% 
  select(-.epred) %>% distinct()

head(s.gain.fitted)


fig_2b <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = s.gain , x = trt.y, colour = 	"#C0C0C0"), 
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = s.gain.fitted,
             aes(x = trt.y, y = P_Estimate, colour = trt.y), size = 3) +
  geom_errorbar(data = s.gain.fitted,
                aes(x = trt.y, ymin = P_Estimate_lower, ymax = P_Estimate_upper, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0" ,"black", "#046C9A"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim(0, 20)+
  labs(x='',
       y = 'Average species gain',
       title= 'a) Species gain (s.gain)') 


fig_2b


# SL

s.sl.fitted <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m =  max(year.y.m),
            year.y =  max(year.y)) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(fitted = map(data, ~epred_draws(sl.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 

head(s.sl.fitted)

s.sl.fitted.df  <- s.sl.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(s.sl.fitted.df)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.sl.fitted.df, file = 'fitted_s.sl.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.sl.Rdata')

s.sl.fitted <- s.sl.fitted.df %>%
  select(-.draw) %>%
  group_by(Trt_group, year.y, year.y.m) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>% 
  select(-.epred) %>% distinct()

head(s.sl.fitted)


fig_2c <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = SL, x = trt.y, colour = 	"#C0C0C0"), 
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = s.sl.fitted,
             aes(x = trt.y, y = P_Estimate, colour = trt.y), size = 3) +
  geom_errorbar(data = s.sl.fitted,
                aes(x = trt.y, ymin = P_Estimate_lower, ymax = P_Estimate_upper, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0" ,"black", "#B40F20"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim(-250, 250)+
  labs(x='',
       y = expression(paste('Average change in biomass (g/' ,m^2, ')')),
       title= 'a) Biomass change associated \n with species loss (SL)') 


fig_2c


#SG

summary(sg.3_p)

s.sg.fitted <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m =  max(year.y.m),
            year.y =  max(year.y)) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(fitted = map(data, ~epred_draws(sg.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 

head(s.sg.fitted)

s.sg.fitted.df  <- s.sg.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(s.sg.fitted.df)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.sg.fitted.df, file = 'fitted_s.sg.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.sg.Rdata')

s.sg.fitted <- s.sg.fitted.df %>%
  select(-.draw) %>%
  group_by(Trt_group, year.y, year.y.m) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>% 
  select(-.epred) %>% distinct()

head(s.sg.fitted)


fig_2d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = SG , x = trt.y, colour = 	"#C0C0C0"), 
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = s.sg.fitted,
             aes(x = trt.y, y = P_Estimate, colour = trt.y), size = 3) +
  geom_errorbar(data = s.sg.fitted,
                aes(x = trt.y, ymin = P_Estimate_lower, ymax = P_Estimate_upper, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0" ,"black", "#046C9A"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim(-250, 250)+
  labs(x='',
       y = '',
       title= 'd) Biomass change associated \n with species gain (SG)') 


fig_2d





summary(cde.3_p)

s.cde.fitted <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m =  max(year.y.m),
            year.y =  max(year.y)) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(fitted = map(data, ~epred_draws(cde.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 

head(s.cde.fitted)

s.cde.fitted.df  <- s.cde.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(s.cde.fitted.df)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.cde.fitted.df, file = 'fitted_s.cde.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.cde.Rdata')

s.cde.fitted <- s.cde.fitted.df %>%
  select(-.draw) %>%
  group_by(Trt_group, year.y, year.y.m) %>%
  filter(.epred > quantile(.epred, probs=0.025),
         .epred < quantile(.epred, probs=0.975)) %>% sample_n(1000)  %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>%
  select(-.epred) %>% distinct()

head(s.cde.fitted)


fig_2e <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = CDE , x = trt.y, colour = 	"#C0C0C0"), 
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = s.cde.fitted,
             aes(x = trt.y, y = P_Estimate, colour = trt.y), size = 3) +
  geom_errorbar(data = s.cde.fitted,
                aes(x = trt.y, ymin = P_Estimate_lower, ymax = P_Estimate_upper, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0" ,"black", "#F98400"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim(-250, 250)+
  labs(x='',
       y = '',
       title= 'e) Biomass change associated \n with persistent species (PS)') 


fig_2e


# 9X14 LANDSCAPE
(fig_2a | fig_2b)/ (fig_2c | fig_2d | fig_2e)




