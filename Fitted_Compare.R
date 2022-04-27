


# compare temporal intercepts?



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

head(p.all)

p.all %>% ungroup() %>% select(year_max) %>% distinct() %>% mutate(mean(year_max))

head(p.all)

p.all.max <- p.all %>% 
  mutate( year.y == as.numeric(year.y)) %>%
  #group_by(site_code) %>%
  filter(year.y ==  max(year.y)) %>%
  mutate(value = "max")


p.all.min <- p.all %>% 
  mutate( year.y == as.numeric(year.y)) %>%
  #group_by(site_code) %>%
  filter(year.y ==  min(year.y)) %>%
  mutate(value = "min")

  
p.all.mm <- p.all.max %>% rbind(p.all.min) %>%
  mutate(value = fct_relevel(value, c("min","max")))  

  
p.all$site_code <- as.factor(p.all$site_code)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)
p.all$year.y<-as.numeric(p.all$year.y)

View(p.all.mm)


load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sl.Rdata') # sl.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sg.Rdata') # sg.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/cde.Rdata') # CDE.s

load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sloss.Rdata') # s.loss.s
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Fits/3/sgain.Rdata') # s.gain.s

s.loss.fitted <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m,
            year.y,
            ) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(fitted = map(data, ~epred_draws(sloss.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 


head(s.loss.fitted)


s.loss.fitted.df  <- s.loss.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

View(s.loss.fitted.df)


View(s.loss.fitted.df)

s.loss.fitted <- s.loss.fitted.df %>%
  select(-.draw) %>%
  group_by(Trt_group, year.y, year.y.m) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>% 
  select(-.epred) %>% distinct()

head(s.loss.fitted)


s.loss.fitted.min <- s.loss.fitted %>%  filter(year.y %in% c(1)) %>% 
  mutate(value = "min")

s.loss.fitted.max <- s.loss.fitted %>% 
  filter(year.y %in% c(13)) %>% 
  mutate(value = "max")

View(s.loss.fitted.max)

s.loss.mm <- s.loss.fitted.max %>% rbind(s.loss.fitted.min) %>%
  mutate(value = fct_relevel(value, c("min","max")))  

head(s.loss.mm)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.loss.mm, file = 'fitted_s.loss_compare.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.loss_compare.Rdata')


fig_2a <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  # raw points
  geom_point(data = p.all.mm,
             aes(y = s.loss.n , x = trt.y, colour = trt.y,  shape= value, group = value),
             position = position_jitterdodge(
               jitter.width = 0.15,
               jitter.height = 1,
               dodge.width = 0.75,
               seed = NA
             ),
             size = 1, alpha = 0.2) +
  geom_point(data = s.loss.mm,
             aes(x = trt.y , y = P_Estimate, colour = trt.y, shape = value, group = value), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = s.loss.mm,
                aes(x = trt.y , ymin = P_Estimate_lower, ymax = P_Estimate_upper,  colour = trt.y, group = value),
                  position = position_dodge(width = 0.75),
                 size = 1, width = 0) +
  scale_color_manual(name = "Treatment",
    values =  c(	"black",  "#B40F20"), labels = c("Control","NPK"))  +
  scale_shape_manual(name = "Average change between t0 & tn",
    values = c(16, 17), labels = c("Year 1", "Max Year") )+ 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim(-20, 0)+
  labs(x='',
       y = 'Average total number of species lost',
       title= 'a) Number of species lost (s.loss)') 


fig_2a

#sgain


summary(sgain.3_p)

s.gain.fitted <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m,
            year.y ) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(fitted = map(data, ~epred_draws(sgain.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 

head(s.gain.fitted)

s.gain.fitted.df  <- s.gain.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(s.gain.fitted.df)


s.gain.fitted <- s.gain.fitted.df %>%
  select(-.draw) %>%
  group_by(Trt_group, year.y, year.y.m) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>% 
  select(-.epred) %>% distinct()

head(s.gain.fitted)



s.gain.fitted.min <- s.gain.fitted %>%  filter(year.y %in% c(1)) %>% 
  mutate(value = "min")

s.gain.fitted.max <- s.gain.fitted %>% 
  filter(year.y %in% c(13)) %>% 
  mutate(value = "max")

View(s.gain.fitted.max)

s.gain.mm <- s.gain.fitted.max %>% rbind(s.gain.fitted.min) %>%
  mutate(value = fct_relevel(value, c("min","max")))  

head(s.gain.mm)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.gain.mm, file = 'fitted_s.gain_compare.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_s.gain_compare.Rdata')


fig_2b <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  # raw points
  geom_point(data = p.all.mm,
             aes(y = s.gain , x = trt.y, colour = trt.y,  shape= value, group = value),
             position = position_jitterdodge(
               jitter.width = 0.15,
               jitter.height = 1,
               dodge.width = 0.75,
               seed = NA
             ),
             size = 1, alpha = 0.2) +
  geom_point(data = s.gain.mm,
             aes(x = trt.y , y = P_Estimate, colour = trt.y, shape = value, group = value), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = s.gain.mm,
                aes(x = trt.y , ymin = P_Estimate_lower, ymax = P_Estimate_upper,  colour = trt.y, group = value),
                position = position_dodge(width = 0.75),
                size = 1, width = 0) +
  scale_color_manual(name = "Treatment",
                     values =  c(	"black",  "#046C9A"), labels = c("Control","NPK"))  +
  scale_shape_manual(name = "Average change between t0 & tn",
                     values = c(16, 17), labels = c("Year 1", "Max Year") )+ 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim(0, 20)+
  labs(x='',
       y = 'Average total number of species gained',
       title= 'b) Number of species gained (s.gain)') 


fig_2b




# SL

s.sl.fitted <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m ,
            year.y ) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(fitted = map(data, ~epred_draws(sl.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 

head(s.sl.fitted)

s.sl.fitted.df  <- s.sl.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(s.sl.fitted.df)

s.sl.fitted <- s.sl.fitted.df %>%
  select(-.draw) %>%
  group_by(Trt_group, year.y, year.y.m) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>% 
  select(-.epred) %>% distinct()

head(s.sl.fitted)


s.sl.fitted.min <- s.sl.fitted %>%  filter(year.y %in% c(1)) %>% 
  mutate(value = "min")

s.sl.fitted.max <- s.sl.fitted %>% 
  filter(year.y %in% c(13)) %>% 
  mutate(value = "max")

head(s.sl.fitted.max)

s.sl.mm <- s.sl.fitted.max %>% rbind(s.sl.fitted.min) %>%
  mutate(value = fct_relevel(value, c("min","max")))  

head(s.loss.mm)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.sl.mm, file = 'fitted_sl_compare.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_sl_compare.Rdata')


fig_2c <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  # raw points
  geom_point(data = p.all.mm,
             aes(y = SL, x = trt.y, colour = trt.y,  shape= value, group = value),
             position = position_jitterdodge(
               jitter.width = 0.15,
               jitter.height = 1,
               dodge.width = 0.75,
               seed = NA
             ),
             size = 1, alpha = 0.2) +
  geom_point(data = s.sl.mm,
             aes(x = trt.y , y = P_Estimate, colour = trt.y, shape = value, group = value), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = s.sl.mm,
                aes(x = trt.y , ymin = P_Estimate_lower, ymax = P_Estimate_upper,  colour = trt.y, group = value),
                position = position_dodge(width = 0.75),
                size = 1, width = 0) +
  scale_color_manual(name = "Treatment",
                     values =  c(	"black",  "#B40F20"), labels = c("Control","NPK"))  +
  scale_shape_manual(name = "Average change between t0 & tn",
                     values = c(16, 17), labels = c("Year 1", "Max Year") )+ 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim(-250, 250)+
  labs(x='',
       y = expression(paste('Average total change in biomass (g/' ,m^2, ')')),
       title= 'c) Biomass change associated \n with species loss (SL)') 


fig_2c



#SG

summary(sg.3_p)

s.sg.fitted <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m ,
            year.y ) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(fitted = map(data, ~epred_draws(sg.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 

head(s.sg.fitted)

s.sg.fitted.df  <- s.sg.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(s.sg.fitted.df)

s.sg.fitted <- s.sg.fitted.df %>%
  select(-.draw) %>%
  group_by(Trt_group, year.y, year.y.m) %>%
  mutate( P_Estimate = mean(.epred),
          P_Estimate_lower = quantile(.epred, probs=0.025),
          P_Estimate_upper = quantile(.epred, probs=0.975) ) %>% 
  select(-.epred) %>% distinct()

head(s.sg.fitted)


s.sg.fitted.min <- s.sg.fitted %>%  filter(year.y %in% c(1)) %>% 
  mutate(value = "min")

s.sg.fitted.max <- s.sg.fitted %>% 
  filter(year.y %in% c(13)) %>% 
  mutate(value = "max")

head(s.sg.fitted.max)

s.sg.mm <- s.sg.fitted.max %>% rbind(s.sg.fitted.min) %>%
  mutate(value = fct_relevel(value, c("min","max")))  

head(s.sg.mm)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.sg.mm, file = 'fitted_sg_compare.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_sg_compare.Rdata')


fig_2d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  # raw points
  geom_point(data = p.all.mm,
             aes(y = SG, x = trt.y, colour = trt.y,  shape= value, group = value),
             position = position_jitterdodge(
               jitter.width = 0.15,
               jitter.height = 1,
               dodge.width = 0.75,
               seed = NA
             ),
             size = 1, alpha = 0.2) +
  geom_point(data = s.sg.mm,
             aes(x = trt.y , y = P_Estimate, colour = trt.y, shape = value, group = value), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = s.sg.mm,
                aes(x = trt.y , ymin = P_Estimate_lower, ymax = P_Estimate_upper,  colour = trt.y, group = value),
                position = position_dodge(width = 0.75),
                size = 1, width = 0) +
  scale_color_manual(name = "Treatment",
                     values =  c(	"black",   "#046C9A"), labels = c("Control","NPK"))  +
  scale_shape_manual(name = "Average change between t0 & tn",
                     values = c(16, 17), labels = c("Year 1", "Max Year") )+ 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="bottom") +
  ylim(-250, 250)+
  labs(x='',
       y = '',
       title= 'd) Biomass change associated \n with species gain (SG)') 


fig_2d

# cde
summary(cde.3_p)

s.cde.fitted <- p.all %>% 
  mutate(Trt_group = trt.y) %>%
  group_by(Trt_group, trt.y) %>% 
  summarise(year.y.m ,
            year.y ) %>%
  nest(data = c(trt.y, year.y.m, year.y)) %>%
  mutate(fitted = map(data, ~epred_draws(cde.3_p, newdata= .x, re_formula = ~(trt.y * year.y.m) ))) 

head(s.cde.fitted)

s.cde.fitted.df  <- s.cde.fitted %>% 
  unnest(cols = c(fitted)) %>% select(-data) %>%
  select(-c(.row, .chain, .iteration))

head(s.cde.fitted.df)

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



s.cde.fitted.min <- s.cde.fitted %>%  filter(year.y %in% c(1)) %>% 
  mutate(value = "min")

s.cde.fitted.max <- s.cde.fitted %>% 
  filter(year.y %in% c(13)) %>% 
  mutate(value = "max")

head(s.cde.fitted.max)

s.cde.mm <- s.cde.fitted.max %>% rbind(s.cde.fitted.min) %>%
  mutate(value = fct_relevel(value, c("min","max")))  

head(s.sg.mm)

setwd('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/')
save(s.cde.mm, file = 'fitted_cde_compare.Rdata')
load('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/Model_Extract/fitted_sg_compare.Rdata')


fig_2e <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  # raw points
  geom_point(data = p.all.mm,
             aes(y = CDE, x = trt.y, colour = trt.y,  shape= value, group = value),
             position = position_jitterdodge(
               jitter.width = 0.15,
               jitter.height = 1,
               dodge.width = 0.75,
               seed = NA
             ),
             size = 1, alpha = 0.2) +
  geom_point(data = s.cde.mm,
             aes(x = trt.y , y = P_Estimate, colour = trt.y, shape = value, group = value), 
             position = position_dodge(width = 0.75), size = 3) +
  geom_errorbar(data = s.cde.mm,
                aes(x = trt.y , ymin = P_Estimate_lower, ymax = P_Estimate_upper,  colour = trt.y, group = value),
                position = position_dodge(width = 0.75),
                size = 1, width = 0) +
  scale_color_manual(name = "Treatment",
                     values =  c(	"black",   "#F98400"), labels = c("Control","NPK"))  +
  scale_shape_manual(name = "Average change between t0 & tn",
                     values = c(16, 17), labels = c("Year 1", "Max Year") )+ 
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



# 10X14 LANDSCAPE
(fig_2a | fig_2b)/ (fig_2c | fig_2d | fig_2e)




