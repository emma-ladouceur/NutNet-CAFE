
# packages
library(tidyverse)
library(brms)
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

colnames(p.all)

sloss_c <- conditional_effects(sloss.3_p, effects = 'trt.y', re_formula = NA, method = 'fitted')  # conditional effects

fig_2a <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = s.loss.n , x = trt.y, colour = 	"#C0C0C0"), 
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = sloss_c$trt.y,
             aes(x = trt.y, y = estimate__, colour = trt.y), size = 3) +
  geom_errorbar(data = sloss_c$trt.y,
                aes(x = trt.y, ymin = lower__, ymax = upper__, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0" ,"black", "#B40F20"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim(-10, 0)+
  labs(x='',
       y = 'Species Loss',
       title= 'a) Species loss (s.loss)') 


fig_2a

sgain_c <- conditional_effects(sgain.3_p, effects = 'trt.y', re_formula = NA, method = 'fitted')  # conditional effects

fig_2b <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = s.gain, x = trt.y, colour = 	"#C0C0C0"), 
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = sgain_c$trt.y,
             aes(x = trt.y, y = estimate__, colour = trt.y), size = 3) +
  geom_errorbar(data = sgain_c$trt.y,
                aes(x = trt.y, ymin = lower__, ymax = upper__, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0" ,"black", "#046C9A"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim( 0, 10)+
  labs(x='',
       y = 'Species gain',
       title= 'a) Species gain (s.gain)') 


fig_2b


sl_c <- conditional_effects(sl.3_p, effects = 'trt.y', re_formula = NA, method = 'fitted')  # conditional effects

fig_2c <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = SL, x = trt.y, colour = 	"#C0C0C0"), 
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = sl_c$trt.y,
             aes(x = trt.y, y = estimate__, colour = trt.y), size = 3) +
  geom_errorbar(data = sl_c$trt.y,
                aes(x = trt.y, ymin = lower__, ymax = upper__, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0" ,"black", "#B40F20"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim(-200, 0)+
        labs(x='',
          y = expression(paste('Change in Biomass (g/' ,m^2, ')')),
          title= 'c) Biomass change associated \n with species loss (SL)') 


fig_2c

sg_c <- conditional_effects(sg.3_p, effects = 'trt.y', re_formula = NA, method = 'fitted')  # conditional effects

fig_2d <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = SG, x = trt.y, colour = 	"#C0C0C0"), 
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = sg_c$trt.y,
             aes(x = trt.y, y = estimate__, colour = trt.y), size = 3) +
  geom_errorbar(data = sg_c$trt.y,
                aes(x = trt.y, ymin = lower__, ymax = upper__, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c(	"#C0C0C0" ,"black", "#046C9A"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim( 0, 200)+
  labs(x='',
       y = '',
      # y = expression(paste('Change in Biomass (g/' ,m^2, ')')),
       title= 'd) Biomass change associated \n with species gain (SG)') 


fig_2d


cde_c <- conditional_effects(cde.3_p, effects = 'trt.y', re_formula = NA, method = 'fitted')  # conditional effects

head(cde_c)

fig_2e <- ggplot() + 
  geom_hline(yintercept = 0,linetype="longdash") +
  geom_point(data = p.all,
             aes(y = CDE, x = trt.y,  colour = 	"#C0C0C0"
                 ),
             size = 1, alpha = 0.2, position = position_jitter(width = 0.05, height=0.45)) +
  geom_point(data = cde_c$trt.y,
             aes(x = trt.y, y = estimate__, colour = trt.y), size = 3) +
  geom_errorbar(data = cde_c$trt.y,
                aes(x = trt.y, ymin = lower__, ymax = upper__, colour = trt.y),
                size = 1, width = 0) +
  scale_color_manual(values =  c("#C0C0C0",	"black", "#F98400"))  + 
  ggtitle((expression(paste(italic(alpha), '-scale', sep = ''))))+
  theme_bw(base_size=18)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                               plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = 0.2, unit = "cm"),
                               plot.title=element_text(size=18, hjust=0.5),
                               strip.background = element_blank(),legend.position="none") +
  ylim(-100, 200)+
  labs(x='',
       y = '',
      # y = expression(paste('Change in Biomass (g/' ,m^2, ')')),
       title= 'c) Biomass change associated \n with persistent species (PS)') 


fig_2e


(fig_2a | fig_2b)/ (fig_2c | fig_2d | fig_2e)





