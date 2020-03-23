
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)
library("scales")
library(viridis)

# FIGURE 4
# POSTERIORS ACROSS GROUPS

# models
load('~/Dropbox/Projects/NutNet/Model_fits/bm.Rdata') # plot.bm.s
load('~/Dropbox/Projects/NutNet/Model_fits/rich.Rdata') # plot.rich.g

load('~/Dropbox/Projects/NutNet/Model_fits/sl.n.Rdata') # sl.s
load('~/Dropbox/Projects/NutNet/Model_fits/sg.Rdata') # sg.s
load('~/Dropbox/Projects/NutNet/Model_fits/cde.Rdata') # CDE.s


# site level meta data for posrteriors
# calculated to site level details found in Climate_Data.R
# latitude and longitude dont match due to decimal rounding
# lat.x long.x is nutnet site, lat.y long.y is world clim
meta <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_clim.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(meta)
View(meta)


#  mods study level dat
study_levels <- plot.rich.g$data %>% 
  as_tibble() %>% 
  distinct(site_code) %>% 
  mutate(level =  site_code) %>%
  nest_legacy(level)

parnames(plot.rich.g)
study_sample_posterior <- study_levels %>%
  mutate(sl.ctl = purrr::map(data, ~posterior_samples(sl.s,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000,
                                                                           min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          sg.ctl = purrr::map(data, ~posterior_samples(sg.s,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 1000,
                                                                           min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          cde.ctl = purrr::map(data, ~posterior_samples(CDE.s,
                                                       pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         rich.ctl = purrr::map(data, ~posterior_samples(plot.rich.g,
                                                        pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
                                                        exact = TRUE,
                                                        subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         bm.ctl = purrr::map(data, ~posterior_samples(plot.bm.s,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         sl.trt = purrr::map(data, ~posterior_samples(sl.s, 
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000,min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
          sg.trt = purrr::map(data, ~posterior_samples(sg.s,
                                                       pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 1000,
                                                                            min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         cde.trt = purrr::map(data, ~posterior_samples(CDE.s,
                                                       pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         rich.trt = purrr::map(data, ~posterior_samples(plot.rich.g,
                                                        pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                        exact = TRUE,
                                                        subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         bm.trt = purrr::map(data, ~posterior_samples(plot.bm.s,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()))



sl.trt.i_fixef <- fixef(sl.s)
sg.trt.i_fixef <- fixef(sg.s)
CDE.trt.i_fixef <- fixef(CDE.s)
plot.rich.im_fixef <- fixef(plot.rich.g)
plot.bm.im_fixef <- fixef(plot.bm.s)



sl_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(sl.ctl,sl.trt) %>% 
  mutate(response = 'sl',
         sl.ctl_global_slope = sl.trt.i_fixef['year.y.m','Estimate'],
         sl.ctl_upper_slope = sl.trt.i_fixef['year.y.m','Q97.5'],
         sl.ctl_lower_slope = sl.trt.i_fixef['year.y.m','Q2.5'],
         sl.trt_global_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sl.trt_upper_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sl.trt_lower_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Q2.5']) 


head(sl_posterior)
# sl_posterior$sl<-sl_posterior$sl.ctl+sl_posterior$sl.trt
# sl_posterior$sl.global<-sl_posterior$sl.ctl_global_slope + sl_posterior$sl.trt_global_slope
# sl_posterior$sl.upper<-sl_posterior$sl.ctl_upper_slope + sl_posterior$sl.trt_upper_slope
# sl_posterior$sl.lower<-sl_posterior$sl.ctl_lower_slope + sl_posterior$sl.trt_lower_slope
sl.p<-sl_posterior %>% inner_join(meta, by = 'site_code')

sl.p$starting.richness <- ifelse(sl.p$r.rich >= 1 & sl.p$r.rich <= 5, '1-5 species',
                                   ifelse(sl.p$r.rich >=6 & sl.p$r.rich <=10, '6-10',
                                          ifelse(sl.p$r.rich >=11 & sl.p$r.rich <=15, '11-15',    
                                                 ifelse(sl.p$r.rich >=16 & sl.p$r.rich <=20, '16-20',
                                                        ifelse(sl.p$r.rich >=21 & sl.p$r.rich <=25, '21-25',
                                                               ifelse(sl.p$r.rich >=26, '>26', 'other'))))))

sl.p$starting.richness <- factor(sl.p$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sl.p$anthropogenic<-as.factor(sl.p$anthropogenic)
sl.p$grazed<-as.factor(as.character(sl.p$grazed))
sl.p$managed<-as.factor(as.character(sl.p$managed))
sl.p$burned<-as.factor(as.character(sl.p$burned))

sl.p$site_rich_range <- ifelse(sl.p$site_richness >= 2 & sl.p$site_richness <= 44, '2-40 species',
                               ifelse(sl.p$site_richness >=45 & sl.p$site_richness <=69, '45-69',
                                      ifelse(sl.p$site_richness >=70 & sl.p$site_richness <=90, '70-90',    
                                             ifelse(sl.p$site_richness >=91 & sl.p$site_richness <=119, '90-119',
                                                    ifelse(sl.p$site_richness >=120 & sl.p$site_richness <=144, '120-144',
                                                           ifelse(sl.p$site_richness >=145, '>145', 'other'))))))

View(sl.p)

levels(sl.p$NDep.cats)
write.csv(sl.p,"~/Dropbox/Projects/NutNet/Data/sl.n_posteriors.csv")

# SG
sg_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(sg.ctl,sg.trt) %>% 
  mutate(response = 'sg',
         sg.ctl_global_slope = sg.trt.i_fixef['year.y.m','Estimate'],
         sg.ctl_upper_slope = sg.trt.i_fixef['year.y.m','Q97.5'],
         sg.ctl_lower_slope = sg.trt.i_fixef['year.y.m','Q2.5'],
         sg.trt_global_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sg.trt_upper_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sg.trt_lower_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],
  ) 


# sg_posterior$sg<-sg_posterior$sg.ctl+sg_posterior$sg.trt
# sg_posterior$sg.global<-sg_posterior$sg.ctl_global_slope + sg_posterior$sg.trt_global_slope
# sg_posterior$sg.upper<-sg_posterior$sg.ctl_upper_slope + sg_posterior$sg.trt_upper_slope
# sg_posterior$sg.lower<-sg_posterior$sg.ctl_lower_slope + sg_posterior$sg.trt_lower_slope
sg.p<-sg_posterior %>% inner_join(meta, by = 'site_code')


sg.p$starting.richness <- ifelse(sg.p$r.rich >= 1 & sg.p$r.rich <= 5, '1-5 species',
                                   ifelse(sg.p$r.rich >=6 & sg.p$r.rich <=10, '6-10',
                                          ifelse(sg.p$r.rich >=11 & sg.p$r.rich <=15, '11-15',    
                                                 ifelse(sg.p$r.rich >=16 & sg.p$r.rich <=20, '16-20',
                                                        ifelse(sg.p$r.rich >=21 & sg.p$r.rich <=25, '21-25',
                                                               ifelse(sg.p$r.rich >=26, '>26', 'other'))))))


sg.p$starting.richness <- factor(sg.p$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))

sg.p$anthropogenic<-as.factor(sg.p$anthropogenic)

sg.p$grazed<-as.factor(as.character(sg.p$grazed))
sg.p$managed<-as.factor(as.character(sg.p$managed))
sg.p$burned<-as.factor(as.character(sg.p$burned))

sg.p$site_rich_range <- ifelse(sg.p$site_richness >= 2 & sg.p$site_richness <= 44, '2-40 species',
                               ifelse(sg.p$site_richness >=45 & sg.p$site_richness <=69, '45-69',
                                      ifelse(sg.p$site_richness >=70 & sg.p$site_richness <=90, '70-90',    
                                             ifelse(sg.p$site_richness >=91 & sg.p$site_richness <=119, '90-119',
                                                    ifelse(sg.p$site_richness >=120 & sg.p$site_richness <=144, '120-144',
                                                           ifelse(sg.p$site_richness >=145, '>145', 'other'))))))

View(sg.p)
write.csv(sg.p,"~/Dropbox/Projects/NutNet/Data/sg_posteriors.csv")

# CDE
cde_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(cde.ctl,cde.trt) %>% 
  mutate(response = 'cde',
         cde.ctl_global_slope = CDE.trt.i_fixef['year.y.m','Estimate'],
         cde.ctl_upper_slope = CDE.trt.i_fixef['year.y.m','Q97.5'],
         cde.ctl_lower_slope = CDE.trt.i_fixef['year.y.m','Q2.5'],
         cde.trt_global_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         cde.trt_upper_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         cde.trt_lower_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],
  ) 

# cde_posterior$cde<-cde_posterior$cde.ctl+cde_posterior$cde.trt
# cde_posterior$cde.global<-cde_posterior$cde.ctl_global_slope + cde_posterior$cde.trt_global_slope
# cde_posterior$cde.upper<-cde_posterior$cde.ctl_upper_slope + cde_posterior$cde.trt_upper_slope
# cde_posterior$cde.lower<-cde_posterior$cde.ctl_lower_slope + cde_posterior$cde.trt_lower_slope
cde.p<-cde_posterior %>% inner_join(meta, by = 'site_code')


cde.p$starting.richness <- ifelse(cde.p$r.rich >= 1 & cde.p$r.rich <= 5, '1-5 species',
                                    ifelse(cde.p$r.rich >=6 & cde.p$r.rich <=10, '6-10',
                                           ifelse(cde.p$r.rich >=11 & cde.p$r.rich <=15, '11-15',    
                                                  ifelse(cde.p$r.rich >=16 & cde.p$r.rich <=20, '16-20',
                                                         ifelse(cde.p$r.rich >=21 & cde.p$r.rich <=25, '21-25',
                                                                ifelse(cde.p$r.rich >=26, '>26', 'other'))))))




cde.p$starting.richness <- factor(cde.p$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
cde.p$anthropogenic<-as.factor(cde.p$anthropogenic)
cde.p$grazed<-as.factor(as.character(cde.p$grazed))
cde.p$burned<-as.factor(as.character(cde.p$burned))

cde.p$site_rich_range <- ifelse(cde.p$site_richness >= 2 & cde.p$site_richness <= 44, '2-40 species',
                                ifelse(cde.p$site_richness >=45 & cde.p$site_richness <=69, '45-69',
                                       ifelse(cde.p$site_richness >=70 & cde.p$site_richness <=90, '70-90',    
                                              ifelse(cde.p$site_richness >=91 & cde.p$site_richness <=119, '90-119',
                                                     ifelse(cde.p$site_richness >=120 & cde.p$site_richness <=144, '120-144',
                                                            ifelse(cde.p$site_richness >=145, '>145', 'other'))))))


write.csv(cde.p,"~/Dropbox/Projects/NutNet/Data/cde_posteriors.csv")



sl.p <- read.csv("~/Dropbox/Projects/NutNet/Data/sl.n_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
colims <- read.csv("~/Dropbox/Projects/NutNet/Data/colims.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sl.p<-left_join(sl.p,colims,by="site_code")
summary(sl.p)


sl.p<-na.omit(sl.p, cols="NDep.cats")
summary(sl.p.n)
View(sl.p)
colnames(sl.p)

sl.p$anthropogenic<-as.factor(sl.p$anthropogenic)
sl.p$grazed<-as.factor(as.character(sl.p$grazed))
sl.p$managed<-as.factor(as.character(sl.p$managed))
sl.p$burned<-as.factor(as.character(sl.p$burned))
sl.p$NDep.cats <- factor(sl.p$NDep.cats , levels=c("30.01-35.91",'25.01-30.00',"20.01-25.00","15.01-20.00","10.01-15.00","5.01-10.00","2.51-5.00","1.00-2.50",'< 1','NA'))
sl.p$starting.richness <- factor(sl.p$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sl.p$site_rich_range <- factor(sl.p$site_rich_range, levels=c("2-40 species","45-69","70-90","90-119","120-144",">145"))

levels(sl.p$NDep.cats)

sl<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = sl.p %>% distinct(sl.trt_lower_slope, sl.trt_upper_slope),
            aes(xmin = sl.trt_lower_slope, xmax =  sl.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  # geom_density_ridges(data = sl.p,
  #                     aes(x = sl.ctl + unique(sl.ctl_global_slope),
  #                         # y = anthropogenic ,
  #                         # y = managed,
  #                         y = grazed,
  #                         color= "grey"
  #                     ),
  #                     scale = 1, alpha = 0.6,
  #                     linetype = 0) +
  geom_density_ridges(data = sl.p,
                      aes(x = sl.trt + unique(sl.trt_global_slope), 
                          #y = anthropogenic ,
                           #y = grazed , 
                          #y= managed ,
                         #y = site_dom,
                          #y = Realm,
                          #y = bioregion,
                          #y = biome,
                          #y= colimitation,
                          y= NDep.cats,
                         #fill=anthropogenic
                           #fill = starting.richness
                          #fill = site_dom
                           #fill= site_rich_range
                           #fill= NDep.cats
                          #fill= Realm
                          #fill= biome
                          fill=colimitation
                          ), 
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis(name = #'site_rich_range',
                      # 'starting.richness' ,  
                       #'site_rich_range',
                       #'anthropogenic',
                       #'NDep.cats',
                      #'biome' ,
                      #'site_dom',
                      #'Realm',
                     'colimitation',
                      discrete=TRUE) +
  # scale_fill_manual(values = c("1-5 species" = "#E5BA3AFF",
  #                              "6-10" = "#75B41EFF",
  #                              "11-15" ="#5AC2F1FF",
  #                              "16-20"= "#0C5BB0FF",
  #                              "21-25" = "#972C8DFF",
  #                              ">26" = "#E0363AFF", drop =FALSE))+
  geom_vline(data = sl.p,
             aes(xintercept = sl.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(
       x = 'Species Loss',
       title= 'Biomass Change') +
  #xlim(-0.50,0.50) +
  #scale_x_continuous(trans = reverse_trans()) +
  theme(panel.grid = element_blank(),
        #axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="none")

sl

sg.p <- read.csv("~/Dropbox/Projects/NutNet/Data/sg_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
colims <- read.csv("~/Dropbox/Projects/NutNet/Data/colims.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

sg.p<-left_join(sg.p,colims,by="site_code")


colnames(sg.p)
View(sg.p)
sg.p$starting.richness <- factor(sg.p$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
sg.p$anthropogenic<-as.factor(sg.p$anthropogenic)
sg.p$grazed<-as.factor(as.character(sg.p$grazed))
sg.p$managed<-as.factor(as.character(sg.p$managed))
sg.p$burned<-as.factor(as.character(sg.p$burned))
sg.p$NDep.cats <- factor(sg.p$NDep.cats , levels=c("30.01-35.91",'25.01-30.00',"20.01-25.00","15.01-20.00","10.01-15.00","5.01-10.00","2.51-5.00","1.00-2.50",'< 1','NA'))
sg.p$site_rich_range <- factor(sg.p$site_rich_range, levels=c("2-40 species","45-69","70-90","90-119","120-144",">145"))
  
levels(sg.p$NDep.cats)
head(sg.p)

sg.p<-na.omit(sg.p, cols="NDep.cats")

sg<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = sg.p %>% distinct(sg.trt_lower_slope, sg.trt_upper_slope),
            aes(xmin = sg.trt_lower_slope, xmax =  sg.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  # geom_density_ridges(data = sg.p,
  #                     aes(x = sg.ctl + unique(sg.ctl_global_slope),
  #                         y = anthropogenic,
  #                         color = "grey"
  #                     ),
  #                     scale = 1, alpha = 0.6,
  #                     linetype = 0) +
  geom_density_ridges(data = sg.p,
                      aes(x = sg.trt + unique(sg.trt_global_slope), 
                           #y = anthropogenic,
                           #y = grazed, 
                          #y= managed,
                          #y = site_dom,
                          #y = Realm,
                          #y = bioregion,
                          #y = biome,
                          y= NDep.cats,
                          #y=colimitation,
                          #fill=anthropogenic
                          fill=colimitation
                          # fill = starting.richness
                          #fill = site_dom
                          #fill= site_rich_range
                          #fill= NDep.cats
                          #fill= Realm
                          #fill=biome
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis(name = #'site_dom'
                         # 'starting.richness' ,
                      # 'colimitation',
                       #'site_rich_range',
                        # 'NDep.cats',
                       #'Realm',
                       'biome',
                       #'anthropogenic',
                       discrete=TRUE) +
  # scale_fill_manual(values = c("1-5 species" = "#E5BA3AFF",
  #                              "6-10" = "#75B41EFF",
  #                              "11-15" ="#5AC2F1FF",
  #                              "16-20"= "#0C5BB0FF",
  #                              "21-25" = "#972C8DFF",
  #                              ">26" = "#E0363AFF", drop =FALSE))+
  geom_vline(data = sg.p,
             aes(xintercept = sg.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y='',
       x = 'Species Gains',
       title= '') +
  #xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom"
        )

sg

cde.p <- read.csv("~/Dropbox/Projects/NutNet/Data/cde_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
colims <- read.csv("~/Dropbox/Projects/NutNet/Data/colims.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

cde.p<-left_join(cde.p,colims,by="site_code")


head(cde.p)
cde.p$starting.richness <- factor(cde.p$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
cde.p$anthropogenic<-as.factor(cde.p$anthropogenic)

cde.p$grazed<-as.factor(as.character(cde.p$grazed))
cde.p$managed<-as.factor(as.character(cde.p$managed))
cde.p$burned<-as.factor(as.character(cde.p$burned))
cde.p$NDep.cats <- factor(cde.p$NDep.cats , levels=c("30.01-35.91",'25.01-30.00',"20.01-25.00","15.01-20.00","10.01-15.00","5.01-10.00","2.51-5.00","1.00-2.50",'< 1','NA'))
cde.p$site_rich_range <- factor(cde.p$site_rich_range, levels=c("2-40 species","45-69","70-90","90-119","120-144",">145"))

levels(cde.p$NDep.cats)

View(cde.p)
cde.p<-na.omit(cde.p, cols="NDep.cats")

cde<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = cde.p %>% distinct(cde.trt_lower_slope, cde.trt_upper_slope),
            aes(xmin = cde.trt_lower_slope, xmax =  cde.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  # geom_density_ridges(data = cde.p,
  #                     aes(x = cde.ctl + unique(cde.ctl_global_slope),
  #                         y = anthropogenic,
  #                         color = "grey"
  #                     ),
  #                     scale = 1, alpha = 0.6,
  #                     linetype = 0) +
  geom_density_ridges(data = cde.p,
                      aes(x = cde.trt + unique(cde.trt_global_slope), 
                          #y = interaction(anthropogenic,site_dom),
                          #y = anthropogenic,
                           #y = grazed , 
                          #y = site_dom,
                          #y = managed ,
                          #y = Realm,
                          #y = bioregion,
                          #y = biome,
                          #y=colimitation,
                          y=NDep.cats,
                          #y = starting.richness
                          fill=colimitation
                          #fill = site_rich_range
                           #fill = starting.richness
                          # fill = site_dom
                          # fill= site_rich_range
                           #fill = NDep.cats
                          #fill=biome
                          #fill= Realm
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis(name = #'site_dom'
                        #'starting.richness' ,
                      'colimitation',
                       # 'site_rich_range',
                       #'biome', 
                       #'NDep.cats',
                         #'Realm',
                       discrete=TRUE) +
  # scale_fill_manual(values = c("1-5 species" = "#E5BA3AFF",
  #                              "6-10" = "#75B41EFF",
  #                              "11-15" ="#5AC2F1FF",
  #                              "16-20"= "#0C5BB0FF",
  #                              "21-25" = "#972C8DFF",
  #                              ">26" = "#E0363AFF", drop =FALSE))+
  geom_vline(data = cde.p,
             aes(xintercept = cde.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs( y = '' ,
       x = 'Persistent Species',
       title= ''
       ) +
  xlim(-150,150) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="none")

cde

grid_arrange_shared_legend(sl,sg,cde,nrow=1,ncol=3)


get_legend<-function(myggplot){
  tmp <- ggplot_gtable(ggplot_build(myggplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)
}

legend <- get_legend(sg)
# 3. Remove the legend from the plot
#+++++++++++++++++++++++
sg <- sg + theme(legend.position="none")
# 4. Arrange ggplot2 graphs with a specific width
grid.arrange(sl, sg, cde, ncol=3, widths=c(3.2, 2.3, 2.3))

grid.arrange(sl, sg, cde, legend, ncol=3, nrow = 2, 
             layout_matrix = rbind(c(1,2,3), c(4,4,4)),
             widths = c(3.4, 2.3, 2.3), heights = c(3.5, 0.4))

#########################
#########################
# BIOMASS RICHNESS MODELS
#########################
#########################

#plot.rich.im_fixef
rich_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(rich.ctl,rich.trt) %>% 
  mutate(response = 'rich',
         rich.ctl_global_slope = plot.rich.im_fixef['year_trt','Estimate'],
         rich.ctl_upper_slope = plot.rich.im_fixef['year_trt','Q97.5'],
         rich.ctl_lower_slope = plot.rich.im_fixef['year_trt','Q2.5'],
         rich.trt_global_slope = plot.rich.im_fixef['trtNPK:year_trt','Estimate'],
         rich.trt_upper_slope = plot.rich.im_fixef['trtNPK:year_trt','Q97.5'],
         rich.trt_lower_slope = plot.rich.im_fixef['trtNPK:year_trt','Q2.5'],
  ) 

# View(cde_posterior)
# rich.ctl <- c("site_code","rich.ctl" , "response","rich.ctl_global_slope", "rich.ctl_upper_slope" , "rich.ctl_lower_slope")
# rich_posterior.ctl <- rich_posterior[rich.ctl]
# rich.trt <- c("site_code","rich.trt" , "response","rich.trt_global_slope", "rich.trt_upper_slope" , "rich.trt_lower_slope")
# rich_posterior.trt <- rich_posterior[rich.trt]


# rich_posterior$rich<-rich_posterior$rich.ctl+rich_posterior$rich.trt
# rich_posterior$rich.global<-rich_posterior$rich.ctl_global_slope + rich_posterior$rich.trt_global_slope
# rich_posterior$rich.upper<-rich_posterior$rich.ctl_upper_slope + rich_posterior$rich.trt_upper_slope
# rich_posterior$rich.lower<-rich_posterior$rich.ctl_lower_slope + rich_posterior$rich.trt_lower_slope
rich.p<-rich_posterior %>% inner_join(meta, by = 'site_code')


rich.p$starting.richness <- ifelse(rich.p$r.rich >= 1 & rich.p$r.rich <= 5, '1-5 species',
                                     ifelse(rich.p$r.rich >=6 & rich.p$r.rich <=10, '6-10',
                                            ifelse(rich.p$r.rich >=11 & rich.p$r.rich <=15, '11-15',    
                                                   ifelse(rich.p$r.rich >=16 & rich.p$r.rich <=20, '16-20',
                                                          ifelse(rich.p$r.rich >=21 & rich.p$r.rich <=25, '21-25',
                                                                 ifelse(rich.p$r.rich >=26, '>26', 'other'))))))


rich.p$starting.richness <- factor(rich.p$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
rich.p$anthropogenic<-as.factor(rich.p$anthropogenic)
rich.p$grazed<-as.factor(as.character(rich.p$grazed))
rich.p$managed<-as.factor(as.character(rich.p$managed))
rich.p$burned<-as.factor(as.character(rich.p$burned))

rich.p$site_rich_range <- ifelse(rich.p$site_richness >= 2 & rich.p$site_richness <= 44, '2-40 species',
                                 ifelse(rich.p$site_richness >=45 & rich.p$site_richness <=69, '45-69',
                                        ifelse(rich.p$site_richness >=70 & rich.p$site_richness <=90, '70-90',    
                                               ifelse(rich.p$site_richness >=91 & rich.p$site_richness <=119, '90-119',
                                                      ifelse(rich.p$site_richness >=120 & rich.p$site_richness <=144, '120-144',
                                                             ifelse(rich.p$site_richness >=145, '>145', 'other'))))))


write.csv(rich.p,"~/Dropbox/Projects/NutNet/Data/rich_posteriors.csv")

# Biomass
bm_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest_legacy(bm.ctl,bm.trt) %>% 
  mutate(response = 'bm',
         bm.ctl_global_slope = plot.bm.im_fixef['year_trt','Estimate'],
         bm.ctl_upper_slope = plot.bm.im_fixef['year_trt','Q97.5'],
         bm.ctl_lower_slope = plot.bm.im_fixef['year_trt','Q2.5'],
         bm.trt_global_slope = plot.bm.im_fixef['trtNPK:year_trt','Estimate'],
         bm.trt_upper_slope = plot.bm.im_fixef['trtNPK:year_trt','Q97.5'],
         bm.trt_lower_slope = plot.bm.im_fixef['trtNPK:year_trt','Q2.5'],
  ) 

# bm.ctl <- c("site_code","bm.ctl" , "response","bm.ctl_global_slope", "bm.ctl_upper_slope" , "bm.ctl_lower_slope")
# bm_posterior.ctl <- bm_posterior[bm.ctl]
# bm.trt <- c("site_code","bm.trt" , "response","bm.trt_global_slope", "bm.trt_upper_slope" , "bm.trt_lower_slope")
# bm_posterior.trt <- bm_posterior[bm.trt]
# 
# 
# bm_posterior$bm<-bm_posterior$bm.ctl+bm_posterior$bm.trt
# bm_posterior$bm.global<-bm_posterior$bm.ctl_global_slope + bm_posterior$bm.trt_global_slope
# bm_posterior$bm.upper<-bm_posterior$bm.ctl_upper_slope + bm_posterior$bm.trt_upper_slope
# bm_posterior$bm.lower<-bm_posterior$bm.ctl_lower_slope + bm_posterior$bm.trt_lower_slope
bm.p<-bm_posterior %>% inner_join(meta, by = 'site_code')

# View(bm.p.c)


bm.p$starting.richness <- ifelse(bm.p$r.rich >= 1 & bm.p$r.rich <= 5, '1-5 species',
                                   ifelse(bm.p$r.rich >=6 & bm.p$r.rich <=10, '6-10',
                                          ifelse(bm.p$r.rich >=11 & bm.p$r.rich <=15, '11-15',    
                                                 ifelse(bm.p$r.rich >=16 & bm.p$r.rich <=20, '16-20',
                                                        ifelse(bm.p$r.rich >=21 & bm.p$r.rich <=25, '21-25',
                                                               ifelse(bm.p$r.rich >=26, '>26', 'other'))))))

nrow(bm_posterior)
nrow(rich_posterior)

bm.p$starting.richness <- factor(bm.p$starting.richness , levels=c("1-5 species","6-10","11-15","16-20","21-25",">26"))
bm.p$anthropogenic<-as.factor(bm.p$anthropogenic)
bm.p$grazed<-as.factor(as.character(bm.p$grazed))
bm.p$managed<-as.factor(as.character(bm.p$managed))
bm.p$burned<-as.factor(as.character(bm.p$burned))

bm.p$site_rich_range <- ifelse(bm.p$site_richness >= 2 & bm.p$site_richness <= 44, '2-40 species',
                               ifelse(bm.p$site_richness >=45 & bm.p$site_richness <=69, '45-69',
                                      ifelse(bm.p$site_richness >=70 & bm.p$site_richness <=90, '70-90',    
                                             ifelse(bm.p$site_richness >=91 & bm.p$site_richness <=119, '90-119',
                                                    ifelse(bm.p$site_richness >=120 & bm.p$site_richness <=144, '120-144',
                                                           ifelse(bm.p$site_richness >=145, '>145', 'other'))))))

write.csv(bm.p,"~/Dropbox/Projects/NutNet/Data/bm_posteriors.csv")



rich.p <- read.csv("~/Dropbox/Projects/NutNet/Data/rich_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

colnames(rich.p)


levels(rich.p$NDep.cats)

rich.p$NDep.cats <- factor(rich.p$NDep.cats , levels=c("30.01-35.91","20.01-25.00","15.01-20.00","10.01-15.00","5.01-10.00","2.51-5.00","1.00-2.50",'< 1'))

rh<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = rich.p %>% distinct(rich.trt_lower_slope, rich.trt_upper_slope),
            aes(xmin = rich.trt_lower_slope, xmax =  rich.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  # geom_density_ridges(data = rich.p,
  #                     aes(x = rich.ctl + unique(rich.ctl_global_slope),
  #                         y = anthropogenic,
  #                         color= "grey"
  #                     ),
  #                     scale = 1, alpha = 0.6,
  #                     linetype = 0) +
  geom_density_ridges(data = rich.p,
                      aes(x = rich.trt + unique(rich.trt_global_slope), 
                          #y = anthropogenic,
                           y = grazed, 
                          # y= managed,
                          #y = site_dom,
                           fill = starting.richness
                           #fill = site_dom
                           #fill = site_rich_range
                           #fill = NDep.cats
                          #fill = Realm
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = #'site_dom'
                          'starting.richness' 
                          #'site_rich_range'
                          #'NDep.cats'
                         #'Realm'
                       ) +
  # scale_fill_manual(values = c("1-5 species" = "#E5BA3AFF",
  #                              "6-10" = "#75B41EFF",
  #                              "11-15" ="#5AC2F1FF",
  #                              "16-20"= "#0C5BB0FF",
  #                              "21-25" = "#972C8DFF",
  #                              ">26" = "#E0363AFF", drop =FALSE))+
  geom_vline(data = rich.p,
             aes(xintercept = rich.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'Grazed',
       x = 'Richness') +
  #xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")

rf


bm.p <- read.csv("~/Dropbox/Projects/NutNet/Data/bm_posteriors.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

bm.p$NDep.cats <- factor(bm.p$NDep.cats , levels=c("30.01-35.91","20.01-25.00","15.01-20.00","10.01-15.00","5.01-10.00","2.51-5.00","1.00-2.50",'< 1'))
View(bm.p)


colnames(bm.p)
bh<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = bm.p %>% distinct(bm.trt_lower_slope, bm.trt_upper_slope),
            aes(xmin = bm.trt_lower_slope, xmax =  bm.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  # geom_density_ridges(data = bm.p,
  #                     aes(x = bm.ctl + unique(bm.ctl_global_slope),
  #                         y = anthropogenic,
  #                         color= "grey"
  #                     ),
  #                     scale = 1, alpha = 0.6,
  #                     linetype = 0) +
  geom_density_ridges(data = bm.p,
                      aes(x = bm.trt + unique(bm.trt_global_slope), 
                          #y = anthropogenic,
                           y = grazed , 
                          # y= managed ,
                          #y= site_dom,
                           fill = starting.richness
                          # fill = site_dom
                           #fill= site_rich_range
                           #fill= NDep.cats
                          #fill= Realm
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = #'site_dom'
                          'starting.richness' 
                         #'site_rich_range'
                         #'NDep.cats'
                         #'Realm'
                         ) +
  # scale_fill_manual(values = c("1-5 species" = "#E5BA3AFF",
  #                              "6-10" = "#75B41EFF",
  #                              "11-15" ="#5AC2F1FF",
  #                              "16-20"= "#0C5BB0FF",
  #                              "21-25" = "#972C8DFF",
  #                              ">26" = "#E0363AFF", drop =FALSE))+
   geom_vline(data = bm_posterior,
             aes(xintercept = bm.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'Grazed',
       x = 'Biomass') +
  #xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")
bf

grid_arrange_shared_legend(ra,rh,rs,ba,bh,bs,nrow=2,ncol=3)

# ANTHROPOGENIC
# HERBIVORY
# EXOTIC VS. NATIVE DOMINATED

# Starting Richness
# SITE DIVERSITY
# BIOGEO / CLIMATE REALM
# N. DEPOSITION

# CO-LIMITED (STAN'S PAPER--show him coef effects and ask how he would determine) PLOTS WITH EFFECT VS. PLOTS WITH NO EFFECT


# FACETED AS LOSSES, GAINS, CDE? WITH POSTERIORS GROUPED AS ABOVE IN DIFF COLOURS
# CONTROLS GREY IN THE BACKGROUND


