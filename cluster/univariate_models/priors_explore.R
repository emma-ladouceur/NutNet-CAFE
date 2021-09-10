
library(tidyverse)
library(brms)
library(rstan)
library(rstanarm)

# price partitions
p.all <- read.csv('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv', header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

p.all$site_code<-as.factor(p.all$site_code)
p.all$site.year.id<-as.factor(p.all$site.year.id)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)

p.all <- p.all %>% group_by(site_code) %>% filter(year_max >= 3) %>%
  ungroup()

head(p.all)

sd(p.all$SL, na.rm=T)

get_prior(SL ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
          data = p.all,
          family = student())


# same as sl_sb
sl.3_test2_nu10 <- brm(SL ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
                 data = p.all, family=student(), cores = 4, chains = 4#,
                 # prior = c(
                 #   prior(normal(-22,30), class = Intercept),
                 #   prior(normal(-30,10), class = b, coef = trt.yNPK),
                 #           prior(normal(0,10), class = b, coef = year.y.m),
                 #           prior(normal(0,10), class = b, coef = trt.yNPK:year.y.m),
                 #           prior(normal(0,10), class = sd),
                 #           prior(normal(0,10), class = sigma),
                 #           prior(constant(10), class = nu)),
                 # control = list(max_treedepth = 12),
                 # sample_prior = 'only'
                 )

#pp_check(sl.3_test2_nu10)


get_prior(SG ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
          data = p.all,
          family = student())


get_prior(CDE ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
          data = p.all,
          family = student())


get_prior(s.loss.n ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
          data = p.all,
          family = student())


get_prior(s.gain ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
          data = p.all,
          family = student())



# biomass richness plot level stuff
plot <- read.csv('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv', header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))



get_prior(rich ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
          data = plot)


get_prior(strip.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
          data = plot,
          family = student())





library(tidyverse)
library(brms)
library(cmdstanr)

p.all <- read.csv('~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/nutnet_cumulative_time.csv', header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

p.all$site_code<-as.factor(p.all$site_code)
p.all$site.year.id<-as.factor(p.all$site.year.id)
p.all$block<-as.factor(p.all$block)
p.all$plot<-as.factor(p.all$plot)

p.all <- p.all %>% group_by(site_code) %>% filter(year_max >= 3) %>%
  ungroup()

plot(density(rgamma(1000, 2, scale = 1/0.1))) # default
plot(density(rgamma(1000, 2, scale = 0.1))) # 
plot(density(rgamma(1000, 1, scale = 1/0.1))) # slender
plot(density(rgamma(1000, 1, scale = 1/0.2))) # slender

sl.3_test <- brm(SL ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
                 data = p.all, family=student(), cores = 4, chains = 4,
                 iter = 5000, warmup = 1000,
                 prior = c(
                   prior(normal(-22,30), class = Intercept),
                   prior(normal(-30,10), class = b, coef = trt.yNPK),
                   prior(normal(0,10), class = b, coef = year.y.m),
                   prior(normal(0,10), class = b, coef = trt.yNPK:year.y.m),
                   prior(normal(0,10), class = sd),
                   prior(normal(0,10), class = sigma),
                   prior(gamma(1,0.1), class = nu)),
                  control = list(adapt_delta = 0.99),
                 #sample_prior = 'only',
                 backend = 'cmdstanr'
                )



fig_s5e <- pp_check(sl.3_p) + theme_classic() + 
  labs( title = "e)",
        x= expression(paste(atop(paste('Biomass change (g/' ,m^2, ') / year'), 'associated with species sloss (SL)'))),
        #expression(paste('Biomass change (g/' ,m^2, ') \n associated with species sloss (SL)')), 
        y = "Density") + 
  scale_x_continuous(limits = c(-1300, 100)) + theme(legend.position="none")

fig_s5e


# adapt_delta = 0.9 = 7 divergent transitions
# adapt_delta = 0.99 = no divergent transitions, but no convergence either

summary(sl.3_test)

mcmc_plot(sl.3_test, type = 'nuts_divergence')
pp_check(sl.3_test)
pairs(sl.3_test)


nuts_params(sl.3_test_p12) %>% 
  filter(Parameter == "divergent__") %>% 
  count(Value)


sg.3_p <- brm(SG ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot),
                 data = p.all, family=student(), cores = 4, chains = 4,
                 iter = 5000, warmup = 1000,
                 # prior = c(
                 #   prior(normal(22,33), class = Intercept),
                 #   prior(normal(33,10), class = b, coef = trt.yNPK),
                 #   prior(normal(0,10), class = b, coef = year.y.m),
                 #   prior(normal(0,10), class = b, coef = trt.yNPK:year.y.m),
                 #   prior(normal(0,10), class = sd),
                 #   prior(normal(0,10), class = sigma),
                 #   prior(constant(10), class = nu)),
                 control = list(adapt_delta = 0.99),
                 #sample_prior = 'only',
                 backend = 'cmdstanr'
)


summary(sg.3_p)


fig_s5f <- pp_check(sg.3_p) + theme_classic() + 
  labs(title = "f)",
       x= expression(paste(atop(paste('Biomass change (g/' ,m^2, ') / year'), 'associated with species gain (SG)'))),
       y = "") + 
  scale_x_continuous(limits = c(-100, 1300)) + theme(legend.position="none")

fig_s5f



cde.3_p <- brm(CDE ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot), 
               data = p.all, family=student(), cores = 4, chains = 4,
                iter = 5000, warmup = 1000,
               prior = c(
                 prior(normal(41,160), class = Intercept),
                 prior(normal(160,10), class = b, coef = trt.yNPK),
                 prior(normal(0,10), class = b, coef = year.y.m),
                 prior(normal(0,10), class = b, coef = trt.yNPK:year.y.m),
                 prior(normal(0,10), class = sd),
                 prior(normal(0,10), class = sigma),
                 prior(constant(10), class = nu)),
               control = list(adapt_delta = 0.99),
               #sample_prior = 'only',
               backend = 'cmdstanr'
)


summary(cde.3_p)



sloss.3_p <- brm(s.loss.n ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot), 
                 data = p.all, family=student(), cores = 4, chains = 4,
                 iter=5000, warmup = 1000,
                 prior = c(
                   prior(normal(-4,3), class = Intercept),
                   prior(normal(3,1), class = b, coef = trt.yNPK),
                   prior(normal(0,1), class = b, coef = year.y.m),
                   prior(normal(0,1), class = b, coef = trt.yNPK:year.y.m),
                   prior(normal(0,1), class = sd),
                   prior(normal(0,1), class = sigma),
                   prior(constant(1), class = nu)),
                 control = list(adapt_delta = 0.99),
                 #sample_prior = 'only',
                 backend = 'cmdstanr'
)

summary(sloss.3_p)
#needs increased tree depth?
# doesnt even need priors?


sgain.3_p <- brm(s.gain ~  trt.y * year.y.m + (trt.y * year.y.m |  site_code/block/plot), 
                 data = p.all, family=student(), cores = 4, chains = 4,
                 iter=5000, warmup = 1000,
                 prior = c(
                   prior(normal(3,3), class = Intercept),
                   prior(normal(3,1), class = b, coef = trt.yNPK),
                   prior(normal(0,1), class = b, coef = year.y.m),
                   prior(normal(0,1), class = b, coef = trt.yNPK:year.y.m),
                   prior(normal(0,1), class = sd),
                   prior(normal(0,1), class = sigma),
                   prior(constant(1), class = nu)),
                 control = list(adapt_delta = 0.99),
                 #sample_prior = 'only',
                 backend = 'cmdstanr'
)


summary(sgain.3_p)


plot <- read.csv("~/GRP GAZP Dropbox/Emma Ladouceur/_Projects/NutNet/Data/plot.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot$site_code<-as.factor(plot$site_code)
plot$block<-as.factor(plot$block)
plot$plot<-as.factor(plot$plot)

plot <- plot %>% group_by(site_code) %>% filter(year_max >= 3) %>%
  ungroup()

bm.3_p <- brm(strip.mass ~ trt * year_trt + (trt * year_trt | site_code/block/plot), 
              data = plot, family=student(), cores = 4, chains = 4,
              iter=5000, warmup = 1000,
              prior = c(
                prior(normal(285,287), class = Intercept),
                prior(normal(257,10), class = b, coef = trtNPK),
                prior(normal(0,10), class = b, coef = year_trt),
                prior(normal(0,10), class = b, coef = trtNPK:year_trt),
                prior(normal(0,10), class = sd),
                prior(normal(0,10), class = sigma),
                prior(constant(10), class = nu)),
              control = list(adapt_delta = 0.99),
              #sample_prior = 'only',
              backend = 'cmdstanr'
)

summary(bm.3_p)


rich.3_p <- brm(rich ~ trt * year_trt + (trt * year_trt | site_code/block/plot),
                data = plot, cores = 4, chains = 4,
                iter=5000, warmup = 1000,
                prior = c(
                  prior(normal(8,5), class = Intercept),
                  prior(normal(5,1), class = b, coef = trtNPK),
                  prior(normal(0,1), class = b, coef = year_trt),
                  prior(normal(0,1), class = b, coef = trtNPK:year_trt),
                  prior(normal(0,1), class = sd),
                  prior(normal(0,1), class = sigma)
                  ),
                control = list(adapt_delta = 0.99),
                #sample_prior = 'only',
                backend = 'cmdstanr'
)

summary(rich.3_p)






