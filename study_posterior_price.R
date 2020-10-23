


load('~/Dropbox/Projects/NutNet/Data/study.p.effs.Rdata')

sloss.t <- study.sloss.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(sloss.trt.rate.p = eff) %>%
  select(-response,-eff)

sloss.c <- study.sloss.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(sloss.ctl.rate.p = eff) %>%
  select(-response,-eff)

sloss.eff <- left_join(sloss.t,sloss.c)

sgain.t <- study.sgain.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(sgain.trt.rate.p = eff) %>%
  select(-response,-eff)

sgain.c <- study.sgain.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(sgain.ctl.rate.p = eff) %>%
  select(-response,-eff)

sgain.eff <- left_join(sgain.t,sgain.c)


sl.t <- study.sl.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(sl.trt.rate.p = eff) %>%
  select(-response,-eff)

sl.c <- study.sl.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(sl.ctl.rate.p = eff) %>%
  select(-response,-eff)

sl.eff <- left_join(sl.t,sl.c)

sg.t <- study.sg.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(sg.trt.rate.p = eff) %>%
  select(-response,-eff)

sg.c <- study.sg.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(sg.ctl.rate.p = eff) %>%
  select(-response,-eff)

sg.eff <- left_join(sg.t,sg.c)

cde.t <- study.cde.p %>% select(site_code,eff,response) %>% filter(response == "NPK") %>%
  mutate(cde.trt.rate.p = eff) %>%
  select(-response,-eff)

cde.c <- study.cde.p %>% select(site_code,eff,response) %>% filter(response == "Control") %>%
  mutate(cde.ctl.rate.p = eff) %>%
  select(-response,-eff)

cde.eff <- left_join(cde.t,cde.c)


sloss.sgain.effs<- left_join(sloss.eff,sgain.eff)

sg.sl.eff<-left_join(sg.eff,sl.eff)


price.eff<-left_join(sg.sl.eff,cde.eff)


all.effs <- left_join(price.eff,sloss.sgain.effs)



study.price.cloud<-ggplot()+
  facet_wrap(~site_code) +
  geom_vline(xintercept = 0, linetype="longdash") + geom_hline(yintercept = 0,linetype="longdash") + theme_classic()+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_rect(colour="black", fill="white"),legend.position="bottom")+
  #treatment effects
  geom_segment(data = all.effs,
               aes(x = 0,
                   xend = sloss.trt.rate.p  ,
                   y = 0,
                   yend = sl.trt.rate.p   ),
               colour= "#B40F20",
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = all.effs,
               aes(x = sloss.trt.rate.p ,
                   xend = (sloss.trt.rate.p)+(sgain.trt.rate.p ) ,
                   y = sl.trt.rate.p ,
                   yend = (sl.trt.rate.p)+(sg.trt.rate.p  ) ),
               colour= "#046C9A",
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  geom_segment(data = all.effs,
               aes(x = (sloss.trt.rate.p)+(sgain.trt.rate.p),
                   xend = (sloss.trt.rate.p)+(sgain.trt.rate.p),
                   y = (sl.trt.rate.p)+(sg.trt.rate.p ),
                   yend =(sl.trt.rate.p)+(sg.trt.rate.p)+ (cde.trt.rate.p  )),
               colour= "#F98400",
               arrow=arrow(type="closed",length=unit(0.1,"cm"))) +
  labs(x = 'Rate of change in species (species/year)',
       y = expression(paste('Rate of change in biomass (g/' ,m^2, '/year)')),
       # title= 'Rate of change / year '
       title = '')


study.price.cloud

