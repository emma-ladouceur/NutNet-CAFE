


# get study-level posterior samples
library(tidyverse)
library(brms)
library(ggridges)
library(gridExtra)
library(grid)



# TIDYVERSE SHARED LEGEND
#https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs

grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
  
}


setwd('~/Dropbox/Projects/NutNet/Model_fits/')

#model names plot.rich.im,plot.bm.im,
load('~/Dropbox/Projects/NutNet/Model_fits/plot.nutnet.i.models.Rdata')
# model name sl.trt.i,sg.trt.i,CDE.trt.i
load('~/Dropbox/Projects/NutNet/Model_fits/price_trt_interact_time.Rdata')

#shane
#model names plot.rich.im,plot.bm.im,
load('~/Dropbox/NutNet/Model_fits/plot.nutnet.i.models.Rdata')
# model name sl.trt.i,sg.trt.i,CDE.trt.i
load('~/Dropbox/NutNet/Model_fits/price_trt_interact_time.Rdata')


# get the metadata (I want to group the posteriors)
dat <- read.csv('~/Dropbox/Projects/NutNet/Data/price_models.csv', sep=',') %>% 
  as_tibble() 
#shane
dat <- read.csv('~/Dropbox/NutNet/Data/price_models.csv', sep=',') %>% 
  as_tibble() 

meta<-distinct(dat,site_code, continent, habitat)
View(meta)
# study-levels (use model with fewest missing values)
study_levels <- plot.rich.im$data %>% 
  as_tibble() %>% 
  distinct(site_code) %>% 
  mutate(level =  site_code) %>%
  nest(level)

parnames(plot.rich.im)
study_sample_posterior <- study_levels %>%
  mutate(sl.ctl = purrr::map(data, ~posterior_samples(sl.trt.i, 
                                                     pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                     exact = TRUE,
                                                     subset = floor(runif(n = 1000,
                                                                          min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         sg.ctl = purrr::map(data, ~posterior_samples(sg.trt.i, 
                                                    pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                    exact = TRUE,
                                                    subset = floor(runif(n = 1000,
                                                                         min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         cde.ctl = purrr::map(data, ~posterior_samples(CDE.trt.i,
                                                  pars = paste('r_site_code[', as.character(.x$level), ',year.y.m]', sep=''),
                                                  exact = TRUE,
                                                  subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         rich.ctl = purrr::map(data, ~posterior_samples(plot.rich.im,
                                                     pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
                                                     exact = TRUE,
                                                     subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         bm.ctl = purrr::map(data, ~posterior_samples(plot.bm.im,
                                                    pars = paste('r_site_code[', as.character(.x$level), ',year_trt]', sep=''),
                                                    exact = TRUE,
                                                    subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         sl.trt = purrr::map(data, ~posterior_samples(sl.trt.i, 
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000,min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         sg.trt = purrr::map(data, ~posterior_samples(sg.trt.i, 
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000,
                                                                           min = 1, max = 2000))) %>% unlist() %>% as.numeric()),
         cde.trt = purrr::map(data, ~posterior_samples(CDE.trt.i,
                                                       pars = paste('r_site_code[', as.character(.x$level), ',trt.yNPK:year.y.m]', sep=''),
                                                       exact = TRUE,
                                                       subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         rich.trt = purrr::map(data, ~posterior_samples(plot.rich.im,
                                                        pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                        exact = TRUE,
                                                        subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()),
         bm.trt = purrr::map(data, ~posterior_samples(plot.bm.im,
                                                      pars = paste('r_site_code[', as.character(.x$level), ',trtNPK:year_trt]', sep=''),
                                                      exact = TRUE,
                                                      subset = floor(runif(n = 1000, 1, max = 2000))) %>%  unlist() %>%  as.numeric()))

View(study_sample_posterior)
View(sl.trt.i_fixef)

sl.trt.i_fixef <- fixef(sl.trt.i)
sg.trt.i_fixef <- fixef(sg.trt.i)
CDE.trt.i_fixef <- fixef(CDE.trt.i)
plot.rich.im_fixef <- fixef(plot.rich.im)
plot.bm.im_fixef <- fixef(plot.bm.im)



sl_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest(sl.ctl,sl.trt) %>% 
  mutate(response = 'sl',
         sl.ctl_global_slope = sl.trt.i_fixef['year.y.m','Estimate'],
         sl.ctl_upper_slope = sl.trt.i_fixef['year.y.m','Q97.5'],
         sl.ctl_lower_slope = sl.trt.i_fixef['year.y.m','Q2.5'],
         sl.trt_global_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sl.trt_upper_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sl.trt_lower_slope = sl.trt.i_fixef['trt.yNPK:year.y.m','Q2.5']) %>% 
  left_join(meta, 
           by = 'site_code')


View(sl_posterior)


View(sg.trt.i_fixef)
sg_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest(sg.ctl,sg.trt) %>% 
  mutate(response = 'sg',
         sg.ctl_global_slope = sg.trt.i_fixef['year.y.m','Estimate'],
         sg.ctl_upper_slope = sg.trt.i_fixef['year.y.m','Q97.5'],
         sg.ctl_lower_slope = sg.trt.i_fixef['year.y.m','Q2.5'],
         sg.trt_global_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         sg.trt_upper_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         sg.trt_lower_slope = sg.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],
         ) %>% 
  left_join(meta, 
            by = 'site_code')

cde_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest(cde.ctl,cde.trt) %>% 
  mutate(response = 'cde',
         cde.ctl_global_slope = CDE.trt.i_fixef['year.y.m','Estimate'],
         cde.ctl_upper_slope = CDE.trt.i_fixef['year.y.m','Q97.5'],
         cde.ctl_lower_slope = CDE.trt.i_fixef['year.y.m','Q2.5'],
         cde.trt_global_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Estimate'],
         cde.trt_upper_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Q97.5'],
         cde.trt_lower_slope = CDE.trt.i_fixef['trt.yNPK:year.y.m','Q2.5'],
  ) %>% 
  left_join(meta, 
            by = 'site_code')

library("scales")

slf<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = sl_posterior %>% distinct(sl.trt_lower_slope, sl.trt_upper_slope),
            aes(xmin = sl.trt_lower_slope, xmax =  sl.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = sl_posterior,
                      aes(x = sl.trt + unique(sl.trt_global_slope), 
                          y = habitat,
                          fill = habitat
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = sl_posterior,
                      aes(x = sl.ctl + unique(sl.ctl_global_slope), 
                          y = habitat,
                          color= "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = 'habitat') +
  geom_vline(data = sl_posterior,
             aes(xintercept = sl.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'Habitat',
       x = 'Biomass change due to Species Loss') +
  xlim(-0.50,0.50) +
  scale_x_continuous(trans = reverse_trans()) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")




sgf<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = sg_posterior %>% distinct(sg.trt_lower_slope, sg.trt_upper_slope),
            aes(xmin = sg.trt_lower_slope, xmax =  sg.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = sg_posterior,
                      aes(x = sg.trt + unique(sg.trt_global_slope), 
                          y = habitat,
                          fill= habitat
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = sg_posterior,
                      aes(x = sg.ctl + unique(sg.ctl_global_slope), 
                          y = habitat,
                          color = "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = 'habitat') +
  geom_vline(data = sg_posterior,
             aes(xintercept = sg.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'Habitat',
       x = 'Biomass change due to Species Gains') +
  xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")


cdef<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = cde_posterior %>% distinct(cde.trt_lower_slope, cde.trt_upper_slope),
            aes(xmin = cde.trt_lower_slope, xmax =  cde.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = cde_posterior,
                      aes(x = cde.trt + unique(cde.trt_global_slope), 
                          y = habitat,
                          fill= habitat
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = cde_posterior,
                      aes(x = cde.ctl + unique(cde.ctl_global_slope), 
                          y = habitat,
                          color = "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = 'habitat') +
  geom_vline(data = cde_posterior,
             aes(xintercept = cde.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'Habitat',
       x = 'Biomass change in Persistent Species') +
  xlim(-150,150) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")

grid_arrange_shared_legend(slf,sgf,cdef,nrow=1)


plot <- read.csv('~/Dropbox/Projects/NutNet/Data/plot_calc.csv', sep=',') %>% 
  as_tibble() 

plotmeta<-distinct(plot,site_code, continent, habitat)

#plot.rich.im_fixef
rich_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest(rich.ctl,rich.trt) %>% 
  mutate(response = 'rich',
         rich.ctl_global_slope = plot.rich.im_fixef['year_trt','Estimate'],
         rich.ctl_upper_slope = plot.rich.im_fixef['year_trt','Q97.5'],
         rich.ctl_lower_slope = plot.rich.im_fixef['year_trt','Q2.5'],
         rich.trt_global_slope = plot.rich.im_fixef['trtNPK:year_trt','Estimate'],
         rich.trt_upper_slope = plot.rich.im_fixef['trtNPK:year_trt','Q97.5'],
         rich.trt_lower_slope = plot.rich.im_fixef['trtNPK:year_trt','Q2.5'],
  ) %>% 
  left_join(plotmeta, 
            by = 'site_code')


bm_posterior <- study_sample_posterior  %>% 
  select(-data) %>% 
  unnest(bm.ctl,bm.trt) %>% 
  mutate(response = 'bm',
         bm.ctl_global_slope = plot.bm.im_fixef['year_trt','Estimate'],
         bm.ctl_upper_slope = plot.bm.im_fixef['year_trt','Q97.5'],
         bm.ctl_lower_slope = plot.bm.im_fixef['year_trt','Q2.5'],
         bm.trt_global_slope = plot.bm.im_fixef['trtNPK:year_trt','Estimate'],
         bm.trt_upper_slope = plot.bm.im_fixef['trtNPK:year_trt','Q97.5'],
         bm.trt_lower_slope = plot.bm.im_fixef['trtNPK:year_trt','Q2.5'],
  ) %>% 
  left_join(plotmeta, 
            by = 'site_code')


rf<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = rich_posterior %>% distinct(rich.trt_lower_slope, rich.trt_upper_slope),
            aes(xmin = rich.trt_lower_slope, xmax =  rich.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = rich_posterior,
                      aes(x = rich.trt + unique(rich.trt_global_slope), 
                          y = habitat,
                          fill = habitat
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = rich_posterior,
                      aes(x = rich.ctl + unique(rich.ctl_global_slope), 
                          y = habitat,
                          color= "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = 'habitat') +
  geom_vline(data = rich_posterior,
             aes(xintercept = rich.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'Habitat',
       x = 'Habitat-level Richness') +
  #xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")



bf<-ggplot() +
  #facet_grid( ~ habitat, scale = 'free') +
  geom_rect(data = bm_posterior %>% distinct(bm.trt_lower_slope, bm.trt_upper_slope),
            aes(xmin = bm.trt_lower_slope, xmax =  bm.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = bm_posterior,
                      aes(x = bm.trt + unique(bm.trt_global_slope), 
                          y = habitat,
                          fill = habitat
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = bm_posterior,
                      aes(x = bm.ctl + unique(bm.ctl_global_slope), 
                          y = habitat,
                          color= "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = 'habitat') +
  geom_vline(data = bm_posterior,
             aes(xintercept = bm.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'Habitat',
       x = 'Habitat-level Biomass') +
  #xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")

grid_arrange_shared_legend(rf,bf,nrow=1)









slf2<-ggplot() +
  #facet_grid( ~ continent, scale = 'free') +
  geom_rect(data = sl_posterior %>% distinct(sl.trt_lower_slope, sl.trt_upper_slope),
            aes(xmin = sl.trt_lower_slope, xmax =  sl.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = sl_posterior,
                      aes(x = sl.trt + unique(sl.trt_global_slope), 
                          y = continent,
                          fill = continent
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = sl_posterior,
                      aes(x = sl.ctl + unique(sl.ctl_global_slope), 
                          y = continent,
                          color= "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = 'continent') +
  geom_vline(data = sl_posterior,
             aes(xintercept = sl.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'continent',
       x = 'Biomass change due to Species Loss') +
  xlim(-0.50,0.50) +
  scale_x_continuous(trans = reverse_trans()) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")




sgf2<-ggplot() +
  #facet_grid( ~ continent, scale = 'free') +
  geom_rect(data = sg_posterior %>% distinct(sg.trt_lower_slope, sg.trt_upper_slope),
            aes(xmin = sg.trt_lower_slope, xmax =  sg.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = sg_posterior,
                      aes(x = sg.trt + unique(sg.trt_global_slope), 
                          y = continent,
                          fill= continent
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = sg_posterior,
                      aes(x = sg.ctl + unique(sg.ctl_global_slope), 
                          y = continent,
                          color = "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = 'continent') +
  geom_vline(data = sg_posterior,
             aes(xintercept = sg.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'continent',
       x = 'Biomass change due to Species Gains') +
  xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")


cdef2<-ggplot() +
  #facet_grid( ~ continent, scale = 'free') +
  geom_rect(data = cde_posterior %>% distinct(cde.trt_lower_slope, cde.trt_upper_slope),
            aes(xmin = cde.trt_lower_slope, xmax =  cde.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = cde_posterior,
                      aes(x = cde.trt + unique(cde.trt_global_slope), 
                          y = continent,
                          fill= continent
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = cde_posterior,
                      aes(x = cde.ctl + unique(cde.ctl_global_slope), 
                          y = continent,
                          color = "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = 'continent') +
  geom_vline(data = cde_posterior,
             aes(xintercept = cde.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'continent',
       x = 'Biomass change in Persistent Species') +
  xlim(-150,150) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")

grid_arrange_shared_legend(slf2,sgf2,cdef2,nrow=1)




rf2<-ggplot() +
  #facet_grid( ~ continent, scale = 'free') +
  geom_rect(data = rich_posterior %>% distinct(rich.trt_lower_slope, rich.trt_upper_slope),
            aes(xmin = rich.trt_lower_slope, xmax =  rich.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = rich_posterior,
                      aes(x = rich.trt + unique(rich.trt_global_slope), 
                          y = continent,
                          fill = continent
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = rich_posterior,
                      aes(x = rich.ctl + unique(rich.ctl_global_slope), 
                          y = continent,
                          color= "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = 'continent') +
  geom_vline(data = rich_posterior,
             aes(xintercept = rich.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'continent',
       x = 'continent-level Richness') +
  #xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")



bf2<-ggplot() +
  #facet_grid( ~ continent, scale = 'free') +
  geom_rect(data = bm_posterior %>% distinct(bm.trt_lower_slope, bm.trt_upper_slope),
            aes(xmin = bm.trt_lower_slope, xmax =  bm.trt_upper_slope), ymin = -Inf, ymax = Inf,
            alpha = 0.3) +
  geom_density_ridges(data = bm_posterior,
                      aes(x = bm.trt + unique(bm.trt_global_slope), 
                          y = continent,
                          fill = continent
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  geom_density_ridges(data = bm_posterior,
                      aes(x = bm.ctl + unique(bm.ctl_global_slope), 
                          y = continent,
                          color= "grey"
                      ),
                      scale = 1, alpha = 0.6,
                      linetype = 0) +
  scale_fill_viridis_d(name = 'continent') +
  geom_vline(data = bm_posterior,
             aes(xintercept = bm.trt_global_slope)) +
  geom_vline(xintercept = 0, lty = 2) +
  theme_bw() +
  labs(y = 'continent',
       x = 'continent-level Biomass') +
  #xlim(-0.50,0.50) +
  theme(panel.grid = element_blank(),
        axis.text.y = element_blank(),
        legend.key = element_blank(),
        legend.position="bottom")

grid_arrange_shared_legend(rf2,bf2,nrow=1)
