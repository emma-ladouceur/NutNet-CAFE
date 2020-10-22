#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny); library(tidyverse); library(ggplot2); library(shinythemes); library(viridis); library(patchwork)

site_dat <- read.csv("NN-CAFE/Data/Table_S1.csv")  

load('NN-CAFE/Data/rich.mod.dat.Rdata')
load('NN-CAFE/Data/bm.mod.dat.Rdata')
load('NN-CAFE/Data/sgain_dat.Rdata')
load('NN-CAFE/Data/sloss.n.mod.dat.Rdata')
load('NN-CAFE/Data/sl.n.mod.dat.Rdata')
load('NN-CAFE/Data/sg_dat.Rdata')
load('NN-CAFE/Data/cde.mod.dat.Rdata')
load('~/Dropbox/Projects/NutNet/Data/study.p.effs.Rdata')
load('~/Dropbox/Projects/NutNet/Data/p.effs.Rdata')




sitenames <- as.character(unique(site_dat$site_code))


# Define UI for application that draws a histogram
ui <- fixedPage(theme = shinytheme("readable"),

    fixedRow(
        column(12,
               h1("Nutrient Network Site Response to NPK"),
               br(),
               selectInput("selected_site", "Chosen Site", 
                           sitenames, selected = "hopl.us", multiple = FALSE,
                           selectize = TRUE, width = NULL, size = NULL),
               h3(textOutput("sitename")),
               
   
               column(12,
                      br(), br(),
                      h2("Linear Regressions: Site as a Random Effect"),
                      br(),
                      "To visualize differences in estimated response trajectories, 3D visualizations of filtered treatment response coefficients are presented below. In this figure, each point corresponds to the estimated change in cover (at the log2 scale) for each species within a site, per year of treatment.",
                      br(), br(),
                      "The 1:1:1 line presented illustrates a one-dimensional (neutral) expectation of nutrient enrichment response, where species responses are directionally consistent across treatments, only varying in total magnitude. Differences between species observed responses and their projection on this line are highlighted by dashed lines.",
                      br(), br(),
                      "Total variance of species responses captured by this projection will range between 0-1. When this neutral expectation fails to characterized response patterns, observed responses will deviate strongly from this 1:1:1 line (variance captured = 0); consistently one-dimensional responses across species will fall largely long this 1:1:1 line (variance captured = 1)."
               ),
               fixedRow(column(12, align = "center",
                               plotOutput('richbmviz', height = 500, width = 800),
                               br(), br(),
               )
               ),
               fixedRow(column(12, align = "center",
                               plotOutput('slosssgainviz', height = 500, width = 800),
                               br(), br(),
               )
               ),
               fixedRow(column(12, align = "center",
                               plotOutput('priceviz', height = 500, width = 1100),
                               br(), br(),
               )
               ),
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$sitename <- renderText(paste("Site:", 
                                        unique(site_dat$site_name[site_dat$site_code == input$selected_site]),
                                        "| Country:",
                                        unique(site_dat$country[site_dat$site_code == input$selected_site])))
    
    output$richbmviz <- renderPlot({
        
        sitedat <- site_dat %>% 
            filter(site_code == input$selected_site) 
        
        yr<-plot.rich_coef3 %>% select(site_code,xmax)
        plot.rich_fitted.npk <- plot.rich_fitted.npk %>% left_join(yr)
        plot.rich_fitted.ctl <- plot.rich_fitted.ctl %>% left_join(yr)
        
           rich.r<- ggplot()+
            geom_point(data = plot.rich_fitted.npk %>% filter(site_code == input$selected_site) ,
                       aes(x = year_trt, y = all.div), colour = "#0B775E",
                       size = 1.3,alpha=0.5) +
               geom_point(data = plot.rich_fitted.ctl %>% filter(site_code == input$selected_site) ,
                          aes(x = year_trt, y = all.div),  colour = "black",
                          size = 1.3, shape=1, alpha=0.5) +
            geom_segment(data = plot.rich_coef3 %>% filter(site_code == input$selected_site) ,
                         aes(x = xmin, 
                             xend = xmax,
                             y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                             yend =  (Intercept + TE + (ISlope+TESlope) * xmax),
                             group = site_code), colour = "#0B775E",
                         size = .7) +
               geom_segment(data = plot.rich_coef3 %>% filter(site_code == input$selected_site) ,
                            aes(x = xmin, 
                                xend = xmax,
                                y = (Intercept   + (ISlope) * xmin),
                                yend =  (Intercept  + (ISlope) * xmax),
                                group = site_code), colour = "black", linetype = "dashed",
                            size = .7) +
             ylim(0,40)+
            scale_x_continuous(breaks=c(0,1,3,6,9,12),limits=c(0, 12)) +
            labs(
                x = 'Year',
                y = ' Species richness', title= 'Species richness  ') +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                               plot.title = element_text(size=12))
           
           
           rich.eff<-ggplot() + 
               geom_point(data = study.rich.p %>% filter(site_code == input$selected_site), aes(x = response, y = eff, color=response),size = 2) +
               geom_errorbar(data = study.rich.p %>% filter(site_code == input$selected_site), aes(x = response, ymin = eff_lower,
                                                        ymax = eff_upper, color=response),
                             width = 0, size = 0.7) +
               # facet_wrap(~Model)+
               labs(x = '',
                    # y= expression(paste('Effect of NPK on Species Richness'))
                    y='')+
               geom_hline(yintercept = 0, lty = 2) +
               # scale_y_continuous(breaks=c(0,-0.5)) +
               scale_color_manual(values = c("#000000","#0B775E")) +
               theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                            plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                                            axis.text.y = element_text(size=6),
                                            axis.text.x = element_text(size=6),
                                            title=element_text(size=8),
                                            strip.background = element_blank(),legend.position="none")
           
           
           richviz <- rich.r +  annotation_custom(ggplotGrob(rich.eff), xmin = 7, xmax = 12, 
                                           ymin = 28, ymax = 40)
           
           yr<-plot.bm_coef3 %>% select(site_code,xmax)
           plot.bm_fitted.npk <- plot.bm_fitted.npk %>% left_join(yr)
           plot.bm_fitted.ctl <- plot.bm_fitted.ctl %>% left_join(yr)
           
           
          bm.r <- ggplot() +
               geom_point(data = plot.bm_fitted.npk %>% filter(site_code == input$selected_site),
                          aes(x = year_trt, y = plot.mass), alpha=0.5,colour = "#0B775E",
                          size = 1.3,alpha=0.7) +
              geom_point(data = plot.bm_fitted.ctl %>% filter(site_code == input$selected_site) ,
                         aes(x = year_trt, y = plot.mass), alpha=0.5, colour = "black",
                         size = 1.3, shape=1, alpha=0.7) +
               geom_segment(data = plot.bm_coef3 %>% filter(site_code == input$selected_site),
                            aes(x = xmin, 
                                xend = xmax,
                                y = (Intercept + TE  + (ISlope+TESlope) * xmin),
                                yend = (Intercept + TE + (ISlope+TESlope) * xmax),
                                group = site_code),  colour = "#0B775E",
                            size = 0.7) +
              geom_segment(data = plot.bm_coef3 %>% filter(site_code == input$selected_site) ,
                           aes(x = xmin, 
                               xend = xmax,
                               y = (Intercept   + (ISlope) * xmin),
                               yend =  (Intercept  + (ISlope) * xmax),
                               group = site_code), colour = "black", linetype = "dashed",
                           size = .7) +
               # uncertainy in fixed effect
               labs(x='Year',
                    #x = 'Years',
                    y = expression(paste('Biomass (g/',m^2, ')')), title= 'Biomass') +
              ylim(0,2000)+
               scale_x_continuous(breaks=c(0,1,3,6,9,12),limits=c(0, 12)) +
               theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                                  plot.title = element_text(size=12))
           
          bm.eff<-ggplot() + 
              geom_point(data = study.bm.p %>% filter(site_code == input$selected_site), aes(x = response, y = eff, color=response),size = 2) +
              geom_errorbar(data = study.bm.p %>% filter(site_code == input$selected_site), aes(x = response, ymin = eff_lower,
                                                                                                  ymax = eff_upper, color=response),
                            width = 0, size = 0.7) +
              # facet_wrap(~Model)+
              labs(x = '',
                   # y= expression(paste('Effect of NPK on Species bmness'))
                   y='')+
              geom_hline(yintercept = 0, lty = 2) +
              # scale_y_continuous(breaks=c(0,-0.5)) +
              scale_color_manual(values = c("#000000","#0B775E")) +
              theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                           plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                                           axis.text.y = element_text(size=6),
                                           axis.text.x = element_text(size=6),
                                           title=element_text(size=8),
                                           strip.background = element_blank(),legend.position="none")
          
          
          bmviz <- bm.r +  annotation_custom(ggplotGrob(bm.eff), xmin = 7, xmax = 12, 
                                             ymin = 1400 ,ymax = 2000)
           (richviz | bmviz)
        
        
    })
    
    
    output$slosssgainviz <- renderPlot({
        
        sitedat <- site_dat %>% 
            filter(site_code == input$selected_site) 
        
        yr<-sloss.trt_coef3 %>% select(site_code,xmax)
        sloss.trt_fitted.npk <- sloss.trt_fitted.npk %>% left_join(yr)
        sloss.trt_fitted.ctl <- sloss.trt_fitted.ctl %>% left_join(yr)
        
        sloss.trt_coef3$xs<-1
        
        sloss.r <- ggplot()  +
            geom_point(data = sloss.trt_fitted.npk %>% filter(site_code == input$selected_site),
                       aes(x = year.y, y = s.loss.n,),  alpha=0.5, color = "#B40F20",
                       size = 1.3, alpha=0.7) +
            geom_point(data = sloss.trt_fitted.ctl %>% filter(site_code == input$selected_site) ,
                       aes(x = year.y, y = s.loss.n), alpha=0.5, colour = "black",
                       size = 1.3, shape=1, alpha=0.7) +
            geom_segment(data = sloss.trt_coef3 %>% filter(site_code == input$selected_site),
                         aes(x = xs,
                             xend = xmax,
                             y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                             yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                             ),color = "#B40F20",
                         size = .7) +
            geom_segment(data = sloss.trt_coef3 %>% filter(site_code == input$selected_site) ,
                         aes(x = xmin, 
                             xend = xmax,
                             y = (Intercept   + (ISlope) * xmin),
                             yend =  (Intercept  + (ISlope) * xmax),
                             group = site_code), colour = "black", linetype = "dashed",
                         size = .7) +
            scale_x_continuous(breaks=c(1,3,6,9,12), limits=c(0,12)) +
             ylim(-20,0) +
            labs(x = 'Year',
                 y = expression(paste('Species Loss')), title= 'Species Loss') +
           # scale_color_viridis(discrete=FALSE,name="Length of Study") +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                               #plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                               axis.title.x = element_text(size=9),
                               axis.title.y = element_text(size=9),
                               axis.text=element_text(size=9))
        
        sloss.eff<-ggplot() + 
            geom_point(data = study.sloss.p %>% filter(site_code == input$selected_site), aes(x = response, y = eff,color=response),size = 2) +
            geom_errorbar(data = study.sloss.p %>% filter(site_code == input$selected_site), aes(x = response,ymin = eff_lower,
                                              ymax = eff_upper,color=response),
                          width = 0, size = 0.7) +
            #facet_wrap(~Model)+
            labs(x = '',
                 # y= expression(paste('Effect of NPK on Species Loss'))
                 y='') +
            geom_hline(yintercept = 0, lty = 2) +
            #scale_y_continuous(breaks=c(-0.5,-0.2,0)) +
            scale_color_manual(values = c("#000000","#B40F20")) +
            theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                         plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                                         axis.text.y = element_text(size=6),
                                         axis.text.x = element_text(size=6),
                                         title=element_text(size=8),
                                         strip.background = element_blank(),legend.position="none")
        
        
        slossviz <- sloss.r +  annotation_custom(ggplotGrob(sloss.eff),  xmin = 7, xmax = 12, 
                                                 ymin = -20, ymax = -13)
        
        yr<-sgain.trt_coef3 %>% select(site_code,xmax)
        sgain.trt_fitted.npk <- sgain.trt_fitted.npk %>% left_join(yr)
        sgain.trt_fitted.ctl <- sgain.trt_fitted.ctl %>% left_join(yr)
        sgain.trt_coef3$xs<-1
        
        sgain.r <- ggplot()  +
            geom_point(data = sgain.trt_fitted.npk  %>% filter(site_code == input$selected_site),
                       aes(x = year.y, y = s.gain),  alpha=0.5,color= "#046C9A",
                       size = 1.3,  alpha=0.7) +
            geom_point(data = sgain.trt_fitted.ctl %>% filter(site_code == input$selected_site) ,
                       aes(x = year.y, y = s.gain), alpha=0.5, colour = "black",
                       size = 1.3, shape=1, alpha=0.7) +
            geom_segment(data = sgain.trt_coef3  %>% filter(site_code == input$selected_site),
                         aes(x = xs,
                             xend = xmax,
                             y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                             yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                             ),color= "#046C9A",
                         size = .7) +
            geom_segment(data = sgain.trt_coef3 %>% filter(site_code == input$selected_site) ,
                         aes(x = xmin, 
                             xend = xmax,
                             y = (Intercept   + (ISlope) * xmin),
                             yend =  (Intercept  + (ISlope) * xmax),
                             group = site_code), colour = "black", linetype = "dashed",
                         size = .7) +
            scale_x_continuous(breaks=c(1,3,6,9,12),limits=c(0,12)) +
            ylim(0,20) +
            labs(x = 'Year',
                 y = expression(paste('Species Gain')), title= 'Species Gain') +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                               # plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                               axis.title.x = element_text(size=9),
                               axis.title.y = element_text(size=9),
                               axis.text=element_text(size=9))
        
        sgain.eff<-ggplot() + 
            geom_point(data = study.sgain.p %>% filter(site_code == input$selected_site), aes(x = response, y = eff,color=response),size = 2) +
            geom_errorbar(data =  study.sgain.p %>% filter(site_code == input$selected_site), aes(x = response,ymin = eff_lower,
                                              ymax = eff_upper,color=response),
                          width = 0, size = 0.7) +
            # facet_wrap(~Model)+
            labs(x = '',
                 # y= expression(paste('Effect of NPK on Species Gain'))
                 y='') +
            geom_hline(yintercept = 0, lty = 2) +
            #scale_y_continuous(breaks=c(0,0.05,0.3)) +
            scale_color_manual(values = c("#000000","#046C9A")) +
            theme_bw(base_size=14)+theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                                         plot.margin= margin(t = 0.2, r = 0.2, b = -0.2, l = -0.2, unit = "cm"),
                                         axis.text.y = element_text(size=6),
                                         axis.text.x = element_text(size=6),
                                         title=element_text(size=8),
                                         strip.background = element_blank(),legend.position="none")
        
        
        sgainviz <- sgain.r +  annotation_custom(ggplotGrob(sgain.eff), xmin = 7, xmax = 12, 
                                                 ymin = 13, ymax = 20)
        
        
         (slossviz | sgainviz)
        
        
    })
    
    
    output$priceviz <- renderPlot({
        
        sitedat <- site_dat %>% 
            filter(site_code == input$selected_site) 
        
        #SL
        yr<-sl.trt_coef3 %>% select(site_code,xmax)
        sl.trt_fitted.npk <- sl.trt_fitted.npk %>% left_join(yr)
        sl.trt_fitted.ctl <- sl.trt_fitted.ctl %>% left_join(yr)
        sl.trt_coef3$xs<-1
        
        slviz <- ggplot()  +
            geom_point(data = sl.trt_fitted.npk %>% filter(site_code == input$selected_site),
                       aes(x = year.y, y = SL),  alpha=0.5,color = "#B40F20",
                       size = 1.3, alpha=0.7) +
            geom_point(data = sl.trt_fitted.ctl %>% filter(site_code == input$selected_site) ,
                       aes(x = year.y, y = SL), alpha=0.5, colour = "black",
                       size = 1.3, shape=1, alpha=0.7) +
            geom_segment(data = sl.trt_coef3 %>% filter(site_code == input$selected_site),
                         aes(x = xs,
                             xend = xmax,
                             y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                             yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                             ),color = "#B40F20",
                         size = .7) +
            geom_segment(data = sl.trt_coef3 %>% filter(site_code == input$selected_site) ,
                         aes(x = xmin, 
                             xend = xmax,
                             y = (Intercept   + (ISlope) * xmin),
                             yend =  (Intercept  + (ISlope) * xmax),
                             group = site_code), colour = "black", linetype = "dashed",
                         size = .7) +
            # scale_x_continuous(breaks=c(1,3,6,9,12)) +
            # ylim(0,20) +
            labs(x = 'Year',
                 y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
                 title= 'Biomass Change Due To Species Loss') +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                               # plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                               axis.title.x = element_text(size=9),
                               axis.title.y = element_text(size=9),
                               axis.text=element_text(size=9))
        
        
        yr<-sg.trt_coef3 %>% select(site_code,xmax)
        sg.trt_fitted.npk <- sg.trt_fitted.npk %>% left_join(yr)
        sg.trt_fitted.ctl <- sg.trt_fitted.ctl %>% left_join(yr) 
        sg.trt_coef3$xs<-1
        
        sgviz <- ggplot()  +
            geom_point(data = sg.trt_fitted.npk %>% filter(site_code == input$selected_site),
                       aes(x = year.y, y = SG), alpha=0.5,color= "#046C9A",
                       size = 1.3, alpha=0.7) +
            geom_point(data = sg.trt_fitted.ctl %>% filter(site_code == input$selected_site) ,
                       aes(x = year.y, y = SG), alpha=0.5, colour = "black",
                       size = 1.3, shape=1, alpha=0.7) +
            geom_segment(data = sg.trt_coef3 %>% filter(site_code == input$selected_site),
                         aes(x = xs,
                             xend = xmax,
                             y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                             yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                             ), color= "#046C9A",
                         size = .7) +
            geom_segment(data = sg.trt_coef3 %>% filter(site_code == input$selected_site) ,
                         aes(x = xmin, 
                             xend = xmax,
                             y = (Intercept   + (ISlope) * xmin),
                             yend =  (Intercept  + (ISlope) * xmax),
                             group = site_code), colour = "black", linetype = "dashed",
                         size = .7) +
            # scale_x_continuous(breaks=c(1,3,6,9,12)) +
            # ylim(0,20) +
            labs(x = 'Year',
                 y = expression(paste('Change in Biomass (g/' ,m^2, ')')),  title= 'Biomass Change Due To Species Gain') +
           # scale_color_viridis(discrete=FALSE,name="Length of Study") +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                               #plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                               axis.title.x = element_text(size=9),
                               axis.title.y = element_text(size=9),
                               axis.text=element_text(size=9))
        
    
        yr<-cde_coef3 %>% select(site_code,xmax)
        cde_fitted.npk <- cde_fitted.npk %>% left_join(yr)
        cde_fitted.ctl <- cde_fitted.ctl %>% left_join(yr)
        cde_coef3$xs<-1
        
        cdeviz <- ggplot()  +
            geom_point(data = cde_fitted.npk %>% filter(site_code == input$selected_site),
                       aes(x = year.y, y = CDE),  alpha=0.5,color="#F98400",
                       size = 1.3,  alpha=0.7) +
            geom_point(data = cde_fitted.ctl %>% filter(site_code == input$selected_site) ,
                       aes(x = year.y, y = CDE), alpha=0.5, colour = "black",
                       size = 1.3, shape=1, alpha=0.7) +
            geom_segment(data = cde_coef3 %>% filter(site_code == input$selected_site),
                         aes(x = xs,
                             xend = xmax,
                             y = (Intercept + TE + (ISlope+TESlope) *  cxmin),
                             yend = (Intercept + TE + (ISlope+TESlope)  * cxmax),
                             ), color="#F98400",
                         size = .7) +
            geom_segment(data = cde_coef3 %>% filter(site_code == input$selected_site) ,
                         aes(x = xmin, 
                             xend = xmax,
                             y = (Intercept   + (ISlope) * xmin),
                             yend =  (Intercept  + (ISlope) * xmax),
                             group = site_code), colour = "black", linetype = "dashed",
                         size = .7) +
            # scale_x_continuous(breaks=c(1,3,6,9,12)) +
            # ylim(0,20) +
            labs(x = 'Year',
                 y = expression(paste('Change in Biomass (g/' ,m^2, ')')), 
                 title= 'Persistent Species Change in Biomass') +
            theme_bw() + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), strip.background = element_blank(),legend.position="bottom",
                               #plot.margin= margin(t = -0.5, r = 0.2, b = 0.5, l = 0.2, unit = "cm"),
                               axis.title.x = element_text(size=9),
                               axis.title.y = element_text(size=9),
                               axis.text=element_text(size=9))
        
        
        
        
        (slviz | sgviz | cdeviz)
        
        
    })
    
       
    
    }



# Run the application 
shinyApp(ui = ui, server = server)
