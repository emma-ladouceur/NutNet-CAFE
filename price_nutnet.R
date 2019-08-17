
library(gridExtra)
library(grid)
library(ggplot2)
library(reshape2)
library(MCMCglmm)
library(tidyr)

library(priceTools)
library(dplyr)



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


#GLOBAL WORLDWIDE OVERALL
#emmas links
sp <- read.csv("~/Dropbox/Projects/NutNet/Data/biomass_calc2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p <- read.csv("~/Dropbox/Projects/NutNet/Data/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

p.all <- read.csv("~/Dropbox/Projects/NutNet/Data/cumulative_time_only2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


pp3<-pp3[complete.cases(pp3$SG),]
pp3<-pp3[complete.cases(pp3$SL),]
summary(pp3)
dat1<-pp3[pp3$trt.xy %in% c('Control_Control'),]
dat2<-pp3[pp3$trt.xy %in% c('NPK_NPK'),]

View(dat1)
#all sp
#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat1,type='cafe',xlim=c(0,17),ylim=c(0,700),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('Control')+theme_classic()
p2 <- leap.zig(dat2,type='cafe',xlim=c(0,17),ylim=c(0,700),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('NPK')+theme_classic()
p3 <- leap.zig(dat1,type='cafe',xlim=c(0,17),ylim=c(0,700),standardize = FALSE,raw.points = T)+ 
  annotate("text", x = mean(dat1$x.rich), y = mean(dat1$x.func), 
           label = "*",size=8)+ggtitle('Control')+theme_classic()
p4 <- leap.zig(dat2,type='cafe',xlim=c(0,17),ylim=c(0,700),standardize = FALSE,raw.points = T)+ 
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('NPK')+theme_classic()


grid_arrange_shared_legend(p1,p2,p3,p4,ncol=2,nrow=2)


# MATRIX OF EVERY GODDAMNED SITE FOR STUPID OOOOO FACTOR
dat1$site_code<-as.factor(dat1$site_code)
levels(dat1$site_code)

dat1<-pp3[pp3$trt.xy %in% c('NPK_NPK'),]
levels(dat1$site_code)
View(dat1)


dat2<-dat1[dat1$site_code %in% c('arch.us'),]
dat3<-dat1[dat1$site_code %in% c('azi.cn'),]
dat4<-dat1[dat1$site_code %in% c('badlau.de'),]
dat5<-dat1[dat1$site_code %in% c('bari.ar'),]
dat6<-dat1[dat1$site_code %in% c('barta.us'),]
#dat7<-dat1[dat1$site_code %in% c('bayr.de'),]
dat7<-dat1[dat1$site_code %in% c('bldr.us'),]
#dat9<-dat1[dat1$site_code %in% c('bnbt.us'),]
dat8<-dat1[dat1$site_code %in% c('bnch.us'),]
dat9<-dat1[dat1$site_code %in% c('bogong.au'),]
dat10<-dat1[dat1$site_code %in% c('burrawan.au'),]
dat11<-dat1[dat1$site_code %in% c('burren.ie'),]
dat12<-dat1[dat1$site_code %in% c('cbgb.us'),]

dat13<-dat1[dat1$site_code %in% c('cdcr.us'),]
dat14<-dat1[dat1$site_code %in% c('cdpt.us'),]
dat15<-dat1[dat1$site_code %in% c('cereep.fr'),]
dat16<-dat1[dat1$site_code %in% c('chilcas.ar'),]
dat17<-dat1[dat1$site_code %in% c('comp.pt'),]
dat18<-dat1[dat1$site_code %in% c('cowi.ca'),]
dat19<-dat1[dat1$site_code %in% c('doane.us'),]
dat20<-dat1[dat1$site_code %in% c('elliot.us'),]
dat21<-dat1[dat1$site_code %in% c('ethass.au'),]
dat22<-dat1[dat1$site_code %in% c('frue.ch'),]
dat23<-dat1[dat1$site_code %in% c('gilb.za'),]

dat24<-dat1[dat1$site_code %in% c('glcr.us'),]
dat25<-dat1[dat1$site_code %in% c('hall.us'),]
dat26<-dat1[dat1$site_code %in% c('hart.us'),]
dat27<-dat1[dat1$site_code %in% c('hero.uk'),]
dat28<-dat1[dat1$site_code %in% c('hnvr.us'),]
dat29<-dat1[dat1$site_code %in% c('hopl.us'),]
dat30<-dat1[dat1$site_code %in% c('jena.de'),]
dat31<-dat1[dat1$site_code %in% c('kbs.us'),]
dat32<-dat1[dat1$site_code %in% c('kibber.in'),]
dat33<-dat1[dat1$site_code %in% c('kidman.au'),]
dat34<-dat1[dat1$site_code %in% c('kilp.fi'),]

dat35<-dat1[dat1$site_code %in% c('kiny.au'),]
dat36<-dat1[dat1$site_code %in% c('koffler.ca'),]
dat35<-dat1[dat1$site_code %in% c('konz.us'),]
dat36<-dat1[dat1$site_code %in% c('lake.us'),]
dat37<-dat1[dat1$site_code %in% c('lancaster.uk'),]
dat38<-dat1[dat1$site_code %in% c('look.us'),]
dat39<-dat1[dat1$site_code %in% c('marc.ar'),]
dat40<-dat1[dat1$site_code %in% c('mcla.us'),]
dat41<-dat1[dat1$site_code %in% c( 'msla.us'),]
dat42<-dat1[dat1$site_code %in% c('msum.us'),]
dat43<-dat1[dat1$site_code %in% c('mtca.au'),]
dat44<-dat1[dat1$site_code %in% c('nilla.au'),]
dat45<-dat1[dat1$site_code %in% c('nioo.nl'),]

dat46<-dat1[dat1$site_code %in% c('pape.de'),]
dat47<-dat1[dat1$site_code %in% c('pich.ec'),]
dat48<-dat1[dat1$site_code %in% c('ping.au'),]
dat49<-dat1[dat1$site_code %in% c('pinj.au'),]
dat50<-dat1[dat1$site_code %in% c('podo.ec'),]
dat51<-dat1[dat1$site_code %in% c('potrok.ar'),]
dat52<-dat1[dat1$site_code %in% c('rook.uk'),]
dat53<-dat1[dat1$site_code %in% c('saana.fi'),]
dat54<-dat1[dat1$site_code %in% c('sage.us'),]
dat55<-dat1[dat1$site_code %in% c('sava.us'),]
dat56<-dat1[dat1$site_code %in% c('sedg.us'),]

dat57<-dat1[dat1$site_code %in% c('sereng.tz'),]
dat58<-dat1[dat1$site_code %in% c('sevi.us'),]
dat59<-dat1[dat1$site_code %in% c('sgs.us'),]
dat60<-dat1[dat1$site_code %in% c('shps.us'),]
dat61<-dat1[dat1$site_code %in% c('sier.us'),]
dat62<-dat1[dat1$site_code %in% c('smith.us'),]
dat63<-dat1[dat1$site_code %in% c('spin.us'),]
dat64<-dat1[dat1$site_code %in% c('summ.za'),]
dat65<-dat1[dat1$site_code %in% c('temple.us'),]
dat65<-dat1[dat1$site_code %in% c('trel.us'),]
dat66<-dat1[dat1$site_code %in% c('tyso.us'),]
dat67<-dat1[dat1$site_code %in% c('ufrec.us'),]

dat68<-dat1[dat1$site_code %in% c('ukul.za'),]
dat69<-dat1[dat1$site_code %in% c('unc.us'),]
dat70<-dat1[dat1$site_code %in% c('valm.ch'),]
dat71<-dat1[dat1$site_code %in% c('yarra.au'),]


View(dat44)
#!!!!!!!!!!!!!!!!!!
#gotta change names to match

#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat2,type='cafe',  standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('arch.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p2 <- leap.zig(dat3,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('azi.cn')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p3 <- leap.zig(dat4,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat4$x.rich), y = mean(dat4$x.func), 
           label = "*",size=8)+ggtitle('badlau.de')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p4 <- leap.zig(dat5,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat5$x.rich), y = mean(dat5$x.func), 
           label = "*",size=8)+ggtitle('bari.ar')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p5 <- leap.zig(dat6,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat6$x.rich), y = mean(dat6$x.func), 
           label = "*",size=8)+ggtitle('barta.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p6 <- leap.zig(dat7,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat7$x.rich), y = mean(dat7$x.func), 
           label = "*",size=8)+ggtitle('bldr.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p7 <- leap.zig(dat8,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat8$x.rich), y = mean(dat8$x.func), 
           label = "*",size=8)+ggtitle('bnch.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p8 <- leap.zig(dat9,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat9$x.rich), y = mean(dat9$x.func), 
           label = "*",size=8)+ggtitle('bogong.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p9 <- leap.zig(dat10,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat10$x.rich), y = mean(dat10$x.func), 
           label = "*",size=8)+ggtitle('burrawan.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p10 <- leap.zig(dat11,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat11$x.rich), y = mean(dat11$x.func), 
           label = "*",size=8)+ggtitle('burren.ie')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p11 <- leap.zig(dat12,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat12$x.rich), y = mean(dat12$x.func), 
           label = "*",size=8)+ggtitle('cbgb.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

p12 <- leap.zig(dat13,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat13$x.rich), y = mean(dat13$x.func), 
           label = "*",size=8)+ggtitle('cdcr.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p13 <- leap.zig(dat14,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat14$x.rich), y = mean(dat14$x.func), 
           label = "*",size=8)+ggtitle('cdpt.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p14 <- leap.zig(dat15,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat15$x.rich), y = mean(dat15$x.func), 
           label = "*",size=8)+ggtitle('cdcr.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p15 <- leap.zig(dat16,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat16$x.rich), y = mean(dat16$x.func), 
           label = "*",size=8)+ggtitle('cdpt.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p16 <- leap.zig(dat17,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat17$x.rich), y = mean(dat17$x.func), 
           label = "*",size=8)+ggtitle('cereep.fr')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p17 <- leap.zig(dat18,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat18$x.rich), y = mean(dat18$x.func), 
           label = "*",size=8)+ggtitle('chilcas.ar')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p18 <- leap.zig(dat19,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat19$x.rich), y = mean(dat19$x.func), 
           label = "*",size=8)+ggtitle('comp.pt')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
# p19 <- leap.zig(dat20,type='cafe', standardize = FALSE,raw.points = F)+ 
#   annotate("text", x = mean(dat20$x.rich), y = mean(dat20$x.func), 
#            label = "*",size=8)+ggtitle('cowi.ca')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p20 <- leap.zig(dat21,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat21$x.rich), y = mean(dat21$x.func), 
           label = "*",size=8)+ggtitle('doane.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p21 <- leap.zig(dat22,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat22$x.rich), y = mean(dat22$x.func), 
           label = "*",size=8)+ggtitle('frue.ch')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p22 <- leap.zig(dat23,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat23$x.rich), y = mean(dat23$x.func), 
           label = "*",size=8)+ggtitle('gilb.za')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

p23 <- leap.zig(dat24,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat24$x.rich), y = mean(dat24$x.func), 
           label = "*",size=8)+ggtitle('glcr.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p24 <- leap.zig(dat25,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat25$x.rich), y = mean(dat25$x.func), 
           label = "*",size=8)+ggtitle('hall.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p25 <- leap.zig(dat26,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat26$x.rich), y = mean(dat26$x.func), 
           label = "*",size=8)+ggtitle('hart.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
# p26 <- leap.zig(dat27,type='cafe', standardize = FALSE,raw.points = F)+ 
#   annotate("text", x = mean(dat27$x.rich), y = mean(dat27$x.func), 
#            label = "*",size=8)+ggtitle('hnvr.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p27 <- leap.zig(dat28,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat28$x.rich), y = mean(dat28$x.func), 
           label = "*",size=8)+ggtitle('hopl.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p28 <- leap.zig(dat29,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat29$x.rich), y = mean(dat29$x.func), 
           label = "*",size=8)+ggtitle('jena.de')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p29 <- leap.zig(dat30,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat30$x.rich), y = mean(dat30$x.func), 
           label = "*",size=8)+ggtitle('kbs.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p30 <- leap.zig(dat31,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat31$x.rich), y = mean(dat31$x.func), 
           label = "*",size=8)+ggtitle('kibber.in')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p31 <- leap.zig(dat32,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat32$x.rich), y = mean(dat32$x.func), 
           label = "*",size=8)+ggtitle('kidman.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p32 <- leap.zig(dat33,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat33$x.rich), y = mean(dat33$x.func), 
           label = "*",size=8)+ggtitle('kilp.fi')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p33 <- leap.zig(dat34,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat34$x.rich), y = mean(dat34$x.func), 
           label = "*",size=8)+ggtitle('koffler.ca')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

p34 <- leap.zig(dat35,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat35$x.rich), y = mean(dat35$x.func), 
           label = "*",size=8)+ggtitle('konz.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p35 <- leap.zig(dat36,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat36$x.rich), y = mean(dat36$x.func), 
           label = "*",size=8)+ggtitle('lake.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p36 <- leap.zig(dat37,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat37$x.rich), y = mean(dat37$x.func), 
           label = "*",size=8)+ggtitle('lancaster.uk')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p37 <- leap.zig(dat38,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat38$x.rich), y = mean(dat38$x.func), 
           label = "*",size=8)+ggtitle('look.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p38 <- leap.zig(dat39,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat39$x.rich), y = mean(dat39$x.func), 
           label = "*",size=8)+ggtitle('marc.ar')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p39 <- leap.zig(dat40,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat40$x.rich), y = mean(dat40$x.func), 
           label = "*",size=8)+ggtitle('mcla.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p40 <- leap.zig(dat41,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat41$x.rich), y = mean(dat41$x.func), 
           label = "*",size=8)+ggtitle('msla.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p41 <- leap.zig(dat42,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat42$x.rich), y = mean(dat42$x.func), 
           label = "*",size=8)+ggtitle('msum.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p42 <- leap.zig(dat43,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat43$x.rich), y = mean(dat43$x.func), 
           label = "*",size=8)+ggtitle('mtca.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
# p43 <- leap.zig(dat44,type='cafe', standardize = FALSE,raw.points = F)+ 
#   annotate("text", x = mean(dat44$x.rich), y = mean(dat44$x.func), 
#            label = "*",size=8)+ggtitle('pape.de')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p44 <- leap.zig(dat45,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat45$x.rich), y = mean(dat45$x.func), 
           label = "*",size=8)+ggtitle('pich.ec')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

p45 <- leap.zig(dat46,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat46$x.rich), y = mean(dat46$x.func), 
           label = "*",size=8)+ggtitle('ping.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
# p46 <- leap.zig(dat47,type='cafe', standardize = FALSE,raw.points = F)+ 
#   annotate("text", x = mean(dat47$x.rich), y = mean(dat47$x.func), 
#            label = "*",size=8)+ggtitle('pinj.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p47 <- leap.zig(dat48,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat48$x.rich), y = mean(dat48$x.func), 
           label = "*",size=8)+ggtitle('podo.ec')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p48 <- leap.zig(dat49,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat49$x.rich), y = mean(dat49$x.func), 
           label = "*",size=8)+ggtitle('potrok.ar')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
# p49 <- leap.zig(dat50,type='cafe', standardize = FALSE,raw.points = F)+ 
#   annotate("text", x = mean(dat50$x.rich), y = mean(dat50$x.func), 
#            label = "*",size=8)+ggtitle('sage.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p50 <- leap.zig(dat51,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat51$x.rich), y = mean(dat51$x.func), 
           label = "*",size=8)+ggtitle('sava.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
# p51 <- leap.zig(dat52,type='cafe', standardize = FALSE,raw.points = F)+ 
#   annotate("text", x = mean(dat52$x.rich), y = mean(dat52$x.func), 
#            label = "*",size=8)+ggtitle('sedg.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p52 <- leap.zig(dat53,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat53$x.rich), y = mean(dat53$x.func), 
           label = "*",size=8)+ggtitle('sereng.tz')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p53 <- leap.zig(dat54,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat54$x.rich), y = mean(dat54$x.func), 
           label = "*",size=8)+ggtitle('sevi.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p54 <- leap.zig(dat55,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat55$x.rich), y = mean(dat55$x.func), 
           label = "*",size=8)+ggtitle('sgs.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p55 <- leap.zig(dat56,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat56$x.rich), y = mean(dat56$x.func), 
           label = "*",size=8)+ggtitle('shps.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

p56 <- leap.zig(dat57,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat57$x.rich), y = mean(dat57$x.func), 
           label = "*",size=8)+ggtitle('sier.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6),)
p57 <- leap.zig(dat58,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat58$x.rich), y = mean(dat58$x.func), 
           label = "*",size=8)+ggtitle('smith.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p58 <- leap.zig(dat59,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat59$x.rich), y = mean(dat59$x.func), 
           label = "*",size=8)+ggtitle('spin.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p59 <- leap.zig(dat60,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat60$x.rich), y = mean(dat60$x.func), 
           label = "*",size=8)+ggtitle('summ.za')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p60 <- leap.zig(dat61,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat61$x.rich), y = mean(dat61$x.func), 
           label = "*",size=8)+ggtitle('temple.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p61 <- leap.zig(dat62,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat62$x.rich), y = mean(dat62$x.func), 
           label = "*",size=8)+ggtitle('tyso.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p62 <- leap.zig(dat63,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat63$x.rich), y = mean(dat63$x.func), 
           label = "*",size=8)+ggtitle('ufrec.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p63 <- leap.zig(dat64,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat64$x.rich), y = mean(dat64$x.func), 
           label = "*",size=8)+ggtitle('ukul.za')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
# p64 <- leap.zig(dat65,type='cafe', standardize = FALSE,raw.points = F)+ 
#   annotate("text", x = mean(dat65$x.rich), y = mean(dat65$x.func), 
#            label = "*",size=8)+ggtitle('unc.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p65 <- leap.zig(dat66,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat66$x.rich), y = mean(dat66$x.func), 
           label = "*",size=8)+ggtitle('valm.ch')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p66 <- leap.zig(dat67,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat67$x.rich), y = mean(dat67$x.func), 
           label = "*",size=8)+ggtitle('yarra.au ')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p67 <- leap.zig(dat68,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat68$x.rich), y = mean(dat68$x.func), 
           label = "*",size=8)+ggtitle('yarra.au ')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p68 <- leap.zig(dat69,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat69$x.rich), y = mean(dat69$x.func), 
           label = "*",size=8)+ggtitle('yarra.au ')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p69 <- leap.zig(dat70,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat70$x.rich), y = mean(dat70$x.func), 
           label = "*",size=8)+ggtitle('yarra.au ')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p70 <- leap.zig(dat71,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat71$x.rich), y = mean(dat71$x.func), 
           label = "*",size=8)+ggtitle('yarra.au ')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

#PLOT USING GR

#PLOT USING GRID.ARRANGE WITH TIDYVERSE LEGEND FUNCTION
grid_arrange_shared_legend(p1,p2,p3,p4,p5,p7,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p20,p21,p22,p23,p24,p25,p27,p28,p29,p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,p41,p42,p45,p48,p50,p52,p53,p54,p55,p56,p57,p58,p59,p60,p61,p62,p63,p65,p66,p67,p68,p69,p70,ncol=11,nrow=6)

#Standard axes
#inner
#+theme(axis.text.x=element_blank(),axis.text.y=element_blank())
#left column
#+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.title.=element_blank(),)
#left column middle row
#+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
#bottom row
#+theme(axis.title.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y = element_blank(),axis.title.y=element_blank())
#bottom row middle
#+theme(axis.text.y=element_blank(),axis.ticks.y = element_blank(),axis.title.y=element_blank())

# xlim=c(0,25),ylim=c(0,1500),

#+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

#theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(dat2,type='cafe', xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat2$x.rich), y = mean(dat2$x.func), 
           label = "*",size=8)+ggtitle('arch.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p2 <- leap.zig(dat3,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat3$x.rich), y = mean(dat3$x.func), 
           label = "*",size=8)+ggtitle('azi.cn')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p3 <- leap.zig(dat4,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat4$x.rich), y = mean(dat4$x.func), 
           label = "*",size=8)+ggtitle('badlau.de')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p4 <- leap.zig(dat5,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat5$x.rich), y = mean(dat5$x.func), 
           label = "*",size=8)+ggtitle('bari.ar')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p5 <- leap.zig(dat6,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat6$x.rich), y = mean(dat6$x.func), 
           label = "*",size=8)+ggtitle('barta.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p6 <- leap.zig(dat7,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat7$x.rich), y = mean(dat7$x.func), 
           label = "*",size=8)+ggtitle('bayr.de')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p7 <- leap.zig(dat8,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat8$x.rich), y = mean(dat8$x.func), 
           label = "*",size=8)+ggtitle('bldr.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p8 <- leap.zig(dat9,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat9$x.rich), y = mean(dat9$x.func), 
           label = "*",size=8)+ggtitle('bnbt.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p9 <- leap.zig(dat10,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat10$x.rich), y = mean(dat10$x.func), 
           label = "*",size=8)+ggtitle('bnch.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p10 <- leap.zig(dat11,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat11$x.rich), y = mean(dat11$x.func), 
           label = "*",size=8)+ggtitle('bogong.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p11 <- leap.zig(dat12,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat12$x.rich), y = mean(dat12$x.func), 
           label = "*",size=8)+ggtitle('burrawan.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

p12 <- leap.zig(dat13,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat13$x.rich), y = mean(dat13$x.func), 
           label = "*",size=8)+ggtitle('burren.ie')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p13 <- leap.zig(dat14,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat14$x.rich), y = mean(dat14$x.func), 
           label = "*",size=8)+ggtitle('cbgb.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p14 <- leap.zig(dat15,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat15$x.rich), y = mean(dat15$x.func), 
           label = "*",size=8)+ggtitle('cdcr.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p15 <- leap.zig(dat16,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat16$x.rich), y = mean(dat16$x.func), 
           label = "*",size=8)+ggtitle('cdpt.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p16 <- leap.zig(dat17,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat17$x.rich), y = mean(dat17$x.func), 
           label = "*",size=8)+ggtitle('cereep.fr')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p17 <- leap.zig(dat18,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat18$x.rich), y = mean(dat18$x.func), 
           label = "*",size=8)+ggtitle('chilcas.ar')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p18 <- leap.zig(dat19,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat19$x.rich), y = mean(dat19$x.func), 
           label = "*",size=8)+ggtitle('comp.pt')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p19 <- leap.zig(dat20,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat20$x.rich), y = mean(dat20$x.func), 
           label = "*",size=8)+ggtitle('cowi.ca')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p20 <- leap.zig(dat21,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat21$x.rich), y = mean(dat21$x.func), 
           label = "*",size=8)+ggtitle('doane.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p21 <- leap.zig(dat22,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat22$x.rich), y = mean(dat22$x.func), 
           label = "*",size=8)+ggtitle('frue.ch')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p22 <- leap.zig(dat23,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat23$x.rich), y = mean(dat23$x.func), 
           label = "*",size=8)+ggtitle('gilb.za')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

p23 <- leap.zig(dat24,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat24$x.rich), y = mean(dat24$x.func), 
           label = "*",size=8)+ggtitle('glcr.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p24 <- leap.zig(dat25,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat25$x.rich), y = mean(dat25$x.func), 
           label = "*",size=8)+ggtitle('hall.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p25 <- leap.zig(dat26,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat26$x.rich), y = mean(dat26$x.func), 
           label = "*",size=8)+ggtitle('hart.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p26 <- leap.zig(dat27,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat27$x.rich), y = mean(dat27$x.func), 
           label = "*",size=8)+ggtitle('hnvr.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p27 <- leap.zig(dat28,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat28$x.rich), y = mean(dat28$x.func), 
           label = "*",size=8)+ggtitle('hopl.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p28 <- leap.zig(dat29,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat29$x.rich), y = mean(dat29$x.func), 
           label = "*",size=8)+ggtitle('jena.de')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p29 <- leap.zig(dat30,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat30$x.rich), y = mean(dat30$x.func), 
           label = "*",size=8)+ggtitle('kbs.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p30 <- leap.zig(dat31,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat31$x.rich), y = mean(dat31$x.func), 
           label = "*",size=8)+ggtitle('kibber.in')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p31 <- leap.zig(dat32,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat32$x.rich), y = mean(dat32$x.func), 
           label = "*",size=8)+ggtitle('kidman.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p32 <- leap.zig(dat33,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat33$x.rich), y = mean(dat33$x.func), 
           label = "*",size=8)+ggtitle('kilp.fi')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p33 <- leap.zig(dat34,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat34$x.rich), y = mean(dat34$x.func), 
           label = "*",size=8)+ggtitle('koffler.ca')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

p34 <- leap.zig(dat35,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat35$x.rich), y = mean(dat35$x.func), 
           label = "*",size=8)+ggtitle('konz.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p35 <- leap.zig(dat36,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat36$x.rich), y = mean(dat36$x.func), 
           label = "*",size=8)+ggtitle('lake.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p36 <- leap.zig(dat37,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat37$x.rich), y = mean(dat37$x.func), 
           label = "*",size=8)+ggtitle('lancaster.uk')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p37 <- leap.zig(dat38,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat38$x.rich), y = mean(dat38$x.func), 
           label = "*",size=8)+ggtitle('look.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p38 <- leap.zig(dat39,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat39$x.rich), y = mean(dat39$x.func), 
           label = "*",size=8)+ggtitle('marc.ar')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p39 <- leap.zig(dat40,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat40$x.rich), y = mean(dat40$x.func), 
           label = "*",size=8)+ggtitle('mcla.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p40 <- leap.zig(dat41,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat41$x.rich), y = mean(dat41$x.func), 
           label = "*",size=8)+ggtitle('msla.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p41 <- leap.zig(dat42,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat42$x.rich), y = mean(dat42$x.func), 
           label = "*",size=8)+ggtitle('msum.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p42 <- leap.zig(dat43,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat43$x.rich), y = mean(dat43$x.func), 
           label = "*",size=8)+ggtitle('mtca.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p43 <- leap.zig(dat44,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat44$x.rich), y = mean(dat44$x.func), 
           label = "*",size=8)+ggtitle('pape.de')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p44 <- leap.zig(dat45,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat45$x.rich), y = mean(dat45$x.func), 
           label = "*",size=8)+ggtitle('pich.ec')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

p45 <- leap.zig(dat46,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat46$x.rich), y = mean(dat46$x.func), 
           label = "*",size=8)+ggtitle('ping.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p46 <- leap.zig(dat47,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat47$x.rich), y = mean(dat47$x.func), 
           label = "*",size=8)+ggtitle('pinj.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p47 <- leap.zig(dat48,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat48$x.rich), y = mean(dat48$x.func), 
           label = "*",size=8)+ggtitle('podo.ec')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p48 <- leap.zig(dat49,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat49$x.rich), y = mean(dat49$x.func), 
           label = "*",size=8)+ggtitle('potrok.ar')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p49 <- leap.zig(dat50,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat50$x.rich), y = mean(dat50$x.func), 
           label = "*",size=8)+ggtitle('sage.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p50 <- leap.zig(dat51,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat51$x.rich), y = mean(dat51$x.func), 
           label = "*",size=8)+ggtitle('sava.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p51 <- leap.zig(dat52,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat52$x.rich), y = mean(dat52$x.func), 
           label = "*",size=8)+ggtitle('sedg.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p52 <- leap.zig(dat53,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat53$x.rich), y = mean(dat53$x.func), 
           label = "*",size=8)+ggtitle('sereng.tz')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p53 <- leap.zig(dat54,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat54$x.rich), y = mean(dat54$x.func), 
           label = "*",size=8)+ggtitle('sevi.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p54 <- leap.zig(dat55,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat55$x.rich), y = mean(dat55$x.func), 
           label = "*",size=8)+ggtitle('sgs.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p55 <- leap.zig(dat56,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat56$x.rich), y = mean(dat56$x.func), 
           label = "*",size=8)+ggtitle('shps.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

p56 <- leap.zig(dat57,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat57$x.rich), y = mean(dat57$x.func), 
           label = "*",size=8)+ggtitle('sier.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6),)
p57 <- leap.zig(dat58,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat58$x.rich), y = mean(dat58$x.func), 
           label = "*",size=8)+ggtitle('smith.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p58 <- leap.zig(dat59,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat59$x.rich), y = mean(dat59$x.func), 
           label = "*",size=8)+ggtitle('spin.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p59 <- leap.zig(dat60,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat60$x.rich), y = mean(dat60$x.func), 
           label = "*",size=8)+ggtitle('summ.za')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p60 <- leap.zig(dat61,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat61$x.rich), y = mean(dat61$x.func), 
           label = "*",size=8)+ggtitle('temple.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p61 <- leap.zig(dat62,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat62$x.rich), y = mean(dat62$x.func), 
           label = "*",size=8)+ggtitle('tyso.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p62 <- leap.zig(dat63,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat63$x.rich), y = mean(dat63$x.func), 
           label = "*",size=8)+ggtitle('ufrec.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p63 <- leap.zig(dat64,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat64$x.rich), y = mean(dat64$x.func), 
           label = "*",size=8)+ggtitle('ukul.za')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p64 <- leap.zig(dat65,type='cafe',xlim=c(0,25),ylim=c(0,1500), standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat65$x.rich), y = mean(dat65$x.func), 
           label = "*",size=8)+ggtitle('unc.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p65 <- leap.zig(dat66,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat66$x.rich), y = mean(dat66$x.func), 
           label = "*",size=8)+ggtitle('valm.ch')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p66 <- leap.zig(dat67,type='cafe', xlim=c(0,25),ylim=c(0,1500),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat67$x.rich), y = mean(dat67$x.func), 
           label = "*",size=8)+ggtitle('yarra.au ')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

p67 <- leap.zig(dat68,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat68$x.rich), y = mean(dat68$x.func), 
           label = "*",size=8)+ggtitle('yarra.au ')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p68 <- leap.zig(dat69,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat69$x.rich), y = mean(dat69$x.func), 
           label = "*",size=8)+ggtitle('yarra.au ')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p69 <- leap.zig(dat70,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat70$x.rich), y = mean(dat70$x.func), 
           label = "*",size=8)+ggtitle('yarra.au ')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p70 <- leap.zig(dat71,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat71$x.rich), y = mean(dat71$x.func), 
           label = "*",size=8)+ggtitle('yarra.au ')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

#PLOT USING GRID.ARRANGE WITH TIDYVERSE LEGEND FUNCTION
grid_arrange_shared_legend(p1,p2,p3,p4,p5,p7,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,p41,p42,p43,p45,p46,p48,p49,p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p60,p61,p62,p63,p64,p65,p66,p67,p68,p69,p70,p71,ncol=11,nrow=7)




###############################################################################################
###############################################################################################
###################################raw dat#######################################################\
###############################################################################################
###############################################################################################


p.all$s.loss <- -1*(p.all$x.rich - p.all$c.rich)
p.all$s.gain <- p.all$y.rich - p.all$c.rich
p.all$s.change <- p.all$y.rich - p.all$x.rich



#price boxplots


# grid arrange shared legend function
# TIDYVERSE SHARED LEGEND
# https://github.com/tidyverse/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs


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





#trt
colnames(test)
test$s.loss <- -1*(test$x.rich - test$c.rich)
test$s.gain <- test$y.rich - test$c.rich
test$s.change <- test$y.rich - test$x.rich
test$s.change<-test$y.rich-test$x.rich
test$c.func<-test$y.func-test$x.func

s.loss<-ggplot(test, aes(x=trt.y, y=s.loss, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs(title= 'c) Species Loss') +
  ylim(-10,2)+
  theme_bw() + theme(axis.text.x=element_blank()) 
s.loss

s.gain<-ggplot(test, aes(x=trt.y, y=s.gain, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs(title= 'd) Species Gains') +
  ylim(-2,10)+
  theme_bw() + theme(axis.text.x=element_blank()) 

SL<-ggplot(test, aes(x=trt.y, y=SL, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs(title= 'e) EF : Species Loss') +
  ylim(-300,10)+
  theme_bw() + theme(axis.text.x=element_blank()) 

SG<-ggplot(test, aes(x=trt.y, y=SG, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs( title= 'f) EF: Species Gains') +
  ylim(-10,300)+
  theme_bw() + theme(axis.text.x=element_blank()) 

CDE<-ggplot(test, aes(x=trt.y, y=CDE, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs( title= 'g) Context Dependent Effect') +
  ylim(-300,1000)+
  theme_bw() + theme(axis.text.x=element_blank()) 


rich<-ggplot(test, aes(x=trt.y, y=y.rich, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs(title= 'a) Richness') +
  ylim(0,40)+
  theme_bw() + theme(axis.text.x=element_blank()) 

bm<-ggplot(test, aes(x=trt.y, y=y.func, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs( title= 'b) Live Biomass') +
  ylim(0,2000)+
  theme_bw() + theme(axis.text.x=element_blank()) 

s.change.f<-ggplot(test, aes(x=trt.y, y=s.change, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs( title= 'h) Richness Change') +
  # ylim(-300,1000)+
  theme_bw() + theme(axis.text.x=element_blank()) 

c.func.f<-ggplot(test, aes(x=trt.y, y=c.func, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs( title= 'i) Biomass Change') +
  # ylim(-300,1000)+
  theme_bw() + theme(axis.text.x=element_blank()) 

grid_arrange_shared_legend(rich,bm,s.loss,s.gain,SL,SG,CDE,s.change.f,c.func.f,nrow=3,ncol=3)



colnames(npk)
npk$f.year_trt<-as.factor(as.character(npk$year.y))
levels(npk$f.year_trt)
npk$f.year_trt <- factor(npk$f.year_trt, levels = c("0","1","2","3","4","5","6","7","8","9","10","11"))
npk$s.loss <- -1*(npk$x.rich - npk$c.rich)
npk$s.gain <- npk$y.rich - npk$c.rich
npk$s.change <- npk$y.rich - npk$x.rich
npk$s.change<-npk$y.rich-npk$x.rich
npk$c.func<-npk$y.func-npk$x.func

rich<-ggplot(npk, aes(x=f.year_trt, y=y.rich, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(title= 'a) Richness') +
  ylim(0,40)+
  theme_bw() + theme(axis.text.x=element_blank()) 

bm<-ggplot(npk, aes(x=f.year_trt, y=y.func, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs( title= 'b) Live Biomass') +
  ylim(0,2000)+
  theme_bw() + theme(axis.text.x=element_blank()) 


s.loss<-ggplot(npk, aes(x=f.year_trt, y=s.loss, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs( title= 'c) Species Loss') +
  ylim(-10,2)+
  theme_bw() + theme(axis.text.x=element_blank()) 
s.loss

s.gain<-ggplot(npk, aes(x=f.year_trt, y=s.gain, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs( title= 'd) Species Gains') +
  ylim(-2,10)+
  theme_bw() + theme(axis.text.x=element_blank()) 

SL<-ggplot(npk, aes(x=f.year_trt, y=SL, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(title= 'e) EF : Species Loss') +
  ylim(-300,10)+
  theme_bw() + theme(axis.text.x=element_blank()) 

SG<-ggplot(npk, aes(x=f.year_trt, y=SG, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(title= 'f) EF: Species Gains') +
  ylim(-10,300)+
  theme_bw() + theme(axis.text.x=element_blank()) 

c.rich<-ggplot(npk, aes(x=f.year_trt, y=c.rich, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(title= 'g) Persistent Species') +
 # ylim(-300,1000)+
  theme_bw() + theme(axis.text.x=element_blank()) 

CDE<-ggplot(npk, aes(x=f.year_trt, y=CDE, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs( title= 'g) Biomass Change in Persistent Species') +
  ylim(-300,1000)+
  theme_bw() + theme(axis.text.x=element_blank()) 

s.change.f<-ggplot(npk, aes(x=f.year_trt, y=s.change, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(title= 'h) Richness Change') +
  # ylim(-300,1000)+
  theme_bw() + theme(axis.text.x=element_blank()) 

c.func.f<-ggplot(npk, aes(x=f.year_trt, y=c.func, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(title= 'i) Biomass Change') +
  # ylim(-300,1000)+
  theme_bw() + theme(axis.text.x=element_blank()) 

grid_arrange_shared_legend(rich,bm,nrow=1,ncol=2)

grid_arrange_shared_legend(s.loss,s.gain,SL,SG,CDE,nrow=2,ncol=3)

grid_arrange_shared_legend(rich,bm,s.loss,s.gain,SL,SG,CDE,nrow=3,ncol=3)

grid_arrange_shared_legend(rich,bm,s.loss,s.gain,SL,SG,CDE,s.change.f,c.func.f,nrow=3,ncol=3)
