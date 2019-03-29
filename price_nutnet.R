
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

p.all <- read.csv("~/Desktop/Academic/Data/NutNet/nutnet_price_all2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))
p <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))

p.all2<-separate(p.all,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)
list1<-distinct(p,site_code,site_name)
p.all3<-inner_join(p.all2,list1,by="site_code")

View(p.all3)
colnames(p.all3)

p.all4<-subset(p.all3, year.y!=0)
View(p.all4)

#take max value of year.y??


ctl<-p.all3[p.all3$trt.x %in% c('Control'),]
ctl2<-ctl[ctl$trt.y %in% c('Control'),]
ctl3<-ctl2[complete.cases(ctl2), ]
View(ctl3)
#max values only
ctl4<-ctl3 %>% group_by(site_code) %>% top_n(1, year.y)
ctl4<-ctl4[ctl4$block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'),]

View(ctl4)
npk<-p.all3[p.all3$trt.x %in% c('NPK'),]
npk2<-npk[npk$trt.y %in% c('NPK'),]
npk3<-npk2[complete.cases(npk2), ]
View(npk3)
#max values only
npk4<-npk3 %>% group_by(site_code) %>% top_n(1, year.y)
View(npk4)
npk4<-npk4[npk4$block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'),]


??leap.zig
theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
p1 <- leap.zig(ctl4,type='cafe',xlim=c(0,15),ylim=c(0,700),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(ctl4$x.rich), y = mean(ctl4$x.func), 
           label = "*",size=8)+ggtitle('Control 0 Control N')+theme_classic()+theme(axis.title.x=element_blank(),
                                                                                   axis.text.x=element_blank(),
                                                                                   axis.ticks.x=element_blank())
p2 <- leap.zig(npk4,type='cafe',xlim=c(0,15),ylim=c(0,700),standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(npk4$x.rich), y = mean(npk4$x.func), 
           label = "*",size=8)+ggtitle('NPK 0 NPK N')+theme_classic()+theme(axis.title.x=element_blank(),
                                                                            axis.text.x=element_blank(),
                                                                            axis.ticks.x=element_blank(),
                                                                            axis.text.y=element_blank(),
                                                                            axis.ticks.y = element_blank())

p2
grid_arrange_shared_legend(p1,p2,ncol=2,nrow=1)

View(ctl4)
test<-bind_rows(npk4,ctl4)
View(test)
test.partitions(test,type='cafe',treat.var = 'trt.y',control = 'Control',print=F,plot=T)



#inner
#+theme(axis.text.x=element_blank(),axis.text.y=element_blank())
#left column
#+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank(),axis.text.y=element_blank())
#left column middle row
#+theme(axis.title.x=element_blank(),axis.text.x=element_blank(),axis.ticks.x=element_blank())
#bottom row
#+theme(axis.title.x=element_blank(),axis.text.y=element_blank(),axis.ticks.y = element_blank(),axis.title.y=element_blank())
#bottom row middle
#+theme(axis.text.y=element_blank(),axis.ticks.y = element_blank(),axis.title.y=element_blank())

#,xlim=c(0,35),ylim=c(0,2500),

#test.partitions(pp3,type='price',treat.var = 'trt_year',control = 'Control_0 Control_3',print=F,plot=T)





# MATRIX OF EVERY GODDAMNED SITE FOR STUPID OOOOO FACTOR
npk3$site_code<-as.factor(npk3$site_code)
levels(npk3$site_code)
levels(npk3$site_name)
list2<-distinct(npk3,site_code,site_name)
list2
#npk4<-npk4[npk4$block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'),]
dat1<-npk4

levels(npk4$site_code)

View(npk3)

dat2<-dat1[dat1$site_code %in% c('arch.us'),]
dat3<-dat1[dat1$site_code %in% c('azi.cn'),]
dat4<-dat1[dat1$site_code %in% c('badlau.de'),]
dat5<-dat1[dat1$site_code %in% c('bari.ar'),]
dat6<-dat1[dat1$site_code %in% c('barta.us'),]
dat7<-dat1[dat1$site_code %in% c('bayr.de'),]
dat8<-dat1[dat1$site_code %in% c('bldr.us'),]
dat9<-dat1[dat1$site_code %in% c('bnbt.us'),]
dat10<-dat1[dat1$site_code %in% c('bnch.us'),]
dat11<-dat1[dat1$site_code %in% c('bogong.au'),]
dat12<-dat1[dat1$site_code %in% c('burrawan.au'),]

dat13<-dat1[dat1$site_code %in% c('burren.ie'),]
dat14<-dat1[dat1$site_code %in% c('cbgb.us'),]
dat15<-dat1[dat1$site_code %in% c('cdcr.us'),]
dat16<-dat1[dat1$site_code %in% c('cdpt.us'),]
dat17<-dat1[dat1$site_code %in% c('cereep.fr'),]
dat18<-dat1[dat1$site_code %in% c('chilcas.ar'),]
dat19<-dat1[dat1$site_code %in% c('comp.pt'),]
dat20<-dat1[dat1$site_code %in% c('cowi.ca'),]
dat21<-dat1[dat1$site_code %in% c('doane.us'),]
dat22<-dat1[dat1$site_code %in% c('frue.ch'),]
dat23<-dat1[dat1$site_code %in% c('gilb.za'),]

dat24<-dat1[dat1$site_code %in% c('glcr.us'),]
dat25<-dat1[dat1$site_code %in% c('hall.us'),]
dat26<-dat1[dat1$site_code %in% c('hart.us'),]
dat27<-dat1[dat1$site_code %in% c('hnvr.us'),]
dat28<-dat1[dat1$site_code %in% c('hopl.us'),]
dat29<-dat1[dat1$site_code %in% c('jena.de'),]
dat30<-dat1[dat1$site_code %in% c('kbs.us'),]
dat31<-dat1[dat1$site_code %in% c('kibber.in'),]
dat32<-dat1[dat1$site_code %in% c('kidman.au'),]
dat33<-dat1[dat1$site_code %in% c('kilp.fi'),]
dat34<-dat1[dat1$site_code %in% c('koffler.ca'),]

dat35<-dat1[dat1$site_code %in% c('konz.us'),]
dat36<-dat1[dat1$site_code %in% c('lake.us'),]
dat37<-dat1[dat1$site_code %in% c('lancaster.uk'),]
dat38<-dat1[dat1$site_code %in% c('look.us'),]
dat39<-dat1[dat1$site_code %in% c('marc.ar'),]
dat40<-dat1[dat1$site_code %in% c('mcla.us'),]
dat41<-dat1[dat1$site_code %in% c( 'msla.us'),]
dat42<-dat1[dat1$site_code %in% c('msum.us'),]
dat43<-dat1[dat1$site_code %in% c('mtca.au'),]
dat44<-dat1[dat1$site_code %in% c('pape.de'),]
dat45<-dat1[dat1$site_code %in% c('pich.ec'),]

dat46<-dat1[dat1$site_code %in% c('ping.au'),]
dat47<-dat1[dat1$site_code %in% c('pinj.au'),]
dat48<-dat1[dat1$site_code %in% c('podo.ec'),]
dat49<-dat1[dat1$site_code %in% c('potrok.ar'),]
dat50<-dat1[dat1$site_code %in% c('sage.us'),]
dat51<-dat1[dat1$site_code %in% c('sava.us'),]
dat52<-dat1[dat1$site_code %in% c('sedg.us'),]
dat53<-dat1[dat1$site_code %in% c('sereng.tz'),]
dat54<-dat1[dat1$site_code %in% c('sevi.us'),]
dat55<-dat1[dat1$site_code %in% c('sgs.us'),]
dat56<-dat1[dat1$site_code %in% c('shps.us'),]

dat57<-dat1[dat1$site_code %in% c('sier.us'),]
dat58<-dat1[dat1$site_code %in% c('smith.us'),]
dat59<-dat1[dat1$site_code %in% c('spin.us'),]
dat60<-dat1[dat1$site_code %in% c('summ.za'),]
dat61<-dat1[dat1$site_code %in% c('temple.us'),]
dat62<-dat1[dat1$site_code %in% c('tyso.us'),]
dat63<-dat1[dat1$site_code %in% c('ufrec.us'),]
dat64<-dat1[dat1$site_code %in% c('ukul.za'),]
dat65<-dat1[dat1$site_code %in% c('unc.us'),]
dat66<-dat1[dat1$site_code %in% c('valm.ch'),]
dat67<-dat1[dat1$site_code %in% c('yarra.au'),]





theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
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
           label = "*",size=8)+ggtitle('bayr.de')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p7 <- leap.zig(dat8,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat8$x.rich), y = mean(dat8$x.func), 
           label = "*",size=8)+ggtitle('bldr.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p8 <- leap.zig(dat9,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat9$x.rich), y = mean(dat9$x.func), 
           label = "*",size=8)+ggtitle('bnbt.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p9 <- leap.zig(dat10,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat10$x.rich), y = mean(dat10$x.func), 
           label = "*",size=8)+ggtitle('bnch.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p10 <- leap.zig(dat11,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat11$x.rich), y = mean(dat11$x.func), 
           label = "*",size=8)+ggtitle('bogong.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p11 <- leap.zig(dat12,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat12$x.rich), y = mean(dat12$x.func), 
           label = "*",size=8)+ggtitle('burrawan.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

p12 <- leap.zig(dat13,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat13$x.rich), y = mean(dat13$x.func), 
           label = "*",size=8)+ggtitle('burren.ie')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p13 <- leap.zig(dat14,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat14$x.rich), y = mean(dat14$x.func), 
           label = "*",size=8)+ggtitle('cbgb.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
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
p19 <- leap.zig(dat20,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat20$x.rich), y = mean(dat20$x.func), 
           label = "*",size=8)+ggtitle('cowi.ca')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
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
p26 <- leap.zig(dat27,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat27$x.rich), y = mean(dat27$x.func), 
           label = "*",size=8)+ggtitle('hnvr.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
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
p43 <- leap.zig(dat44,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat44$x.rich), y = mean(dat44$x.func), 
           label = "*",size=8)+ggtitle('pape.de')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p44 <- leap.zig(dat45,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat45$x.rich), y = mean(dat45$x.func), 
           label = "*",size=8)+ggtitle('pich.ec')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))

p45 <- leap.zig(dat46,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat46$x.rich), y = mean(dat46$x.func), 
           label = "*",size=8)+ggtitle('ping.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p46 <- leap.zig(dat47,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat47$x.rich), y = mean(dat47$x.func), 
           label = "*",size=8)+ggtitle('pinj.au')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p47 <- leap.zig(dat48,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat48$x.rich), y = mean(dat48$x.func), 
           label = "*",size=8)+ggtitle('podo.ec')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p48 <- leap.zig(dat49,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat49$x.rich), y = mean(dat49$x.func), 
           label = "*",size=8)+ggtitle('potrok.ar')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p49 <- leap.zig(dat50,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat50$x.rich), y = mean(dat50$x.func), 
           label = "*",size=8)+ggtitle('sage.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p50 <- leap.zig(dat51,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat51$x.rich), y = mean(dat51$x.func), 
           label = "*",size=8)+ggtitle('sava.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p51 <- leap.zig(dat52,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat52$x.rich), y = mean(dat52$x.func), 
           label = "*",size=8)+ggtitle('sedg.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
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
p64 <- leap.zig(dat65,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat65$x.rich), y = mean(dat65$x.func), 
           label = "*",size=8)+ggtitle('unc.us')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p65 <- leap.zig(dat66,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat66$x.rich), y = mean(dat66$x.func), 
           label = "*",size=8)+ggtitle('valm.ch')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))
p66 <- leap.zig(dat67,type='cafe', standardize = FALSE,raw.points = F)+ 
  annotate("text", x = mean(dat67$x.rich), y = mean(dat67$x.func), 
           label = "*",size=8)+ggtitle('yarra.au ')+theme_classic()+ theme(plot.title = element_text(size=9),axis.title.x=element_blank(),axis.title.y=element_blank(),axis.text=element_text(size=6))


#PLOT USING GRID.ARRANGE WITH TIDYVERSE LEGEND FUNCTION
grid_arrange_shared_legend(p1,p2,p3,p4,p5,p7,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24,p25,p26,p27,p28,p29,p30,p31,p32,p33,p34,p35,p36,p37,p38,p39,p40,p41,p42,p43,p45,p46,p48,p49,p50,p51,p52,p53,p54,p55,p56,p57,p58,p59,p60,p61,p62,p63,p64,p65,p66,ncol=11,nrow=6)



###############################################################################################
###############################################################################################
###################################old stuff#######################################################\
###############################################################################################
###############################################################################################
rm(list=ls())

library(ggplot2)
library(tidyverse)
library(brms)
library(gridExtra)
library(grid)
library(bayesplot)
library(priceTools)



p.all <- read.csv("~/Desktop/Academic/Data/NutNet/nutnet_price_all2.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


colnames(p.all)
p.all2<-p.all[p.all$trt.x %in% c('NPK'),]
p.all3<-p.all2[p.all2$trt.y %in% c('NPK'),]
levels(p.all3$trt_year)
View(p.all3)
p.all4<-separate(p.all3,site.year.id.x,into=c("site_code","year.x"),sep = "_", remove=FALSE)

p.all4$f.year.y<-as.factor(p.all4$year.y)
p.all4$plot<-as.factor(p.all4$plot)
p.all4$site_code<-as.factor(p.all4$site_code)
#price vectors
p.all.c<-p.all3[complete.cases(p.all3), ]

theme_update(panel.border = element_rect(linetype = "solid", colour = "black"))
leap.zig(p.all.c,type='cafe',xlim=c(0,15),ylim=c(0,700),standardize = FALSE,raw.points = FALSE)+ 
  annotate("text", x = mean(p.all.c$x.rich), y = mean(p.all.c$x.func), 
           label = "*",size=8)+ggtitle('NPK through time')+theme_classic()

head(p.all.c)
levels(p.all$trt_year)
p.all.p<-p.all[p.all$trt_year %in% c('Control_0 Control_0'),]
View(p.all.p)
p.all.test<-bind_rows(p.all3,p.all.p)
View(p.all.test)
p.all.c.t<-p.all.test[complete.cases(p.all.test),]
p.all.c.t$year.y<-as.factor(as.character(p.all.c.t$year.y))
View(p.all.c.t)
p.all.c.t$year.y <- factor(p.all.c.t$year.y, levels = c("0","1","2","3","4","5","6","7","8","9","10","11"))

View(p.all.c.t)

test.partitions(p.all.c.t,type='cafe',treat.var = 'year.y',control = '0',print=F,plot=T)


p.all.c.t$s.loss <- -1*(p.all.c.t$x.rich - p.all.c.t$c.rich)
p.all.c.t$s.gain <- p.all.c.t$y.rich - p.all.c.t$c.rich
p.all.c.t$s.change <- p.all.c.t$y.rich - p.all.c.t$x.rich

p.all.c.t<-p.all.c.t[p.all.c.t$block %in% c('1 1','2 2','3 3','4 4','5 5','6 6'),]

View(p.all)

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





s.loss<-ggplot(p.all.c.t, aes(x=year.y, y=s.loss, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=year.y)) +
  labs(x = 'Species Loss',
       y = 'Year.y', title= 'c) Species Loss') +
  ylim(-10,2)+
  theme_bw() + theme(axis.text.x=element_blank()) 
s.loss

s.gain<-ggplot(p.all.c.t, aes(x=year.y, y=s.gain, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=year.y)) +
  labs(x = 'Species Gains',
       y = 'Year.y', title= 'd) Species Gains') +
  ylim(-2,10)+
  theme_bw() + theme(axis.text.x=element_blank()) 

SL<-ggplot(p.all.c.t, aes(x=year.y, y=SL, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=year.y)) +
  labs(x = 'EF: SL',
       y = 'Year.y', title= 'e) EF : Species Loss') +
  ylim(-300,10)+
  theme_bw() + theme(axis.text.x=element_blank()) 

SG<-ggplot(p.all.c.t, aes(x=year.y, y=SG, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=year.y)) +
  labs(x = 'EF : SG',
       y = 'Year.y', title= 'f) EF: Species Gains') +
  ylim(-10,300)+
  theme_bw() + theme(axis.text.x=element_blank()) 

CDE<-ggplot(p.all.c.t, aes(x=year.y, y=CDE, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=year.y)) +
  labs(x = 'CDE',
       y = 'Year.y', title= 'g) Context Dependent Effect') +
  ylim(-300,1000)+
  theme_bw() + theme(axis.text.x=element_blank()) 

#trt
test$s.loss <- -1*(test$x.rich - test$c.rich)
test$s.gain <- test$y.rich - test$c.rich
test$s.change <- test$y.rich - test$x.rich


s.loss<-ggplot(test, aes(x=trt.y, y=s.loss, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs(x = 'Species Loss',
       y = 'trt.y', title= 'c) Species Loss') +
  ylim(-10,2)+
  theme_bw() + theme(axis.text.x=element_blank()) 
s.loss

s.gain<-ggplot(test, aes(x=trt.y, y=s.gain, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs(x = 'Species Gains',
       y = 'trt.y', title= 'd) Species Gains') +
  ylim(-2,10)+
  theme_bw() + theme(axis.text.x=element_blank()) 

SL<-ggplot(test, aes(x=trt.y, y=SL, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs(x = 'EF: SL',
       y = 'trt.y', title= 'e) EF : Species Loss') +
  ylim(-300,10)+
  theme_bw() + theme(axis.text.x=element_blank()) 

SG<-ggplot(test, aes(x=trt.y, y=SG, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs(x = 'EF : SG',
       y = 'trt.y', title= 'f) EF: Species Gains') +
  ylim(-10,300)+
  theme_bw() + theme(axis.text.x=element_blank()) 

CDE<-ggplot(test, aes(x=trt.y, y=CDE, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt.y)) +
  labs(x = 'CDE',
       y = 'trt.y', title= 'g) Context Dependent Effect') +
  ylim(-300,1000)+
  theme_bw() + theme(axis.text.x=element_blank()) 


rich<-ggplot(p, aes(x=trt, y=rich, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt)) +
  labs(x = 'Richness',
       y = 'Year', title= 'a) Richness') +
  ylim(0,40)+
  theme_bw() + theme(axis.text.x=element_blank()) 

bm<-ggplot(p, aes(x=trt, y=live_mass, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=trt)) +
  labs(x = 'Biomass',
       y = 'Year', title= 'b) Live Biomass') +
  ylim(0,2000)+
  theme_bw() + theme(axis.text.x=element_blank()) 

grid_arrange_shared_legend(rich,bm,s.loss,s.gain,SL,SG,CDE,nrow=3,ncol=3)



p <- read.csv("/Users/el50nico/Desktop/Academic/Data/NutNet/DataOutput/plot_calc.csv",header=T,fill=TRUE,sep=",",na.strings=c(""," ","NA","NA ","na"))


plot<-p[p$trt %in% c('NPK'),]
plot$f.year_trt<-as.factor(as.character(plot$year_trt))
plot$f.year_trt <- factor(plot$f.year_trt, levels = c("0","1","2","3","4","5","6","7","8","9","10","11"))


rich<-ggplot(plot, aes(x=f.year_trt, y=rich, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(x = 'Richness',
       y = 'Year', title= 'a) Richness') +
  ylim(0,40)+
  theme_bw() + theme(axis.text.x=element_blank()) 

bm<-ggplot(plot, aes(x=f.year_trt, y=live_mass, variable)) +
  stat_summary(fun.y=mean, geom="point", 
               size=0.2)+
  geom_boxplot(aes(color=f.year_trt)) +
  labs(x = 'Biomass',
       y = 'Year', title= 'b) Live Biomass') +
  ylim(0,2000)+
  theme_bw() + theme(axis.text.x=element_blank()) 

grid_arrange_shared_legend(rich,bm,nrow=1,ncol=2)

grid_arrange_shared_legend(s.loss,s.gain,SL,SG,CDE,nrow=2,ncol=3)

grid_arrange_shared_legend(rich,bm,s.loss,s.gain,SL,SG,CDE,nrow=3,ncol=3)

