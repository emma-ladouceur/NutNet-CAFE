
#number of observations
niter<-100

#make fake data
SL<-data.frame(S=-abs(rnorm(niter, mean = 0.2, sd=0.05)),
               B=-abs(rnorm(niter, mean = -7.5, sd=1)))

SG<-data.frame(S=-abs(rnorm(niter, mean = -0.15, sd=0.05)),
               B=abs(rnorm(niter, mean = 4.7, sd=0.5)))

CS<-data.frame(S=0,B=rnorm(niter, mean=9.2, sd=1))

#get means for each
SL_mean<-colMeans(SL)
SG_mean<-colMeans(SG)
CS_mean<-colMeans(CS)

#re-write values as contrasts
SL_contr<-data.frame(S=SL$S-SL_mean["S"],
                     B=SL$B-SL_mean["B"])
SG_contr<-data.frame(S=SG$S-SG_mean["S"],
                     B=SG$B-SG_mean["B"])
CS_contr<-data.frame(S=CS$S-CS_mean["S"],
                     B=CS$B-CS_mean["B"])


#method 1
#plot
meanpts<-data.frame(rbind(SL_mean,
                          SL_mean+SG_mean,
                          SL_mean+SG_mean+CS_mean))


plot(x=meanpts$S, y=meanpts$B, xlim=c(-0.5, 0), ylim=c(-20,20))
abline(h=0, v=0)

#plotting_points
SL_plotting<-data.frame(S=SL_contr$S+meanpts$S[1],
                        B=SL_contr$B+meanpts$B[1])

SG_plotting<-data.frame(S=SG_contr$S+meanpts$S[2],
                        B=SG_contr$B+meanpts$B[2])

CS_plotting<-data.frame(S=CS_contr$S+meanpts$S[3],
                        B=CS_contr$B+meanpts$B[3])


points(SL_plotting$S, SL_plotting$B, cex=0.2, col="red")
points(SG_plotting$S, SG_plotting$B, cex=0.2, col="blue")
points(CS_plotting$S, CS_plotting$B, cex=0.2, col="green")

#This plot shows uncertainty in the parameter estimates themselves






#method 2
plot(x=meanpts$S, y=meanpts$B, xlim=c(-0.5, 0), ylim=c(-20,20))
abline(h=0, v=0)

#plotting_points
SL_plotting<-data.frame(S=SL$S,
                        B=SL$B)

SG_plotting<-data.frame(S=SL$S+SG$S,
                        B=SL$B+SG$B)

CS_plotting<-data.frame(S=SL$S+SG$S+CS$S,
                        B=SL$B+SG$B+CS$B)


points(SL_plotting$S, SL_plotting$B, cex=0.2, col="red")
points(SG_plotting$S, SG_plotting$B, cex=0.2, col="blue")
points(CS_plotting$S, CS_plotting$B, cex=0.2, col="green")

#This plot shows uncertainty in *where we end up because of outputs of the partition*





#method 3
#plot
plot(x=meanpts$S, y=meanpts$B, xlim=c(-0.5, 0), ylim=c(-20,20))
abline(h=0, v=0)

#plotting_points
SL_plotting<-data.frame(S=SL$S,
                        B=SL$B)

SG_plotting<-data.frame(S=SL$S+SG$S-SL_contr$S,
                        B=SL$B+SG$B-SL_contr$B)

resid_var_S<-0.005
adj_contr_S<-rnorm(niter, mean = 0, sd=sqrt(resid_var_S))
CS_plotting<-data.frame(S=SL$S+SG$S-SL_contr$S-SG_contr$S+adj_contr_S,
                        B=SL$B+SG$B+CS$B-SL_contr$B-SG_contr$B)


points(SL_plotting$S, SL_plotting$B, cex=0.2, col="red")
points(SG_plotting$S, SG_plotting$B, cex=0.2, col="blue")
points(CS_plotting$S, CS_plotting$B, cex=0.2, col="green")

#This plot allows us to add variance wherever we'd like to



