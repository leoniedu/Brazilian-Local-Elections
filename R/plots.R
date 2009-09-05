library(ggplot2)
source("../R/functions.R")
spath <- function(x) paste("../results/",office,"/",year,"/",x,sep="")

year <- 2000
office <- "Vereador"

## read data
load(paste("../data/data",year,office,".RData",sep=""))
load(file=paste("../data/coalitions",year,".RData",sep=""))

## read MCMC simulations
##load("model.bug2009-04-28.RData")
##load("../results/Vereador/2000/model.bug2009-05-01small.RData")

##load("../results/Vereador/2000/model.bug2009-07-24.RData")
#load("../results/Vereador/2004/model.bug2009-07-26.RData")
load("../results/Vereador/2000/model.bug2009-07-27.RData")
##load("../results/Vereador/2004/model.bug2009-07-26small.RData")





## jags <- jags1.s
## ##jags <- window(jags1.s,start=2000)
## tmp <- codadrop(jags,parms.tokeep=c("mu.x\\[1\\]","mu.x\\[2\\]"))
## sc <- lapply(tmp,function(x) (x[,2]-x[,1])/2)
## mn <- lapply(tmp,function(x) x[,1])
## jags.star <- jags
## j1 <- c(grep(c("x\\["),varnames(jags)))
## for (i in 1:length(jags.star)) {
##     jags.star[[i]][,j1] <- (jags.star[[i]][,j1]-mn[[i]])/sc[[i]] -1
##     jags.star[[i]][,-j1] <- (jags.star[[i]][,-j1])/abs(sc[[i]])
## }
## rm(jags)


jags.star <- jags1.s
rm(jags1.s)




library(plyr)
jagsfit <- coda2bugs(jags.star
                     ,parms.tokeep="mu"
                     ##,parms.todrop=c("g\\[","^x\\[")
                     )
tmp <- merge(data.frame(party=lp,mu.x=jagsfit$mean$mu.x## ,v=jagsfit$mean$v
                        ),beta.all.pp)
tmp <- merge(with(jagsfit$mean,data.frame(party=lp,mu.x)),beta.all.pp)
t1 <- ddply(tmp,"t",function(z) with(z,cor(z$mu.x,z$Mean.p)))
save(t1,file=spath("corrRCcoalit.RData"))
plot(t1)


##fix this!
##jags.star <- jags.star[c(2,4)]

##FIX: TAKE THIS OUT. THIS SHOULD NOT BE NECESSARY IF CONVERGENCE IS REACHED
## ii <- apply(jags.star[,grep("^x\\[1,.*]",varnames(jags.star))][[1]],2,median)>0
## if (sum(ii)>0) {
##     ij <- grep(paste("^x\\[.*,",which(ii),"\\]",sep=""),varnames(jags.star))    
##     jags.star[[1]][,ij] <- jags.star[[1]][,ij]*-1
## }
## ii <- apply(jags.star[,grep("^x\\[1,.*]",varnames(jags.star))][[2]],2,median)>0
## if (sum(ii)>0) {
##     ij <- grep(paste("^x\\[.*,",which(ii),"\\]",sep=""),varnames(jags.star))    
##     jags.star[[2]][,ij] <- jags.star[[2]][,ij]*-1
## }



##plot ideal points
##x.bugs <- coda2bugs(jags.star,parms.tokeep="^mu")



## Plot predicted probabilities
b.bugs <- coda2bugs(jags.star,parms.tokeep="b")$sims.list$b

vec <- seq(0,1.81,length=100)
vec.t <- exp(vec+log(9))
xm <- sapply(vec,function(x) b.bugs[,1]+b.bugs[,2]*x)
xm.m <- melt(xm)
xms <- data.frame(t(apply(xm,2,quantile,probs=c(.025,.5,.975))))

nsims <- nrow(b.bugs)
xm.m <- data.frame(magnitude=sort(unique(gr$total.elected)))
xm.m$z.log.magnitude <- log(xm.m$magnitude)-log(9)
xm.m <- merge(data.frame(sim=sample(1:nsims,500)),xm.m,all=TRUE)
xm.m$yhat <- with(xm.m,b.bugs[sim,1]+z.log.magnitude*b.bugs[sim,2])
xm.m$phat <- plogis(xm.m$yhat)

##sims <- sample(1:nsims,500)
sims <- 1:nsims
ps1 <- .05
ps2 <- .05
xm.m <- get.pred(seq(0,2,length=11),sims=sims)
xm.m$magnitude <- 9
xm.rm <- xm.m
xm.m$magnitude <- 15
xm.rm <- rbind(xm.rm,xm.m)
xm.m$magnitude <- 20
xm.rm <- rbind(xm.rm,xm.m)
xm.m <- xm.rm
xm.m$party.size1 <- .05
xm.m$party.size2 <- .05
xm.rm <- xm.m
xm.m$party.size1 <- .25
xm.m$party.size2 <- .05
xm.rm <- rbind(xm.rm,xm.m)
xm.m$party.size1 <- .25
xm.m$party.size2 <- .25
xm.rm <- rbind(xm.rm,xm.m)

xm.rm <- within(xm.rm,{
    yhat1 <- b.bugs[sims,1]+b.bugs[sims,2]*log(party.size1)+
        b.bugs[sims,3]*(log(magnitude)-log(9))-x
    yhat2 <- b.bugs[sims,1]+b.bugs[sims,2]*log(party.size2)+
        b.bugs[sims,3]*(log(magnitude)-log(9))-x
    phat <- plogis(yhat1)*plogis(yhat2)
    `Number of seats`=factor(magnitude)
})




## FIX: Limits in ggplot selects out the observations out of range
## when calculating (for example) confidence intervals
s.bugs.m <- melt(do.call(rbind,jags.star[,c("sigma.g","sigma.x")]))
s.bugs.m$Coefficient <- factor(s.bugs.m$X2,levels=c("sigma.g","sigma.x"),labels=c("sigma city","sigma x"))
m <- ggplot(s.bugs.m,aes(x=as.numeric(Coefficient),y=value))
m <- m+ stat_summary(fun.data=iqr,geom="pointrange")

##m <- m+scale_x_continuous(name="",breaks=c(1:3),labels=levels(s.bugs.m$Coefficient),limits=c(.9,3.1),expand=c(0,0))
m <- m+scale_x_continuous(name="",breaks=c(1:2),labels=c(expression(sigma[city]),expression(sigma[x])),limits=c(.9,2.1),expand=c(0,0))
m <- m+theme_bw3()+scale_y_continuous(name="",limits=c(0,1.5),expand=c(0,0))##+coord_flip()
##m <- m+geom_line(data=data.frame(x=c(.9,3.1),y=c(0,0)),aes(x=x,y=y),colour="red",size=.6)
m <- m+coord_flip()
m

     
pdf(file=spath("sigma.pdf"),width=3,height=2)
print(m)
dev.off()


b.bugs.m <- melt(do.call(rbind,jags.star[,grep("^b",varnames(jags.star))]))
b.bugs.m$Coefficient <- factor(b.bugs.m$X2,levels=c("b[1]","b[2]","b[3]","b[4]"),labels=c("Intercept","Ln party size","Ln number of seats","Party of the governor"))

b.bugs.c <- recast(data.frame(b.bugs.m),Coefficient+variable~.,measure.var="value",fun.aggregate=quantile,probs=c(.025,.5,.975))

m <- qplot(Coefficient,X50.,data=b.bugs.c,ymin=X2.5.,ymax=X97.5.,geom="pointrange")+scale_y_continuous(limits=c(min(b.bugs.c[,-c(1:2)])*1.1,1),expand=c(0,0))+coord_flip()+theme_bw3()
m

pdf(file=spath("bCIplot.pdf"),width=4,height=3)
print(m)
dev.off()

xm.rm$party.size <- factor(with(xm.rm,paste("size of party i,j: ",party.size1,",",party.size2,sep="")))



m <- ggplot(xm.rm,aes(x=x,y=phat,fill=`Number of seats`))
m <- m+ stat_summary(fun.data=iqr,geom="ribbon")
m <- m+facet_wrap(~party.size)+scale_y_continuous(limits=c(0,.65),expand=c(0,0),name="Probability of coalition formation")+scale_x_continuous(limits=c(0,2),name="Spatial distance")
m <- m+theme_bw2()+opts(legend.position = c(0.65,0.35))
## a center line
m <- m+stat_summary(fun.data=iqr,geom="ribbon",probs=c(.49,.5,.51))
m

pdf(file=spath("predictions.pdf"))
print(m)
dev.off()


m <- ggplot(xm.rm,aes(x=x,y=phat,fill=`party.size`))
m <- m+ stat_summary(fun.data=iqr,geom="ribbon",alpha=2/3)
m <- m+facet_wrap(~magnitude,scales = "free_y")+scale_y_continuous(name="Probability of coalition formation")+scale_x_continuous(limits=c(0,2),name="Spatial distance")
m <- m+theme_bw2()
## a center line
m <- m+stat_summary(fun.data=iqr,geom="ribbon",probs=c(.49,.5,.51))
m



xyplot(mu.x~Mean.p|factor(t),data=tmp)


(s.i <- (year-1796)/4)
##s.i <- 50

pprior <- subset(beta.all.pp,t==s.i)
## pp is ppb in 2000
#pprior$party <- car::recode(pprior$party,"'pp'='ppb'")
fit.party <- merge(
                   with(jagsfit$mean,data.frame(party=lp,mean.mu.x=mu.x,sd.mu.x=jagsfit$sd$mu.x## ,mu.v
                                                )),
                   pprior[,c("party","Mean.p","SD.p")],all=TRUE)
dv2 <- merge(dv,mun.data[,c("uf","municipio")],all=TRUE)
dv2 <- recast(dv2,party~variable,measure.var=c("votes","elected","candidates"),fun.aggregate=sum,na.rm=TRUE)
dv2$party=gsub(" +","",tolower(dv2$party))
dv2 <- merge(fit.party,dv2,all=TRUE)
se <- ggplot(dv2, aes(x = mean.mu.x, ymin=Mean.p - SD.p, ymax=Mean.p + SD.p, y=Mean.p))
se <- se+geom_abline(a=0,b=1,linetype=2,colour="gray")+geom_point(aes(size=votes/1000),colour=rgb(.95,0,0,.7))+geom_linerange(colour="gray")+ theme_bw2() + coord_equal()+geom_text(label=toupper(dv2$party),size=I(3),hjust=1,vjust=-1)+geom_segment(aes(x=mean.mu.x-sd.mu.x,xend = mean.mu.x+sd.mu.x, yend =Mean.p) ,colour="gray")
se <- se+xlab("Ideology - electoral coalitions")+ylab('Ideology - roll call votes (Câmara dos Deputados)')
##correlation
with(subset(dv2,!party%in%c("pt","pfl")),cor(mean.mu.x,Mean.p,use="pair"))
se

pdf(file=spath("coalExample.pdf"),width=5,height=5)
print(se)
dev.off()

## a single state
xstat <- coda2bugs(jags.star,parms.tokeep=c("mu.x",paste("^x",sep="")))
tmp <- melt(xstat$mean$x);names(tmp) <- c("party","state","mean.x")
fit.party.s <- tmp
tmp <- melt(xstat$sd$x);names(tmp) <- c("party","state","sd.x")
fit.party.s <- merge(fit.party.s,tmp)
fit.party.s$party <- factor(fit.party.s$party,labels=lp)
fit.party.s$state <- factor(fit.party.s$state,labels=levels(gr$uf))

##FIX: show color for four largest parties only
tmp <- merge(fit.party.s,data.frame(party=lp,mu.x=xstat$mean$mu.x,sd.mu.x=xstat$sd$mu.x))
##qplot(mean.x,mu.x,data=tmp)+facet_wrap(~state)
tmp$x.name <- reorder(tmp$state,tmp$mean.x,function(x) max(x)-min(x))
tmp$x <- as.numeric(tmp$x.name)
tmp1 <- subset(tmp,!party%in%c("pt","pfl","pmdb","psdb"))
##tmp1 <- tmp
p <- ggplot(tmp1, aes(y=x, x=mean.x, group=party,colour=party,label=party))
tmp2 <- subset(tmp,party%in%c("pt","pfl","pmdb","psdb"))
p <- p+geom_point(data=tmp2,aes=aes(y=x,x=mean.x,colour=party),size=4)
##p <- p+scale_colour_brewer()+theme_bw2()
p <- p+scale_colour_hue()
p <- p+scale_y_continuous(name="",breaks=1:length(levels(tmp$x.name)),labels=levels(tmp$x.name))
p <- p+opts(panel.grid.minor = theme_line(colour = NA, size = 0.5))
##p <- p+scale_colour_brewer(palette="Set2")
p <- p+scale_colour_hue()                        
p <- p+scale_x_continuous(name="")
p <- p + geom_text(colour=rgb(.1,.1,.1,.7),size=2.5,angle=0,justification="centre")
p##+coord_flip()

pdf(file=spath("stateideology.pdf"))
print(p)
dev.off()



tmp$state <- reorder(tmp$state,tmp$mean.x,FUN=var)
p <- qplot(state,mean.x,data=subset(tmp,party%in%c("pt","pfl","pmdb","psdb")
                                    ),geom="line",colour=party,group=party,size=I(2))
p <- p+theme_bw2()
##p <- p+opts(legend.position = "none")
p+geom_line(data=subset(tmp,!party%in%c("pt","pfl","pmdb","psdb")),aes=aes(x=state,y=mean.x),colour="gray70",size=.5)##+coord_flip()


fit.party.s <- merge(fit.party.s,pprior[,c("t","Mean.p","SD.p","party")])


tmp <- ddply(fit.party.s,"state",function(x) with(x,data.frame(state=state[1],p=cor(mean.x,Mean.p,use="pair"))))
tmp$state <- reorder(tmp$state,tmp$p)
qplot(state,p,data=tmp)


fit.party.s$party <- reorder(fit.party.s$party,fit.party.s$Mean.p)

## se <- ggplot(subset(fit.party.s ##,state%in%c("SP","BA")
##                     ), aes(x = mean.x, ymin=Mean.p - SD.p, ymax=Mean.p + SD.p, y=Mean.p,label=party,colour=party))
## ##se <- se+geom_abline(a=0,b=1,linetype=2,colour="gray")
## ## se+geom_point()## + coord_equal()
## ##+geom_text(size=I(3),hjust=1.5,vjust=2)
## se <- se+
##     geom_segment(aes(x=mean.x-sd.x,xend = mean.x+sd.x, yend =Mean.p) ,colour="gray")+
##     theme_bw()+
##     xlab("Ideology - electoral coalitions")+
##     ylab('Ideology - Camara (national) roll call votes')+
##     facet_wrap(~state)+
##     ##geom_segment(aes(x=mean.x,xend = mean.x,y=Mean.p - SD.p, yend =Mean.p+SD.p) ,colour="black")
##     geom_linerange(colour="gray")+
##     geom_point(size=I(1.2)) + coord_equal()
## ##geom_segment(aes(x=mean.x,xend =mean.x,y=Mean.p-SD.p, yend =Mean.p+SD.p) ,colour="black",size=I(2))
## se

## s <- which(levels(gr$uf)=="PR")
## xstat <- coda2bugs(jags.star,parms.tokeep=c("mu.x",paste("^x\\[.*,",s,"\\]",sep="")))
## plot(xstat)
## xstat
## fit.party.s <- merge(
##                      data.frame(party=lp,mean.x=xstat$mean$x[,1],sd.x=xstat$sd$x[,1]),
##                      pprior[,c("party","Mean.p","SD.p")],all=TRUE)
## with(fit.party.s,cor(mean.x,Mean.p,use="pair"))                     

se <- ggplot(fit.party.s, aes(x = mean.x, ymin=Mean.p - SD.p, ymax=Mean.p + SD.p, y=Mean.p))
se <- se+geom_abline(a=0,b=1,linetype=2,colour="gray")+geom_linerange(colour="gray")+ theme_bw2() ## + coord_equal()
se <- se+geom_text(label=toupper(fit.party.s$party),size=I(3),hjust=1.5,vjust=2)+geom_segment(aes(x=mean.x-sd.x,xend = mean.x+sd.x, yend =Mean.p) ,colour="gray")+geom_point()
se+xlab("Ideology - electoral coalitions")+ylab('Ideology - Camara (national) roll call votes')

