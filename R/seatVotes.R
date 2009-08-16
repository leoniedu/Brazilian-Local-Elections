###################################################
### chunk number 1: selyear
###################################################

library(reshape)
library(ggplot2)

year <- 2000
load(file=paste("../data/candidates",year,".RData",sep=""))



###################################################
### chunk number 2: select
###################################################

dnow <- unique(subset(dnow
                      ,office=="Vereador"
                      ))
dnow <- clean.df(dnow)

dnow$munuf <- paste(dnow$municipio,dnow$uf,sep=";")
dnow$coalmunuf <- paste(dnow$coalition,dnow$municipio,dnow$uf,sep=";")
dnow$partymunuf <- paste(dnow$party,dnow$municipio,dnow$uf,sep=";")



###################################################
### chunk number 3: individual
###################################################

dnow <- subset(dnow
               ##,votes>0
               ,tolower(vote.type)=="individual"
               )





###################################################
### chunk number 4: byparty
###################################################

dnow$candidates <- 1
dnow.p <- recast(dnow,municipio+uf+party+office+coalition~variable,measure.var=c("votes","elected","candidates"),fun.aggregate="sum")
## number of parties in the coalition
dnow.p <- data.frame(dnow.p)
dnow.p$coalmunuf <- with(dnow.p,paste(coalition,municipio,uf,sep=";"))

dnow.p$npartycoal <- as.numeric(as.character(with(dnow.p,ave(as.character(party),coalmunuf,FUN=function(x) length(unique(x))))))




###################################################
### chunk number 5: bymun
###################################################

dnow.m <- recast(dnow,municipio+uf+office~variable,measure.var=c("votes","elected","candidates"),fun.aggregate="sum")
dnow.m <- data.frame(dnow.m)



###################################################
### chunk number 6: pm
###################################################

dnow.pm <- merge(dnow.p,dnow.m,suffixes=c(".party",".mun"),by=c("municipio","uf"))
## vote share
dnow.pm$vsharemun <- with(dnow.pm,votes.party/votes.mun)
## candidate as share of seats
dnow.pm$candseats <- with(dnow.pm,candidates.party/elected.mun)



###################################################
### chunk number 7: im
###################################################

dnow.1 <- merge(dnow,dnow.p,suffixes=c(".i",".party"),by=c("municipio","uf","party"))
dnow.2 <- merge(dnow,dnow.m,suffixes=c(".i",".mun"),by=c("municipio","uf"))
##save(dnow,dnow.1,dnow.2,file="~/Desktop/tmp.RData")


###dnow.pim <- merge(clean.df(dnow.1),clean.df(dnow.2))


dnow.pim <- merge(dnow.1,dnow.2)
dim(dnow.pim)

## vote share
dnow.pim$vshare <- with(dnow.pim,votes.i/votes.mun)
## vote share in party
dnow.pim$pvshare <- with(dnow.pim,votes.i/votes.party)
## vote share in party

## party vote rank
foo <- function(x) rank(x,ties.method="min")
dnow.pim$vrank <- with(dnow.pim,ave(-votes.i,partymunuf,FUN=foo))

## number of candidates
dnow.pim$Q <- with(dnow.pim,ave(votes.i,partymunuf,FUN=length))


res <- recast(dnow.pim,vrank+Q~.,measure.var="pvshare",
              fun.aggregate=function(x) {
                probs=c(.1,.5,.9)
                res <- quantile(x,probs=probs,na.rm=TRUE)
                res <- c(length(x),res)
                names(res) <- c("count",paste("p",probs*1000,sep=''))
                res
              }
              )

res <- res[order(res$Q,res$vrank),]




foo <- function(x) {
  res <- try(coef(summary(lm(log(pvshare)~vrank,data=subset(x,pvshare>0))))[2,1:2])
  if ("try-error"%in%class(res)) res <- c(NA,NA)
  res
}

res <- ddply(dnow.pim,"Q",foo)
summary(lm(Estimate~I(1/Q),data=res))

bar <- function(x) {
  res <- try(coef(summary(lm(log(pvshare)~Q,data=subset(x,pvshare>0))))[2,1:2])
  if ("try-error"%in%class(res)) res <- c(NA,NA)
  res
}

res <- ddply(dnow.pim,"vrank",bar)
with(subset(res,vrank<20),plot(vrank,Estimate))

## with(dnow.pm,plot(log(vsharemun),log(candseats),pch="."))

## qplot(log(vsharemun),log(candseats),alpha=I(1/10),data=dnow.pm,geom="smooth")

## tmp <- subset(dnow.pm,vsharemun>0 & is.finite(candseats))
## summary(lm(log(candseats)~log(vsharemun),data=tmp))

## summary(lm(log(candseats)~log(vsharemun)+log(npartycoal),data=tmp))

## m1 <- (lm(log(candseats)~log(vsharemun)+log(elected.mun)+log(npartycoal),data=tmp))



