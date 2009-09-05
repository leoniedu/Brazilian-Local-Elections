library(R2WinBUGS)
library(rjags)
year <- 2000
office <- "Vereador"
n.chains <- 2

small.sample <- TRUE
one.machine <- TRUE

mult <- 1
n.iter <- 2000*mult
n.burnin <- 500*mult
n.thin <- 1

mnow <- "model.bug"
fname <- paste("../results/",office,"/",year,"/",mnow,Sys.Date(),if (small.sample) "small",".RData",sep="")

tmp <- lapply(c("rjagsCluster.R","utils.R","~/reps/r-mcmc-cluster/mcmc-cluster.R"),
              function(x) try(source(x)))

##load data
load(paste("../data/data",year,if (small.sample) "small",office,".RData",sep="")) 

load("../data/govs.RData")
govs$year.next <- govs$year+2
names(govs)[names(govs)=="party"] <- "party.gov"
if (year==2000) {
  govs$party.gov[govs$party.gov=="ppb"] <- "pp"
}


gr$year <- year
gr <- merge(gr,govs,by.y=c("state","year.next"),by.x=c("uf","year"),all.x=TRUE)
gr$party.gov <- as.numeric(factor(gr$party.gov,levels=lp))
##set to 0 if party.gov is not in lp
gr$party.gov[is.na(gr$party.gov)] <- 0
## below threshold
gr$th1 <- with(gr,prop.v.x<(1/total.elected))
gr$th2 <- with(gr,prop.v.y<(1/total.elected))

y <- gr$y

##res,out, constrain, m2
load(file="tmp.RData")
constrain <- function(x,lo,hi) ifelse(x<lo,lo,ifelse(x>hi,hi,x))
dnew <- gr[,c("total.votes","total.elected","prop.v.x","prop.v.y")]
names(dnew)[1:2] <- c("votes.mun","elected.mun")
dnew$coalit <- 1
dnew$votes.party<- dnew$prop.v.x*dnew$votes.mun
## predicted number of candidates if forming a coalition
dnew$Qcoalit.i <- constrain(round(predict(m2,newdata=dnew,type="response")),1,round(dnew$elected.mun*1.5-1))
dnew$votes.party <- dnew$prop.v.y*dnew$votes.mun
dnew$Qcoalit.j <- constrain(round(predict(m2,newdata=dnew,type="response")),1,round(dnew$elected.mun*1.5-1))
dnew$votes.party <- NULL


out <- function(i,j,pi,pj,M){
  N <- max(round((pi+pj)*M),1)
  ni <- paste("Q",i)
  nj <- paste("Q",j)
  p <- c(pi,pj)
  r1 <- cbind(res[[ni]]*p[1],res[[nj]]*p[2])
  o <- t(apply(r1,1,order,decreasing=TRUE))
  party <- t(apply(o,1,function(x) c(rep(1,i),rep(2,j))[x]))[,1:min(N,i+j)]
  res <- (c(sum(party==1),sum(party==2))/length(party)) ## proportion of seats assigned for the two parties
  res1 <- (floor(p*M)) ## expected seats without coalitions
  ##print (paste("expected seats without coalitions", paste(res1,collapse=",")))
  res <- res*N ## expected seats
  ##print (paste("expected seats with coalitions", paste(res,collapse=",")))
  res <- res/M ## expected proportion of seats in the disctrict
  res1 <- res1/M ## without coal, prop of seats
  ##res <- res/p ## with coal, as a ratio of the vote proportion
  ##res1 <- res1/p ## without coal, as ratio of vote proportion
  ##print(res1)
  ##print(res)
  res-res1 ## difference
  ##res
}

## a measure of predicted seats gain if forming a coalition
benefit <- t(sapply(1:nrow(dnew),function(z) {
  if ((z%%1000)==0) {
    print(paste(z,round(z/nrow(dnew)*100)))
  }
  res <- with(dnew,out(Qcoalit.i[z],Qcoalit.j[z],prop.v.x[z],prop.v.y[z],M=elected.mun[z]))
  res
})
             )

save(benefit,file="~/Desktop/benefit.RData")

benefit.i <- benefit[,1]
benefit.j <- benefit[,2]


lmun <- levels(gr$citystate)
party.i <- as.numeric(with(gr,factor(party.x,levels=lp)))
party.j <- as.numeric(with(gr,factor(party.y,levels=lp)))
party.gov.i <- as.numeric(party.i==gr$party.gov)
party.gov.j <- as.numeric(party.j==gr$party.gov)
group <- as.numeric(with(gr,factor(citystate,levels=lmun)))
state.i <-  as.numeric(gr$uf)
## state.j is for municipality level data
state.j <- as.numeric(factor(sapply(strsplit(as.character(lmun),";"),function(x) x[2]),levels=levels(gr$uf)))
n.states <- max(state.i)
n.groups <- max(group)
n.parties <- llp
n.rows <- nrow(gr)
## data
n.rows
votes.i <- gr$prop.v.x
votes.j <- gr$prop.v.y
log.magnitude <- log(gr$total.elected)-log(9)
log.votes.i <- log(gr$prop.v.x)
log.votes.j <- log(gr$prop.v.y)
##jagsz.data.simple <- Hmisc::llist(y,party.i,party.j,group,n.groups,n.parties,n.rows)
hdi <- gr$hdi_educ2000-.5



pm <- merge(data.frame(party=gsub(" +","",tolower(lp)),id=1:llp),pprior[,c("party","Mean.p","SD.p")],all.x=TRUE)
pm <- pm[order(pm$id),]
print(dim(gr))
theta <- pm$Mean.p
theta.sd <- pm$SD.p

## x inits using Legis
inits <- function(){
    ##needs, lp,llp,pprior,n.groups
    suppressWarnings(x.start <- rnorm(llp,theta,theta.sd))
    xmiss <- is.na(x.start) 
    x.start[xmiss] <- rnorm(sum(xmiss),0,1)
    x.start[1:2] <- c(-2,2)
    sigma.x <- runif(1,0,1)
    sigma.g <- runif(1,0,1)
    xmat <- matrix(rnorm(n.parties*n.states,x.start,sigma.x),
                   ncol=n.states,byrow=FALSE)
    xmat[1,] <- -1
    xmat[2,] <- 1
    list(sigma.x=sigma.x,
         sigma.g=sigma.g,
         b=runif(6,-2,0),
         g=rnorm(n.groups,0,sigma.g),
         mu.x=x.start,
         x=xmat
         )
}





set.seed(20091975)
res <- NULL

parameters.to.save <- c("g","sigma.g"
                        ,"x","sigma.x"
                        ,"mu.x"
                        ##,"v", "sigma.v","mu.v"
                        ##,"mu.g"
                        ##,"sigma.x.all","sigma.v.all","mu.v.all"
                        ##,"sigma.g.all"
                        ,"b","c1"##,"c2","pol"
                        ##,"s","sigma.s"
                        )



th.i <- gr$th1
th.j <- gr$th2

jags.data <- Hmisc::llist(y,party.i,party.j,state.i,state.j,group,n.groups,n.parties,n.rows,n.states,log.votes.i,log.votes.j,votes.i,votes.j,log.magnitude,hdi,party.gov.i,party.gov.j,th.i,th.j,benefit.i,benefit.j)

source("bugsmodels.R")
write.model(model.bugs.14,con="model.bug")


if (one.machine) {
  try(rm(jags1,jags1.s))
  system.time(jags1 <- jags.model(file=mnow,data=jags.data, inits=inits, n.adapt=n.burnin,n.chains=n.chains))
  system.time(jags1.s <- coda.samples(jags1,variable.names=parameters.to.save, n.iter=n.iter,thin=n.thin))
} else {
    library(snow)
    ## partial
    if (!exists("cl")) cl <- makeCluster(n.chains, type = "SOCK")
    ##data needed for the initial values function
    ##clusterExport(cl,c("llp","theta","theta.sd","n.states","n.parties"))
    clusterExport(cl,c("llp","theta","theta.sd","n.states","n.parties","n.groups"))
    try(rm(jags1,jags1.s))
    system.time(jags1 <- cluster.jags.model(cl,file=mnow,data=jags.data, inits=inits, n.adapt=n.burnin))
    system.time(jags1.s <- cluster.coda.samples(cl,jags1,variable.names=parameters.to.save, n.iter=n.iter,thin=n.thin))
}
    
save(jags1,jags1.s,file=fname)
bang()
