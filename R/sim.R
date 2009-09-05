library(R2WinBUGS)
library(rjags)
year <- 2000
office <- "Vereador"
n.chains <- 1

small.sample <- FALSE
one.machine <- TRUE


mult <- .001
n.iter <- 1000*mult
n.burnin <- 500*mult
n.thin <- 1

mnow <- "model.bug"
fname <- paste("../results/",office,"/",year,"/",mnow,Sys.Date(),if (small.sample) "small",".RData",sep="")




tmp <- lapply(c("rjagsCluster.R","utils.R","~/reps/r-mcmc-cluster/mcmc-cluster.R"),
       function(x) try(source(x)))

##load data data2004smallVereador.RData
load(paste("../data/data",year,if (small.sample) "small",office,".RData",sep="")) 

y <- gr$y




lmun <- levels(gr$citystate)
party.i <- as.numeric(with(gr,factor(party.x,levels=lp)))
party.j <- as.numeric(with(gr,factor(party.y,levels=lp)))
group <- as.numeric(with(gr,factor(citystate,levels=lmun)))
state.i <-  as.numeric(gr$uf)
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
    x.start[1:2] <- c(-1.1,1.1)
    sigma.x <- runif(1,0,.1)
    sigma.g <- runif(1,0,1)    
    list(sigma.x=sigma.x,
         sigma.g=sigma.g,
         b=runif(3,-2,0),
         g=rnorm(n.groups,0,sigma.g),
         mu.x=x.start,
         x=matrix(rnorm(n.parties*n.states,x.start,sigma.x),ncol=n.states,byrow=FALSE)
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
                        ,"b","c1"
                        ##,"s","sigma.s"
                        )










jags.data <- Hmisc::llist(y,party.i,party.j,state.i,state.j,group,n.groups,n.parties,n.rows,n.states,log.votes.i,log.votes.j,votes.i,votes.j,log.magnitude,hdi)
source("bugsmodels.R")
write.model(model.bugs.08,con="model.bug")


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
