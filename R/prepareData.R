library(reshape)
set.seed(20091975)
year <- 2000
office.now <- "Vereador"   
small.sample <- FALSE
fname <- paste("../data/data",year,if(small.sample) "small",office.now,".RData",sep="")


##office.now <- "Prefeito"
load(file="../data/betaAll.RData")
load(file=paste("../data/coalitions",year,".RData",sep=""))
## load functions
source("coalF.R")
mund <- read.dta("../data/municipal_o.dta")
## which election
(s.i <- (year-1796)/4)
##priors
pprior <- subset(beta.all.pp,t==s.i)
print(year)
print(s.i)

## vote share data
dv <- subset(dnow.vp,office==office.now)
dv$total.votes <- with(dv,ave(votes,municipio,uf,FUN=sum))
dv$total.elected <- with(dv,ave(elected,municipio,uf,FUN=sum))
dv$prop.v <- with(dv,votes/total.votes)
dv$citystate <- with(dv,paste(municipio,uf,sep=";"))
## add ccode (munipality code) to vote share data
tmp <- mund[!is.na(mund$hdi_educ2000),c("city","uf","ccode")]
names(tmp)[1] <- "municipio"
dv <- merge(dv,tmp)


##vote shares
vs <- recast(dv,party~variable,measure.var=c("elected","votes"),fun.aggregate=sum)
vs <- vs[order(-vs$votes),]
## vs$cs.e <- cumsum(vs$elected)/sum(vs$elected)
## vs$cs.v <- cumsum(vs$votes)/sum(vs$votes)
vs$prop.v <- vs$votes/sum(vs$votes)
vs$prop.e <- vs$elected/sum(vs$elected)
vs$partyin <- (gsub(" ","",vs$party)%in%pprior$party)
print(vs,digits=2)
## lp <- unique(c(as.character(vs$party[(vs$prop.v>=.02)#|(vs$partyin)
##                                      ])))
lp <- as.character(vs$party[1:10])
with(vs,sum(elected[party%in%lp])/sum(elected))
with(vs,sum(votes[party%in%lp])/sum(votes))
lp


dim(coalitions)
coalitions <- subset(coalitions,(office==office.now) & (gsub(" ","",party)%in%lp) & (votes>0))
dim(coalitions)
coalitions <- merge(coalitions,tmp)
dim(coalitions)


##single state
##coalitions <- subset(coalitions,uf=="RS")

tmp <- strsplit(coalitions$coalition,"/")
tmp <- lapply(tmp,sort)
tmp <- lapply(tmp,function(x) paste(x,collapse="/"))
tmp <- unlist(tmp)
tmp <- unique(tmp)
##number of unique electoral coalitions
length(tmp)
length(unique(coalitions$party))


library(reshape)
mun.data <- na.omit(coalitions)
mun.data <- mun.data[!duplicated(data.frame(mun.data$coalition,mun.data$municipio,mun.data$uf,mun.data$office)),]
mun.data <- recast(mun.data,uf+municipio+ccode~variable,measure.var=c("votes","elected"),fun.aggregate=sum)
if (small.sample) {
    ## sample N municipalities by state, without replacement
    set.seed(10091997)
    ufs <- unique(mun.data$uf)
    ufs <- sample(ufs,15)
    tmp <- do.call(rbind,with(mun.data, lapply(ufs,function(x) data.frame(uf=x, municipio=sample(municipio[uf==x],min(15,sum(uf==x),prob=votes[uf==x]))))))
    sum(mun.data$votes)
    mun.data <- merge(tmp,mun.data)
    ##sample N municipalities
    ##mun.data <- mun.data[sample(1:nrow(mun.data),200,prob=mun.data$votes),]
    ##mun.data <- mun.data[sample(1:nrow(mun.data),1000),]
}
dim(mun.data)
sum(mun.data$votes)

dnow <- subset(coalitions)

dnow <- merge(mun.data,dnow,by=c("municipio","uf"),suffixes=c(".mun",""))
tx <- table(dnow$party)##/nrow(mun.data)
tx <- tx[tx>=25]
tx
lp <- names(sort(-tx))
(lp <- c("pt","pfl",lp[!lp%in%c("pt","pfl")]))
(llp <- length(lp))
length(unique(with(dnow,paste(municipio,uf))))
dnow <- subset(dnow,party%in%lp)
dnow$row_names <- NULL
dnow$votes[is.na(dnow$votes)] <- 0

tmp <- merge(dnow,dnow,by=exclude(c("party","row_names"),names(dnow)),all=TRUE)
dim(tmp)
library(reshape)
dnow$i <- 1
tmp$y <- 1
tmp$citystate <- with(tmp,paste(municipio,uf,sep=";"))
mun <- unique(tmp$citystate)
gr <- data.frame(expand.grid(mun,factor(lp),factor(lp)))
names(gr) <- c("citystate","party.x","party.y")
print(dim(gr))
gr <- merge(gr,subset(tmp,select=c(uf,citystate,party.x,party.y,coalition,y)),all.x=TRUE)
gr[which(is.na(gr$y)),"y"] <- 0
gr$uf <-    factor(sapply(strsplit(as.character(gr$citystate),";"),function(x) x[2]))
## subset out the party decisions when the party does not compete in the election
alld <- by(gr,gr$citystate,function(tmp) {
    allp <- with(tmp,unique(c(as.character(party.x[y>0]),as.character(party.y[y>0]))))
    tmp[with(tmp,(party.x%in%allp) & (party.y%in%allp)),]
})
alld <- do.call(rbind,alld)


##gr[which(is.na(gr$y)),"y"] <- 0        
##library(pcaMethods)
allpc <- recast(subset(alld,y>0),party.x~party.y,measure.var="y",fun.aggregate=sum,id.var=exclude("y",names(alld)))
x <- allpc[,-1];x <- apply(x,1,function(x) x/max(x));rownames(x) <- colnames(x) <- names(allpc[-1])##;x <- x[1:7,1:7]
##x[1:nrow(x),1:nrow(x)] <- runif(length(x))
##plot.coal(round(x*100),reorder=TRUE,breaks=c(100*c(0,.05,.10,.15,.2,.3)))




cat("\ngr with non-competing parties:",nrow(gr),"\n")
gr <- alld
##subset out elections with just one coalition
cat("\ngr without non-competing parties:",nrow(gr),"\n")
## subset out party.x=party.y
gr <- subset(gr,as.numeric(party.x)<as.numeric(party.y))
cat("\ngr x<y:",nrow(gr),"\n")
gr <- subset(gr,as.numeric(party.x)<as.numeric(party.y))
##sao paulo is state 1
tmp <- merge(gr,dv[,c("prop.v","citystate","party")],by.x=c("citystate","party.x"),by.y=c("citystate","party"),all.x=TRUE)
names(tmp)[which(names(tmp)=="prop.v")] <- "prop.v.x"
tmp <- merge(tmp,dv[,c("prop.v","citystate","party")],by.x=c("citystate","party.y"),by.y=c("citystate","party"),all.x=TRUE)
names(tmp)[which(names(tmp)=="prop.v")] <- "prop.v.y"
tmp <- merge(tmp,unique(dv[,c("citystate","total.votes","total.elected","ccode")]),by="citystate")
dim(tmp)
tmp <- merge(tmp,mund[,c("ccode","hdi_educ2000")])
tmp$prop.v.x[is.na(tmp$prop.v.x)] <- 0
tmp$prop.v.y[is.na(tmp$prop.v.y)] <- 0
##exclude party decisions in which one of the parties has zero votes
tmp <- subset(tmp,(prop.v.x>0) & (prop.v.y>0))
gr <- tmp
##large party as x
pl <- with(gr,prop.v.x>prop.v.y)
gr[!pl,c("party.x","prop.v.x","party.y","prop.v.y")] <- tmp[!pl,c("party.y","prop.v.y","party.x","prop.v.x")]
gr <- gr[,sort(names(gr))]
dim(gr)



gr$uf <- relevel(gr$uf,"SP")
gr$citystate <- factor(gr$citystate)

##save data
save(lp,llp,beta.all.pp,mun.data,gr,pprior,lp,dv,file=fname)



