library(reshape)
source("../R/functions.R")


## get governors
govs <- read.csv("~/reps/Brazilian-Local-Elections/data/govs.csv")
names(govs) <- tolower(names(govs))
names(govs)[1] <- "state"
govs.names <- govs[,c(1,seq(2,ncol(govs),2))]
govs.parties <- govs[,c(1,seq(3,ncol(govs),2))]
govs.names <- melt(govs.names,id.var="state")
govs.names$v <- "name"
govs.parties <- melt(govs.parties,id.var="state")
govs.parties$v <- "party"
govs <- rbind(govs.names,govs.parties)
govs$year <- gsub("x","",govs$variable)
govs$year <- gsub("^\\.|\\.1$","",govs$year)
govs <- recast(data.frame(govs),state+year~v,measure.var="value",id.var=c("state","year","v"))
govs$state <- toupper(state.l2a(govs$state))
govs$party <- tolower(govs$party)
govs$year <- as.numeric(as.character(govs$year))
save(govs,file="../data/govs.RData")

## check munuf=="ABAETETUBA;PA"

### data from the TSE website http://www.tse.gov.br/internet/eleicoes/2004/result_blank.htm
year <- 2000
testing <- FALSE
testing.subset <- function() subset(tmp,uf%in%c("PA"))
## by candidate
dir <- paste("../data/",year,"/candidato/",sep="")
tmp <- lapply(list.files(dir),function(x) read.csv(paste(dir,x,sep=""),skip=7,fileEncoding="latin1",row.names=NULL,colClasses="character",header=FALSE)[,1:10])
tmp <- do.call(rbind,tmp)
for (i in c(5,7,9)) {
    ##     tmp[,i] <- gsub("\\.","",tmp[,i])
    ##     tmp[,i] <- gsub(",",".",tmp[,i])
    tmp[,i] <- gsub(",","",tmp[,i])
    tmp[,i] <- as.numeric(tmp[,i])
}
names(tmp) <- c("office","uf","municipio","party","number","name","votes","outcome","pctvotes","coalition")
tmp0 <- tmp

##testing
if (testing) tmp <- testing.subset()

tmp$party <- gsub(" ","",tmp$party)
tmp$coalition <- with(tmp,ifelse(coalition=="(sem coligação)",party,coalition))
tmp$coalition <- gsub(" ","",tmp$coalition)
tmp$coalition <- gsub("^/","",tmp$coalition)
## coalition includes PPB instead of PP in 2000  (party name changes)
## we fix it here (for 2000 only)
if (year==2000) {
    tmp$coalition <- gsub("PPB","PP",tmp$coalition)
}
tmp$vote.type <- "individual"
data.cand <- subset(tmp,select=c("municipio","uf","party","office","votes","vote.type","outcome","name"))

##coalition data
data.coal <- unique(subset(tmp,select=c("municipio","uf","office","party","coalition")))
## make sure coalition includes current party
## the line below does not work because it will differ across parties
## in the same coalition
## tmp$coalition <- paste(tmp$party,tmp$coalition,sep="/")
## so what wedo is:
## 1) generate a new coalitio variable with all parties that had individual
## candidate votes
data.coal$cz <- with(data.coal,paste(coalition,office,municipio,uf,sep=";"))

## split the original coalition variable
tmpc <- unique(data.coal$coalition)
system.time({
    tmpc <- lapply(tmpc,function(x) {
        data.frame(coalition=x,party=strsplit(x,"/"))
    })
    tmpc <- lapply(tmpc,function(x) {
        names(x)[2] <- "party"
        x
    })
    tmpc <- do.call(rbind,tmpc)
})

## merge it back to the coalition data
tmpc <- unique(merge(tmpc,subset(data.coal,select=-party),all=TRUE,by="coalition"))
tmpc <- unique(rbind(tmpc,data.coal))

## 2) now we fix the coalition variable by pasting the unique parties together
tmpc$coalition <- with(tmpc,ave(as.character(party),cz,FUN=function(x) paste(sort(unique(x)),collapse="/")))
tmpc$cz <- NULL

data.coal <- tmpc
rm(tmpc)


## by party (necessary to get the number of votes by party -- including vote for parties instead of candidates) http://www.tse.gov.br/internet/eleicoes/2000/quad_part_cargo_blank.htm
dir <- paste("../data/",year,"/partido_cargo/",sep="")
tmp <- lapply(list.files(dir),function(x) read.csv(paste(dir,x,sep=""),skip=7,fileEncoding="latin1",row.names=NULL,colClasses="character",header=FALSE)[,1:7]
              )
tmp <- do.call(rbind,tmp)
for (i in c(5:7)) {
    tmp[,i] <- gsub(",","",tmp[,i])
    tmp[,i] <- as.numeric(tmp[,i])
}
names(tmp) <- c("uf","municipio","party","office","elected","votes","individual.votes")
if (testing) tmp <- testing.subset()
tmp$party <- gsub(" ","",tmp$party)
tmp$vote.type <- "party"
tmp <- subset(tmp,votes>0,select=c("municipio","uf","party","office","votes","vote.type"))
data.party <- tmp
data.party$name <- data.party$outcome <- "party vote"
rm(tmp)

tmp <- rbind(data.party,data.cand)
tmp$party <- gsub(" ","",tmp$party)
data.coal$party <- gsub(" ","",data.coal$party)
dnow <- merge(tmp,data.coal,all.x=TRUE)

dnow$elected <- 0
dnow$elected[dnow$outcome%in%c("Eleito por Média","Eleito")] <- 1


dnow$coalition <- as.character(ifelse(dnow$coalition=="",as.character(dnow$party),as.character(dnow$coalition)))
dnow$coalition <- tolower(gsub(" ","",as.character(dnow$coalition)))
dnow$party <- tolower(dnow$party)


save(dnow,file=paste("../data/candidates",year,".RData",sep=""))




dnow.vp <- with(subset(dnow),{
    data.frame(office,uf,municipio,party=party,
               coalition,
               elected,
               votes,
               election=year,vote.type)
})


dnow.vp$candidates <- ifelse(dnow.vp$vote.type=='individual',1,0)

##by party
dnow.vp <- recast(dnow.vp,office+uf+municipio+party+coalition~variable,measure.var=c("votes","elected","candidates"),fun.aggregate=sum,na.rm=TRUE)
dnow.vp <- data.frame(dnow.vp)

##by mun
## dnow.vpm <- recast(dnow.vp,office+uf+municipio~variable,measure.var=c("votes","elected","candidates"),fun.aggregate=sum,na.rm=TRUE)

##by coalition
dnow.vpc <- recast(dnow.vp,office+uf+municipio+coalition~variable,measure.var=c("votes","elected"),fun.aggregate=sum,na.rm=TRUE)
data.coal$coalition <- tolower(data.coal$coalition)
data.coal$party <- tolower(data.coal$party)
coalitions <- merge(dnow.vpc,data.coal)
save(dnow.vp,coalitions,file=paste("../data/coalitions",year,".RData",sep=""))


## compare seats with and without coalitions

dnow$coalition <- ifelse(is.na(dnow$coalition),as.character(dnow$party),as.character(dnow$coalition))
dnow.vp$coalition <- ifelse(is.na(dnow.vp$coalition),as.character(dnow.vp$party),as.character(dnow.vp$coalition))
dnow$munuf <- paste(dnow$municipio,dnow$uf,sep=";")
dnow.vp$munuf <- paste(dnow.vp$municipio,dnow.vp$uf,sep=";")

## out figure how many seats by group
office <- "Vereador"
comp <- bg(subset(dnow,office=="Vereador"),group="party")
save(comp,file=spath("comparison.RData"))
