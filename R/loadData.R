library(reshape)
### data from the TSE website http://www.tse.gov.br/internet/eleicoes/2004/result_blank.htm
year <- 2004
testing <- FALSE
testing.subset <- function() subset(tmp,uf%in%c("AC","RR"))
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
##testing
if (testing) tmp <- testing.subset()
tmp$coalition <- gsub(" |^/","",with(tmp,ifelse(coalition=="(sem coligação)",party,coalition)))
tmp$vote.type <- "individual"
tmp$party <- gsub(" ","",tmp$party)
data.cand <- subset(tmp,select=c("municipio","uf","party","office","votes","vote.type","outcome","name"))
##coalition data
data.coal <- unique(subset(tmp,select=c("municipio","uf","office","coalition")))
##split coalitions
tmpc <- lapply(unique(data.coal$coalition),function(x) data.frame(coalition=x,party=c(strsplit(x,"/"))[[1]]))
tmpc <- do.call(rbind,tmpc)
data.coal <- merge(data.coal,tmpc,by=c("coalition"))
rm(tmp)


## by party (necessary to get the number of votes by party -- including vote for parties instead of candidates)
dir <- "../data/2004/partido_cargo/"
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
dnow <- merge(tmp,data.coal,all.x=TRUE)
dnow$elected <- 0
dnow$elected[dnow$outcome%in%c("Eleito por Média","Eleito")] <- 1
dnow.vp <- with(subset(dnow),{
    data.frame(office,uf,municipio,party=tolower(party),
               coalition=tolower(gsub(" ","",as.character(coalition))),
               elected=elected,
               votes=votes,
               election=year,vote.type)
})


dnow.vp$coalition <- as.character(ifelse(dnow.vp$coalition=="",as.character(dnow.vp$party),as.character(dnow.vp$coalition)))
dnow.vp$candidates <- ifelse(dnow.vp$vote.type=='individual',1,0)

##by party
dnow.vp <- recast(dnow.vp,office+uf+municipio+party+coalition~variable,measure.var=c("votes","elected","candidates"),fun.aggregate=sum,na.rm=TRUE)
dnow.vp <- data.frame(dnow.vp)

##by mun
dnow.vpm <- recast(dnow.vp,office+uf+municipio~variable,measure.var=c("votes","elected","candidates"),fun.aggregate=sum,na.rm=TRUE)

##by coalition
dnow.vpc <- recast(dnow.vp,office+uf+municipio+coalition~variable,measure.var=c("votes","elected"),fun.aggregate=sum,na.rm=TRUE)
tmpc$coalition <- tolower(tmpc$coalition)
tmpc$party <- tolower(tmpc$party)
coalitions <- merge(dnow.vpc,tmpc)
save(dnow.vp,coalitions,file=paste("../data/coalitions",year,".RData",sep=""))
