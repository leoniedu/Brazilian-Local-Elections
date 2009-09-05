plot.text <- function(x,y,labels,cex=.75,...) {
    plot(x,y,type="n",main=paste("r=",round(cor(x,y,use="pair"),2),sep="")
         ,...)
    text(x,y,labels=labels,cex=cex)    
}

exclude <- function(x,y) y[!y%in%x]

##USING PCA
get.pca <- function(dnow) {
    tmp <- merge(dnow,dnow,by=exclude(c("party2","row_names"),names(dnow)),all=TRUE)
    tmp <- subset(tmp,party2.x!=party2.y)
    library(reshape)
    dnow$i <- 1
    tmp$y <- 1
    gr <- data.frame(expand.grid(mun,factor(lp),factor(lp)))        
    names(gr) <- c("municipio","party2.x","party2.y")
    print(dim(gr))
    gr <- subset(gr,party2.x!=party2.y)
    gr <- merge(gr,subset(tmp,select=c(uf,municipio,party2.x,party2.y,coalition,y)),all.x=TRUE)
    gr[which(is.na(gr$y)),"y"] <- 0        
    library(pcaMethods)
    allpc <- recast(gr,party2.x~party2.y,measure.var="y",fun.aggregate=sum,id.var=exclude("y",names(gr)))
    ##exclude parties not competing
    pc <- which(apply(allpc[,-1],2,sum)!=0)
    allpc <- allpc[pc,c(1,pc+1)]       
    ag <- as.matrix(allpc[,-1])        
    diag(ag) <- apply(ag,1,sum)
    ##ag0 <- ag
    ag <- ag/diag(ag)
    pc1 <- ag
    pc1 <- prep(pc1,scale="UV",center=TRUE)        
    pc1 <- pca(pc1,method="ppca",nPcs=3)
    ## scores
    data.frame(party=tolower(allpc$party2.x),pc1@scores)
}







