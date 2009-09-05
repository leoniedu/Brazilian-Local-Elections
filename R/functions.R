fx <- function(x,...) {
  x <- c(quantile(x,probs=c(0.1,.5,.9),...),length(x))
  names(x) <- c("lo","median","hi","n")
  x
}

spath <- function(x) paste("../results/",office,"/",year,"/",x,sep="")

rrank <- function(...) rank(...,ties.method="first")

dhondt <- function(df,eq=TRUE) {
  ## FIx: create options to name the votes, magnitude
  ##cat(".")
  v <- df$votes
  M <- df$total.elected[1]
  if (eq) {
    ## electoral quotient (less than this and no one gets elected)
    neq <- v<(sum(v)/M)
    v[neq] <- 0
  }
  s <- rep(0,length(v))
  for (i in 1:M) {
    j <- which.max(v/(s+1))
    s[j] <- s[j]+1
  }
  data.frame(df,elected.calc=s)
}

bg <- function(data,group="coalition") {
  data$candidates <- data$vote.type=="individual"
  ##data <- subset(dnow,office=="Vereador" & uf=="AC")
    data$group <- data[,group]
    ##data <- subset(data,municipio=="SANTA ROSA")
    data <- recast(data,group+munuf~variable,measure.var=c("votes","elected","candidates"),fun.aggregate=sum)
    data$total.elected <- with(data,ave(elected,munuf,FUN=sum))
    res <- ddply(data,.(munuf),dhondt)
    names(res)[names(res)=="group"] <- group
    res
}

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

iqr <- function(x, ..., probs=c(0.025,.5, 0.975)) { 
    qs <- quantile(as.numeric(x),na.rm = T,probs=probs) 
    names(qs) <- c("ymin","y", "ymax") 
    qs 
} 

get.pred <- function(x=seq(0,2,length=100),sims) {
    xm.m <- data.frame(x)
    xm.m <- merge(data.frame(sim=sims),xm.m,all=TRUE)
    xm.m
}

theme_bw2 <- function (base_size = 12) 
{
    structure(list(axis.line = theme_blank(), axis.text.x = theme_text(size = base_size * 0.8, lineheight = 0.9, vjust = 1), axis.text.y = theme_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1), axis.ticks = theme_segment(colour = "black", size = 0.2), axis.title.x = theme_text(size = base_size , vjust = 1
 ), axis.title.y = theme_text(size = base_size, angle = 90, vjust = .5), axis.ticks.length = unit(0.3, "lines"), axis.ticks.margin = unit(0.5, "lines"), legend.background = theme_rect(colour = NA), legend.key = theme_rect(colour = "grey80"), legend.key.size = unit(1.2, "lines"), legend.text = theme_text(size = base_size * 0.8), legend.title = theme_text(size = base_size * 0.8, face = "bold", hjust = 0), legend.position = "right", panel.background = theme_rect(fill = "white", colour = NA), panel.border = theme_rect(fill = NA, colour = "grey50"), panel.grid.major = theme_line(colour = "grey90", size = 0.2), panel.grid.minor = theme_line(colour = "grey98", size = 0.5), panel.margin = unit(0.25, "lines"), strip.background = theme_rect(fill = "grey80", colour = "grey50"), strip.label = function(variable, value) value, strip.text.x = theme_text(size = base_size * 0.8), strip.text.y = theme_text(size = base_size * 0.8, angle = -90), plot.background = theme_rect(colour = NA), plot.title = theme_text(size = base_size * 1.2), plot.margin = unit(c(1, 1, 0.5, 0.5), "lines")), class = "options")
}
##qplot(rnorm(100),runif(100))+theme_bw2()

theme_bw3 <- function (base_size = 12) 
{
    structure(list(axis.line = theme_blank(),
                   axis.text.x = theme_text(size = base_size * 0.8, lineheight = 0.9, vjust = 1),
                   axis.text.y = theme_text(size = base_size * 0.8, lineheight = 0.9, hjust = 1),
                   axis.ticks = theme_segment(colour = "black", size = 0.2),
                   axis.title.x = theme_text(size = base_size , vjust = 1),
                   axis.title.y = theme_text(size = base_size, angle = 90, vjust = .5),
                   axis.ticks.length = unit(0.3, "lines"),
                   axis.ticks.margin = unit(0.5, "lines"),
                   legend.background = theme_rect(colour = NA),
                   legend.key = theme_rect(colour = "grey80"),
                   legend.key.size = unit(1.2, "lines"),
                   legend.text = theme_text(size = base_size * 0.8),
                   legend.title = theme_text(size = base_size * 0.8, face = "bold", hjust = 0),
                   legend.position = "right", panel.background = theme_rect(fill = "white", colour = NA),
                   panel.border = theme_rect(fill = NA, colour = "grey50"),
                   panel.grid.major = theme_line(colour = "grey90", size = 0.2),
                   panel.grid.major.y = theme_line(colour = "grey90", size = 0.2),
                   panel.grid.minor = theme_line(colour = NA, size = 0.5),
                   panel.margin = unit(0.25, "lines"),
                   strip.background = theme_rect(fill = "grey80", colour = "grey50"),
                   strip.label = function(variable, value) value, strip.text.x = theme_text(size = base_size * 0.8),
                   strip.text.y = theme_text(size = base_size * 0.8, angle = -90),
                   plot.background = theme_rect(colour = NA),
                   plot.title = theme_text(size = base_size * 1.2),
                   plot.margin = unit(c(1, 1, 0.5, 0.5),
                   "lines")),
              class = "options")
}
