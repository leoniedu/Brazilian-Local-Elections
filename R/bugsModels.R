## s is not identified, so we exclude it from this model
model.bugs.08 <- function() {
    sdp <- 10
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(x[party.i[i],state.i[i]]-x[party.j[i],state.i[i]])
        xb1[i] <- -d[i]+g[group[i]]+b[1]+b[2]*log.votes.i[i]+
            b[3]*log.magnitude[i]
        xb2[i] <- -d[i]+g[group[i]]+b[1]+b[2]*log.votes.j[i]+
            b[3]*log.magnitude[i]
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##correct
    c1 <- (sum((p>.5)*y)+sum((p<.5)*(1-y)))/n.rows
    ##  priors
    tau.x <- pow(sigma.x,-2)
    sigma.x ~ dunif(0,sdp)
    for (i in 1:3) {
        b[i] ~ dnorm(0,pow(sdp,-2))
    }
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(0,pow(sdp,-2))
        for (j in 1:n.states) {
            x[i,j] ~ dnorm(mu.x[i],tau.x)
        }
    }
    for (i in 1:n.groups) {
        g[i] ~ dnorm(0,tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
}


model.bugs.07 <- function() {
    sdp <- 10
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(x[party.i[i],state.i[i]]-x[party.j[i],state.i[i]])
        xb1[i] <- -d[i]+g[group[i]]+b[1]+b[2]*log.votes.i[i]+
            b[3]*log.magnitude[i]+s[state.i[i]]
        xb2[i] <- -d[i]+g[group[i]]+b[1]+b[2]*log.votes.j[i]+
            b[3]*log.magnitude[i]+s[state.i[i]]
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##correct
    c1 <- (sum((p>.5)*y)+sum((p<.5)*(1-y)))/n.rows
    ##  priors
    tau.x <- pow(sigma.x,-2)
    sigma.x ~ dunif(0,sdp)
    for (i in 1:3) {
        b[i] ~ dnorm(0,pow(sdp,-2))
    }
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(0,pow(sdp,-2))
        for (j in 1:n.states) {
            x[i,j] ~ dnorm(mu.x[i],tau.x)
        }
    }
    for (i in 1:n.groups) {
        g[i] ~ dnorm(0,tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
    for (i in 1:n.states) {
        s[i] ~ dnorm(0,tau.s)
    }
    sigma.s ~ dunif(0,sdp)
    tau.s <- pow(sigma.s,-2)
}


model.bugs.07s <- function() {
    sdp <- 10
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(x[party.i[i],state.i[i]]-x[party.j[i],state.i[i]])
        xb1[i] <- -d[i]+g[group[i]]+b[1]+b[2]*log.votes.i[i]+
            b[3]*log.magnitude[i]+s[state.i[i]]
        xb2[i] <- -d[i]+g[group[i]]+b[1]+b[2]*log.votes.j[i]+
            b[3]*log.magnitude[i]+s[state.i[i]]
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##correct
    c1 <- (sum((p>.5)*y)+sum((p<.5)*(1-y)))/n.rows
    ##  priors
    tau.x <- pow(sigma.x,-2)
    sigma.x ~ dunif(0,sdp)
    for (i in 1:3) {
        b[i] ~ dnorm(0,pow(sdp,-2))
    }
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(0,pow(sdp,-2))
        for (j in 1:n.states) {
            x[i,j] <- mu.x[i]
        }
    }
    for (i in 1:n.groups) {
        g[i] ~ dnorm(0,tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
    for (i in 1:n.states) {
        s[i] ~ dnorm(0,tau.s)
    }
    sigma.s ~ dunif(0,sdp)
    tau.s <- pow(sigma.s,-2)
}


model.bugs.06 <- function() {
    sdp <- 10
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(x[party.i[i],state.i[i]]-x[party.j[i],state.i[i]])
        log.sumv[i] <- log(sum(max(votes.i[i]+votes.j[i],.0001)))
        xb1[i] <- -d[i]+g[group[i]]+b[1]+b[2]*log.votes.i[i]+
            b[3]*(log.sumv[i])+b[4]*log.magnitude[i]+s[state.i[i]]
        xb2[i] <- -d[i]+g[group[i]]+b[1]+b[2]*log.votes.j[i]+
            b[3]*(log.sumv[i])+b[4]*log.magnitude[i]+s[state.i[i]]
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##correct
    c1 <- (sum((p>.5)*y)+sum((p<.5)*(1-y)))/n.rows
    ##  priors
    tau.x <- pow(sigma.x,-2)
    sigma.x ~ dunif(0,sdp)
    for (i in 1:4) {
        b[i] ~ dnorm(0,pow(sdp,-2))
    }
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(0,pow(sdp,-2))
        for (j in 1:n.states) {
            x[i,j] ~ dnorm(mu.x[i],tau.x)
        }
    }
    for (i in 1:n.groups) {
        g[i] ~ dnorm(0,tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
    for (i in 1:n.states) {
        s[i] ~ dnorm(0,tau.s)
    }
    sigma.s ~ dunif(0,sdp)
    tau.s <- pow(sigma.s,-2)
}

model.bugs.05 <- function() {
    sdp <- 100
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(x[party.i[i],state.i[i]]-x[party.j[i],state.i[i]])
        sumv[i] <- max(votes.i[i]+votes.j[i],.0001)
        xb1[i] <- -d[i]+g[group[i]]+b[1]+b[2]*votes.i[i]+b[3]*(votes.i[i]^2)+
            b[4]*(votes.i[i]/sumv[i])+b[5]*log.magnitude[i]+b[6]*hdi[i]
        xb2[i] <- -d[i]+g[group[i]]+b[1]+b[2]*votes.j[i]+b[3]*(votes.j[i]^2)+
            b[4]*(votes.j[i]/sumv[i])+b[5]*log.magnitude[i]+b[6]*hdi[i]               
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##correct
    c1 <- (sum((p>.5)*y)+sum((p<.5)*(1-y)))/n.rows
    ##  priors
    tau.x <- pow(sigma.x,-2)
    sigma.x ~ dunif(0,sdp)
    for (i in 1:6) {
        b[i] ~ dnorm(0,pow(sdp,-2))
    }
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(0,pow(sdp,-2))
        for (j in 1:n.states) {
            x[i,j] ~ dnorm(mu.x[i],tau.x)
        }
    }
    for (i in 1:n.groups) {
        g[i] ~ dnorm(0,tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
}

model.bugs.04 <- function() {
    sdp <- 100
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(mu.x[party.i[i]]-mu.x[party.j[i]])
        sumv[i] <- max(votes.i[i]+votes.j[i],.0001)
        xb1[i] <- -d[i]+g[group[i]]+b[1]+b[2]*votes.j[i]+b[3]*(votes.j[i]/sumv[i])+
            b[4]*log(magnitude[i])
        xb2[i] <- -d[i]+g[group[i]]+b[1]+b[2]*votes.i[i]+b[3]*(votes.i[i]/sumv[i])+
            b[4]*log(magnitude[i])                
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##correct
    c1 <- (sum((p>.5)*y)+sum((p<.5)*(1-y)))/n.rows
    ##  priors
    for (i in 1:4) {
        b[i] ~ dnorm(0,pow(sdp,-2))
    }
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(0,pow(sdp,-2))
        mu.v[i] ~ dnorm(0,pow(sdp,-2))
    }
    for (i in 1:n.groups) {
        g[i] ~ dnorm(0,tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
}

model.bugs.03 <- function() {
    sdp <- 100
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(mu.x[party.i[i]]-mu.x[party.j[i]])
        sumv[i] <- votes.i[i]+votes.j[i]+.001
        xb1[i] <- -d[i]+g[group[i]]+b[1]+b[2]*votes.j[i]+b[3]*(votes.j[i]/sumv[i])+
            b[4]*log(magnitude[i])
        xb2[i] <- -d[i]+g[group[i]]+b[1]+b[2]*votes.i[i]+b[3]*(votes.i[i]/sumv[i])+
            b[4]*log(magnitude[i])                
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    
    ##correct
    c1 <- (sum((p>.5)*y)+sum((p<.5)*(1-y)))/n.rows
    ##  priors
    for (i in 1:4) {
        b[i] ~ dnorm(0,pow(sdp,-2))
    }
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(0,pow(sdp,-2))
    }
    for (i in 1:n.groups) {
        g[i] ~ dnorm(0,tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
}

model.bugs.02 <- function() {
    sdp <- 10
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(mu.x[party.i[i]]-mu.x[party.j[i]])
        sumv[i] <- votes.i[i]+votes.j[i]+.001
        xb1[i] <- -d[i]+g[group[i]]+b[1]+b[2]*votes.j[i]+b[3]*(votes.j[i]/sumv[i])+
            b[4]*log(magnitude[i])
        xb2[i] <- -d[i]+g[group[i]]+b[1]+b[2]*votes.i[i]+b[3]*(votes.i[i]/sumv[i])+
            b[4]*log(magnitude[i])                
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##correct
    c1 <- (sum((p>.5)*y)+sum((p<.5)*(1-y)))/n.rows
    ##  priors
    for (i in 1:4) {
        b[i] ~ dnorm(0,pow(sdp,-2))
    }
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(0,pow(sdp,-2))
    }
    for (i in 1:n.groups) {
        g[i] ~ dnorm(0,tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
}

model.bugs.01 <- function() {
    sdp <- 10
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(x[party.i[i],state.i[i]]-x[party.j[i],state.i[i]])
        sumv[i] <- votes.i[i]+votes.j[i]+.001
        xb1[i] <- -d[i]+g[group[i]]+b[1]*votes.j[i]+b[2]*(votes.j[i]/sumv[i])+
            b[3]*log(magnitude[i])
        xb2[i] <- -d[i]+g[group[i]]+b[1]*votes.i[i]+b[2]*(votes.i[i]/sumv[i])+
            b[3]*log(magnitude[i])                
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##correct
    c1 <- (sum((p>.5)*y)+sum((p<.5)*(1-y)))/n.rows
    ##  priors
    for (i in 1:3) {
        b[i] ~ dnorm(0,pow(sdp,-2))
    }
    tau.x <- pow(sigma.x,-2)
    sigma.x ~ dunif(0,sdp)
    tau.v <- pow(sigma.v,-2)
    sigma.v ~ dunif(0,sdp)
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(0,pow(sdp,-2))
        mu.v[i] ~ dnorm(0,pow(sdp,-2))
        for (j in 1:n.states) {
            x[i,j] ~ dnorm(mu.x[i],tau.x)
            v[i,j] ~ dnorm(mu.v[i],tau.v)
        }
    }
    for (i in 1:n.states) {
        mu.g[i]~dnorm(0,pow(sdp,-2))
    }
    ##mu.g[1] <- 0
    for (i in 1:n.groups) {
        g[i] ~ dnorm(mu.g[state.j[i]],tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
}



## partial observability probit, independent errors
### multilevel model (hyper priors for party ideal points, parameters)
model.simple.bugs2 <- function() {
    sdp <- 20
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(mu.x[party.i[i]]-mu.x[party.j[i]])
        xb1[i] <- -d[i]+v[party.i[i]]+g[group[i]]
        xb2[i] <- -d[i]+v[party.j[i]]+g[group[i]]
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##  priors
    mu.x.all ~ dnorm(0,pow(sdp,-2))
    mu.v.all ~ dnorm(0,pow(sdp,-2))
    tau.x <- pow(sigma.x,-2)
    sigma.x ~ dunif(0,sdp)
    tau.v <- pow(sigma.v,-2)
    sigma.v ~ dunif(0,sdp)
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(mu.x.all,tau.x)
        v[i] ~ dnorm(mu.v.all,tau.v)
    }
    for (i in 1:n.groups) {
        g[i] ~ dnorm(0,tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
}


## partial observability probit, independent errors
model.simple.bugs <- function() {
    sdp <- 20
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(mu.x[party.i[i]]-mu.x[party.j[i]])
        xb1[i] <- -d[i]+v[party.i[i]]+g[group[i]]
        xb2[i] <- -d[i]+v[party.j[i]]+g[group[i]]
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##  priors
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(0,pow(sdp,-2))
        v[i] ~ dnorm(0,pow(sdp,-2))
    }
    for (i in 1:n.groups) {
        g[i] ~ dnorm(0,tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
}


## partial observability probit, independent errors
### multilevel model
model.bugs <- function() {
    sdp <- 20
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(x[party.i[i],state.i[i]]-x[party.j[i],state.i[i]])
        xb1[i] <- -d[i]+v[party.i[i],state.i[i]]+g[group[i]]
        xb2[i] <- -d[i]+v[party.j[i],state.i[i]]+g[group[i]]
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##  priors
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(0,pow(sdp,-2))
        mu.v[i] ~ dnorm(0,pow(sdp,-2))
        tau.x[i] <- pow(sigma.x[i],-2)
        sigma.x[i] ~ dunif(0,sdp)
        tau.v[i] <- pow(sigma.v[i],-2)
        sigma.v[i] ~ dunif(0,sdp)
        for (j in 1:n.states) {
            x[i,j] ~ dnorm(mu.x[i],tau.x[i])
            v[i,j] ~ dnorm(mu.v[i],tau.v[i])
        }
    }
    for (i
         in 1:n.groups) {
        g[i] ~ dnorm(0,tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
}





## partial observability probit, independent errors
### multilevel model
model.bugs2 <- function() {
    sdp <- 20
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(x[party.i[i],state.i[i]]-x[party.j[i],state.i[i]])
        xb1[i] <- -d[i]+v[party.i[i],state.i[i]]+g[group[i]]
        xb2[i] <- -d[i]+v[party.j[i],state.i[i]]+g[group[i]]
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##  priors
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(0,pow(sdp,-2))
        mu.v[i] ~ dnorm(0,pow(sdp,-2))
        tau.x[i] <- pow(sigma.x[i],-2)
        sigma.x[i] ~ dunif(0,sdp)
        tau.v[i] <- pow(sigma.v[i],-2)
        sigma.v[i] ~ dunif(0,sdp)
        for (j in 1:n.states) {
            x[i,j] ~ dnorm(mu.x[i],tau.x[i])
            v[i,j] ~ dnorm(mu.v[i],tau.v[i])
        }
    }
    for (i in 1:n.states) {
        mu.g[i] ~ dnorm(0,pow(sdp,-2))
        sigma.g[i] ~ dunif(0,sdp)
        tau.g[i] <- pow(sigma.g[i],-2)
    }
    for (j in 1:n.groups) {
        g[j] ~ dnorm(mu.g[state.j[j]],tau.g[state.j[j]])
    }
}



## partial observability probit, independent errors
### multilevel model
model.bugs3 <- function() {
    sdp <- 1000
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(x[party.i[i],state.i[i]]-x[party.j[i],state.i[i]])
        xb1[i] <- -d[i]+v[party.i[i],state.i[i]]+g[group[i]]
        xb2[i] <- -d[i]+v[party.j[i],state.i[i]]+g[group[i]]
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##  priors
    ##     mu.x.all ~ dnorm(0,pow(sdp,-2))
    ##     mu.v.all ~ dnorm(0,pow(sdp,-2))
    for (i in 1:n.states) {
        mu.g[i] ~ dnorm(0,tau.g.all)
    }
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(0,pow(sdp,-2))
        mu.v[i] ~ dnorm(0,pow(sdp,-2))
        for (j in 1:n.states) {
            x[i,j] ~ dnorm(mu.x[i],tau.x)
            v[i,j] ~ dnorm(mu.v[i],tau.v)
        }
    }
    tau.x <- pow(sigma.x,-2)
    sigma.x ~ dunif(0,sdp)
    tau.v <- pow(sigma.v,-2)
    sigma.v ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
    sigma.g ~ dunif(0,sdp)
    tau.g.all <- pow(sigma.g.all,-2)
    sigma.g.all ~ dunif(0,sdp)
    for (j in 1:n.groups) {
        g[j] ~ dnorm(mu.g[state.j[j]],tau.g)
    }
}





## partial observability probit, independent errors
### multilevel model
model.bugs4 <- function() {
    sdp <- 20
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(x[party.i[i],state.i[i]]-x[party.j[i],state.i[i]])
        xb1[i] <- -d[i]+v[party.i[i],state.i[i]]+g[group[i]]
        xb2[i] <- -d[i]+v[party.j[i],state.i[i]]+g[group[i]]
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##  priors
    tau.x.all <- pow(sigma.x.all,-2)
    sigma.x.all ~ dunif(0,sdp)
    tau.v.all <- pow(sigma.v.all,-2)
    sigma.v.all ~ dunif(0,sdp)
    tau.g.all <- pow(sigma.g.all,-2)
    sigma.g.all ~ dunif(0,sdp)
    mu.v.all ~ dnorm(0,pow(sdp,-2))
    mu.x.all ~ dnorm(0,pow(sdp,-2))
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(mu.x.all,tau.x.all)
        mu.v[i] ~ dnorm(mu.v.all,tau.v.all)
        tau.x[i] <- pow(sigma.x[i],-2)
        sigma.x[i] ~ dunif(0,sdp)
        tau.v[i] <- pow(sigma.v[i],-2)
        sigma.v[i] ~ dunif(0,sdp)
        for (j in 1:n.states) {
            x[i,j] ~ dnorm(mu.x[i],tau.x[i])
            v[i,j] ~ dnorm(mu.v[i],tau.v[i])
        }
    }
    for (i in 1:n.states) {
        mu.g[i] ~ dnorm(0,tau.g.all)
        sigma.g[i] ~ dunif(0,sdp)
        tau.g[i] <- pow(sigma.g[i],-2)
    }
    for (j in 1:n.groups) {
        g[j] ~ dnorm(mu.g[state.j[j]],tau.g[state.j[j]])
    }
}




## single tau.x, tau.v
## partial observability probit, independent errors
### multilevel model
model.bugs5 <- function() {
    sdp <- 20
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(x[party.i[i],state.i[i]]-x[party.j[i],state.i[i]])
        xb1[i] <- -d[i]+v[party.i[i],state.i[i]]+g[group[i]]
        xb2[i] <- -d[i]+v[party.j[i],state.i[i]]+g[group[i]]
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##  priors
    tau.x <- pow(sigma.x,-2)
    sigma.x ~ dunif(0,sdp)
    tau.v <- pow(sigma.v,-2)
    sigma.v ~ dunif(0,sdp)
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(0,pow(sdp,-2))
        mu.v[i] ~ dnorm(0,pow(sdp,-2))
        for (j in 1:n.states) {
            x[i,j] ~ dnorm(mu.x[i],tau.x)
            v[i,j] ~ dnorm(mu.v[i],tau.v)
        }
    }
    for (i in 2:n.states) {
        mu.g[i]~dnorm(0,pow(sdp,-2))
    }
    mu.g[1] <- 0
    for (i in 1:n.groups) {
        g[i] ~ dnorm(mu.g[state.j[i]],tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
}


## single tau.v,tau.x
## partial observability probit, independent errors
### multilevel model
model.bugs6 <- function() {
    sdp <- 20
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(x[party.i[i],state.i[i]]-x[party.j[i],state.i[i]])
        xb1[i] <- -d[i]+v[party.i[i],state.i[i]]+g[group[i]]
        xb2[i] <- -d[i]+v[party.j[i],state.i[i]]+g[group[i]]
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##  priors
    tau.x.all <- pow(sigma.x.all,-2)
    sigma.x.all ~ dunif(0,sdp)    
    tau.v.all <- pow(sigma.v.all,-2)
    sigma.v.all ~ dunif(0,sdp)    
    tau.x <- pow(sigma.x,-2)
    sigma.x ~ dunif(0,sdp)
    tau.v <- pow(sigma.v,-2)
    sigma.v ~ dunif(0,sdp)
    mu.x.all ~ dnorm(0,pow(sdp,-2))
    mu.v.all ~ dnorm(0,pow(sdp,-2))
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(mu.x.all,tau.x.all)
        mu.v[i] ~ dnorm(mu.v.all,tau.v.all)
        for (j in 1:n.states) {
            x[i,j] ~ dnorm(mu.x[i],tau.x)
            v[i,j] ~ dnorm(mu.v[i],tau.v)
        }
    }
    for (i in 2:n.states) {
        mu.g[i]~dnorm(0,pow(sdp,-2))
    }
    mu.g[1] <- 0
    for (i in 1:n.groups) {
        g[i] ~ dnorm(mu.g[state.j[i]],tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
}



## single tau.v,tau.x
## partial observability probit, independent errors
### multilevel model
model.bugs7 <- function() {
    sdp <- 20
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(x[party.i[i],state.i[i]]-x[party.j[i],state.i[i]])
        xb1[i] <- -d[i]+v[party.i[i],state.i[i]]+g[group[i]]
        xb2[i] <- -d[i]+v[party.j[i],state.i[i]]+g[group[i]]
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##  priors
    tau.x.all <- pow(sigma.x.all,-2)
    sigma.x.all ~ dunif(0,sdp)    
    tau.v.all <- pow(sigma.v.all,-2)
    sigma.v.all ~ dunif(0,sdp)    
    tau.x <- pow(sigma.x,-2)
    sigma.x ~ dunif(0,sdp)
    tau.v <- pow(sigma.v,-2)
    sigma.v ~ dunif(0,sdp)
    tau.g.all <- pow(sigma.g.all,-2)
    sigma.g.all ~ dunif(0,sdp)
    mu.x.all ~ dnorm(0,pow(sdp,-2))
    mu.v.all ~ dnorm(0,pow(sdp,-2))
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(mu.x.all,tau.x.all)
        mu.v[i] ~ dnorm(mu.v.all,tau.v.all)
        for (j in 1:n.states) {
            x[i,j] ~ dnorm(mu.x[i],tau.x)
            v[i,j] ~ dnorm(mu.v[i],tau.v)
        }
    }
    for (i in 1:n.states) {
        mu.g[i]~dnorm(0,tau.g.all)
    }
    for (i in 1:n.groups) {
        g[i] ~ dnorm(mu.g[state.j[i]],tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
}



## single tau.v,tau.x
## partial observability probit, independent errors
### multilevel model
model.bugs8 <- function() {
    sdp <- 10
    for (i in 1:n.rows) {
        y[i]~dbern(p[i])
        d[i] <- abs(x[party.i[i],state.i[i]]-x[party.j[i],state.i[i]])
        xb1[i] <- -d[i]+v[party.i[i],state.i[i]]+g[group[i]]
        xb2[i] <- -d[i]+v[party.j[i],state.i[i]]+g[group[i]]
        p[i] <- phi(xb1[i])*phi(xb2[i])
    }
    ##  priors
    tau.x.all <- pow(sigma.x.all,-2)
    sigma.x.all ~ dunif(0,sdp)    
    tau.v.all <- pow(sigma.v.all,-2)
    sigma.v.all ~ dunif(0,sdp)    
    tau.x <- pow(sigma.x,-2)
    sigma.x ~ dunif(0,sdp)
    tau.v <- pow(sigma.v,-2)
    sigma.v ~ dunif(0,sdp)
    mu.x.all ~ dnorm(0,pow(sdp,-2))
    mu.v.all ~ dnorm(0,pow(sdp,-2))
    for (i in 1:n.parties) {
        mu.x[i] ~ dnorm(mu.x.all,tau.x.all)
        mu.v[i] ~ dnorm(mu.v.all,tau.v.all)
        for (j in 1:n.states) {
            x[i,j] ~ dnorm(mu.x[i],tau.x)
            v[i,j] ~ dnorm(mu.v[i],tau.v)
        }
    }
    for (i in 1:n.groups) {
        g[i] ~ dnorm(0,tau.g)
    }
    sigma.g ~ dunif(0,sdp)
    tau.g <- pow(sigma.g,-2)
}








