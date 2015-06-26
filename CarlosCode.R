library(stats4)

continuousLogit.loglikelihood<-function(t,L,U,a,b) {
    V_t <- a + b*t
    continuouslogit.denom<-function(t){
        V_t.d <- a + b*t
        return(exp(V_t.d))
    }
    prob<-exp(V_t)/(integrate(continuouslogit.denom,lower=L,upper=U)$value)
    return(-sum(log(prob)))
}

continuousLogit.density<-function(t,L,U,a,b) {
    V_t <- a + b*t
    continuouslogit.denom<-function(t){
        V_t.d <- a + b*t
        return(exp(V_t.d))
    }
    prob<-exp(V_t)/(integrate(continuouslogit.denom,lower=L,upper=U)$value)
    return(prob)
}

continuousLogit.cdf<-function(t,L,U,a,b) {
    prob<-NULL
    for (i in 1:length(t)) {
        prob<-c(prob,integrate(continuousLogit.density,lower=L,upper=t[i],L=L,U=U,a=a,b=b)$value)
    }
    return(prob)
}

candidate.density<-function(x) {    
    return(rep(1,length(x)))    
}

curve(continuousLogit.density(t=x,L=1,U=3,a=2,b=0.4), from=1, to=3,add=FALSE,xlab="t",ylab="Density",ylim=c(0,1.2))
curve(candidate.density(x), from=1, to=3,add=TRUE,xlab="t",ylab="Density")
curve(continuousLogit.cdf(t=x,L=1,U=3,a=2,b=0), from=1,to=3,add=FALSE,xlab="t",ylab="Probability")

#Acceptance-Rejection

simulate_continuouslogit<-function(nsims,M){    
    candidate.density<-function(x) {        
        if (x>=1 && x<=3) {            
            1            
        } else 0        
    }    
    
    n.draws<-0    
    draws<- c()    
    x.grid<-seq(1,3,0.01)        
    while (n.draws < nsims) {                
        x.c <- runif(1,1,3)        
        accept.prob <- continuousLogit.density(x.c,L=1,U=3,a=1,b=0.5)/(M*candidate.density(x.c))        
        u<-runif(1,0,1)        
        if (u <= accept.prob) {            
            draws <- c(draws, x.c)            
            n.draws<-n.draws+1            
        }        
    }    
    return(draws)    
}



#Estimate ContinuousLogit - Identified Model

obschoices<-simulate_continuouslogit(1000,1)
print(fit<-mle(continuousLogit.loglikelihood, start=list(b=0), fixed=list(a=0,L=1,U=3,t=obschoices), method="BFGS",control=list(trace=TRUE, REPORT=1)))
print(summary(fit))
print(vcov(fit))



#Estimate ContinuousLogit - Unidentified Model

obschoices<-simulate_continuouslogit(1000,1)
print(fit<-mle(continuousLogit.loglikelihood, start=list(a=0,b=0), fixed=list(L=1,U=3,t=obschoices), method="BFGS",control=list(trace=TRUE, REPORT=1)))
print(summary(fit))
print(vcov(fit))
