#Discretized Multinomial Logit
#Function defining travel speed function, use similar formula from paper: Time of Day Modeling in a Tour-Based Context: The Tel-Aviv Experience 

defineTravelTimeMatrix <- function(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,df){
  fTime <- function(x){
    #        a*x^4+b*x^3+c*x^2+d*x+e
    resu = matrix(0,nrow(x),ncol(x))
    delay<-df$delay
    distance<-df$distance
    fspeed<-df$ODfspeed
    for (i in 1:nrow(x)){
      for (j in 1:ncol(x)){
        resu[i,j]=distance[i]/(fspeed[i]*exp(a1+a2*log(distance[i])+a3*delay[i]+a4*delay[i]*exp(sin(x[i,j]*2*pi/24))+a5*delay[i]*exp((sin(x[i,j]*2*pi/24)))^2+a6*delay[i]*exp((sin(x[i,j]*2*pi/24)))^3+a7*delay[i]*exp((sin(x[i,j]*2*pi/24)))^4
                                             + a8*delay[i]*exp(cos(x[i,j]*2*pi/24))+a9*delay[i]*exp((cos(x[i,j]*2*pi/24)))^2+a10*delay[i]*exp((cos(x[i,j]*2*pi/24)))^3+a11*delay[i]*exp((cos(x[i,j]*2*pi/24)))^4
                                             +a12*delay[i]*exp(sin(x[i,j]*4*pi/24))+a13*delay[i]*exp((sin(x[i,j]*4*pi/24)))^2+a14*delay[i]*exp((sin(x[i,j]*4*pi/24)))^3+a15*delay[i]*exp((sin(x[i,j]*4*pi/24)))^4
                                             + a16*delay[i]*exp(cos(x[i,j]*4*pi/24))+a17*delay[i]*exp((cos(x[i,j]*4*pi/24)))^2+a18*delay[i]*exp((cos(x[i,j]*4*pi/24)))^3+a19*delay[i]*exp((cos(x[i,j]*4*pi/24)))^4))
      }
    }
    resu #+ matrix(runif(nrow(x)*ncol(x),min = 0,max = 1),nrow(x),ncol(x))
  }
  fTime
}

defineTravelCostMatrix <- function(a1,a2){
  fCost <- function(x){
    #        a*x^4+b*x^3+c*x^2+d*x+e
    resu = matrix(0,nrow(x),ncol(x))
    for (i in 1:nrow(x)){
      for (j in 1:ncol(x)){   
        resu[i,j] <- 0
        if (x[i,j] > 5 & x[i,j] < 10) {
          resu[i,j] <- a1
        } else if ( x[i,j] > 15 & x[i,j] < 22) {
          resu[i,j] <- a2
        }    
        
        
      }
    }
    resu #+ matrix(runif(nrow(x)*ncol(x),min = 0,max = 1),nrow(x),ncol(x))
  }
  fCost
}


#Continuous Logit
#Function defining travel speed function, use similar formula from paper: Time of Day Modeling in a Tour-Based Context: The Tel-Aviv Experience 
defineTravelTimeFunction<- function(a1,a2,a3,a4,a5,a6,a7,a8,a9,a10,a11,a12,a13,a14,a15,a16,a17,a18,a19,df){
  fTime <- function(x, id){
    #        a*x^4+b*x^3+c*x^2+d*x+e
    re <- x
    delay<-df$delay[id]
    distance<-df$distance[id]
    fspeed<-df$ODfspeed[id]
    for (i in 1:length(x)){
      re[i]=distance/(fspeed*exp(a1+a2*log(distance)+a3*delay+a4*delay*exp(sin(x[i]*2*pi/24))+a5*delay*exp((sin(x[i]*2*pi/24)))^2+a6*delay*exp((sin(x[i]*2*pi/24)))^3+a7*delay*exp((sin(x[i]*2*pi/24)))^4
                                 + a8*delay*exp(cos(x[i]*2*pi/24))+a9*delay*exp((cos(x[i]*2*pi/24)))^2+a10*delay*exp((cos(x[i]*2*pi/24)))^3+a11*delay*exp((cos(x[i]*2*pi/24)))^4
                                 +a12*delay*exp(sin(x[i]*4*pi/24))+a13*delay*exp((sin(x[i]*4*pi/24)))^2+a14*delay*exp((sin(x[i]*4*pi/24)))^3+a15*delay*exp((sin(x[i]*4*pi/24)))^4
                                 + a16*delay*exp(cos(x[i]*4*pi/24))+a17*delay*exp((cos(x[i]*4*pi/24)))^2+a18*delay*exp((cos(x[i]*4*pi/24)))^3+a19*delay*exp((cos(x[i]*4*pi/24)))^4))
    }
    re
  }
  fTime
}


#Step function
defineTravelCostFunction<- function(a1,a2){
  fCost <- function(x, id){
    #        a*x^4+b*x^3+c*x^2+d*x+e
    re <- x
    for (i in 1:length(x)){
      re[i] <- 0
      if (x[i] > 5 & x[i] < 10) {
        re[i] <- a1
      } else if ( x[i] > 15 & x[i] < 22) {
        re[i] <- a2
      }
      
    }
    re
  }
  fCost
}


continuousLogit.density_simulate<-function(t,L,U,ID,a,b,c,d,e,ff,g,h,i,j,k,networktraveltime_cl,networktravelcost_cl ) {
  
  exp.V_t <- function(t,ID){
    V_t <- a + b*60*networktraveltime_cl(t,ID)+c*networktravelcost_cl(t,ID)+d*sin(t*2*pi/24)+e*sin(t*4*pi/24)+ff*sin(t*6*pi/24)+g*sin(t*8*pi/24)+h*cos(t*2*pi/24)+i*cos(t*4*pi/24)+j*cos(t*6*pi/24)+k*cos(t*8*pi/24)
    return(exp(V_t))  
  }
  
  continuouslogit.denom<-function(ID){
    denom_integral <- integrate(exp.V_t,lower=L,upper=U,ID=ID,rel.tol=1e-10)
    #print(paste("Absolute Error of Denominator Integral of ",toString(ID)," is ",toString(denom_integral $abs.error)))
    return(denom_integral$value)
  }
  
  
  prob<-exp.V_t(t,ID)/continuouslogit.denom(ID)
  return(prob)
}


#Acceptance-Rejection -- Simulation
simulate_continuouslogit<-function(M,nobs,networktraveltime,networktravelcost){
  
  candidate.density<-function(x) {
    if (x>=0 && x<=24) {
      1
    } else 0
  }
  
  draws<- c()
  
  for (i in 1:nobs) {
    
    n.draws<-0
    print(i)
    while (n.draws < 1) {
      
      x.c <- runif(1,0,24)
      accept.prob <- continuousLogit.density_simulate(x.c,L=0,U=24,a=0,b=-0.5,c=0,d=3.70,e=2.43,ff=2.46,g=0.55,h=-4.73,i=-0.51,j=2.40,k=1.34,ID=i,networktraveltime_cl=networktraveltime,networktravelcost_cl=networktravelcost)/(M*candidate.density(x.c))
      #accept.prob <- continuousLogit.density_plot(x.c,L=0,U=24,a=0,b=0,c=0,d=3.70,e=0,ff=0,g=0,h=0,i=0,j=0,k=0,ID=i)/(M*candidate.density(x.c))
      u<-runif(1,0,1)
      if (u <= accept.prob) {
        draws <- c(draws, x.c)
        n.draws<-n.draws+1
      }	
    }
    
  }
  
  return(draws)
}

simulationRun <- function(nobs,timeInterval,M=0.3){
  library(stats4)
  df <- data.frame(ID=1:nobs, stringsAsFactors=FALSE)
  
  #OD distance is Gamma distributed
  dist <- rgamma(nobs,2,0.25)
  
  df$distance<-dist
  
  #OD Freeflow Speed is Discrete Uniform distributed
  fspeed <- sample(c(40,50,60,70,80,90),nobs,replace=T)
  
  df$ODfspeed<-fspeed
  
  #OD Peak Speed is Discrete Uniform distributed
  pspeed <- sample(c(15,20,25,30,35,40),nobs,replace=T)
  
  df$ODpspeed<-pspeed
  
  #Delay
  delay <- 1 - (pspeed/fspeed)
  
  df$delay<-delay
  
  
  #timeInterval represents the time interval we want to have (unit: min). For example, if timeInterval = 60, we have 1hour time interval, then we have 24 alternatives
  #timeInterval = 60
  nalt= 1440/timeInterval
  delta_t<-1/(60/timeInterval)
  #base is the alternative matrix, nrow = nobs, ncol = nalt, for each row, change from 1/nalt to 24, representing nalt alternatives
  base = (t(matrix(1:nalt, nalt, nobs))-0.5)/(60/timeInterval)
  
  #Assume function for travel time and TTR
  networktraveltime_mnl <- defineTravelTimeMatrix(0.0317,0.0072,-1.4865,-11.2967,12.5130,-5.6582,0.9182,7.0780,-5.6571,1.8769,-0.2359,-1.9606,1.7074,-0.5433,0.0630,2.3598,-0.7701,0.1102,0.0018,df)
  networktraveltime_cl<-defineTravelTimeFunction(0.0317,0.0072,-1.4865,-11.2967,12.5130,-5.6582,0.9182,7.0780,-5.6571,1.8769,-0.2359,-1.9606,1.7074,-0.5433,0.0630,2.3598,-0.7701,0.1102,0.0018,df)
  networktravelcost_mnl<-defineTravelCostMatrix(1,2)
  networktravelcost_cl<-defineTravelCostFunction(1,2)
  traveltime_mnl<-networktraveltime_mnl(base)
  traveltime_cl<-networktraveltime_cl(0:1)
  #  networktravelcost_cl(3,1)
  #  networktravelcost_mnl(base)
  
  df$choice<-simulate_continuouslogit(M,nobs,networktraveltime_cl,networktravelcost_cl)
  
  #Discretized Multinomial Logit
  #Convert observed choices to MNL alternative choices
  pos_alt<-NULL
  choice_mnl<-NULL
  for (i in 1:nobs){
    pos_alt[i] <- sum(as.numeric(df$choice[i] > base[1,]))
    
    if (df$choice[i] <= (pos_alt[i])/(60/timeInterval)){
      if (pos_alt[i]==0){pos_alt[i]<-1}
      choice_mnl[i]<-base[1, pos_alt[i]]
    } else {
      choice_mnl[i]<-base[1,pos_alt[i]+1]
    }
  }
  df$choice_mnl<-choice_mnl
  multinomialLogit.loglikelihood_midpoint<-function(a,b,c,d,e,ff,g,h,i,j,k,nobs,timeInterval){
    #Define Choice Probability - Multinomial Logit
    delta_t<-1/(60/timeInterval)
    nalt= 1440/timeInterval
    base = (t(matrix(1:nalt, nalt, nobs))-0.5)/(60/timeInterval)
    ChoiceProb<-function(chosenV,sysV){
      return(exp(chosenV)/rowSums(exp(sysV)))
    }
    
    #Define Systematic Utilities for Discretisize MNL
    V_ti <- mat.or.vec(nobs, nalt) #systematic utilities for Choice Model
    V_chosen <- mat.or.vec(nobs, nalt) # Chosen systematic utility for Choice Model
    
    V_ti <- a + b*60*networktraveltime_mnl(base) + c*networktravelcost_mnl(base)+d*sin(base*2*pi/24)+e*sin(base*4*pi/24)+ff*sin(base*6*pi/24)+g*sin(base*8*pi/24)+h*cos(base*2*pi/24)+i*cos(base*4*pi/24)+j*cos(base*6*pi/24)+k*cos(base*8*pi/24) 
    
    for (i in 1:nobs){
      V_chosen[i,] <- as.numeric(df$choice_mnl[i] == base[i,])*V_ti[i,]
    }
    
    V_chosen <- rowSums(V_chosen)
    
    prob<-mat.or.vec(nobs,1)
    
    prob<-ChoiceProb(V_chosen,V_ti)
    
    return(-sum(log(prob)))
    
  }
  #fit_mnl <- with(df,mle(multinomialLogit.loglikelihood_midpoint, start=list(b=-0.5,d=3.70,e=2.43,ff=2.46,g=0.55,h=-4.73,i=-0.51,j=2.40,k=1.34), fixed=list(a=0,c=0,nobs=nobs,timeInterval=timeInterval), method="L-BFGS-B",lower=rep(-5,9),upper=rep(5,9),control=list(trace=TRUE, REPORT=1)))
  #print(summary(fit_mnl))
  #print(vcov(fit_mnl))
  
  
  continuousLogit.loglikelihood_midpoint<-function(t,a,b,c,d,e,ff,g,h,i,j,k,nobs,timeInterval){
    
    exp.V_t <- function(t,ID){
      V_t <- a + b*60*networktraveltime_cl(t,ID)+c*networktravelcost_cl(t,ID)+d*sin(t*2*pi/24)+e*sin(t*4*pi/24)+ff*sin(t*6*pi/24)+g*sin(t*8*pi/24)+h*cos(t*2*pi/24)+i*cos(t*4*pi/24)+j*cos(t*6*pi/24)+k*cos(t*8*pi/24)
      return(exp(V_t))	
    }
    
    expV_t<-Vectorize(exp.V_t)
    i.expV_t<-expV_t(t,1:nobs)
    
    V_ti <- mat.or.vec(nobs, nalt) #systematic utilities for Choice Model
    V_ti <- a + b*60*networktraveltime_mnl(base) + c*networktravelcost_mnl(base)+d*sin(base*2*pi/24)+e*sin(base*4*pi/24)+ff*sin(base*6*pi/24)+g*sin(base*8*pi/24)+h*cos(base*2*pi/24)+i*cos(base*4*pi/24)+j*cos(base*6*pi/24)+k*cos(base*8*pi/24) + log(delta_t) 		
    
    
    continuouslogit.denom<-function(ID){
      return(sum(exp(V_ti[ID,])))
    }
    
    denom<-Vectorize(continuouslogit.denom)
    i.denom<-denom(1:nobs)
    
    prob<-i.expV_t/i.denom
    
    
    loglik <- sum(log(prob))
    
    
    
    
    return(-loglik)
    
  }
  fit_mcl<-with(df,mle(continuousLogit.loglikelihood_midpoint, start=list(b=-0.5,d=3.70,e=2.43,ff=2.46,g=0.55,h=-4.73,i=-0.51,j=2.40,k=1.34), fixed=list(a=0,c=0,t=df$choice, nobs=nobs,timeInterval=timeInterval), method="BFGS",control=list(trace=TRUE, REPORT=1,maxit=300)))
  
  
  number = length(df$choice)
  # a = list(mnl = coef(fit_mnl),mcl = coef(fit_mcl)[(number+1):(number+13)])
  #coef(fit_mnl)
  coef(fit_mcl)[(number+1):(number+13)]
}


# set.seed(8)
# iterationRun <- function(){
#   library(stats4)
#   nsims<-72
#   M <- 0.3
#   #beta_Matrix_midMNL <- data.frame(beta = c("a","b","c","d","e","ff","g","h","i","j","k","nobs","timeInterval"))
#   beta_Matrix_midCL <- data.frame(beta = c("a","b","c","d","e","ff","g","h","i","j","k","nobs","timeInterval"))
#   #t,a,b,c,d,e,ff,g,h,i,j,k
#   for (nobs in c(100,500)){
#     for(timeInterval in c(15,30,60)){
#       for (k in 1:nsims) {
#         # number = floor(runif(1, 1,101)) 
#         number = Sys.getpid()
#         name = paste(nobs,"obs", timeInterval,"min",k,"iter","core",number,sep="")
#         #a = simulationRun(nobs,timeInterval,M,k)
#         #beta_Matrix_midMNL[,name] <- a$mnl
#         #beta_Matrix_midCL[,name] <- a$mcl
#         #beta_Matrix_midMNL[,name] <- simulationRun(nobs,timeInterval,M,k)
#         beta_Matrix_midCL[,name]=simulationRun(nobs,timeInterval,M,k)
#         print(k)       
#       }
#     }
#   }
#   #  number = floor(runif(1, 1,101)) 
#   number = Sys.getpid()
#   #write.table(beta_Matrix_midMNL,paste("betas_Matrix_NOBS100A500_INTERVAL15_30_60","random",number,".txt",sep=""))
#   write.table(beta_Matrix_midCL,paste("betas_Matrix","core",number,".txt",sep=""))
#   #beta_Matrix_midMNL
#   beta_Matrix_midCL
# }

combineResults <- function(results,nobs,timeInterval){
  name = paste('min',timeInterval,'obs',nobs,'CLresults.csv',sep = '')
  resultsCombine =data.frame()
  for(i in 1:length(results)){
    resultsCombine = rbind(resultsCombine,results[[i]])
  }
  names(resultsCombine) = c("a","b","c","d","e","ff","g","h","i","j","k","nobs","timeInterval")
  write.csv(resultsCombine,name)
  resultsCombine
}

library(doParallel)
library(foreach)
detectCores()
cl<- makeCluster(40)
registerDoParallel(cl)
getDoParWorkers()

set.seed(8)
start.time <- Sys.time()
results = foreach(i = 1:1000) %dopar% simulationRun(100,60)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
min60obs100CLresultsCombine = combineResults(results,100,60)

start.time <- Sys.time()
results = foreach(i = 1:1000) %dopar% simulationRun(100,30)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
min30obs100CLresultsCombine = combineResults(results,100,30)

start.time <- Sys.time()
results = foreach(i = 1:1000) %dopar% simulationRun(100,15)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
min15obs100CLresultsCombine = combineResults(results,100,15)


start.time <- Sys.time()
results = foreach(i = 1:1000) %dopar% simulationRun(500,60)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
min60obs500CLresultsCombine = combineResults(results,500,60)

start.time <- Sys.time()
results = foreach(i = 1:1000) %dopar% simulationRun(500,30)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
min30obs500CLresultsCombine = combineResults(results,500,30)

start.time <- Sys.time()
results = foreach(i = 1:1000) %dopar% simulationRun(500,15)
end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken
min15obs500CLresultsCombine = combineResults(results,500,15)

min15obs500CLresults = read.csv("min15obs500CLresults.csv")[,2:14]
min30obs500CLresults = read.csv("min30obs500CLresults.csv")[,2:14]
min60obs500CLresults = read.csv("min60obs500CLresults.csv")[,2:14]
min15obs100CLresults = read.csv("min15obs100CLresults.csv")[,2:14]
min30obs100CLresults = read.csv("min30obs100CLresults.csv")[,2:14]
min60obs100CLresults = read.csv("min60obs100CLresults.csv")[,2:14]
CLresults = rbind(min15obs500CLresults,min30obs500CLresults,min60obs500CLresults,min15obs100CLresults,min30obs100CLresults,min60obs100CLresults)
write.csv(CLresults,"CLresults.csv")
min15obs500CLresultsMean = apply(min15obs500CLresults,2,mean)
min30obs500CLresultsMean = apply(min30obs500CLresults,2,mean)
min60obs500CLresultsMean = apply(min60obs500CLresults,2,mean)
min15obs100CLresultsMean = apply(min15obs100CLresults,2,mean)
min30obs100CLresultsMean = apply(min30obs100CLresults,2,mean)
min60obs100CLresultsMean = apply(min60obs100CLresults,2,mean)
resultsMean = rbind(min15obs500CLresultsMean,min30obs500CLresultsMean,min60obs500CLresultsMean,min15obs100CLresultsMean,min30obs100CLresultsMean,min60obs100CLresultsMean,truevalues)
write.csv(resultsMean,"CLresultsMean.csv")
min15obs500CLresultsError = min15obs500CLresultsMean[1:11] - truevalues
min30obs500CLresultsError = min30obs500CLresultsMean[1:11] - truevalues
min60obs500CLresultsError = min60obs500CLresultsMean[1:11] - truevalues
min15obs100CLresultsError = min15obs100CLresultsMean[1:11] - truevalues
min30obs100CLresultsError = min30obs100CLresultsMean[1:11] - truevalues
min60obs100CLresultsError = min60obs100CLresultsMean[1:11] - truevalues
resultsError = rbind(min15obs500CLresultsError,min30obs500CLresultsError,min60obs500CLresultsError,min15obs100CLresultsError,min30obs100CLresultsError,min60obs100CLresultsError)
write.csv(resultsError,"CLresultsError.csv")
min15obs500CLresultssd = apply(min15obs500CLresults,2,sd)
min30obs500CLresultssd = apply(min30obs500CLresults,2,sd)
min60obs500CLresultssd = apply(min60obs500CLresults,2,sd)
min15obs100CLresultssd = apply(min15obs100CLresults,2,sd)
min30obs100CLresultssd = apply(min30obs100CLresults,2,sd)
min60obs100CLresultssd = apply(min60obs100CLresults,2,sd)
resultssd = rbind(min15obs500CLresultssd,min30obs500CLresultssd,min60obs500CLresultssd,min15obs100CLresultssd,min30obs100CLresultssd,min60obs100CLresultssd)
write.csv(resultssd,"CLresultssd.csv")
# 
# min60obs100CLresultsCombine =data.frame()
# for(i in 1:length(results)){
#   min60obs100CLresultsCombine = rbind(min60obs100CLresultsCombine,results[[i]])
# }
# names(min60obs100CLresultsCombine) = c("a","b","c","d","e","ff","g","h","i","j","k","nobs","timeInterval")
# write.csv(min60obs100CLresultsCombine,'min60obs100CLResults.csv')


truevalues=c(a=0, b=-0.5,c=0, d=3.70,e=2.43,ff=2.46,g=0.55,h=-4.73,i=-0.51,j=2.40,k=1.34)
plot(density(min15obs500CLresults[,2]),lty=1,col="red")
lines(density(min30obs500CLresults[,2]),lty=2,col="dark red")
lines(density(min60obs500CLresults[,2]),lty=3,col = "black")
abline(v=truevalues[2])
legend("topright", c("15min","30min","60min"), lty=c(1,2,3),col=c("red","dark red","darkgrey"))


# clusterExport(cl,c("defineTravelTimeMatrix","defineTravelCostMatrix","defineTravelTimeFunction","defineTravelCostFunction","continuousLogit.density_simulate","multinomialLogit.loglikelihood_midpoint","simulate_continuouslogit","simulationRun","iterationRun"))
# beta_list = clusterCall(cl,iterationRun)
# #clusterApply(cl, rep(1,3),iterationRun())
# combineResults <- function(beta_list){
#   re = beta_list[[1]]
#   for (i in 1:(length(beta_list)-1)){
#     re = cbind(re,beta_list[[i+1]])
#   }
#   re
# }
# beta_listCombine = combineResults(beta_list)
# write.table(beta_listCombine,paste("betas_Matrix_NOBS100A500_INTERVAL15_30_60_Combine.txt",sep=""))
# stopCluster(cl)
#write.table(beta_Matrix,"betas_Matrix.txt")
#truevalues=c(a=0, b=-0.5,c=0, d=3.70,e=2.43,ff=2.46,g=0.55,h=-4.73,i=-0.51,j=2.40,k=1.34)
# beta_estimate = read.csv("~/betas_Matrix_NOBS100A500_INTERVAL15_30_60_Combine.csv")
# beta_estimate_timeIntervalGroup = data.frame(beta_estimate,group=factor(beta_estimate$timeInterval))
# beta_estimate_nobs500 = beta_estimate[beta_estimate$nobs == 500,]
# truevalues=c(a=0, b=-0.5,c=0, d=3.70,e=2.43,ff=2.46,g=0.55,h=-4.73,i=-0.51,j=2.40,k=1.34)
# plot(density(beta_estimate_nobs500[beta_estimate_nobs500$timeInterval==15,][,3]),lty=1,col="red")
# lines(density(beta_estimate_nobs500[beta_estimate_nobs500$timeInterval==30,][,3]),lty=2,col="dark red")
# lines(density(beta_estimate_nobs500[beta_estimate_nobs500$timeInterval==60,][,3]),lty=3,col = "black")
# abline(v=truevalues[2])
# legend("topright", c("15min","30min","60min"), lty=c(1,2,3),col=c("red","dark red","darkgrey"))
