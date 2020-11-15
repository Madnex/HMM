library(depmix)
library(quantmod)
library(depmixS4)
library(parallel)
set.seed(1)
Nklower <- 50
Nkupper <- 150
bullmean <- 0.1
bullvar <- 0.1
bearmean <- -0.05
bearvar <- 0.2
days <- replicate(5, sample(Nklower :Nkupper, 1))



marketbull1 <- rnorm( days[1], bullmean, bullvar )
marketbear2 <- rnorm( days[2], bearmean, bearvar )
marketbull3 <- rnorm( days[3], bullmean, bullvar )
marketbear4 <- rnorm( days[4], bearmean, bearvar )
marketbull5 <- rnorm( days[5], bullmean, bullvar )



trueregimes <-c( rep(1,days[1]), rep(2,days[2]), rep(1,days[3]), rep(2,days[4]), rep(1,days[5]))
returns <-c( marketbull1, marketbear2, marketbull3, marketbear4, marketbull5)
plot(returns, type="l", ylab="Returns")
?depmix
hmm <- depmix(returns ~ 1, family = gaussian(), nstates = 2,data=data.frame(returns=returns))
hmmfit <- fit(hmm, verbose = FALSE)
postprobs <- posterior(hmmfit)
layout(1:2)
plot(postprobs$state, type='s', main='True Regimes', ylab='Regime')
matplot(postprobs[, -1], type='l', main='Regime Posterior Probabilities',ylab='Probability')
legend(x='topright', c('Bull','Bear'), fill=1:2, bty='n')
?getSymbols
getSymbols("^GSPC", from="2004-01-01" )
GSPC
Close<-GSPC$GSPC.Close
Returns<-vector()
for( i in 2:length(Close)){
  Returns[i]<-(as.numeric(Close[i])-as.numeric(Close[i-1]))/as.numeric(Close[i-1])
}
Returns
Close$Returns<-Returns
Close
plot.xts(Close$Returns)



states<-20
hmm <- lapply(2:states,function(i){depmix(Returns ~ 1, family = gaussian(), nstates = i,data=data.frame(Close$Returns))})
hmmfit <- mclapply(1:(states-1),function(i){fit(hmm[[i]], verbose = FALSE)},mc.cores=20)
hmmfit
postprobs <- lapply(1:(states-1),function(i){posterior(hmmfit[[i]])})
postprobs
for(i in 1:4){
  layout(1:2)
  plot(postprobs[[i]]$state, type='s', main='Estimated Regimes', ylab='Regime')
  matplot(postprobs[[i]][, -1], type='l', main='Regime Posterior Probabilities',ylab='Probability')
  legend(x='bottomright', legend=c(1:(i+1)), fill=1:(i+1), bty='n')
}
?legend
AIC<-vector()
for(i in 1:length(hmmfit)){
  AIC[i]<-AIC(hmmfit[[i]])
}
which.min(AIC)



BIC<-vector()
for(i in 1:length(hmmfit)){
  BIC[i]<-BIC(hmmfit[[i]])
}
which.min(BIC)
