# Inferring to the Generalized Partial Credit Model
rm(list=ls())
load('~/Learning/Psychometrics/IRT simulation and recovery/simul_GPCM.RData')

y <- simulation$responses
y <- y+1 # In order to use dcat() in JAGS
n_categories <- 3 # Assumed constant across items
j <- 1:n_categories # In order to use dcat() in JAGS
# j <- 0:(n_categories-1)
n_persons <- nrow(y)
n_items <- ncol(y)
# c_attr <- array(dim=c(n_items,n_categories))
# c_attr[,1] <- 1

observed <- list('y','n_persons','n_items','n_categories')
unobserved <- c('theta_prior','theta_post',
                'alpha_prior','alpha_post',
                'beta_prior','beta_post',
                'c_attr',
                'y_post')

write('
model{
  for(p in 1:n_persons){
    theta_prior[p]~dnorm(0,1)
    theta_post[p]~dnorm(0,1)
  }
  
  for(i in 1:n_items){
    alpha_prior[i]~dnorm(0,1)T(0,)
    alpha_post[i]~dnorm(0,1)T(0,)
    for(j in 1:(n_categories-1)){
      beta_prior[i,j]~dnorm(0,1)
      beta_post[i,j]~dnorm(0,1)
    }
  }
  
  
  for(p in 1:n_persons){
    for(i in 1:n_items){
      c_attr[p,i,1] <- 1
      for(j in 1:(n_categories-1)){
        c_attr[p,i,(j+1)] <- exp(alpha_post[i]*(j*theta_post[p]-sum(beta_post[i,1:j])))
      }
      y[p,i]~dcat(c_attr[p,i,]) # Categorical parameter may be unnormalized
      y_post[p,i]~dcat(c_attr[p,i,])
    }
  }
  
}
','GPCM.bug')
library('R2jags')
bayes <- jags(data = observed,
              parameters.to.save = unobserved,
              model.file = 'GPCM.bug',
              n.iter=5000,
              n.chains=3,
              n.burnin=2000,
              n.thin=5)
unlink('GPCM.bug')
nds <- bayes$BUGSoutput$sims.list

summary(bayes$BUGSoutput$summary[,'Rhat'])
summary(bayes$BUGSoutput$summary[,'n.eff'])



plot(NULL,xlim=c(-5,5),ylim=c(0,n_items+3))
for(i in 1:n_items){
  for(j in 1:(n_categories-1)){
    hist(nds$beta_post[,i,j],plot=F)->ht
    lines(ht$mids,0.5*ht$density+i)
  }
  points(simulation$true_beta[i,],rep(i,n_categories-1),pch=16)
}

plot(NULL,xlim=c(0,5),ylim=c(0,n_items+3))
for(i in 1:n_items){
  for(j in 1:(n_categories-1)){
    hist(nds$alpha_post[,i],plot=F)->ht
    lines(ht$mids,3*ht$density+i)
  }
  points(simulation$true_alpha[i],i,pch=16)
}

prediction <- apply(nds$y_post,MARGIN=c(2,3),FUN=median)
prediction <- prediction-1


# Plotting observed responses
y <- prediction
# j <- simulation$j
# n_categories <- length(j)
# y <- responses
n_categories <- 3
j <- 0:(n_categories-1)
n_persons <- nrow(y)
n_items <- ncol(y)
cat_colors <-  c('#edf8fb','#b3cde3','#8c96c6','#8856a7','#810f7c')[c(1,3,5)]
# ord_pers <- order(apply(y,FUN=sum,MARGIN=1))
# ord_items <- order(apply(y,FUN=sum,MARGIN=2))
ord_pers <- order(apply(simulation$responses,FUN=sum,MARGIN=1))
ord_items <- order(apply(simulation$responses,FUN=sum,MARGIN=2))
y_ord <- y[ord_pers,ord_items]
plot(NULL,xlim=c(1,n_items),ylim=c(1,n_persons))
for(p in 1:n_persons){
  for(i in 1:n_items){
    col <- which(j==y_ord[p,i])
    polygon(x=c(i+0.5,i-0.5,i-0.5,i+0.5),
            y=rep(c(p-0.5,p+0.5),each=2),
            col=cat_colors[col])
  }
}
