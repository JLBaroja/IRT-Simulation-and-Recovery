# Inferring to the Graded Response Model
rm(list=ls())
load('~/Learning/Psychometrics/IRT simulation and recovery/simul_GRM.RData')

y <- simulation$responses
y <- y+1 # In order to use dcat() in JAGS
n_categories <- 3 # Assumed constant across items
j <- 1:n_categories # In order to use dcat() in JAGS
# j <- 0:(n_categories-1)
n_persons <- nrow(y)
n_items <- ncol(y)
# c_attr <- array(dim=c(n_items,n_categories))
# c_attr[,1] <- 1
x <- 1

observed <- list('y','n_persons','n_items','n_categories','x')
unobserved <- c('theta_prior','theta_post',
                'alpha_prior','alpha_post',
                'beta_prior','beta_post',
                'f_ij','pr_y_ij',
                # 'c_attr',
                'y_post')

write('
model{

  for(p in 1:n_persons){
    theta_prior[p]~dnorm(0,1)
    theta_post[p]~dnorm(0,1)
  }
  
  for(i in 1:n_items){
    alpha_prior[i]~dnorm(0,0.01)T(0,)
    alpha_post[i]~dnorm(0,0.01)T(0,)
    #for(j in 1:(n_categories-1)){
    #  beta_prior[i,j]~dnorm(0,1)
    #  beta_post[i,j]~dnorm(0,1)
    #}
    beta_post[i,1]~dnorm(0,1)T(,0)
    beta_post[i,2]~dnorm(0,1)T(0,)
  }
  
  
  for(p in 1:n_persons){
    for(i in 1:n_items){
      pr_y_ij[p,i,1] <- 1
      pr_y_ij[p,i,n_categories+1] <- 0
      for(b in 1:(n_categories-1)){
        pr_y_ij[p,i,(b+1)] <- 1/(1+exp(-alpha_post[i]*(theta_post[p]-beta_post[i,b])))
      }
      for(z in 1:n_categories){
        f_ij[p,i,z] <- pr_y_ij[p,i,z]-pr_y_ij[p,i,z+1]
      }
      y[p,i]~dcat(f_ij[p,i,])
      y_post[p,i]~dcat(f_ij[p,i,])
    }
  }
  
#  pr~dbeta(1,1)
#  x~dbern(pr)
  
}
','GRM.bug')
library('R2jags')
bayes <- jags(data = observed,
              parameters.to.save = unobserved,
              model.file = 'GRM.bug',
              n.iter=5000,
              n.chains=3,
              n.burnin=2000,
              n.thin=5)
unlink('GRM.bug')
nds <- bayes$BUGSoutput$sims.list

summary(bayes$BUGSoutput$summary[,'Rhat'])
summary(bayes$BUGSoutput$summary[,'n.eff'])


plot(NULL,xlim=c(-5,5),ylim=c(0,n_items+3))
abline(v=0,lty='dotted')
for(i in 1:n_items){
  for(j in 1:(n_categories-1)){
    hist(nds$beta_post[,i,j],plot=F)->ht
    lines(ht$mids,0.5*ht$density+i)
  }
  points(simulation$true_beta[i,],rep(i,n_categories-1),pch=16)
}

plot(NULL,xlim=c(0,10),ylim=c(0,n_items+3))
for(i in 1:n_items){
  for(j in 1:(n_categories-1)){
    hist(nds$alpha_post[,i],plot=F)->ht
    lines(ht$mids,5*ht$density+i)
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
y <- simulation$responses
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









for(i1 in 1:dim(nds$f_ij)[1]){
  for(i2 in 1:dim(nds$f_ij)[2]){
    for(i3 in 1:dim(nds$f_ij)[3]){
      # for(i4 in 1:dim(nds$f_ij)[4]){
      for(i4 in c(2)){
        if(nds$f_ij[i1,i2,i3,i4]<0){
          print(c(i1,i2,i3,i4))
        }
      }
    }
  }
}



