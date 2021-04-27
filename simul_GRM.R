# Simulating from the Graded Response Model
# Polytomous model for ordered responses
rm(list=ls())

# Model definition
GRM <- function(alpha_i,betas_ij,th){
  # Returns response probabilities, category and item information, and other relevant
  # quantities per ITEM (detailed by CATEGORY), according to the 
  # Graded Response Model.
  
  # 'alpha_i' is a scalar at ITEM level
  # 'beta_ij' is an array at CATEGORY level
  # 'th' is the value in the latent trait at which to evaluate
  # performance of item i and categories ij
  
  # betas_ij <- c(-2,2,3)
  # alpha_i <- 1
  # th <- 1
  n_cat <- length(betas_ij)+1 # Number of categories
  j <- 0:(n_cat-1)
  pr_y_ij <- array(dim=n_cat+1) # Pr(Yij>=j) j%in%(0,1,...,m-1) 
  pr_y_ij[1] <- 1
  pr_y_ij[length(pr_y_ij)] <- 0
  for(b in 1:(n_cat-1)){
    pos <- b+1
    pr_y_ij[pos] <- 1/(1+exp(-alpha_i*(th-betas_ij[b])))
  }
  # Next lines define Pr(Yij=j), j%in%(0,1,...,m-1)
  # using the "indexing trick":
  indx_low <- 1:(length(pr_y_ij)-1)
  indx_up <- 2:length(pr_y_ij)
  f_ij <- pr_y_ij[indx_low]-pr_y_ij[indx_up]
  # Calculating category information using the same trick:
  # (Getting negative values here... need to revisit calmly)
  indx_low <- 1:(n_cat)
  indx_up <- 2:(n_cat+1)
  I_ij <- (alpha_i^2)*(pr_y_ij[indx_low]*(1-pr_y_ij[indx_low])-pr_y_ij[indx_up]*(1-pr_y_ij[indx_up]))
  I_i <- sum(I_ij)
  
  E_Y_th <- sum(j*f_ij) # Expected category
  
  return(list(j=j,f_ij=f_ij,I_ij=I_ij,
              pr_y_ij=pr_y_ij[1:n_cat],
              # attr=category_attractions,
              E_Y=E_Y_th,I_i=I_i))
  
}




# Towards some plotting functions...

theta <- seq(-10,10,.1)
# At item level...
betas <- seq(-2,2,length.out=3)
alpha <- 2

n_cat <- length(betas)+1
# theta <- 1
cat_attr <- array(dim=c(n_cat,length(theta)))
f_ij <- array(dim=dim(cat_attr))
pr_y_ij <- array(dim=dim(cat_attr))
I_ij <- array(dim=dim(cat_attr))
E_Y <- array(dim=length(theta))
I_i <- array(dim=length(theta))
for(t in 1:length(theta)){
  grm <- GRM(alpha_i = alpha,betas_ij = betas,th=theta[t])
  # cat_attr[,t] <- gpcm$
  f_ij[,t] <- grm$f_ij
  pr_y_ij[,t] <- grm$pr_y_ij
  I_ij[,t] <-grm$I_ij
  E_Y[t] <- grm$E_Y
  I_i[t] <- grm$I_i
}


cat_cols <- c('#82185f','#1c5b5a','#96da31','#239eb3',
              '#097b35','#ec4b24','#781486','#3f436d')
layout(1:2)
plot(NULL,xlim=c(-10,10),ylim=c(0,1))
for(cc in 1:n_cat){
  lines(theta,f_ij[cc,],col=cat_cols[cc],lwd=2)
}
lines(theta,E_Y/length(betas),lwd=2.5)
plot(NULL,xlim=c(-10,10),ylim=c(0,1))
for(cc in 1:n_cat){
  lines(theta,pr_y_ij[cc,],col=cat_cols[cc],lwd=2)
}
# lines(theta,E_Y/length(betas),lwd=2.5)
# plot(NULL,xlim=c(-10,10),ylim=c(-5,5))
# for(cc in 1:(length(betas)+1)){
#   lines(theta,I_ij[cc,],col=cat_cols[cc],lwd=2)
# }
# lines(theta,I_i,lwd=2.5)






# Simulation(s)

# Totals
n_items <- 20
n_categories <- 3
n_persons <- 100

# Person parameters
set.seed(123)
true_theta <- rnorm(n=n_persons)

# Item parameters
true_alpha <- rep(5,n_items)
true_beta <- array(dim=c(n_items,n_categories-1))
# base <- c(-1.5,-.5,.5,1.5) # Different ways to simulate betas
base <- c(-1,1) # Different ways to simulate betas
for(b in 1:n_items){
  true_beta[b,] <- base+rnorm(length(base),sd=0.5)
  # true_beta[b,] <- sort(rnorm(n=n_categories-1))
}

# Category parameters
j <- 0:(n_categories-1)

# Responses simulation
responses <- array(dim=c(n_persons,n_items))
for(p in 1:n_persons){
  for(i in 1:n_items){
    grm <- GRM(alpha_i = true_alpha[i],
                 betas_ij = true_beta[i,],
                 th = true_theta[p])
    responses[p,i] <- sample(j,size=1,prob = grm$f_ij)
  }
}

simulation <- list(responses=responses,
                   true_theta=true_theta,
                   true_alpha=true_alpha,
                   true_beta=true_beta,
                   j=j,n_persons=n_persons,
                   n_items=n_items,n_categories=n_categories,
                   model_description='GRM') # Still not sure how to pass the whole model here
setwd('~/Learning/Psychometrics/IRT simulation and recovery/')
save(simulation,file='simul_GRM.RData')





# Towards more plotting functions...

# Plotting observed responses
y <- simulation$responses
j <- simulation$j
n_categories <- length(j)
# y <- responses
# n_categories <- 3
# j <- 0:(n_categories-1)
n_persons <- nrow(y)
n_items <- ncol(y)
cat_colors <-  c('#edf8fb','#b3cde3','#8c96c6','#8856a7','#810f7c')[c(1,3,5)]
ord_pers <- order(apply(y,FUN=sum,MARGIN=1))
ord_items <- order(apply(y,FUN=sum,MARGIN=2))
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


