# Simulating from the Generalized Partial Credit Model
# Polytomous model for ordered responses
rm(list=ls())

# Model Definition
GPCM <- function(alpha_i,betas_ij,th){
  # Returns response probabilities, category and item information, and other relevant
  # quantities per ITEM (detailed by CATEGORY), according to the 
  # Generalized Partial Credit Model.
  
  # 'alpha_i' is a scalar at ITEM level
  # 'beta_ij' is an array at CATEGORY level
  # 'th' is the value in the latent trait at which to evaluate
  # performance of item i and categories ij
  
  # th <- 0
  # alpha_i <- 1
  # betas_ij <- seq(-2,2,length.out=3)
  n_cat <- length(betas_ij)+1 # Number of categories
  j <- 0:(n_cat-1) # Category values j=0,1,...,n_cat-1
  f_ij <- array(dim=n_cat) # Category characteristic curve
  I_ij <- array(dim=n_cat) # Category information
  category_attractions <- 1 # Category "attraction"
  
  for(b in 1:length(betas_ij)){ # b indexes *positions* in array...
    # ...starting in second category, second position, which is j=1, etc.
    pos <- b+1
    category_attractions[pos] <- exp(alpha_i*(b*th-sum(betas_ij[1:b])))
  }
  
  f_ij <- category_attractions/sum(category_attractions)
  E_Y_th <- sum(j*f_ij) # Expected category
  I_ij <- (j-E_Y_th)^2*f_ij
  I_i <- sum(I_ij) # Item information
  
  return(list(j=j,f_ij=f_ij,I_ij=I_ij,
              attr=category_attractions,
              E_Y=E_Y_th,I_i=I_i))
}

GPCM(alpha_i = 2,betas_ij = seq(-2,2,length.out=3),th = 0)



# Towards some plotting functions...

theta <- seq(-10,10,.1)
# At item level...
betas <- seq(-2,2,length.out=2)
alpha <- 3

n_cat <- length(betas)+1
# theta <- 1
cat_attr <- array(dim=c(n_cat,length(theta)))
f_ij <- array(dim=dim(cat_attr))
I_ij <- array(dim=dim(cat_attr))
E_Y <- array(dim=length(theta))
I_i <- array(dim=length(theta))
for(t in 1:length(theta)){
  gpcm <- GPCM(alpha_i = alpha,betas_ij = betas,th=theta[t])
  # cat_attr[,t] <- gpcm$
  f_ij[,t] <- gpcm$f_ij
  I_ij[,t] <-gpcm$I_ij
  E_Y[t] <- gpcm$E_Y
  I_i[t] <- gpcm$I_i
}


cat_cols <- c('#82185f','#1c5b5a','#96da31','#239eb3',
              '#097b35','#ec4b24','#781486','#3f436d')
layout(1:2)
plot(NULL,xlim=c(-10,10),ylim=c(0,1))
for(cc in 1:n_cat){
  lines(theta,f_ij[cc,],col=cat_cols[cc],lwd=2)
}
lines(theta,E_Y/length(betas),lwd=2.5)
plot(NULL,xlim=c(-10,10),ylim=c(0,0.5))
for(cc in 1:(length(betas)+1)){
  lines(theta,I_ij[cc,],col=cat_cols[cc],lwd=2)
}
lines(theta,I_i,lwd=2.5)






# Simulation(s)

# Totals
n_items <- 20
n_categories <- 3
n_persons <- 100

# Person parameters
set.seed(123)
true_theta <- rnorm(n=n_persons)

# Item parameters
true_alpha <- rep(3,n_items)
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
    gpcm <- GPCM(alpha_i = true_alpha[i],
               betas_ij = true_beta[i,],
               th = true_theta[p])
    responses[p,i] <- sample(j,size=1,prob = gpcm$f_ij)
  }
}

simulation <- list(responses=responses,
                   true_theta=true_theta,
                   true_alpha=true_alpha,
                   true_beta=true_beta,
                   j=j,n_persons=n_persons,
                   n_items=n_items,n_categories=n_categories,
                   model_description='GPCM') # Still not sure how to pass the whole model here
setwd('~/Learning/Psychometrics/IRT simulation and recovery/')
save(simulation,file='simul_GPCM.RData')





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



# Plotting true unobservables
try(dev.off())
# x11()
plot(NULL,xlim=c(-3.5,3.5),ylim=c(0,n_items+1))
for(b in 1:n_items){
  points(true_beta[b,],rep(b,n_categories-1))
}
points(true_theta,rep(n_items+1,n_persons),col='red')







