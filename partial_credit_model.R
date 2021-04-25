# Simulating Generalized Partial Credit Model
# Polytomous model for ordered responses
rm(list=ls())

# Model Definition
GPCM <- function(alpha_i,betas_ij,th){
  # Returns category probabilities, information, and other relevant
  # quantities per ITEM (detailed by CATEGORY), according to the 
  # Generalized Partial Credit Model.
  # 'alpha_i' is a scalar at ITEM level
  # 'beta_ij' is an array at CATEGORY level
  # 'th' is the value in the latent trait at which evaluate
  # performance of item i and categories ij
  
  # th <- 0
  # alpha_i <- 1
  # betas_ij <- seq(-2,2,length.out=3)
  n_cat <- length(betas_ij)+1 # Number of categories
  j <- 0:(n_cat-1) # Category values j=0,1,...,n_cat-1
  f_ij <- array(dim=n_cat) # Category characteristic curve
  I_ij <- array(dim=n_cat) # Category information
  category_attractions <- 1 # Category "attraction"
  
  for(b in 1:length(betas)){ # b indexes *positions* in array...
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
betas <- seq(-2,2,length.out=3)
alpha <- 1

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
lines(theta,e_y_th/length(betas),lwd=2.5)
plot(NULL,xlim=c(-10,10),ylim=c(0,0.5))
for(cc in 1:(length(betas)+1)){
  lines(theta,I_ij[cc,],col=cat_cols[cc],lwd=2)
}
lines(theta,I_i,lwd=2.5)




# Simulation(s)

# Totals
n_items <- 20
n_categories <- 5
n_persons <- 100

# Person parameters
set.seed(123)
true_theta <- rnorm(n=n_persons)

# Item parameters
true_beta <- array(dim=c(n_items,n_categories-1))
base <- c(-1.5,-.5,.5,1.5) # Different ways to simulate betas
for(b in 1:n_items){
  true_beta[b,] <- base+rnorm(length(base),sd=0.3)
  # true_beta[b,] <- sort(rnorm(n=n_categories-1))
}


# Plotting true unobservables
try(dev.off())
# x11()
plot(NULL,xlim=c(-3.5,3.5),ylim=c(0,n_items+1))
for(b in 1:n_items){
  points(true_beta[b,],rep(b,n_categories-1))
}
points(true_theta,rep(n_items+1,n_persons),col='red')




