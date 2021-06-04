setwd('/Users/sarahalkhateeb/Library/Mobile Documents/com~apple~CloudDocs/My files/My files/All/Gothenburg University/Second year/Period 1/Stochastic Optimization Algorithms/Assignments/A4')

#########################################################################################
### Q1

#load a package
library('LearnBayes')
library('mnormt')

## part (a)
# load the "stockvalues" dataset
stock_values <- as.matrix(read.csv("stockvalues.txt"))

log_stock <- log(stock_values) #logged prices

log_stock1 <- log_stock[-1,] # delete the first row from the log_stock data
#new_row <- tail(log_stock, n=1)
log_stock2 <- log_stock[-1006,] # delete the last row from the log_stock data

percent_change <- log_stock1 - log_stock2 #subtract the previous day data from current day data
#calculate the expectation and covariance matrix from logged data
#summary(log_stock)
colmean <- colMeans(percent_change) #find the mean in each column 
mean <- mean(colmean) # the mean of the log prices
#mean

x = percent_change
cov_matrix <- cov(x) # the covariance matrix of log prices 
#Generate a sample of V1,......V1000
n_days = 100
n_samples = 1000
Vq <- rmnorm(n_samples, n_days*colmean, n_days*cov_matrix)
#dimnames(Vq) <- NULL

# part (b)
## 1) each stock receives equal investment, w1=w2....=w7 = 1/7 since all weights add up to 1
w = cbind(1/7,1/7,1/7,1/7,1/7,1/7,1/7)
m = 7
k1 <- -0.5
k2 <- 0.5
k3 <- 1.5

#vq <- as.matrix(Vq)
#Vq <- matrix(1,2,7)
#the expected utility for k1
a2 <- 0.0
for (q in 1:n_samples){
  a <- 0.0
  for (i in 1:m){
    tmp <- (w[i]*exp(Vq[q,i]))
    a <- a + tmp
  }
  tmp <- 1 - (a^-k1)
  tmp <- tmp / k1

  a2 <- a2 + tmp
}
output1 <- a2 / n_samples


#the expected utility for k2
b2 = 0.0
for (q in 1:n_samples){
  b <- 0.0
  for (i in 1:m){
    tmp <- (w[i]*exp(Vq[q,i]))
    b <- b + tmp
  }
  tmp <- 1 - (b^-k2)
  tmp <- tmp / k2
  
  b2 <- b2 + tmp
}
output2 = b2 / n_samples


#the expected utility for k3
c2 <- 0.0
for (q in 1:n_samples){
  c <- 0.0
  for (i in 1:m){
    tmp <- (w[i]*exp(Vq[q,i]))
    c <- c + tmp
  }
  tmp <- 1 - (c^-k3)
  tmp <- tmp / k3
  
  c2 <- c2 + tmp
}
output3 = c2 / n_samples

as.double(output1) #print the output as number
as.double(output2)
as.double(output3)

## 2) The stock with the best expected performance receives all the investment

best_stock <- max(colmean)
best_stock #S4 has the highest mean so we can say it has the best performance

sw <- 1
#the expected utility for S4 with the 3 k's
d = e = f = 0
for (i in (1:1000)){
    d = d + ((1-((sw*exp(Vq[i,4]))^-k1)) / k1)
    e = e + ((1-((sw*exp(Vq[i,4]))^-k2)) / k2)
    f = f + ((1-((sw*exp(Vq[i,4]))^-k3)) / k3)}

u4_k1 <- as.double(d/n_samples)
u4_k2 <- as.double(e/n_samples)
u4_k3 <- as.double(f/n_samples)
#print results in a nice way :)
cat('S4_k1:', u4_k1, '\nS4_k2:', u4_k2, '\nS4_k3:', u4_k3)


# part (c)
library('Rcpp')
library('optimization')
library('ggplot2')

#convert percent_change data to a dataframe
pc <- as.data.frame(percent_change)

#use the optimize function to find optimal weights for S3 and S4
#first we define functions for both k's and for both stocks x1= pc$S3 and x2=pc$S4, 
#w is the weight and since weights add up to 1 if S3 has w then S4 will have 1-w
u_stock1 <- function(w, x1, x2) {
  sum(((1-(((w*exp(x1)) + (((1-w)*exp(x2))))^0.5)) / -0.5)) / n_samples
  }
opt_w1 <-  optimize(u_stock1, lower = 0, upper = 1, x1= pc$S3, x2=pc$S4, maximum = TRUE)

u_stock2 <- function(w, x1, x2) {
  sum(((1-(((w*exp(x1)) + (((1-w)*exp(x2))))^-1.5)) / 1.5)) / n_samples
  }
opt_w2 <- optimize(u_stock2, lower = 0, upper = 1, x1= pc$S3, x2=pc$S4, maximum = TRUE)

opt_w1
opt_w2

# part (d)
#Written question

#########################################################################################3
### Q2

# load the "stockvalues" dataset

mature_Knee <- read.table("matureKnee.txt")
#add y of 1 to the data as a column and add it to the data
mm <- rep(1, 50)
mk <- cbind(mature_Knee, mm)
#change column name to match the column name for the immatureKnee.txt below
colnames(mk)[2] <- "m"

immature_Knee <- read.table("immatureKnee.txt")
#add y of 0 to the data as a column and add it to the data
im <- rep(0, 50)
imk <- cbind(immature_Knee, im)
#change column name to match the column name for the matureKnee.txt above
colnames(imk)[2] <- "m"

#concatenate both data into one dataframe
all_ages <- rbind(imk,mk)
attach(all_ages)

#mean age for each report
m_mean <- mean(as.matrix(read.csv(('matureKnee.txt'))))
im_mean <- mean(as.matrix(read.csv(('immatureKnee.txt'))))
#print means in each data
cat('Mature_knee mean age:', m_mean, '\nImmature_knee mean age:', im_mean)

# part (a)
#define start points for a and b to use in optim function
betas <- c(0, 0)
#define a function for the NLL to minimize
loglike <- function(par,x,y) {
  a <- par[1]
  b <- par[2]
  # Define the logistic function 
  logit <- function(x,a,b) {
    exp(a+b*(x-18))/(1 + exp(a+b*(x-18)))
  }
  p <- logit(x,a,b)
  - sum(y*log(p) + (1-y)*log(1-p))
}
#find the optimal a and b using optim function
MLE <- optim(betas, loglike, x = all_ages$V1, y = all_ages$m)
MLE$par #print the optimal values of a and b

x <- all_ages$V1 #the age values 
y <- all_ages$m # the 0,1 values

best_a <- MLE$par[1] #optimal a value
best_b <- MLE$par[2] #optimal b value

#fit logistic regression to the data with optimal a and b and plot
p <- ggplot(data=all_ages, aes(V1, m)) 
scurve <- function(x){
  y <- exp(best_a+best_b*(x-18))/(1 + exp(best_a+best_b*(x-18)))
  return(y)
}
p + stat_function(fun = scurve, n = 100) + xlab('Age') + 
  ylab('Response') + geom_point(color="blue")



# part (b)
#define seq for a and b equally distanced by 21
a <- seq(from = -0.5, to = 2, length.out = 21)
b <- seq(from = 0.5, to = 3, length.out = 21)

#define prior uniform with prob = 1/441
uniform <- matrix(1/441, nrow=21, ncol = 21) 

prob <- matrix(0, nrow=21, ncol = 21) #21*21 matrix to store the likelihood function results
#find likelihood values for each (a,b) pair
for (i in (1:21)){
  for (k in (1:21)){
    p <- 1
    for (j in (1:100)){
      p <- p * (((exp(a[i] + b[k]*(x[j]-18)) / (1 + (exp(a[i] + b[k]*(x[j]-18)))))^y[j]) * 
                  ((1 - (exp(a[i] + b[k]*(x[j]-18)) / (1 + (exp(a[i] + b[k]*(x[j]-18))))))^(1-y[j])))
    }
    prob[k, i] <- p
  }
}

#find posterior by multiplying prior and likelihood matrices
pp <- uniform * prob 
pp <- pp / sum(pp)

#plot the posterior
image(a, b, pp)


# part (c)
#age distributions 
mu_ <- cbind(18.5, 19.5, 20.58)
alpha_ <- cbind(3, 6, 3)


#plot(x, dgamma(x=x-14, shape = alpha_[1], rate = alpha_[1]/(mu_[1]-14)), 
 #    ylab = "Gamma density", xlab = 'Age')
#plot(x, dgamma(x=x-14, shape = alpha_[2], rate = alpha_[2]/(mu_[2]-14)), 
 #    ylab = "Gamma density", xlab = 'Age')
#plot(x, dgamma(x=x-14, shape = alpha_[3], rate = alpha_[3]/(mu_[3]-14)), 
 #    ylab = "Gamma density", xlab = 'Age')

#plot age distributions 
plot(0, 0, xlim = c(min(x), max(x)), ylim = c(0, 0.22), type = "n", 
     ylab = "Gamma density", xlab = 'Age')
for(i in 1:length(mu_))
  curve(dgamma(x=x-14, shape = alpha_[i], rate = alpha_[i]/(mu_[i]-14)), 
        from = min(x), to = max(x), col = i, add = TRUE)

legend(21, 0.21, legend=c("Mu=18.5, alpha=3", "Mu=19.5, alpha=6", "Mu=20.58, alpha=3"),
       col=c("green", "black", "red"), lty=1:2, cex=0.7)


#for classification children
#gamma density function
agedens <- function(x, alpha, mu) {dgamma(x=x-14, shape=alpha, rate=alpha/(mu-14))}
#cost function
c1 <- function(x) {if (x<=18) {10} else {1}}
c2 <- function(x) {if (x<=18) {10*(18-x)} else {(x-18)}}

#Probability function
f1 <- function(x, a, b) {(1 / (1 + exp(-a-b*(x-18))))}

#using c1
integrate_me <- function(x) {agedens(x, alpha, mu) * f1(x, a, b) * c1(x)}
integrate_me <- Vectorize(integrate_me, "x")
# Define alpha1, mu1, a and b
mus <- cbind(18.5, 19.5, 20.58); alphas <- cbind(3, 6, 3)
mu <- mus[1]; alpha <- alphas[1]; a <- best_a; b <- best_b
int_c1<- integrate(integrate_me, 18, Inf)
# Define alpha2, mu2, a and b
mu <- mus[2]; alpha <- alphas[2]; a <- best_a; b <- best_b
int_c2<- integrate(integrate_me, 18, Inf)
# Define alpha3, mu3, a and b
mu <- mus[3]; alpha <- alphas[3]; a <- best_a; b <- best_b
int_c3<- integrate(integrate_me, 18, Inf)

cls_child1 = int_c1[[1]]; cls_child2 = int_c2[[1]]; cls_child3 = int_c3[[1]]

#using adults c1
integrate_m <- function(x) {agedens(x, alpha, mu) * f1(x, a, b) * c1(x)}
integrate_m <- Vectorize(integrate_m, "x")
# Define alpha1, mu1, a and b
mu <- mus[1]; alpha <- alphas[1]; a <- best_a; b <- best_b
int_a1<- integrate(integrate_m, 0, 18)
# Define alpha2, mu2, a and b
mu <- mus[2]; alpha <- alphas[2]; a <- best_a; b <- best_b
int_a2<- integrate(integrate_m, 0, 18)
# Define alpha3, mu3, a and b
mu <- mus[3]; alpha <- alphas[3]; a <- best_a; b <- best_b
int_a3<- integrate(integrate_m, 0, 18)

cls_adult1 = int_a1[[1]];cls_adult2 = int_a2[[1]];cls_adult3 = int_a3[[1]]

#compare results using c1 cost

cat("Using c1:",
  "\nClassify as adult for mu = 18.5?", cls_child1 > cls_adult1, 
    "\nClassify as adult for mu = 19.5?", cls_child1 > cls_adult1,
    "\nClassify as adult for mu = 20.58?", cls_child1 > cls_adult1)

#using children c2
integrate_me <- function(x) {agedens(x, alpha, mu) * f1(x, a, b) * c2(x)}
integrate_me <- Vectorize(integrate_me, "x")
# Define alpha1, mu1, a and b
mu <- mus[1]; alpha <- alphas[1]; a <- best_a; b <- best_b
int_c11<- integrate(integrate_me, 18, Inf)
# Define alpha2, mu2, a and b
mu <- mus[2]; alpha <- alphas[2]; a <- best_a; b <- best_b
int_c22<- integrate(integrate_me, 18, Inf)
# Define alpha3, mu3, a and b
mu <- mus[3]; alpha <- alphas[3]; a <- best_a; b <- best_b
int_c33<- integrate(integrate_me, 18, Inf)

cls_child11 = int_c11[[1]]; cls_child22 = int_c22[[1]]; cls_child33 = int_c33[[1]]

#using c2 adults 
integratee <- function(x) {agedens(x, alpha, mu) * f1(x, a, b) * c2(x)}
integratee <- Vectorize(integratee, "x")
# Define alpha1, mu1, a and b
mu <- mus[1]; alpha <- alphas[1]; a <- best_a; b <- best_b
int_a11<- integrate(integratee, 0, 18)
# Define alpha2, mu2, a and b
mu <- mus[2]; alpha <- alphas[2]; a <- best_a; b <- best_b
int_a22<- integrate(integratee, 0, 18)
# Define alpha3, mu3, a and b
mu <- mus[3]; alpha <- alphas[3]; a <- best_a; b <- best_b
int_a33<- integrate(integratee, 0, 18)

cls_adult11 = int_a11[[1]]; cls_adult22 = int_a22[[1]]; cls_adult33 = int_a33[[1]]

#compare results using c1 cost

cat("Using c2:",
    "\nClassify as adult for mu = 18.5?", cls_child11 > cls_adult11, 
    "\nClassify as adult for mu = 19.5?", cls_child22 > cls_adult22,
    "\nClassify as adult for mu = 20.58?", cls_child33 > cls_adult33)



# part (d) (similar to part c but we average over the posterior distribution)

a_seq <- seq(from = -0.5, to = 2, length.out = 21)
b_seq <- seq(from = 0.5, to = 3, length.out = 21)

mus <- cbind(18.5, 19.5, 20.58); alphas <- cbind(3, 6, 3)
##children with c1
integrate_me <- function(x) {agedens(x, alpha, mu) * f1(x, a, b) * c1(x)}
integrate_me <- Vectorize(integrate_me, "x")
c1_seq = matrix(, nrow = 21, ncol = 21)
c2_seq = matrix(, nrow = 21, ncol = 21)
c3_seq = matrix(, nrow = 21, ncol = 21)
#here we loop through all a and b values to consider uncertainty
for(i in 1:length(a_seq)){
  for(j in 1:length(b_seq)){
    # Define alpha1, mu1, a and b
    a_val = a_seq[i]
    b_val = b_seq[j]
    mu <- mus[1]; alpha <- alphas[1]; a <- a_val; b <- b_val
    int_c1<- integrate(integrate_me, 18, Inf)
    c1_seq[j,i] <- int_c1[[1]]
    # Define alpha2, mu2, a and b
    mu <- mus[2]; alpha <- alphas[2]; a <- a_val; b <- b_val
    int_c2<- integrate(integrate_me, 18, Inf)
    c2_seq[j,i] <- int_c2[[1]]
    # Define alpha3, mu3, a and b
    mu <- mus[3]; alpha <- alphas[3]; a <- a_val; b <- b_val
    int_c3<- integrate(integrate_me, 18, Inf)
    c3_seq[j,i] <- int_c3[[1]]
  }
}
##adults with c1
integratee <- function(x) {agedens(x, alpha, mu) * f1(x, a, b) * c1(x)}
integratee <- Vectorize(integratee, "x")
a1_seq = matrix(, nrow = 21, ncol = 21)
a2_seq = matrix(, nrow = 21, ncol = 21)
a3_seq = matrix(, nrow = 21, ncol = 21)
for(i in 1:length(a_seq)){
  for(j in 1:length(b_seq)){
    # Define alpha1, mu1, a and b
    a_val = a_seq[i]
    b_val = b_seq[j]
    mu <- mus[1]; alpha <- alphas[1]; a <- a_val; b <- b_val
    int_c1<- integrate(integratee, 0, 18)
    a1_seq[j,i] <- int_c1[[1]]
    # Define alpha2, mu2, a and b
    mu <- mus[2]; alpha <- alphas[2]; a <- a_val; b <- b_val
    int_c2<- integrate(integratee, 0, 18)
    a2_seq[j,i] <- int_c2[[1]]
    # Define alpha3, mu3, a and b
    mu <- mus[3]; alpha <- alphas[3]; a <- a_val; b <- b_val
    int_c3<- integrate(integratee, 0, 18)
    a3_seq[j,i] <- int_c3[[1]]
  }
}
c1_children1 = mean(c1_seq * pp); c1_children2 = mean(c2_seq * pp); c1_children3  = mean(c3_seq * pp)

c1_adults1  = mean(a1_seq * pp); c1_adults2 = mean(a2_seq * pp);c1_adults3 = mean(a3_seq * pp)

#compare results with c1

cat("Using c1:",
    "\nClassify as adult for mu = 18.5?", c1_children1 > c1_adults1, 
    "\nClassify as adult for mu = 19.5?", c1_children2 > c1_adults2,
    "\nClassify as adult for mu = 20.58?", c1_children3 > c1_adults3)


#children with c2
integrate_me2 <- function(x) {agedens(x, alpha, mu) * f1(x, a, b) * c2(x)}
integrate_me2 <- Vectorize(integrate_me2, "x")
c12_seq = matrix(, nrow = 21, ncol = 21)
c22_seq = matrix(, nrow = 21, ncol = 21)
c32_seq = matrix(, nrow = 21, ncol = 21)
for(i in 1:length(a_seq)){
  for(j in 1:length(b_seq)){
    # Define alpha1, mu1, a and b
    a_val = a_seq[i]
    b_val = b_seq[j]
    mu <- mus[1]; alpha <- alphas[1]; a <- a_val; b <- b_val
    int_c1<- integrate(integrate_me2, 18, Inf)
    c12_seq[j,i] <- int_c1[[1]]
    # Define alpha2, mu2, a and b
    mu <- mus[2]; alpha <- alphas[2]; a <- a_val; b <- b_val
    int_c2<- integrate(integrate_me2, 18, Inf)
    c22_seq[j,i] <- int_c2[[1]]
    # Define alpha3, mu3, a and b
    mu <- mus[3]; alpha <- alphas[3]; a <- a_val; b <- b_val
    int_c3<- integrate(integrate_me2, 18, Inf)
    c32_seq[j,i] <- int_c3[[1]]
  }
}

#adults with c2
integratee1 <- function(x) {agedens(x, alpha, mu) * f1(x, a, b) * c2(x)}
integratee1 <- Vectorize(integratee1, "x")
a12_seq = matrix(, nrow = 21, ncol = 21)
a22_seq = matrix(, nrow = 21, ncol = 21)
a32_seq = matrix(, nrow = 21, ncol = 21)
for(i in 1:length(a_seq)){
  for(j in 1:length(b_seq)){
    # Define alpha1, mu1, a and b
    a_val = a_seq[i]
    b_val = b_seq[j]
    mu <- mus[1]; alpha <- alphas[1]; a <- a_val; b <- b_val
    int_c1<- integrate(integratee1, 0, 18)
    a12_seq[j,i] <- int_c1[[1]]
    # Define alpha2, mu2, a and b
    mu <- mus[2]; alpha <- alphas[2]; a <- a_val; b <- b_val
    int_c2<- integrate(integratee1, 0, 18)
    a22_seq[j,i] <- int_c2[[1]]
    # Define alpha3, mu3, a and b
    mu <- mus[3]; alpha <- alphas[3]; a <- a_val; b <- b_val
    int_c3<- integrate(integratee1, 0, 18)
    a32_seq[j,i] <- int_c3[[1]]
  }
}
c2_children1 = mean(c12_seq * pp); c2_children2 = mean(c22_seq * pp); c2_children3 = mean(c32_seq * pp)

c2_adults1 = mean(a12_seq * pp); c2_adults2 = mean(a22_seq * pp); c2_adults3= mean(a32_seq * pp)

#compare results with c2

cat("Using c2:",
    "\nClassify as adult for mu = 18.5?", c2_children1 > c2_adults1, 
    "\nClassify as adult for mu = 19.5?", c2_children2 > c2_adults2,
    "\nClassify as adult for mu = 20.58?", c2_children3 > c2_adults3)


