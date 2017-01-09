rm(list=ls())
options(scipen = 999)
library(Sim.DiffProc)
library(sde)
library(parallel)
library(Rlab)
library(highfrequency)
set.seed(123)
d<-expression(0.05)
s<-expression(0.1)
#X=sde.sim(t0 = 0, T = 1/252, X0 = 0, N = 77, M=5000, delta=((1/252)/77), drift=d, sigma=s,method = c("euler"))
Y=sde.sim(t0 = 0, T = 1/252, X0 = 0, N = 389, M=5000, delta=((1/252)/389), drift=d, sigma=s,method = c("euler"))
#Z=sde.sim(t0 = 0, T = 1/252, X0 = 0, N = 4679, M=5000, delta=((1/252)/4679), drift=d, sigma=s,method = c("euler"))



#z_rv
T <- 1/252
N <- 390
delta<-T/N
n <- sqrt(delta)
sigma<-0.1
IV <- (sigma^2)*T
m2=mean(abs(rnorm(5000, mean = 0, sd=1))^2)
m4=mean(abs(rnorm(5000, mean = 0, sd=1))^4)

B2 <- NULL
for (i in 1:5000){
  B2[i] = sum(diff(Y[,i])^2)
}
B2[i] = (1/m2)*(B2[i])

B4 <- NULL
for (i in 1:5000){
  B4[i] = sum(diff(Y[,i])^4)
}
B4 <- (B4)/(m4*delta)

Z_rv <- NULL
for (i in 1:5000){
  Z_rv[i] = (B2[i] - IV)/(n*sqrt(2*B4[i]))
}

#z_bpv
p=1
q=1
m1= mean(abs(rnorm(5000, mean = 0, sd=1)))
C11=(1/(m1^2))*sqrt((m2^2)+(2*m1^2*m2)-(3*m1^4))
C11pi=(4/pi^2)*sqrt(pi^2+4*pi-12)

B11 <- NULL

for (i in 1:5000){
  B11[i] <- sum(abs(diff(Y[1:389,i])/n)*abs(diff(Y[2:390,i])/n))
}
B11<- B11*(delta/(m1^2))

B1111<-NULL
for (i in 1:5000){
  B1111[i] <- sum(abs(diff(Y[1:387,i])/n)*abs(diff(Y[2:388,i])/n)*abs(diff(Y[3:389,i])/n)*abs(diff(Y[4:390,i])/n))
}
B1111 <- B1111*(delta/(m1^4))

Z_bpv<-NULL
for (i in 1:5000){
  Z_bpv[i] <- (B11[i]- IV)/(n*sqrt(C11pi*C11pi*B1111[i]))
}

plot(density(Z_rv))
qqnorm(Z_rv, xlab = "Theoretical Quantiles", ylab = "Z_rv")
qqline(Z_rv)
plot(density(Z_bpv))
qqnorm(Z_bpv, xlab = "Theoretical Quantiles", ylab = "Z_bpv")
qqline(Z_bpv)


#Problem 2 
mu=0.05
kappa=5
theta=0.1
eta=0.5
rho=-0.8

t <- seq(0, 1/252, by = ((1/252)/389))
sigma=matrix(data=0, nrow=390, ncol=5000)
dsigma=matrix(data=0, nrow=390, ncol=5000)
dX=dsigma
RES=NULL

RES=lapply(1:5000, function(k){
  P=rep(0,389)
  dB <- BM(x = 0, t0 = 0, T = 1/252, N = 390)
  dW <- BM(x = 0, t0 = 0, T = 1/252, N = 390)
  for (l in 1:length(P)){
    dsigma[l+1,k]=kappa*(theta-sigma[l])*diff(t)[l] + eta*sigma[l]*(rho*dB[l]+sqrt(1-(rho^2))*dW[l])
  }
  for (j in 1:length(P)){
    dX[j+1,k]=mu*diff(t)[j] + sqrt(dsigma[,k])[j]*dB[j] 
  }
  res1 <- list(d1=dX[,k], d11=dsigma[,k])
  return(res1)
}
)

X2 <- matrix(0, nrow = 390, ncol = 5000)
sigma2 <- X2

for(i in 1:5000){
  X2[,i] <- RES[[i]]$d1
  sigma2[,i] <- RES[[i]]$d11
}

#z_rv2

IV2=NULL
for(i in 1:5000){
  IV2[i]=sum(sigma2[,i])*delta
}

B22 <- NULL
for (i in 1:5000){
  B22[i] = sum(diff(X2[,i])^2)
}
B22[i] = (1/m2)*(B22[i])

B44 <- NULL
for (i in 1:5000){
  B44[i] = sum(diff(X2[,i])^4)
}
B44 <- (B44)/(m4*delta)

Z_rv2 <- NULL
for (i in 1:5000){
  Z_rv2[i] = (B22[i] - IV2)/(n*sqrt(2*B44[i]))
}

#z_bpv2

B11_2 <- NULL

for (i in 1:5000){
  B11_2[i] <- sum(abs(diff(X2[1:389,i])/n)*abs(diff(X2[2:390,i])/n))
}
B11_2<- B11_2*(delta/(m1^2))

B1111_2<-NULL
for (i in 1:5000){
  B1111_2[i] <- sum(abs(diff(X2[1:387,i])/n)*abs(diff(X2[2:388,i])/n)*abs(diff(X2[3:389,i])/n)*abs(diff(X2[4:390,i])/n))
}
B1111_2 <- B1111_2*(delta/(m1^4))

Z_bpv2<-NULL
for (i in 1:5000){
  Z_bpv2[i] <- (B11_2[i]- IV2)/(n*sqrt(C11pi*C11pi*B1111_2[i]))
}

plot(density(Z_rv2))
qqnorm(Z_rv2, xlab = "Theoretical Quantiles", ylab = "Z_rv2")
qqline(Z_rv2)
plot(density(Z_bpv2))
qqnorm(Z_bpv2, xlab = "Theoretical Quantiles", ylab = "Z_bpv2")
qqline(Z_bpv2)

#Problem 3

mu <- 0.05
kappa <- 5
theta <- 0.1
eta <- 0.5
rho <- -0.8

T <- 1/252
N <- 390
delta<-T/N
n <- sqrt(T/N)
t <- seq(0, 1/252, by = ((1/252)/389))

X3 <- matrix(data = 0, nrow = 390, ncol = 5000)
sigma3 <- X3
ni <- X3

res3 <- NULL

res3 <- lapply(1:5000,  function(k){
  P <- rep(0, 389)
  sig3<-rep(0.01,389)
  B <- sqrt(1/(252*390))*rnorm(390, mean = 0, sd = 1)
  W <- sqrt(1/(252*390))*rnorm(390, mean = 0, sd = 1)
  N <- rexp(390, rate = 3000)
  for(l in 1:length(sig3)){
    sigma3[l+1,k] <- (sig3[l] + kappa*(theta - sig3[l])*diff(t)[l] + 
                        eta*sqrt(sig3[l])*(rho*B[l] +sqrt(1-rho^2)*W[l]))
  }
  S <- NULL
  t1 <- rexp(390, rate = 3000)
  ret <- NULL
  for(i in 1:390){
    t1[i+1] <- t1[i]+t1[i+1]
    S[i]<- ifelse(t1[i] <= 1/252, t1[i] , 0)
    if(S[i] > 0){
      ret[i] <- S[i]
    }
  }
  int <- NULL
  for(j in 1:length(ret)){
    int[j] <- max(which(ret[j] > t))
    
  }
  int <- unique(int)
  bern <- rbern(length(int), 0.6)
  
  for(i in 1:length(int)){
    ni[int[i],k] <- ifelse(bern[i] == 1,  (-1*0.06*rexp(1, rate = 0.0018)),
                           (0.04*rexp(1, rate = 0.008)))
  }
  
  for(d in 1:length(P)){
    X3[d+1,k] <- mu*diff(t)[d] + ni[,k][d]*diff(N)[d] +
      sqrt(sigma3[,k])[d]*B[d]
  }
  obj <- list(d3=X3[,k], d33 =sigma3[,k], dn = ni[,k])
  return(obj)
}
)

sim_X3 <- matrix(0, nrow = 390, ncol = 5000)
sim_sigma3 <- sim_X3
sim_ni <- sim_X3

for(i in 1:5000){
  sim_X3[,i] <- res3[[i]]$d3
  sim_sigma3[,i] <- res3[[i]]$d33
  sim_ni[,i] <- res3[[i]]$dn
  
}

#z_rv3

IV3=NULL
for(i in 1:5000){
  IV3[i]=sum(sim_sigma3[,i])*delta
}

B23 <- NULL
for (i in 1:5000){
  B23[i] = sum(diff(sim_X3[,i])^2)
}
B23[i] = (1/m2)*(B23[i])

B43 <- NULL
for (i in 1:5000){
  B43[i] = sum(diff(sim_X3[,i])^4)
}
B43 <- (B43)/(m4*delta)

Z_rv3 <- NULL
for (i in 1:5000){
  Z_rv3[i] = (B23[i] - IV3)/(n*sqrt(2*B43[i]))
}

#z_bpv3

B11_3 <- NULL

for (i in 1:5000){
  B11_3[i] <- sum(abs(diff(sim_X3[1:389,i])/n)*abs(diff(sim_X3[2:390,i])/n))
}
B11_3<- B11_3*(delta/(m1^2))

B1111_3<-NULL
for (i in 1:5000){
  B1111_3[i] <- sum(abs(diff(sim_X3[1:387,i])/n)*abs(diff(sim_X3[2:388,i])/n)*abs(diff(sim_X3[3:389,i])/n)*abs(diff(sim_X3[4:390,i])/n))
}
B1111_3 <- B1111_3*(delta/(m1^4))

Z_bpv3<-NULL
for (i in 1:5000){
  Z_bpv3[i] <- (B11_3[i]- IV3)/(n*sqrt(C11pi*C11pi*B1111_3[i]))
}

# Estimating  using alpha iterations


RETA <- unlist(lapply(1:ncol(sim_X3), function(k){
  IVT <- B11_3
  IVT <- matrix(IVT, nrow = 50, ncol = 5000, byrow = T)
  alpha <- unique(4*((252*IVT[,k])^0.5)*(389/252)^0.49)
  obj <- ifelse(abs(diff(sim_X3[,k])) <= alpha,
                diff(sim_X3[,k]), 0)
  for(i in 1:49){
    IVT[i+1,k] <- sum(obj^2)
    if(abs(IVT[i+1,k] - IVT[i,k]) > 0.05*IVT[i,k])
    {
      alpha <- 4*((252*IVT[i,k])^0.5)*(389/252)^0.49
      obj <- ifelse(abs(diff(sim_X3[,k])) <= alpha,
                    diff(sim_X3[,k]), 0)
    }else{
      return(alpha)
    }
  }
}
)
)
B2_alpha <- unlist(lapply(1:length(RETA), function(x){
  ret <- ifelse(abs(diff(sim_X3[,x])) <= RETA[x],
                diff(sim_X3[,x]), 0)
  ret <- sum(ret^2)
}
)
)


B4_alpha <- unlist(lapply(1:length(RETA), function(x){
  ret <- ifelse(abs(diff(sim_X3[,x])) <= RETA[x],
                diff(sim_X3[,x]), 0)
  ret <- sum(ret^4)
}
)
)

B4_alpha  <- (B4_alpha*389*252)/3




JV_hat <- unlist(lapply(1:length(RETA), function(x){
  ret <- ifelse(RETA[x] < abs(diff(sim_X3[,x])),
                diff(sim_X3[,x]), 0)
  ret <- sum(ret^2)
}
)
)

r_d_n <- sqrt(1/(252*389))
Z_trv <- NULL
for(i in 1:5000){
  Z_trv[i] <- (B2_alpha[i] - IV3[i])/(r_d_n*sqrt(2*B4_alpha[i]))
}

plot(density(Z_rv3))
qqnorm(Z_rv3, xlab = "Theoretical Quantiles", ylab = "Z_rv3")
qqline(Z_rv3)
plot(density(Z_bpv3))
qqnorm(Z_bpv3, xlab = "Theoretical Quantiles", ylab = "Z_bpv3")
qqline(Z_bpv3)
plot(density(Z_trv))
qqnorm(Z_trv, xlab = "Theoretical Quantiles", ylab = "Z_trv")
qqline(Z_trv)

nzero <- NULL
for(i in 1:ncol(sim_ni)){
  nzero[i] <- length(which(sim_ni[,i] != 0))
}

smean <- mean(nzero) #sample mean of number of jumps

svar <- var(nzero) #sample variance of number of jumps

JV <- NULL
for (i in 1:5000){
  JV[i] = sum((sim_ni[,i])^2)
}

JV_est= (1/n)*(JV_hat-JV)

qqnorm(JV_est, xlab = "Theoretical Quantiles", ylab = "JV_est")
qqline(JV_est)












