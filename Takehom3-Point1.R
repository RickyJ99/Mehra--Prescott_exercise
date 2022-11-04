# THIRD MEAP TAKEHOME - POINT 1:
# Author: Federico Vicentini
# Date: 04/11/2022

# Clear the variables
rm(list = ls())

country=c("A","B","C","D","E")
mu=c(0.023,0.015,0.030,0.018,0.025)
eta=c(0.041,0.030,0.050,0.036,0.040)
rho=c(0.2,0.9,-0.2,0.0,-0.9)
data=data.frame(country,mu,eta,rho)

expval=c(NA,NA,NA,NA,NA)
pi=matrix(NA,2,2)
for(i in 1:length(country)){
  expval[i]=(rho[i]*eta[i]^2)+mu[i]^2
}

expval[1]=rho[1]*eta[1]^2+mu[1]^2
expval

h=c(1+mu+eta)
l=c(1+mu-eta)

phi=c(NA,NA,NA,NA,NA)

for(i in 1:length(country)){
  phi[i]=(expval[i]-4*(h[i]-1)*(l[i]-1))/((h[i]-1)^2+(l[i]-1)^2-4*(h[i]-1)*(l[i]-1))
}

phi

for(i in 1:length(country)){
  pi[1,]=c(phi[i], (1-phi[i]))
  pi[2,]=c((1-phi[i]), phi[i])
  print(country[i])
  print(pi)
}

qh=c(NA,NA,NA,NA,NA)
ql=c(NA,NA,NA,NA,NA)
beta=0.96
gamma=2


for(i in 1:length(country)){
  qh[i]=beta*(((phi[i]*(h[i])^(-1*gamma))+((1-phi[i])*(l[i])^(-1*gamma))))
  ql[i]=beta*(((phi[i]*(l[i])^(-1*gamma))+((1-phi[i])*(h[i])^(-1*gamma))))
}

rfh=c((1/qh)-1)
rfl=c((1/ql)-1)









