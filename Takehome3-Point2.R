# THIRD MEAP TAKEHOME - POINT 2:
# Author: Federico Vicentini
# Date: 03/11/2022

# beta=0.96
# gamma=2
# phi=0.43
# h=1.054
# l=0.982
# epsilon=0.012
# m=rnorm(1, mean=1, sd=0.5)


riskpremium=7.44
riskfree=1.05
x=rnorm(100,1,0.5)
y=rnorm(100,4,3)
dummy=y
#count=0
for(t in 1:length(dummy)){
  if(dummy[t]>4){
    dummy[t]=1
    #count=count+1
  }
  else{
    dummy[t]=0
  }
}

value=abs(1/(1-x))*dummy
plot(value)
