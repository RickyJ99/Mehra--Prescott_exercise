library(matlib)
options(digits = 15) 

v <- function(dummy, x) {
  # value function: given the risk premium rate = 'dummy' and
  # 'x' = risk free rate as input return a value which is higher
  # when x -> 1 and  dummy > 4

  for (t in seq_along(dummy)) {
    if (dummy[t] > 4) {
      dummy[t]  <-  2
    }
    else {
      dummy[t]  <-  1
    }
  }
  value <-  abs(1 / (1 - x)) * dummy
  return(value)
}

riskComputation <- function(m, param){
    #the risk computation function take m as input and
    #compute the risk free / premium rate and recall
    #the value function with such values.
    
    #parameters 
    
    phi     <-   param[4]
    beta    <- 0.96
    gamma   <- param[3]
    h       <- param[1]
    l       <- param[2]
    epsilon <- 0.012

    

    #Generate pi Matrix
    valuesPi        <-   c(phi, epsilon, 1 - phi - epsilon, 1 / 2,
     0, 1 / 2, 1 - phi - epsilon, epsilon, phi)
    Pi              <-   matrix(valuesPi, byrow = TRUE, nrow = 3, ncol = 3)
    Pi

    #a vector that contains the three states of the system
    values_states   <-   c(h,m,l)

    #ouput vector
    out <-  c()

    #stationary prob.
    st_prob <- c(0.5, epsilon, 0.5) * (1 + epsilon)
    st_prob

    #Computing the zero-cupon bond 
    q   <-  beta * (Pi %*% (values_states ^- gamma))
    q

    #compute risk-free rate vector
    risk_free   <-   (1 / (q)) - 1

    #compute mean risk free rate using stationary prob. as a weight
    rf <- risk_free[, 1] %*% st_prob
    as.numeric(rf)

    #risky_rate
    values_matrix <- diag(3) * (values_states^(1 - gamma))

    #Computing Pi star
    Pi_star <-  Pi %*% values_matrix
    Pi_star

    I       <- diag(3) #diagonal matrix
    v1      <- c(1, 1, 1) #[1,1,1]

    #Computing risk premium prices 
    p       <- (inv(I - beta * Pi_star)) %*% (beta * (Pi_star %*% v1))
    p
    
    #Converting prices in rates

    # values in diagonal matrix
    values     <- c((1 + p) * values_states)

    #generate the diagonal matrix
    v2      <- matrix(diag(values), 3, 3)

    #generate the matrix
    v3      <- matrix((p)^(-1), 3, 3)
    
    #computing the matrix to convert the prices into rates
    v4      <- v3 %*% v2

    #computing the average risk rate using an equal weight for the three status
    rr      <- t(st_prob) %*% v4 %*% st_prob
    as.numeric(rr)
    
    #rp      <- p[, 1] %*% st_prob
    

    #compute index
    i       <- v((rr - 1) * 100, rf * 100)

    out     <- c(rf * 100, (rr - 1) * 100, log(i))

    return(out)

}

bisection <- function(param) {
    #bisection variable
    #setting the max min value for m
    max     <- 20.0000
    min     <- 0.0000
    half    <- (max - min) / 2 + min
    i       <- 0 #values from value function
    count   <- 1 #numeber of iteration
    out <-c()
    while (i < 20) {
      print(cbind("Iteration: ", count))

      #A vector which takes the first quartile and the last
      #from the set of possibile values (max-min)
      m1  <- c((max - min) / 4, (max - min) * 3 / 4) + min

      

      bis <- matrix(NA, nrow = 2, ncol = 4)
      #   index    m     free    p   val
      #   1       m1[1]
      #   2       m1[2]

      bis[, 1] <- as.numeric(m1)

      #recall a function to compute risk rate for m[1] and the index
      bis[1, 2:4] <- riskComputation(m1[1], param)

      #recall a function to compute risk rate for m[1] and the index
      bis[2, 2:4] <- riskComputation(m1[2], param)
      
      #select the one with max index(value)
      if (bis[1, 4] > bis[2, 4]) {
        #the m in the first quartile is the one with greater index(value)
        #so the m value that max the value function is in the first half
        #of the distribution
        #so now I will restrict the possibile values of m by half, selecting
        #the first half of the distribution
        max     <- half 
        half    <- (max - min) / 2 + min
        print(bis[1,])
        i       <- bis[1, 4]  #output of the value function given m
        out <- bis[1,]
      } else if (bis[1, 4] < bis[2, 4]) {
        #the same as above, but we take the other half of the distrib.
        min     <- half
        half    <- (max - min) / 2 + min
        print(bis[2,])
        i       <- bis[2, 4]
        out <-  bis[2,]
      } else {
        #In case the value function return the same value for the two m
        #we have convergence so I stop the cycle

        print(bis[1, ])

        i <- 100
      }
      count <- count + 1
    }
    return(out)
}

main <- function(){
  phi     <- 0.43
  gamma   <- 2
  h       <- 1.054
  l       <- 0.982
  param   <- c(h, l, gamma, phi)
  m       <- bisection(param)
  m       <- bisection(param)
  h       <- seq(from = 1.03, to=1.2, by=0.005) 
  l       <- seq(from = 0.83, to=0.99, by=0.005)
  mlist1  <- c(0)
  hlist   <- c(0)
  llist   <- c(0)
  ilist   <- c(0)
  mlist   <- matrix(data=c(mlist1, hlist, llist),1,3)
  for (i in 1:length(h)){
    for (j in 1:length(l)){
      param <- c(h[i], l[j], gamma, phi)
      v = bisection(param)
      print(h[i])
      print(l[j])
      mlist<-rbind(mlist,c(v[1],h[i],l[j]))
    }
  }
  return(mlist)
}
mlist=main()
mlist=mlist[mlist[,1]<1.2,]
mlist=mlist[mlist[,1]>0,]
scatterplot3d(mlist[,2],mlist[,3],mlist[,1])
plot(mlist[,2]-mlist[,3], mlist[,1])
h       <- 1.054
l       <- 0.982
param   <- c(h, l, gamma, phi)
m       <- bisection(param)
