library(matlib)
options(digits = 15)
v <- function(dummy, x) {
  for (t in 1 : length(dummy)) {
    if (dummy[t] > 4) {
      dummy[t]  <-  1
    }
    else {
      dummy[t]  <-  0
    }
  }
  value <-  abs(1 / (1 - x)) * dummy
  return(value)
}

riskComputation <- function(m){
    #parameters 
    
    phi     <- 0.43
    beta    <- 0.96
    gamma   <- 2
    h       <- 1.054
    l       <- 0.982
    epsilon <- 0.012

    valuesPi        <-   c(phi, epsilon, 1 - phi - epsilon, 1 / 2,
     0, 1 / 2, 1 - phi - epsilon, epsilon, phi)
    Pi              <-   matrix(valuesPi, byrow = TRUE, nrow = 3, ncol = 3)
    Pi


    values_states   <-   c(h,m,l)

    #ouput vector
    out <-  c()

    #stationary prob.
    st_prob <- c(0.5, epsilon, 0.5) * (1 + epsilon)
    st_prob

    #ZCB Price 

    q   <-  beta * (Pi %*% (values_states ^- gamma))
    q

    #risk-free rate vector
    risk_free   <-   (1 / (q)) - 1
    risk_free_h <-   risk_free[1, ]
    risk_free_m <-   risk_free[2, ]
    risk_free_l <-   risk_free[3, ]

    #mean risk free rate
    rf <- risk_free[, 1] %*% st_prob
    as.numeric(rf)

    #risky_rate
    values_matrix <- diag(3) * (values_states^(1 - gamma))


    Pi_star <-  Pi %*% values_matrix
    Pi_star
    I       <- diag(3)
    v1      <- c(1, 1, 1)

    p       <- (inv(I - beta * Pi_star)) %*% (beta * (Pi_star %*% v1))
    p
    
    v2      <- matrix(data=0,3,3)
    v2[1,1] <- (1+p[1])*values_states[1]
    v2[2,2] <- (1+p[2])*values_states[2]
    v2[3,3] <- (1+p[3])*values_states[3]
    v3      <- matrix((p)^(-1),3,3)
    
    v4      <- v3%*%v2

    rr      <- t(c(1/3,1/3,1/3))%*%v4%*%st_prob
    as.numeric(rr)
    
    rp      <- p[, 1] %*% st_prob
    

    #compute index
    i       <- v((rr-1)*100, rf * 100)

    out     <- c(rf * 100, (rr-1)*100, log(i))

    return(out)

}

main <- function() {
    #bisection variable
    
    max     <- 20.0000
    min     <- 0.0000
    half    <- (max - min) / 2 + min
    i       <- 0
    count   <- 1
    
    while (i < 20) {
      print(cbind("Iteration: ", count))

      m1  <- c((max - min) / 4, (max - min) * 3 / 4) + min

      

      bis <- matrix(NA, nrow = 2, ncol = 4)
      #   index    m   free    p   val
      #   1
      #   2

      bis[, 1] <- as.numeric(m1)

      #recall a function to compute risk rate for m[1] and the index
      bis[1, 2:4] <- riskComputation(m1[1])

      #recall a function to compute risk rate for m[1] and the index
      bis[2, 2:4] <- riskComputation(m1[2])
      
      #select the one with max index
      if (bis[1, 4] > bis[2, 4]) {
          max     <- half
          half    <- (max - min) / 2 + min
          print(bis[1,])
          i       <- bis[1, 4]
      } else if (bis[1, 4] < bis[2, 4]) {
          min     <- half
          half    <- (max - min) / 2 + min
          print(bis[2,])
          i       <- bis[2, 4]
      } else {
        print(bis[1, ])
        i <- 100
      }
      count <- count + 1
    }
}
main()
