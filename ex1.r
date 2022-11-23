data <- matrix(NA, nrow = 5, ncol = 10)

a <- c(0.023, 0.041, 0.2)
b <- c(0.015, 0.030, .9)
c <- c(0.030, .05, -.2)
d <- c(.018, .036, .0)
e <- c(.025, .045, -0.9)


data[1,1:3 ] <- a
data[2,1:3 ] <- b
data[3,1:3 ] <- c
data[4,1:3 ] <- d
data[5,1:3 ] <- e
col_names <- c("u", "n", "p", "l", "h", "phi", "q_h", "q_l","r_h","r_l")
countries <- c("A", "B", "C", "D", "E")

colnames(data) <- col_names
rownames(data) <- countries

data
beta <- 0.96
gam <- 2
#compute the high and low values 
for (count in 1:5) {
   u <- data[count,1]
   n <- data[count,2]
   p <- data[count,3]
   data[count, 4] <- l <- 1 + u - n
   data[count, 5] <- h <- 1 + u + n
   k <- (p + (u / n)^2) * n^2
   data[count, 6] <- k
   data[count, 6] <- phi <- (k - ((h - 1) * (l - 1))) / (0.5 * (h - 1)^2 + 0.5 *
   (l - 1)^2 - ((h - 1) * (l - 1)))
   #risk free rate 
   data[count, 7] = beta * (phi * h^(-gam)+ (1 - phi) * l^(-gam)) #q_h
   data[count, 8] = beta * ((1 - phi) * h^(-gam) + phi * l^(-gam)) #q_l
   data[count, 9] = data[count, 7]^(-1)-1
   data[count, 10] = data[count, 8]^(-1)-1 #q_l
}
kable(data)

beta=0.97
h=1.12
l=0.97
pi=matrix(NA,2,2)
pi[1,]=c(0.5,0.5)
pi[2,]=c(0.5,0.5)
value_states=c(h,l)
pist= pi %*% diag(2)
m2=inv( diag(2) - (beta * pist) )
p=m2 %*% (beta * pist) %*% c(1,1)
p
