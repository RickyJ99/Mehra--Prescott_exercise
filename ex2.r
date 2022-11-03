max <- 1.054
min <- 0.982
half <- (max-min)/2
m <- c((max-min)/4,(max-min)*3/4)
i <- 0 
while(i<1000){
    bis <- matrix(NA, nrow(2), ncol(4))
    #   index    m   free    p   val
    #   1
    #   2
    #recall a function to compute risk rate for m[1] and the index
    bis[1,] <- function(m)
    #recall a function to compute risk rate for m[1] and the index
    bis[2,] <- function(m)
    #select the one with max index 
    if( bis[1,3]>=bis[2,3]){
        max <- half
        half <- (max-min)/2
        i <- bis[1,3]
    } else {
       min <- half
       half <- (max-min)/2
       i <- bis[2,3]
    }
}