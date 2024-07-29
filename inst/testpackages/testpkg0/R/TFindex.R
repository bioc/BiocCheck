## should not trigger T / F check
test1 <- function() {
    mat1 <- matrix(seq_len(20), nrow=5)
    x <- list(orig = mat1, T = t(mat1))
    x$T
}

## should trigger T / F check
test2 <- function() {
    x <- T
    x
}

test3 <- function(){
    T <- "whatsinaname"
    myX <- seq_len(2)
    names(myX) <- c(T , "rose")
    myX[ T ]
}
