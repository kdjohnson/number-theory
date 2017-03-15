gcd <- function(a, b) {
    if(b == 0) {
        a
    } else {
        gcd(b, a %% b)
    }
}

residueClass <- function(a, n)
    for(i in 1:n) {
    
    }