### The function can remove the zeros at the beginning which are generated by substraction

remove_zero <- function(x){
        y <- x
        for (i in 1:length(x)){
                if (x[i] == "0"){
                   y[i] <- ""     
                }
                else break
        }
        y
}