### This function allows the operation of adding two integers as strings
### The input x and y should based on x > y to avoid incorrect result because this function 
### can not handle with negative numbers

string_substract <- function(x,y){
        x.string <- strsplit(x, split = "")[[1]]
        y.string <- strsplit(y, split = "")[[1]]
        digit <- max(length(x.string), length(y.string))
        n0 <- max(length(x.string), length(y.string)) - min(length(x.string), length(y.string))
        diff <- vector(mode = "character", length = digit)
        if (length(x.string) < length(y.string)){
                x.string <- c(rep("0", n0), x.string)
        }
        else if (length(x.string) > length(y.string)){
                y.string <- c(rep("0", n0), y.string)
        }
        carry <- 0
        for (i in digit :1 ){
                if (as.numeric(x.string[i]) - carry < as.numeric(y.string[i])){
                        diff[i] <- as.character(as.numeric(x.string[i]) - as.numeric(y.string[i]) + 10 - carry)
                        carry <- 1
                }
                else {
                        diff[i] <- as.character(as.numeric(x.string[i]) - as.numeric(y.string[i]) - carry)
                        carry <- 0
                }
        }
        diff_result <- paste(remove_zero(diff), collapse = "")
        if (diff_result == "") diff_result <- "0"
        diff_result
}