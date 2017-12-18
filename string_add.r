### This function allows the operation of adding two integers as strings

string_add <- function(x,y){
        x.string <- strsplit(x, split = "")[[1]]
        y.string <- strsplit(y, split = "")[[1]]
        digit <- max(length(x.string), length(y.string))
        n0 <- max(length(x.string), length(y.string)) - min(length(x.string), length(y.string))
        sum <- vector(mode = "character", length = digit)
        if (length(x.string) < length(y.string)){
                x.string <- c(rep("0", n0), x.string)
        }
        else if (length(x.string) > length(y.string)){
                y.string <- c(rep("0", n0), y.string)
        }
        carry <- 0
        for (i in digit :1 ){
                temp <- as.numeric(x.string[i]) + as.numeric(y.string[i]) + carry
                if (temp >= 10){
                        sum[i] <- as.character(temp - 10)
                        carry <- 1
                        }
                else {
                        sum[i] <- as.character(temp)
                        carry <- 0
                        }
                }
        if (carry == 1) sum_result <- c("1", sum)
        else sum_result <- sum
        sum_result <- paste(remove_zero(sum_result), collapse = "")
        if (sum_result == "") sum_result <- "0"
        sum_result
}