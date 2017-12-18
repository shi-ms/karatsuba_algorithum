### This function allows to calculate the product of two numbers (especially big integers) by
### string operation. The input should be strings and not suggested by using as.character() or ToString()
### due to the accurancy loss when R doing big integer convertion.



karatsuba <- function(x.string, y.string){
### Define x and y as strings. Add zeros if x and y is not the same length
        digit.x <- nchar(x.string)
        digit.y <- nchar(y.string)
        digit <- max(digit.x, digit.y)
        n0 <- max(digit.x, digit.y) - min(digit.x, digit.y)
        sum <- vector(mode = "character", length = digit)
        if (digit.x < digit.y) {
                x.string <- paste(c(rep("0", n0), x.string), collapse = "")
        }
        else if (digit.x > digit.y) {
                y.string <- paste(c(rep("0", n0), y.string), collapse = "")
        }

### The recursive function ends when digit equals to 1
        if (digit == 1){
                product <- as.character(as.numeric(x.string) * as.numeric(y.string))
        }
### Define a,b,c and d as well as a+b, c+d
        else {
                a.string <- substr(x.string, 1, digit %/% 2)
                b.string <- substr(x.string, digit %/% 2 +1, digit)
                c.string <- substr(y.string, 1, digit %/% 2)
                d.string <- substr(y.string, digit %/% 2 +1, digit)
                ab.string <- string_add(a.string, b.string)
                cd.string <- string_add(c.string, d.string)

### Calculate product by karatsuba algorithum : product = ac*10^(digit)+bd+[(a+b)(c+d)-ac-bd]*10^(1/2*digit)
                zero_add <- digit-(digit %/% 2)
                m1 <- karatsuba(a.string,c.string) 
                m1_paste_zero <- paste(c(m1, rep("0", zero_add*2)), collapse = "")
                m2 <- karatsuba(b.string,d.string)
                m1_add_m2 <- string_add(m1, m2)
                m3 <- karatsuba(ab.string, cd.string)
                m3_substract_m1_m2 <- string_substract(m3, m1_add_m2)
                m3_substract_m1_m2_paste_zero <- paste(c(m3_substract_m1_m2, rep("0", zero_add)), collapse = "")

### Add the final product in two steps                
                temp <- string_add(m1_paste_zero, m2)
                product <- string_add(temp, m3_substract_m1_m2_paste_zero)
        }
        product
        
}