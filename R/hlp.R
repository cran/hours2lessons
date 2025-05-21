# Numărul de biți '0' aflați între biți '1' ("ferestre")
cnt_holes <- function(sb) { # sb: șablonul binar al orelor profesorului
    bits <- which(bitwAnd(sb, h2bin) > 0) # rangurile biților '1'
    n <- length(bits)
    bits[n] - bits[1] + 1 - n
}

