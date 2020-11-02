shiftVector <- function (vec, shift) {
  if(length(vec) <= abs(shift)) {
    rep(NA ,length(vec))
  }else{
    if (shift >= 0) {
      c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
    else{
      c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) 
    } 
  }
}

