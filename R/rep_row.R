#functio to repeat a row
rep_row <- function(x, n) {
  matrix(rep(x, each = n), nrow = n)
}
