# To get lagged values in function of CONSECUTIVE observations (here years)
tlag <- function(x, n = 1L, along_with, default = NA) { 
  if (!is.numeric(n) | (length(n)>1)) stop("n must be a numeric of length one")
  index <- match(along_with - n, along_with, incomparables = NA)
  out <- x[index]
  if (!is.na(default)) out[which(is.na(index))] <- default
  out
}

# To get lead values in function of CONSECUTIVE observations (here years)
tlead <- function(x, n = 1L, along_with, default = NA) { 
  if (!is.numeric(n) | (length(n)>1)) stop("n must be a numeric of length one")
  index <- match(along_with + n, along_with, incomparables = NA)
  out <- x[index]
  if (!is.na(default)) out[which(is.na(index))] <- default
  out
}