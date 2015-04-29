
# define function to identify second winner
second <- function(x) max(x[x!=max(x, na.rm=T)], na.rm=T)
