#' Exp transformation
#'
#' User sets rates, which transforms the input variable, outputting in a new table.
#' @param data data.table that holds the variable to be transformed
#' @param variable Character string of the variable to be transformed
#' @param rate One (or many) rates to adstock the variable
#' @keywords Exp
#' @import data.table
#' @export

exp1 <- function (data, variable, rate)
{ 
  x <- data [,get(variable)]
  Exped <- sapply(rate, function(rate)  {
    exp(x*rate)
  })
  out <- as.data.table(Exped)
  setnames(out, names(out), paste(variable, "Exp", as.character(rate), sep = "."))
  
  return(out[])
}
