#' Convert degrees Fahrenheit to Celcius
#'
#' We use the simple formula for temperature conversion
#' to convert temperaturs
#' @param fahr The temperature to be converted in degrees Fahrenheit
#' @return the temperature converted to degrees Celcius
#' @keywords conversion
#' @examples
#' fahr_to_celcius(32)
#' fahr_to_celcius(c(212,100,32))

fahr_to_celcius <- function(fahr){
  # stopifnot(is.numeric(fahr))
  assert_that(is.numeric(fahr))
  celcius <- (fahr-32)*5/9
  return(celcius)
} 


celcius_to_fahr <- function(celsius){
  fahr <- (celsius * 9/5) + 32
  return(fahr)
} 