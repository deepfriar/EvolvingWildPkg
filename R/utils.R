na_if_null <- function(x) {

  return(
    ifelse(is.null(x), NA, x)
  )

}
