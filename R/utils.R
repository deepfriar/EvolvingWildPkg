## Set for numeric/text formatting within scraper
options(
  scipen = 999,
  stringsAsFactors = FALSE
)

na_if_null <- function(x) {

  return(
    ifelse(is.null(x), NA, x)
  )

}
