



library(textcat)


#' Title returns the language of the string
#'
#' @param x 
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#'  get_language(x = "Ottawa est une ville avec beaucoup d'espaces verts, et de très belles pistes cyclables.")
#'  get_language(x = "Ottawa is a city with a lot of green space, and very nice bike paths.")
#' 
get_language <- function(x, ...){
  x |>
    textcat(... = ...)
  
  
  # textcat(c("This is an english sentence.",
  #           "Das ist ein deutscher satz."))
}

#' Title returns if it is a valid language
#'
#' @param lang 
#' @param valid_langs 
#'
#' @return
#' @export
#'
#' @examples
valid_language<- function(lang, valid_langs = c("english", "french")){
  lang %in% valid_langs
}
