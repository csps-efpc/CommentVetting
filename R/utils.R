require(purrr)
require(stats)

#' Title Combines two named lists into a single list
#'
#' @param a 
#' @param b 
#'
#' @return
#' @export
#'
#' @examples
combine_named_lists <- function(a, b){
  purrr::map2(a, b, ~{
    # .x = a[[1]]
    # .y = b[[1]]
    keys <- unique(c(names(.x), names(.y)))
    purrr::map2(.x[keys], .y[keys], \(.x1, .y1){c(.x1, .y1) |> unique()}) |>
      stats::setNames(keys)
  })
}


# .y$text
# .y[[1]]$profanity[[1]]
# .y[[2]]$hate
