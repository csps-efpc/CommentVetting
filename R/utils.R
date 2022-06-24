require(purrr)
require(stats)
library(dplyr)
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
    purrr::map2(.x[keys], .y[keys], \(.x1, .y1){
      #.x1 = ..x[keys][[1]]
      #.y1 = .y[keys][[1]]
      c(.x1, .y1) |> unique()
      }) |>
      stats::setNames(keys)
  })
}


# .y$text
# .y[[1]]$profanity[[1]]
# .y[[2]]$hate




#' Title split a dataframe into list of dataframes
#'
#' @param dat 
#' @param grping 
#' @param lst_attach 
#' @param rest 
#'
#' @return
#' @export
#'
#' @examples
make_grouped_list <- function(dat, grping, lst_attach, rest = 'dat'){
  dat |> 
    group_split(!!sym(grping))  |> # magrittr::extract2(1) ->.grp_dat
    map(\(.grp_dat){
      ret_lst <- list()
      lst_attach |> 
        walk(~{
          ret_lst[[.x]] <<- .grp_dat[[.x]] |> unique()
          .grp_dat[[.x]] <<- NULL
        })
      ret_lst[[rest]] <- .grp_dat
      ret_lst
    })
}