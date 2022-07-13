require(purrr)
require(stats)
library(dplyr)
library(tidytext)
library(quanteda)

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







G_STOP_WORDS <- NULL
#' Title
#'
#' @param lang 
#' @param force_reload 
#'
#' @return vector of words that are 'fine' and should not be matched to anything.
#' @export
#'
#' @examples
get_stop_words <- function(lang, force_reload = FALSE){
  
  if (force_reload){
    G_STOP_WORDS <<- NULL
  }
  
  
  if ( is.null(G_STOP_WORDS)){
    fr_stop <- 
      c("comme", "avoir", "plus", "avant", "être") |>
      c(quanteda::stopwords("french")) |>
      unique() 
    
    en_stop <- 
      tidytext::stop_words$word |>
      c(quanteda::stopwords("english")) |> 
      unique() 
    G_STOP_WORDS <<- 
      dplyr::bind_rows(  
        dplyr::tibble(stops = fr_stop, lng = 'french'),
        dplyr::tibble(stops = en_stop, lng = 'english')
      )  |>
      dplyr::distinct()
    
  }
  
  
  G_STOP_WORDS |>
    dplyr::filter(lng == lang) |>
    dplyr::pull(stops)
}

