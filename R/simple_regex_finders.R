

library(stringr)
library(dplyr)
library(purrr)
library(stringi)
library(stringr)




G_REGEX_CAT <- NULL
#' Title Returns a dataframe with two columns, one with regexs and one with categories.
#'
#' @param fn 
#'
#' @return
#' @export
#'
#' @examples
read_regex_categories <- function(fn = file.path('data', 'regex_cat.csv'), force_reload = FALSE){
  if (force_reload){
    G_REGEX_CAT <<- NULL
  }
  
  
  if (is.null(G_REGEX_CAT)){
    G_REGEX_CAT <<- readr::read_csv(fn)
  }
  G_REGEX_CAT
}
read_regex_categories()


#' Title
#'
#' @param x 
#' @param regex_dat 
#'
#' @return
#' @export
#'
#' @examples
#'  get_regex_combined(x)
#' 
get_regex_combined <- function(x, lang, regex_dat = read_regex_categories()){
  
  x_no_accents <- 
    x |> 
    stringi::stri_trans_general(id = "Latin-ASCII")
    
  
    purrr::map2(x_no_accents, x, ~{
      regex_dat_lng <- 
        regex_dat |> 
        filter(lng == lang) |> 
        select(-lng) 
      lst<- 
        regex_dat_lng |> 
        purrr::pmap(\(rx,cat, ignr_cs){
              #print(ignr_cs)
              # rx <- regex_dat$rx[[1]]
              # cat <- regex_dat$cat[[1]]
              # ignr_cs <- regex_dat$ignr_cs[[1]]
              locations <- .x |> stringr::str_locate_all(regex(pattern = rx, ignore_case = ignr_cs ))
              
              assertthat::assert_that(length(locations) == 1)
              locations <- locations[[1]]
              
              locations |> apply(1, \(st_en){
                    list('found' = stringr::str_sub(.x, st_en[[1]], st_en[[2]]),
                         #location = list(start = st_en[[1]], end = st_en[[2]])
                         location = c(st_en[[1]],st_en[[2]])
                         )
                })
          }) |> stats::setNames(regex_dat_lng$cat) 
      lst[['text']] <- .y
      lst
    })
}

# 
# x = c("for a good time email bob@gmail.com. When done with that email bill@outlook.ca", 
#       'The MP for Ottawa West Nepean can be reached at anita.vandenbeld@parl.gc.ca, while the MPP can be contacted @ 613-721-8075', 
#       'Lawrence Taylor was a coke head. Which explains a few things, Robin William was also, that explains some things also. Maybe we should contact Wllness Togeather Canada at 1-866-585-0445 to help them.', 
#       'run Devil, run Devil run.')
# regex_finds <- get_regex_combined(x)
# proper_nouns_finds <- get_proper_nouns(x)
# 
# 
# length(y)
# final <- 
#   map2(one, two, ~{
#     keys <- unique(c(names(.x), names(.y)))
#     map2(.x[keys], .y[keys], \(.x1, .y1){c(.x1, .y1) |> unique()}) |>
#       setNames(keys)
#   })
# 
# final[[4]]$proper_nouns
# lst1 <- list(a=1)
# lst2 <- list(b=2, a=4)
# 
# 
# 
# keys <- unique(c(names(lst1), names(lst2)))
# setNames(mapply(c, lst1[keys], lst2[keys]), keys)
# 
# names(y[[1]])
# y[[1]]$phone_number
