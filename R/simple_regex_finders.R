

library(stringr)
library(dplyr)
library(purrr)
library(stringi)
library(stringr)
library(numbers)
library(glue)

#' Title check if a given SIN number is valid
#'
#' @param x vector of numbers
#'
#' @return
#' @export
#'
#' @examples
#' validate_sin(x= c("4539 3195 0343 6467","4539 3195 0343 6499", '046 454 286', '321 543 8761  '))
#' 
validate_sin <- function(x){
  x2 <- 
    x |> 
    str_remove_all(pattern = '[^0-9]+')
  (nchar(x2) == 9) & (luhn_checksum(x2))
}
# f <- get('validate_sin')
# f('asdsdfa1')

#' Title runs the lhn number checksum 
#'     https://www.dcode.fr/luhn-algorithm
#'     https://exercism.org/tracks/r/exercises/luhn
#'     https://en.wikipedia.org/wiki/Social_insurance_number
#'
#' @param x vector of strings that has only numbers and spaces, ideally
#' @param mod_div defaults to 10
#'
#' @return vector of booleans
#' @export
#'
#' @examples
#'  luhn_checksum( x= c("4539 3195 0343 6467","4539 3195 0343 6499", '046 454 286', '321 543 8761  '))
#' 
luhn_checksum <- function(x, mod_div = 10){
  str_remove_all(string = x, pattern = '[^0-9]+') |>
    stri_reverse() |> 
    str_split('') |> #  magrittr::extract2(1) 
    purrr::map(\(chrs){
      #print(chrs)
      purrr::map2(chrs, !as.logical(1:length(chrs) %% 2) ,\(ch, multi){
        if (multi) {
          if (as.integer(ch) <= 4){
            as.integer(ch) * 2
          }else{
            as.integer(ch) *2 - 9
          }
        }else {ch}
      }) |>
        str_split('') |> 
        unlist()  |>
        as.integer() |> 
        sum() |>
        numbers::mod(mod_div) == 0
    }) |> 
    unlist()
}




G_REGEX_CAT <- NULL
#' Title Returns a dataframe with two columns, one with regexs and one with categories.
#'
#' @param fn 
#'
#' @return
#' @export
#'
#' @examples
#'  read_regex_categories()
read_regex_categories <- function(fn = file.path('data', 'regex_cat.csv'), force_reload = FALSE){
  if (force_reload){
    G_REGEX_CAT <<- NULL
  }
  
  
  if (is.null(G_REGEX_CAT)){
    G_REGEX_CAT <<- 
      readr::read_csv(fn) |>
      mutate_if(is.character, str_trim)
  }
  G_REGEX_CAT
}
read_regex_categories()



#' Title returns a vector of TRUE of length(x)
#'
#' @param x 
#'
#' @return vector of all TRUE
#' @export
#'
#' @examples
#'   all_true(c('A', 4, FALSE, TRUE, 'asdfasdgasdg', 0))
all_true <- function(x){
  rep(TRUE, length(x))
}



#' Title takes a single string, a regex and a validation function that is a string, find the regex in the string, then validates the data
#'
#' @param x 
#' @param rx 
#' @param ignr_cs 
#' @param func_ch 
#' @param default_func 
#'
#' @return
#' @export
#'
#' @examples
str_locate_rx_check_func <- function(x, 
                                     rx, 
                                     ignr_cs,  
                                     func_ch, 
                                     default_func = all_true
                                     ){
  assertthat::assert_that(length(x) == 1)

  extracted  <- x |> stringr::str_extract_all(regex(pattern = rx, ignore_case = ignr_cs ))
  assertthat::assert_that(length(extracted) == 1)
  extracted <- extracted[[1]]
  
  
  locations <- x |> stringr::str_locate_all(regex(pattern = rx, ignore_case = ignr_cs ))
  assertthat::assert_that(length(locations) == 1)
  locations <- locations[[1]]
  
  
  # func_ch = "validate_sin"
  # func_ch = ""
  
  ######################
  # Get function from R environment
  f <- 
    tryCatch(
      get(func_ch),
      error=function(cond) {
        message(glue('did not find requested validation function. {cond}\n'))
        return(default_func)
      }
    ) 
  
  
  ##########################
  # only keep regex finds that pass the test
  locations_filtered <- locations[f(extracted),,drop = FALSE]
  
  
  assertthat::assert_that(is.matrix(locations_filtered))
  

  locations_filtered |> 
    apply(1,\(st_en){
        tibble::tibble(found = stringr::str_sub(x, st_en[[1]], st_en[[2]]),
           #location = list(start = st_en[[1]], end = st_en[[2]])
           start = st_en[[1]],
           end = st_en[[2]]
      )
    }) |>
    bind_rows(tibble(found = as.character(), start = as.integer(), end = as.integer())) |>
    filter(!is.na(found))
  
  
  
    
  # locations_filtered |> 
  #   apply(1,\(st_en){
  #     list('found' = stringr::str_sub(x, st_en[[1]], st_en[[2]]),
  #          #location = list(start = st_en[[1]], end = st_en[[2]])
  #          location = c(st_en[[1]],st_en[[2]])
  #     )
  #   })
  
}











#' Title
#'
#' @param x 
#' @param regex_dat 
#'
#' @return
#' @export
#'
#' @examples
#'  get_regex_combined(x = c('this is a valid but unassigned SIN 046 454 286, and this is an invalid but properly formated sin 321 543 8761.', 'no Sin here', 'we now have and email address, funny@man.ca' ), lang = 'english')
#'  get_regex_combined(x = 'Ryan Gosling likes to eat poop, because it tastes good. What a fucking Wierdo!, call and complaing at 1 (800) 622-6232.')
#'  get_regex_combined(x = 'Homer Simpson lives at 742 Evergreen Terrance Springfield, Ontario.', lang = 'en')
#'  get_regex_combined(x = 'I like to use ibm.com as an example URL.', lang = 'en')
#'  get_regex_combined(x = "this is a comment '); DROP my_tabel from DB", lang = 'en')
#'  get_regex_combined(x = "<Habazutty>yaddayadda</Habazutty><Vogons /><Targ>blahblah</Targ>", lang = 'en')
get_regex_combined <- function(x, lang, regex_dat = read_regex_categories()){
  assertthat::assert_that(length(lang) == 1)

  #assertthat::assert_that(valid_language(lang))

  
  x_no_accents <-
    x |>
    stringi::stri_trans_general(id = "Latin-ASCII")

  # .text_cleaner = janitor::make_clean_names(str_sub(.text, 1, str_nchar_limit), case = 'title')
  # .text_cleaner_edits = edit_required(.text, .text_cleaner)
  
  lang <- lang_code(lang)
  
  regex_dat_lng <- 
    regex_dat |> 
    filter(str_detect(string = lng, pattern = regex(pattern = lang, ignore_case = TRUE)) | lng == 'ALL') |> 
    #filter(lng == lang) |> 
    select(-lng)  
      
  
  ################################
  # 
  #purrr::map2_dfr(x_no_accents, x, ~{
  purrr::map_dfr(1:length(x), \(.i){
    
    .x <- x_no_accents[[.i]]
    .y <- x[[.i]]
    .text_cleaner_edits = edit_required(.y, .x)
    
    #lst<- 
      regex_dat_lng |> 
      purrr::pmap_dfr(\(rx , cat, sub_cat, ignr_cs, func_ch){
            #rx = regex_dat_lng |> slice(24) |> pull(rx)
            str_locate_rx_check_func(.x, rx, ignr_cs, func_ch) |> 
            mutate(start = (start + str_count(str_sub(.text_cleaner_edits, 1, start), "I")) - str_count(str_sub(.text_cleaner_edits, 1, start), "D")) |>
            mutate(end = (end + str_count(str_sub(.text_cleaner_edits, 1, end), "I")) - str_count(str_sub(.text_cleaner_edits, 1, end), "D")) |>
            mutate(hit_type = cat, sub_type = sub_cat)
         }) |> 
      #mutate(text = .y) |>
      mutate(index = .i)
    # stats::setNames(regex_dat_lng$cat) 
    #lst <-lst[lengths(lst) > 0]
    # lst[['text']] <- .y
    # lst
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
