
library(stringr)
library(stringi)
library(tidytext)
library(quanteda)
library(tibble)
source(file.path('R', 'utils.R'))


source(file.path('R', 'generate_regex.R'))



#' Title converts x to a tibble, with simple error wrapper!
#'
#' @param x 
#' @param def_tibble 
#'
#' @return a tibble, possibly of zero rows
#' @export
#'
#' @examples
as_tibble_error_catch <- function(x, def_tibble = tibble(start = as.integer(), end = as.integer())){
      tryCatch(
        as_tibble(x),
        error=function(cond) {
          message(glue('could not make tibble, returning default tibble. Origional error = {cond}\n'))
          return(def_tibble)
        }
      )
}





#' Title
#'
#' @param x 
#' @param str_nchar_limit 
#' @param regex_cat_phrases 
#' @param input_stop_words 
#'
#' @return data frame with 4 columns
#' @export
#'
#' @examples
#' 
#' x = c('Lawrence Taylor was a coke head. Which explains a why he was an assssssssssho0000000le on the field, Robin William was also, that explains why he was so hyper in his comedy. Maybe we should contact Wllness Togeather Canada at 1-866-585-0445 to help them, or should we say fuckit and be asshole our self and let them be? No Really we should call 866-585-0445, nigger', 'run devil run devil run', 'am I the asshole, no really am I the asshole')
#' x = 'am I the asshole, no really am I the asshole'
#' x = c('run devil run devil run', 'hop skip and jump', 'faster higher stronger')
#' find_categories_from_phrase_regex(x)
#' find_categories_from_phrase_regex(x = 'enculé le la les.')
#' x = "Ryan Gosling likes to eat poop, because it tastes good. What a fucking Wierdo!, call and complaing at 1 (800) 622-6232."
#' x = "Ryan Gosling likes to eat poop, because it tastes good. What a f.u.c.k.i.n.g. Wierdo!, call and complaing at 1 (800) 622-6232."
find_categories_from_phrase_regex <- function(
    x,
    lang,
    str_nchar_limit = 10000,
    regex_cat_phrases = get_regex_categories_phrases(),
    input_stop_words = get_stop_words(lang = lang, force_reload = FALSE)
  ){
  assertthat::assert_that(length(lang) == 1)
  
  
  
  
  x_2 <- 
    x |>
    #str_replace_all(str,stop_words_regex, '') |>
    stringr::str_squish() |>
    stringi::stri_trans_general(id = "Latin-ASCII")  |>
    stringr::str_sub(1, str_nchar_limit) |>
    janitor::make_clean_names(case = 'title') |>
    stringr::str_to_lower()
  
  
  regex_cat_phrase_lang <- 
    regex_cat_phrases |> 
    filter(lng == lang)
  
  
  
  
  #x = "in 1812 madison was mad he was the president, you know but he thought he tell the british where they ought to go"
  #x = "in 1812 madison was  fucking mad he was the president, you know but he thought he tell the british where they ought to go"
  
  
  1:length(x) |> 
  map_dfr(\(.i){
    .text = x[[.i]]
    .text_cleaner = janitor::make_clean_names(str_sub(.text, 1, str_nchar_limit), case = 'title')
    .text_cleaner_edits = edit_required(.text, .text_cleaner)
    .text_2 = x_2[[.i]]
    regex_cat_phrase_lang |>
      mutate(found = str_extract_all(string = .text_2, pattern = regex) ) |> 
      unnest(found, keep_empty  = FALSE)  |>
      unnest(categories, keep_empty  = FALSE) |>
      mutate(location = str_locate_all(str_to_lower(.text_cleaner), stringr::fixed(str_trim(found)))) |>
      unnest(location, keep_empty  = FALSE) |>
      mutate(location = as_tibble_error_catch(location)) |>
      unnest_wider(location) |>
      rename(any_of(c(start = 'V1', end = 'V2'))) |>
      mutate(start = (start + str_count(str_sub(.text_cleaner_edits, 1, start), "I")) - str_count(str_sub(.text_cleaner_edits, 1, start), "D")) |>
      mutate(end = (end + str_count(str_sub(.text_cleaner_edits, 1, end), "I")) - str_count(str_sub(.text_cleaner_edits, 1, end), "D")) |>
      #mutate_at(matches('start'), as.integer)
      mutate(across(any_of(c("start", "end")), as.integer)) |>
      select(-lng, -regex) |>
      distinct(phrase, categories, found, start, end) |>
      mutate(index = .i) 
  }) |>
    mutate(across(any_of(c("phrase", "categories", "found")), as.character)) |>
    filter(!str_trim(found) %in% input_stop_words) |>
    filter(!str_squish(found) %in% input_stop_words) |> 
    mutate(found = str_trim(found)) |> 
    distinct(categories,found, start, end, index, .keep_all = TRUE) |>
    rename(hit_type := categories, sub_type := phrase )
  
  

}  



#' Title converts a phrases dataframe of results into a list
#'
#' @param dat 
#'
#' @return
#' @export
#'
#' @examples
convert_phrases_found_to_list <- function(dat){
  hits <- 
    dat$categories |>
    unique() |>
    purrr::map(\(.c){
      #.c = dat$categories[[1]]
      dat_cat <- dat |> filter(categories == .c)
        dat_cat$phrase |>
        unique() |>
        purrr::map(\(.ph){ 
          #.ph = 'ashole'
          dat_cat_ph <- dat_cat |> filter(phrase  == .ph)
          #ph <- list()
          #ph[[.ph]] <-
            dat_cat_ph |> pmap(\(found, start, end, ...){
              list(found = found,
                   location = c(start, end)
              )
            })
          #ph
        }) |>setNames(dat_cat$phrase |>
                     unique())
    })|> setNames(dat$categories |>
                    unique())
  hits
}

#' Title Identical to find_categories_from_phrase_regex but returns a nested list
#'
#' @param x 
#' @param lang
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
#' find_categories_from_phrase_regex_lst(x = 'Ryan Gosling likes to eat poop, because it tastes good. What a fucking Wierdo!, call and complaing at 1 (800) 622-6232.', lang = 'en')
#' 
#' 
find_categories_from_phrase_regex_lst <- function(
    x,
    lang,
    ...
    
){
  x |> map(~{
    lst <- 
    .x |>
      find_categories_from_phrase_regex(lang = lang) |>
      convert_phrases_found_to_list()
    lst[['text']] <- .x
    return(lst)
  })
}









