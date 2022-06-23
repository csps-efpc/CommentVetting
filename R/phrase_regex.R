
library(stringr)
library(stringi)
library(tidytext)
library(quanteda)
source(file.path('R', 'generate_regex.R'))



G_STOP_WORDS <- NULL
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
#' x = 'Lawrence Taylor was a coke head. Which explains a why he was an assssssssssho0000000le on the field, Robin William was also, that explains why he was so hyper in his comedy. Maybe we should contact Wllness Togeather Canada at 1-866-585-0445 to help them, or should we say fuckit and be asshole our self and let them be? No Really we should call 866-585-0445, nigger'
#' x = 'am I the asshole, no really am I the asshole'
#' x = 'run devil run devil run'
#' find_categories_from_phrase_regex(x)
#' find_categories_from_phrase_regex(x = 'enculé le la les.')
#' 
find_categories_from_phrase_regex <- function(
    x,
    lang,
    str_nchar_limit = 1000,
    regex_cat_phrases = get_regex_categories_phrases(),
    input_stop_words = get_stop_words(lang = lang, force_reload = FALSE)
  ){
  x_2 <- 
    x |>
    #str_replace_all(str,stop_words_regex, '') |>
    stringr::str_squish() |>
    stringi::stri_trans_general(id = "Latin-ASCII")  |>
    stringr::str_sub(1, str_nchar_limit) |>
    janitor::make_clean_names(case = 'title') |>
    stringr::str_to_lower()
  
  
  regex_cat_phrases <- 
    regex_cat_phrases |> 
    filter(lng == lang)
  
  match_extracted <- 
    map2_dfr(regex_cat_phrases$regex,regex_cat_phrases$phrase, ~{
      #.x <- regex_cat_phrases$regex[[1]]
      #print(.y)
      
      founds <- str_extract_all(x_2, .x) |>  unlist()
      
      if (length(founds) == 0){
        return(tibble(found = as.character(), phrase = as.character()))
      }
      #print(founds)
      return(tibble(found = as.character(founds), phrase = as.character(.y)))
      }) |>
    bind_rows(tibble(found = as.character(), phrase = as.character())) |>
    mutate(found = str_squish(found))|>
    filter(!found %in% input_stop_words ) |> #Eliminate stop words that are found 
    distinct()
  
  #match_locations <- 
  map2_dfr(match_extracted$found, match_extracted$phrase,
      ~{
        #.x <- match_extracted$found[[1]]
        #.y =  match_extracted$phrase[[1]]
        locations <- str_locate_all(str_to_lower(janitor::make_clean_names(str_sub(x, 1, str_nchar_limit), case = 'title')), stringr::fixed(str_trim(.x)))
        assertthat::assert_that(length(locations) == 1)
        locations <- locations[[1]]
        tibble(
          found = .x,
          start = locations[,'start'],
          end = locations[,'end'],
          phrase = .y
        )
    }) |> 
    bind_rows(tibble(found = as.character(), start = as.integer(), end = as.integer(), phrase = as.character())) |>
    distinct() |>
      left_join(regex_cat_phrases |> select(phrase, categories), by = "phrase") |>
      unnest(categories) |>
      distinct(start, end, categories, .keep_all = TRUE)
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
#' find_categories_from_phrase_regex_lst(x = x)
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









