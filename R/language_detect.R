
library(textcat)
library(fastText)
library(readr)
library(janitor)
library(stringr)
library(dplyr)







G_LANG_CODE_NAMES <- NULL
#' Title return a tibble that can convert from ISO to language
#'
#' @param fn 
#' @param force_reload 
#'
#' @return
#' @export
#'
#' @examples
read_language_code_names <- function(fn = file.path('language_identification', 'language_code_names.csv'), force_reload = FALSE){
  if (force_reload){
    G_LANG_CODE_NAMES <<- NULL
  }
  
  if (is.null(G_LANG_CODE_NAMES)){
    G_LANG_CODE_NAMES <<- 
      readr::read_csv(fn) |>
      janitor::clean_names() |>
      mutate_all(str_to_lower) |>
      pivot_longer(!iso_639_1_code, values_to = 'lang_name' ) |>
      mutate(lang = str_remove_all(str_extract(name, '^[:alpha:]+_'), "_")) |>
      select(-name)
  }
  G_LANG_CODE_NAMES
}
read_language_code_names()



#' Title Return as Name of language by default the english name of the language given the code
#'
#' @param a_code 
#' @param a_lang 
#'
#' @return
#' @export
#'
#' @examples
#' lang_code_to_name(a_code = c('de', NA))
#' lang_code_to_name('de', a_lang = 'french')
#' lang_code_to_name('de', a_lang = 'german')
lang_code_to_name <- function(a_code, a_lang = 'english'){
  a_code |>
    map(~{
      if (is.na(.x)){
        return('unkown')
      }
      ret_lang_code  <- 
        read_language_code_names() |>
        filter(iso_639_1_code == .x) |>
        filter(lang == a_lang) |>
        pull(lang_name)
      
      if (length(ret_lang_code) != 1){
        return(NA)
      }
      ret_lang_code
    }) |> 
    unlist()
}



#' Title returns the code of a language given the name of the language
#'
#' @param a_lang 
#' @param a_lang_of_lang 
#'
#' @return
#' @export
#'
#' @examples
#'   lang_name_to_code(a_lang = c('english', 'tokipona', 'unkown'))
#'   lang_name_to_code('french')
#'   lang_name_to_code('français', a_lang_of_lang = 'french')
lang_name_to_code <- function(a_lang, a_lang_of_lang = 'english'){
  
  a_lang |> 
    map(~{
      ret_lang_code <- 
        read_language_code_names() |>
          filter(lang_name == .x) |>
          filter(lang == a_lang_of_lang) |>
          pull(iso_639_1_code)
      
      if (length(ret_lang_code) != 1){
        return(NA)
      }
      
      ret_lang_code
      }) |> 
    unlist()
}




#' Title returns the language of the string
#' list of supported languages
#'   https://fasttext.cc/docs/en/language-identification.html
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
get_language <- function(x, file_pretrained = system.file(file.path("language_identification", "lid.176.ftz"), package = "fastText"), ...){
  # x |>
  #   textcat(... = ...)
  #x = str_remove_all("Ton arrière arrière grand père, il a défriché la terre Ton arrière grand père, il a labouré la terre", '[:punct:]')
  #x = "Et pis toé, mon p'tit gars, tu l'sais pu c'que tu vas faire Dans ton p'tit trois et demi bin trop cher, frette en hiver Il te vient des envies de devenir propriétaire Et tu rêves la nuit d'avoir ton petit lopin d'terre"
  
  x |> 
    #str_remove_all('[:punct:]') |>
    language_identification(pre_trained_language_model_path =file_pretrained, k = 3)  |> 
    pivot_longer(cols = matches('^(prob_|iso_lang_)')) |>
    mutate(ilang = str_extract(name, '[:digit:]+')) |>
    mutate(name = str_remove(name, paste0('_',ilang))) |>
    pivot_wider(names_from = name, values_from = value) |>
    mutate(prob = as.double(prob)) |> 
    mutate(lang_name = lang_code_to_name(iso_lang)) |>
    slice_max(prob) |>
    pull(lang_name)
}
# x <- '中国的领导人是习近平'
# x <- 'Für eine gute Zeit rufen Sie'
# x<-'Which ist gut.'

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

