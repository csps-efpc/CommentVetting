
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
#' lang_code_to_name('de')
#' lang_code_to_name('de', a_lang = 'french')
#' lang_code_to_name('de', a_lang = 'german')
lang_code_to_name <- function(a_code, a_lang = 'english'){
  a_code |>
    map(~{
      read_language_code_names() |>
        filter(iso_639_1_code == .x) |>
        filter(lang == a_lang) |>
        pull(lang_name)
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
#'   lang_name_to_code('english')
#'   lang_name_to_code('french')
#'   lang_name_to_code('français', a_lang_of_lang = 'french')
lang_name_to_code <- function(a_lang, a_lang_of_lang = 'english'){
  
  a_lang |> 
    map(~{
        read_language_code_names() |>
          filter(lang_name == .x) |>
          filter(lang == a_lang_of_lang) |>
          pull(iso_639_1_code)
      }) |> 
    unlist()
}




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
get_language <- function(x, file_pretrained = system.file(file.path("language_identification", "lid.176.ftz"), package = "fastText"), ...){
  # x |>
  #   textcat(... = ...)
  
  x |> 
  language_identification(pre_trained_language_model_path =file_pretrained, ... = ...)  |> 
    mutate(lang_name = lang_code_to_name(iso_lang_1)) |>
    pull(lang_name)
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

