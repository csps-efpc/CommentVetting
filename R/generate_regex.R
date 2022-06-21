

#'
#' This File generate_regex.R will take in a file of phrases and generate a regexes to find them
#'
#'
#'

library(readxl)
library(janitor)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
#' Title reads in prases or words and the categories associated with finding them
#'
#' @param fn 
#'
#' @return
#' @export
#'
#' @examples
#' read_phrases_categories()
read_phrases_categories <- function(fn = file.path('data', 'phrase_category.xlsx')){
  readxl::read_xlsx(fn) |>
      janitor::clean_names()
}
  





#' Title get a paired character from the input character
#' 
#' @param x thing to pair
#' 
#' @return
#' @export
#' 
#' @examples
#' str_wrap_other()
#' str_wrap_other('[')
#' str_wrap_other('-')
str_wrap_other <- function(x = '('){
  if (x == '(') return(')')
  if (x == ')') return('(')
  
  if (x == '[') return(']')  
  if (x == ']') return('[')
  
  if (x == '{') return('}')  
  if (x == '}') return('{')  
  
  
  if (x == '<') return('>')  
  if (x == '>') return('<')  
  
  if (x == '«') return('»')  
  if (x == '»') return('«')  
  
  if (x == '‹') return('›')  
  if (x == '›') return('‹')    
  
  if (x == '‘') return('’')  
  if (x == '’') return('‘')      
  
  
  if (x == '“') return('”')  
  if (x == '”') return('“')   
  return(x)  
}








#' Title Wraps a string in some thing
#'
#' @param str thing to wrap
#' @param f front end
#' @param b back end
#'
#' @return
#' @export
#'
#' @examples
#'  str_wrap_with('kitty')
#'  str_wrap_with('kitty', '[')
#'  str_wrap_with('kitty', ']')
#'  str_wrap_with('kitty', '"')
#'  str_wrap_with('kitty', '\\b')
str_wrap_with <- function(str, f = '(', b = str_wrap_other(f)){
  paste0(f,str,b)
}



#' Title
#'
#' @param replacements a vector of replacements
#' @param ... ignored
#' @param allow_multiple_letters If True (Default) allows repeated letters
#' @param include_spaces_between_letters If not NULL allows these items between letters. Default '[\\s\\_\\-]*'
#'
#' @return a string that is a regex for the letter
#' @export
#'
#' @examples
#' regex_replacements(replacements = c('a', '@', '4','/\\', '\\*'), allow_multiple_letters = FALSE)
#' 
regex_replacements <- function(replacements, 
                               ...,
                               allow_multiple_letters = TRUE, 
                               include_spaces_between_letters = '[\\s\\_\\-]*'){
  replacements |>
    sort(decreasing = T) |>
    stringi::stri_trans_general(id = "Latin-ASCII")  |>
    unique() |>
    paste0(collapse = '|') |>
    str_wrap_with() |>
    {\(.){
      if ( ! is.null(include_spaces_between_letters)){  # If this is not NULL it allows thing alternating stings of and number like "BI1I!i!1I!RCH" other wise you might only find BIIIIIIIIIRCH
        paste0(., include_spaces_between_letters) |>
          str_wrap_with()
      } else{
        .
      }
    }}() |>
    #paste0('[\\s\\_\\-]*') |>
    {\(.) if (allow_multiple_letters) paste0(., '+')  else .}()
}





#' Title Read in a letter replacement file
#'
#' @param fn 
#'
#' @return dataframe with two columns alt and letter
#' @export
#'
#' @examples
#'  read_letter_replace_dat()
read_letter_replace_dat <- function(fn = file.path('data', 'letter_replace.csv')){
  readr::read_csv(fn) |>
    janitor::clean_names() |>
    distinct(letter, alt) |> 
    replace_na(list(letter = ' ')) |>
    arrange(letter, alt) 
    
}
G_LETER_REPLACE_DAT <- read_letter_replace_dat()







#' Title returns either the value from regex_letter_replacements_list or regex_replacements(key)
#'
#' @param key key for alternatives 
#' @param ... passed to regex_replacements
#'
#'
#' @return
#' @export
#'
#' @examples
#'   regex_letter_replacements(key = '1')
#'   regex_letter_replacements(key = 'a', allow_leet_speak = T)
#'   regex_letter_replacements(key = 'a', allow_leet_speak = F)
regex_letter_replacements <- function(key, 
                                      ..., 
                                      allow_leet_speak = TRUE){
  
  if (is.null(G_LETER_REPLACE_DAT)){
    G_LETER_REPLACE_DAT <<- read_letter_replace_dat()
  }
  
  replacements <- 
    if (allow_leet_speak){
      G_LETER_REPLACE_DAT |>
        filter(key == letter) |>
        pull(alt) |>
        c(key)#  -> replacements
    }else{
      key
    } 
  regex_replacements(replacements = replacements, ...)
}





#' Title
#'
#' @param words a vector of words
#'
#' @return
#' @export
#'
#' @examples
#'  make_regex_from_words('beach')
#'  make_regex_from_words(words = 'puck')
#'  make_regex_from_words(words = 'white genicide')
#'  bad_regex <- make_regex_from_words(words = c('tigger', 'puck', 'birch', 'Dile', 'friend', 'house'))
#'  bad_regex <- make_regex_from_words()
#'  make_regex_from_words('fuck', include_spaces_between_letters = NULL, allow_leet_speak = FALSE, allow_multiple_letters = FALSE)
make_regex_from_words <- function(words,
                                  include_spaces_between_letters = '[\\s\\_\\-]*',
                                  allow_multiple_letters = TRUE,
                                  allow_leet_speak = TRUE
){
  
  
  #######################
  # remove words with punctuation
  words2 <- 
    words |>
    str_to_lower() |> 
    unique() 
  #str_replace_all('[[:punct:]]+', ' ') |> 
  #{\(.x){.x[str_detect(.x, '\\s', negate = TRUE)]}}() |>
  #{\(.x){.x[str_detect(.x, '\\+', negate = TRUE)]}}() 
  
  
  #words2 = 'ba'
  words2 |>  
    str_replace_all(pattern = '([[:alpha:]])\\1+', replacement = '\\1' ) |> # remove repeated letters 'TIGGER' -> 'these TIGERs are everywhere why '
    strsplit("") |>
    map(~{
      .x |>                                                                 # .x is a single word!  .x <- c("b", "i", "r", "c","h")
        map(regex_letter_replacements, 
            allow_multiple_letters  = allow_multiple_letters , 
            include_spaces_between_letters  = include_spaces_between_letters, 
            allow_leet_speak = allow_leet_speak) |> # coming out of this map is a list of regex for the charaters in the word without the boundry or joining conditions
        {\(.){
          if ( ! is.null(include_spaces_between_letters)){
            paste0(., collapse = include_spaces_between_letters) # collapse to allow detection of space between letters  'B_I_R_C_H'
          } else{
            paste0(., collapse = '')                            # collapse to force letters together  BIRCH
          }
        }}() |>                                                      
        {\(.)paste0('(\\s+|$|^|[:punct:])',.,'(\\s+|$|^|[:punct:])')}()     # What can come before and after the word   !!!!!!!PUCK!!!!
      #{\(.)paste0('\\b',.,'\\b')}()
    }) |> setNames(words2) #%>%
  #map(~{str_detect(string = 'fuc c ccccck k k asdf abb bbbbb aaaa bbbsdfkadf', pattern = .x)})  
}





#' Title
#'
#' @param phrases 
#' @param a_isl2 
#'
#' @return
#' @export
#'
#' @examples
#' get_regex_categories_phrases()
#' 
get_regex_categories_phrases <- function(phrases = read_phrases_categories() , a_isl2 = '[\\s\\_\\-]*'){
    phrases |> 
    distinct(include_spaces_between_letters ,allow_leet_speak , allow_multiple_letters) %>%
    pmap_dfr(\(include_spaces_between_letters, allow_leet_speak, allow_multiple_letters){
      isl <- include_spaces_between_letters
      als <- allow_leet_speak
      aml <- allow_multiple_letters
      
      isl2 <-   
        if (isl){a_isl2
        }else{NULL}    
      
      
      phrases |> filter (include_spaces_between_letters == isl  &
                           allow_leet_speak == als  &
                           allow_multiple_letters == aml) |>
        mutate(regex = unlist(make_regex_from_words(phrase, 
                                                    include_spaces_between_letters = isl2,
                                                    allow_multiple_letters = aml,
                                                    allow_leet_speak = als) ))
    }) |>
    tidyr::pivot_longer(matches('^category_'), values_drop_na = TRUE, values_to = 'categories') |>
    select(-name, -include_spaces_between_letters , -allow_leet_speak , -allow_multiple_letters ) |>
    distinct() |>
    nest(categories = categories) |>
    relocate(regex, .after = last_col()) #|>
  #unnest(categories)
}

###############################################
#
# phrases categories and regex
#
G_REGEX_CATEGORY_PHRASE <-  get_regex_categories_phrases()


# x <- 'the quick brown fox, jumps over the lazy dog.'
# 
# x |> tokenizers::tokenize_words() |> unlist() |>
#   purrr::map(~{
#     str_extract_all(str, make_regex_from_words(.x)[[1]])
#   })
