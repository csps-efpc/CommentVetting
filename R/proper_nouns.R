


library(spacyr)
library(tidyr)
library(dplyr)
library(stringr)
library(glue)
source(file.path('R', 'language_detect.R'))


G_CURR_SPACY_MODEL <- ''


#' Title Reloads the language model if it is a new language
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
reload_language_model <- function(lang){
  
  if (lang == G_CURR_SPACY_MODEL | !valid_language(lang)){
    return(NULL)
  }
  
  
  tryCatch(
    spacyr::spacy_finalize(),
    error=function(cond) {
      message(glue('There is no spacy model, error = "{cond}"'))
      #return(NULL)
    }
  )  
  #spacy_install(lang_models = c("en_core_web_sm"))
  #spacy_install(lang_models = c("fr_core_news_sm"))
  #fr_core_news_md
  if (lang == 'english')  {spacyr::spacy_initialize(model = "en_core_web_sm")}
  if (lang == 'french')  {spacyr::spacy_initialize(model = "fr_core_news_sm")}
  G_CURR_SPACY_MODEL <<- lang
}




#' Title What we should filter for depends on the language model
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
ent_type_from_lang <- function(lang){
  if (lang == 'english')  {return('PERSON')}
  if (lang == 'french')  {return('PER')}
}


#' Title
#'
#' @param x 
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#' y <- get_proper_nouns(x =  c('run fast', 'The Ottawa Senators are not going to win the Stanley Cup. Maybe they should recruit Justin Bieber, I hear he is better at hockey then music. Trudeau could be an enforcer as he was a boxer.', 'Who is the informant Deep Thoat? Its pseudonym of Bob Woodward!'))
#' get_proper_nouns(x = 'run fast')
# x = 'we have had 2 prime ministers named Trudeau , the father Trudeau  and the son Trudeau.'
#' x = "in 1812 madison was mad he was the president, you know but he thought he tell the british where they ought to go"
#' 
get_proper_nouns <- function(x, 
                             lang, 
                             ...,
                             a_type = "all", 
                             a_ent_type = ent_type_from_lang(lang)){
  # x = 'The MP for Ottawa West Nepean can be reached at anita.vandenbeld@parl.gc.ca, while the MPP can be contacted @ 613-721-8075'
  #x = "Les Sénateurs d'Ottawa ne vont pas gagner la Coupe Stanley. Ils devraient peut-être recruter Justin Bieber, il paraît qu'il est meilleur au hockey qu'en musique. Trudeau pourrait être un enforcer comme il était boxeur."
  #x <- c("We have had 2 prime ministers named Trudeau , the father Trudeau  and the son Trudeau. There is also Brian Mulroney. ", 'we need to run, jump and slide', 'The Ottawa Senators are not going to win the Stanley Cup. Maybe they should recruit Justin Bieber, I hear he is better at hockey then music. Trudeau could be an enforcer as he was a boxer.')
  #x = 'run away'
  
  assertthat::assert_that(length(lang)==1)
  
  
  reload_language_model(lang = lang)
  
  spacy_dat <- 
    if (valid_language(lang = lang)){
        x |> 
        spacyr::spacy_extract_entity(type = a_type)
    }else{
      NULL
    }
    
  persons_dat <-  
    if (is.null(spacy_dat)){
        tibble::tibble(index = as.integer(),
                       text = as.character())
    }else{
        spacy_dat |>
        tibble::as_tibble() |>
        dplyr::filter(ent_type == a_ent_type) |> 
        dplyr::mutate(index = as.integer(stringr::str_replace(doc_id, '^text', '')))
    } |>
    dplyr::distinct(text, index)
    

  dplyr::tibble(text = x, index = 1:length(x)) |>
    dplyr::left_join(persons_dat |> rename(found:=text), by = "index") |>
    dplyr::mutate(hit_type = 'proper_nouns') |>
    dplyr::select(index, found, hit_type, text) |>
    dplyr::mutate(location = str_locate_all(text, found)) |>
    tidyr::unnest(cols = location) %>%
    dplyr::mutate(location = as_tibble(location)) |>
    tidyr::unnest_wider(location) |> 
    dplyr::rename(any_of(c(start = 'V1', end = 'V2'))) |>
    select(-text)

  
  
  
  # purrr::map(1:length(x), ~{
  #   text <- x[.x]
  #   found <- persons_dat |> dplyr::filter(index == .x) |> dplyr::pull(text)
  #   list(
  #     text = text,
  #     proper_nouns = purrr::map(found, \(.f){
  #       list(found = .f,
  #            location = stringr::str_locate(string = text, pattern = .f)
  #     )}))
  # })
}




