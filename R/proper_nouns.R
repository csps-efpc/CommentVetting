

library(spacyr)

G_CURR_SPACY_MODEL <- ''
reload_language_model <- function(lang){
  
  if (lang == G_CURR_SPACY_MODEL){
    return()
  }
  
  
  tryCatch(
    spacyr::spacy_finalize(),
    error=function(cond) {
      message(cond)
      return(NULL)
    }
  )  
  
  if (lang == 'english')  {spacyr::spacy_initialize(model = "en_core_web_sm")}
  if (lang == 'french')  {spacyr::spacy_initialize(model = "fr_core_news_sm")}
  G_CURR_SPACY_MODEL <- lang
}

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
#' y <- get_proper_nouns(x =  c('run fast', 'The Ottawa Senators are not going to win the Stanley Cup. Maybe they should recruit Justin Bieber, I hear he is better at hockey then music. Trudeau could be an enforcer as he was a boxer.'))
#' get_proper_nouns(x = 'run fast')

#' 
#' 
get_proper_nouns <- function(x, 
                             lang, 
                             ...,
                             a_type = "all", 
                             a_ent_type = ent_type_from_lang(lang)){
  # x = 'The MP for Ottawa West Nepean can be reached at anita.vandenbeld@parl.gc.ca, while the MPP can be contacted @ 613-721-8075'
  #x = "Les Sénateurs d'Ottawa ne vont pas gagner la Coupe Stanley. Ils devraient peut-être recruter Justin Bieber, il paraît qu'il est meilleur au hockey qu'en musique. Trudeau pourrait être un enforcer comme il était boxeur."
    

  reload_language_model(lang = lang)
  
  
  
  spacy_dat <- 
    x |> 
    spacyr::spacy_extract_entity(type = a_type)
    #spacy_extract_nounphrases()
    #spacyr::spacy_parse() |> tibble::as_tibble() |> dplyr::filter(pos == 'PROPN' )
  
  
  persons_dat <-  
    if (is.null(spacy_dat)){
        tibble::tibble(index = as.integer(),
                       text = as.character())
    }else{
        spacy_dat |>
        tibble::as_tibble() |>
        dplyr::filter(ent_type == a_ent_type) |> 
        dplyr::mutate(index = as.integer(stringr::str_replace(doc_id, '^text', '')))
    }
    
  
     
  
  
  
  purrr::map(1:length(x), ~{
    text <- x[.x]
    found <- persons_dat |> dplyr::filter(index == .x) |> dplyr::pull(text)
    list(
      text = text,
      proper_nouns = purrr::map(found, \(.f){
        list(found = .f,
             location = stringr::str_locate(string = text, pattern = .f)
      )}))
  })
}

