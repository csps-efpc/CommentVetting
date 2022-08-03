

library(readr)
library(spacyr)
library(tidyr)
library(dplyr)
library(stringr)
library(glue)
source(file.path('R', 'language_detect.R'))


G_CURR_SPACY_MODEL <- ''



#' Returns the name of the spacy model 
#'
#' @param lang language
#' @param mdl_size 
#'
#' @return
#' @export
#'
#' @examples
#'    spacy_mdl_nm(lang = 'en')
#'    spacy_mdl_nm(lang = 'english', mdl_size = 'sm')
spacy_mdl_nm <- function(lang, mdl_size = 'md'){
  lang2 <- lang_code(lang)
  if (lang2 =='en'){
    glue::glue('en_core_web_{mdl_size}')
  }else if  (lang2 %in% 'fr'){
    glue::glue('fr_core_news_{mdl_size}') 
  }else{
    ''
  }
}






#' Title Reloads the language model if it is a new language
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#' reload_language_model(lang = 'english')
#' reload_language_model(lang = 'en')
#' reload_language_model(lang = 'de')
reload_language_model <- function(lang){
  
  
  desired_spacy_mdl <- spacy_mdl_nm(lang = lang)
  
  
  if (desired_spacy_mdl == G_CURR_SPACY_MODEL | !valid_language(lang)){
    message(glue('The Model spacy model {desired_spacy_mdl} is already loaded'))
    return(NULL)
  }
  
  
  message(glue('Unloading the spacy model {G_CURR_SPACY_MODEL} and loading {desired_spacy_mdl}.'))
  tryCatch(
    spacyr::spacy_finalize(),
    error=function(cond) {
      message(glue('There is no spacy model, error = "{cond}"'))
      #return(NULL)
    }
  )  
  
  
  
  spacyr::spacy_initialize(model = desired_spacy_mdl)
  G_CURR_SPACY_MODEL <<- desired_spacy_mdl
}









read_entity_type_dat_CACHE <- NULL
#' Read file that tells us how the spacy models entity depending on how they are looking for
#'
#' @param fn 
#' @param force_reload 
#'
#' @return
#' @export
#'
#' @examples
#'    read_entity_type_dat()
#' 
read_entity_type_dat <- function(fn = file.path('data','entity_from_lang_looking_for.csv'), 
                                 force_reload = FALSE){
  if (is.null(read_entity_type_dat_CACHE) | force_reload){
    read_entity_type_dat_CACHE <<-
      readr::read_csv(fn) |> 
        mutate_all(str_trim) 
  }
  read_entity_type_dat_CACHE
}



#' What we should filter for depends on the language model
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#'    ent_type_from_lang('fr', 'proper_nouns')
#'    ent_type_from_lang('english', 'proper_nouns')
#'    ent_type_from_lang('english', 'address')
ent_type_from_lang <- function(lang, a_looking_for, dat = read_entity_type_dat()){
  a_lang <- lang_code(lang)
  
  ret_val <- 
    dat |>
    filter(lang == a_lang) |>
    filter(looking_for == a_looking_for) |>
    pull(entity) |>
    unique()
  
  
  if (length(ret_val) >= 1){
    return(ret_val)
  }
  
  warning(glue('did not find any entities for {lang}, {a_looking_for}'))
  return(ret_val)
}








#' Filter the data based on what you are looking for and what makes a good name
#'
#' @param spacy_entity_rollup dataframe
#' @param lang like 'english'
#' @param a_looking_for 
#' @param valid_name 
#'
#' @return
#' @export
#'
#' @examples
#'   spacy_filter_entity_and_valid(spacy_entity_rollup, 'english', 'proper_nouns')
#' 
spacy_filter_entity_and_valid <- function(spacy_entity_rollup, 
                                          lang, 
                                          a_looking_for,
                                          valid_name = character()){
  
  possibles <- 
    spacy_entity_rollup |>
    filter(entity %in% ent_type_from_lang(lang,  a_looking_for))
  
  possibles <- 
    if (length(valid_name) > 0){
      possibles |> filter(str_squish(str_to_lower(token)) %in% str_squish(str_to_lower(valid_name)))
    } else {possibles}
  
  return(possibles)
}











# x <- c("We have had 2 prime ministers named Trudeau , the father Trudeau  and the son Trudeau. There is also Martin Brian Mulroney. ", 
#        'we need to run, jump and slide', 
#        'The Ottawa Senators are not going to win the Stanley Cup. Maybe they should recruit Justin Bieber, I hear he is better at hockey then music. Trudeau could be an enforcer as he was a boxer.', 
#        'I live at 20 Bainbridge av., Ottawa, Ontario. I lived at 2559 Hopkins rd and before that at 1052-195 Clearview Ave, while the Queen lives at Buckingham Palace, London, England. Nobody lives at 1234 2nd street, which is the same as 1234 second street. Also 20 ave is not an address. Conner McDavid scored 105 points in 56 games in the pandemic season.')
# 
# 
# x <- c(
#   "Nous avons eu deux premiers ministres nommés Trudeau, le père Trudeau et le fils Trudeau. Il y a aussi Martin Brian Mulroney. ",
#   "nous devons courir, sauter et glisser",
#   "Les Sénateurs d'Ottawa ne vont pas gagner la Coupe Stanley. Ils devraient peut-être recruter Justin Bieber, il paraît qu'il est meilleur au hockey qu'en musique. Trudeau pourrait être un enforcer comme il était boxeur.",
#   "J'habite au 20, av. Bainbridge, Ottawa, Ontario. J'ai vécu au 2559 ch Labrosse et avant cela, au 1052-195 Ave Clearview, tandis que la Reine vit au palais de Buckingham, à Londres, en Angleterre. Personne ne vit au 1234 2e rue, qui est le même que le 1234 deuxième rue ainsi que le 1234 2ème rue. De plus, 20 Ave n'est pas une adresse. Conner McDavid a marqué 105 points en 56 matchs lors de la saison pandémique."
# )






#' Returns a dataframe of 'hits' based on the spacy model specified by language, and vector of filters which are bound togeather by rows
#'
#' @param x vector of message to be checked
#' @param lang a language 
#' @param ... 
#' @param hit_filter_funcs a vector of functions that take 
#'
#' @return
#' @export
#'
#' @examples
#'        spacy_entity_rollup<- get_spacy_model_hits(x = "We have had 2 prime ministers named Trudeau , the father Trudeau  and the son Trudeau. There is also Martin Brian Mulroney. But like there is also Trudeau International Airport, which is code YUL.", lang = 'en')
#'        
#'        get_spacy_model_hits(x = "Nous avons eu deux premiers ministres nommés Trudeau, le père Trudeau et le fils Trudeau. Il y a aussi Martin Brian Mulroney. Mais comme il y a aussi l'aéroport international Trudeau, dont le code est YUL.", lang = 'fr')
#'        
#'        
get_spacy_model_hits <- function(x, 
                                 lang,
                                 ...#,
                                 #hit_filter_funcs = c()
                                 ){
  assertthat::assert_that(length(lang)==1)
  
  x_df <- dplyr::tibble(text = x, index = 1:length(x))
  
  
  reload_language_model(lang = lang)
  
  
  
  
  ###########################
  # Run the SPACY Model that was loaded earlier
  spacy_parse_data <- 
    if (valid_language(lang = lang) &  sum(nchar(x)) > 0){
      spacyr::spacy_parse(x, full_parse = TRUE, pos = TRUE, tag = TRUE, lemma = FALSE, entity = TRUE, dependency = TRUE, nounphrase = TRUE) |> tibble::as_tibble() |> 
        dplyr::mutate(index = as.integer(stringr::str_replace(doc_id, '^text', ''))) |>
        select(-doc_id)
      
      #spacyr::spacy_tokenize(x) |> tibble() |> unnest()
      #spacyr::spacy_extract_nounphrases(x) |> tibble()
      #spacyr::spacy_extract_entity(x, type = a_type) |> tibble()
    }else{
      tibble::tibble(sentence_id = as.integer(),
                     token_id  = as.integer(),
                     token = as.character(),
                     pos = as.character(), 
                     tag = as.character(), 
                     head_token_id = as.character(),
                     dep_rel  = as.character(),
                     entity = as.character(),
                     nounphrase = as.character(),
                     whitespace  = as.logical(),
                     index = as.integer())
    } 
  
  
  
  ##############################
  #
  # add the token start character and end character to the spacy model output
  #
  furthest_token_thus_far = rep(0, length(x))
  spacy_parse_data2 <-
    x_df |> 
    left_join(spacy_parse_data, by = "index") |>
    arrange(index, sentence_id, token_id ) |>
    purrr::pmap_dfr(\(text, token, index, ...){
      fttf <- furthest_token_thus_far[[index]]
      
      st_end <- 
        text |> 
        str_sub(start = fttf+1) |> 
        str_locate( pattern = fixed(token))
      st_end <- st_end + furthest_token_thus_far[[index]] 
      furthest_token_thus_far[[index]] <<- st_end[2]
      
      list(index = index, token = token, token_start =st_end[1], token_end =st_end[2], text = text,  ...)
    })
  
  
  
  
  
  spacy_parse_data2 |> count(pos, tag, sort = TRUE)
  spacy_parse_data2 |> count(entity, sort = TRUE)
  
  # https://towardsdatascience.com/explorations-in-named-entity-recognition-and-was-eleanor-roosevelt-right-671271117218
  # PERSON:      People, including fictional.
  # NORP:        Nationalities or religious or political groups.
  # FAC:         Buildings, airports, highways, bridges, etc.
  # ORG:         Companies, agencies, institutions, etc.
  # GPE:         Countries, cities, states.
  # LOC:         Non-GPE locations, mountain ranges, bodies of water.
  # PRODUCT:     Objects, vehicles, foods, etc. (Not services.)
  # EVENT:       Named hurricanes, battles, wars, sports events, etc.
  # WORK_OF_ART: Titles of books, songs, etc.
  # LAW:         Named documents made into laws.
  # LANGUAGE:    Any named language.
  # DATE:        Absolute or relative dates or periods.
  # TIME:        Times smaller than a day.
  # PERCENT:     Percentage, including ”%“.
  # MONEY:       Monetary values, including unit.
  # QUANTITY:    Measurements, as of weight or distance.
  # ORDINAL:     “first”, “second”, etc.
  # CARDINAL:    Numerals that do not fall under another type.
  
  
  
  
  
  
  ###############################
  #
  # Roll up the spacy results by entity keeping the start and end characters
  #
  spacy_entity_rollup <-
    spacy_parse_data2 |> 
    #filter(! is.na(sentence_id )) |> 
    group_by(index, sentence_id) |>
    group_map(\(.dat, .key){
      #rint(glue('dat = {typeof(.dat)}'))
      #rint(glue('key = {.key}'))
      #print(.key)
      
      #.dat <- spacy_parse_data2 |> filter(index  == 1 & is.na(sentence_id))
      .key <- tibble(index  = 1, sentence_id = as.character(NA))
      entities_roll_up <- list()
      i_entity = 1
      
      curr_entity <- list(start = -1, end = -1, entity = '')
      curr_root <- list(start = -1, end = -1)
      curr_noun <- list(start = -1, end = -1)
      
      #print(.dat)
      #.dat$ #|> filter(nchar(entity) > 0) 
      .dat |> 
        purrr::pwalk(\(entity, token_start, token_end, nounphrase, ...){
          if (is.na(nounphrase )){
            return()
          }
          
          if (str_detect(nounphrase , '^beg_root$')){
            curr_root[['start']] <<-token_start
          }
          if (str_detect(nounphrase , '^beg_root$') | (length(nounphrase) > 0 & curr_root[['start']] == -1)){
            curr_root[['end']] <<- token_end
          }
          if (str_detect(nounphrase , '^beg$')){
            curr_noun[['start']] <<- token_start
          }             
          
          if (str_detect(nounphrase , '^beg$') | (length(nounphrase) > 0 & curr_noun[['start']] == -1)){
            curr_noun[['end']] <<-token_end
          } 
          
          
          if (str_detect(entity, '_B$')){
            
            entities_roll_up[[i_entity]] <<- curr_entity
            i_entity <<- i_entity + 1
            
            #print(glue('Begin curr_length = {length(entities_roll_up)}'))
            curr_entity[['start']] <<- token_start
            curr_entity[['end']] <<- token_end
            curr_entity[['entity']] <<- str_remove(string = entity, pattern = '_B$')
          }
          
          
          if (str_detect(entity, '_I$')){
            #print(entities_roll_up)
            
            
            curr_entity[['end']] <<- token_end
            curr_entity[['entity']] <<- str_remove(string = entity, pattern = '_I$')
            
            entities_roll_up[[i_entity]] <<- curr_entity            
            #i_entity <<- i_entity + 1
            #print(entities_roll_up)
            
          }          
          
          if (nchar(entity) == 0){
            #print(entities_roll_up)
            entities_roll_up[[i_entity]] <<- curr_entity
            i_entity <<- i_entity + 1
            #print(i_entity)
            curr_entity <<- list(start = -1, end = -1, entity = '')
          }          
        })
      
      entities_roll_up[[i_entity]] <- curr_entity
      i_entity = i_entity + 1
      
      #print(entities_roll_up)
      entities_roll_up |> 
        purrr::map_dfr(
          as_tibble  
        ) |> 
        mutate(index = .key$index) |>
        mutate(sentence_id = .key$sentence_id) |> 
        filter(nchar(entity) > 0) |>
        mutate(text = .dat$text[[1]]) |>
        mutate(found = str_sub(text, start, end)) |>
        select(-text)
      
    }) |> 
    bind_rows()
  
  
  
  spacy_entity_rollup
  
  
  
  # hit_filter_funcs |> 
  #   map_dfr(\(.f){
  #     .f(spacy_entity_rollup)
  #   })
}

