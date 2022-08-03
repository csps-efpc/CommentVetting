


library(spacyr)
library(tidyr)
library(dplyr)
library(stringr)
library(glue)
source(file.path('R', 'language_detect.R'))
source(file.path('R', 'spacy_model.R'))




#' get the entity that is the proper_noun in spacy based on language
#'
#' @param lang 
#'
#' @return
#' @export
#'
#' @examples
#' ent_type_from_lang_proper_noun('german')
#' ent_type_from_lang_proper_noun('english')
ent_type_from_lang_proper_noun <- function(lang){
  ent_type_from_lang(lang,  'proper_nouns')
}





#' return just the proper nouns
#'
#' @param spacy_entity_rollup 
#' @param lang 
#' @param a_looking_for 
#' @param valid_name 
#'
#' @return
#' @export
#'
#' @examples
#' "We have had 2 prime ministers named Trudeau , the father Trudeau  and the son Trudeau." |> get_spacy_model_hits('en') |> spacy_filter_proper_noun('en')
#' "Nous avons eu deux premiers ministres nommés Trudeau, le père Trudeau et le fils Trudeau."  |> get_spacy_model_hits('fr') |> spacy_filter_proper_noun('fr')
#' 
#' 
spacy_filter_proper_noun <- function(spacy_entity_rollup, lang, a_looking_for = 'proper_nouns', valid_name = character()){
  spacy_entity_rollup |>
    spacy_filter_entity_and_valid(lang = lang, a_looking_for = a_looking_for, valid_name = valid_name)
}






#' return a dataframe of entities found that are 
#'
#' @param x 
#' @param lang 
#' @param a_ent_type 
#'
#' @return
#' @export
#'
#' @examples
#'   get_proper_nouns(x = c("We have had 2 prime ministers named Trudeau , the father Trudeau  and the son Trudeau. There is also a town Trudeau, Ontario."), lang = 'english')
#'   get_proper_nouns(c("Nous avons eu deux premiers ministres nommés Trudeau, le père Trudeau et le fils Trudeau."), lang = 'fr')
#'   x <- "in 1812 madison was mad he was the president, you know but he thought he tell the british where they ought to go"
get_proper_nouns <- function(x, 
                             lang, 
                             a_looking_for = 'proper_nouns',
                             a_ent_type = ent_type_from_lang(lang, a_looking_for)
                             ){
  x  |> 
    get_spacy_model_hits(lang) |> 
    spacy_filter_proper_noun(lang) |>
    mutate(hit_type = a_looking_for) |>
    select(-sentence_id ) |>
    rename(sub_type := entity)
}

#   
#   
#   spacy_parse_data2 |> count(nounphrase)
#   
#   spacy_parse_data2 |> 
#     group_by(index, sentence_id) |>
#     group_map(\(.dat, .key){
#       #print(.dat)
#       # .dat <- spacy_parse_data2 |> filter(index  == 1 & sentence_id == 1)
#       # .key <- tibble(index = 1, sentence_id = 1)
#       roll_ups <- list()
#       i = 1
#       
#       main_noun <- list(start = -1, end = -1)
#       root_noun_phrase <- list(start = -1, end = -1)
#       
# 
#       .dat |> 
#         purrr::pwalk(\(nounphrase, token_start, token_end, entity, ...){
#           
#           if (str_detect(entity, '_B$')){
#             print('do thing')
#             main_noun[['entity']] <<- str_remove(entity, '_B$')
#           }
#           
#           #print(i)
#           if (str_detect(nounphrase, '^beg_root$')){
#             print('^beg_root$')  
#             roll_ups[[i]] <<- list(main_noun = main_noun, root_noun_phrase = root_noun_phrase)
#             i <<- i + 1
#             
#             #print(glue('Begin curr_length = {length(entities_roll_up)}'))
#             root_noun_phrase[['start']] <<- token_start
#             root_noun_phrase[['end']] <<- token_end
#             main_noun[['start']] <<- token_start
#             main_noun[['end']] <<- token_end
#           }
#           
#           if (str_detect(nounphrase, '^beg$')){
#             print('^beg$')  
#             roll_ups[[i]] <<- list(main_noun = main_noun, root_noun_phrase = root_noun_phrase)
#             i <<- i + 1
#             #print(glue('Begin curr_length = {length(entities_roll_up)}'))
#             main_noun[['start']] <<- token_start
#             main_noun[['end']] <<- token_end
#           }          
#           
#           
#           if (str_detect(nounphrase, '^end_root$')){
#             print('^end_root$')  
#             root_noun_phrase[['end']] <<- token_end
#             main_noun[['end']] <<- token_end
#             
#                         
#             roll_ups[[i]] <<- list(main_noun = main_noun, root_noun_phrase = root_noun_phrase)
#             i <<- i + 1
#             
#             main_noun <<- list(start = -1, end = -1)
#             root_noun_phrase <<- list(start = -1, end = -1)
#             
#           }                    
#           
#           
#           if (str_detect(nounphrase, '^mid$')){
#             print('^mid$')              
#             root_noun_phrase[['end']] <<- token_end
#             main_noun[['end']] <<- token_end
#           }          
#           
#           if (nchar(nounphrase) == 0){
#             
#           }          
#         })
#       
#       
#       print('end inner walk')            
#       roll_ups[[i]] <- list(main_noun = main_noun, root_noun_phrase = root_noun_phrase)
#       
#       roll_ups |>
#         purrr::map_dfr(\(.x){
#           tibble(noun_start = .x$main_noun$start,
#                  noun_end = .x$main_noun$end,
#                  entity = .x$main_noun$entity,
#                  root_noun_start = .x$root_noun_phrase$start,
#                  root_noun_end = .x$root_noun_phrase$end,
#                  )
#           
#         }) |>
#          mutate(index = .key$index) |>
#          mutate(sentence_id = .key$sentence_id) |> 
#       #   filter(nchar(type) > 0) |>
#          mutate(text = .dat$text[[1]]) |>
#          # filter(noun_start != -1) |> 
#          mutate(token = str_sub(text, noun_start , noun_end  )) |>
#         mutate(token_parent = str_sub(text, root_noun_start , root_noun_end  )) |>
#         select(-text)
#     }) |> 
#     bind_rows() 
#     
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   
#   spacy_parse_data2 |> 
#     mutate(str_sub(text, token_start, token_end)) |>
#     View()
#   str_locate(str_sub(x[[4]],start = 92+1) , '.')
#   
#   spacy_parse_data
#   
# 
#   spacy_dat2 <-  
#     if (is.null(spacy_dat)){
#       tibble::tibble(index = as.integer(),
#                      text = as.character(),
#                      ent_type = as.character())
#     }else{
#       spacy_dat |>
#         tibble::as_tibble() |>
#         dplyr::mutate(index = as.integer(stringr::str_replace(doc_id, '^text', '')))
#     } |>
#     #dplyr::distinct(text, index, ent_type)  |>
#     rename(found := text) |>
#     right_join(x_df, by = "index")
# 
#   
#   
#   hit_filter_funcs |> 
#     map_dfr(\(.f){
#       .f(spacy_dat2)
#     })
# }



#' 
#' 
#' spacy_hit_filter_orgs <- function(x_df, 
#'                                   a_ent_type = 'ORG',
#'                                   hit_type = 'ORGANIZATION'){
#'   x_df |> 
#'     filter(ent_type == a_ent_type) |> 
#'     dplyr::mutate(hit_type = hit_type)
#' }
#' 
#' 
#' 
#' 
#' 
#' 
#' spacy_hit_filter_proper_nouns <- function(spacy_dat, 
#'                                           a_ent_type = ent_type_from_lang(lang),
#'                                           hit_type = 'proper_nouns'){
#'   
#'   spacy_dat |> filter(pos == 'PROPN') |> 
#'     filter(str_detect(entity, '^PERSON_'))
#'   
#'   spacy_dat |> count(pos)
#'   x_df |> 
#'     filter(ent_type == a_ent_type) |> 
#'     dplyr::mutate(hit_type = hit_type)
#' }
#' 
#' 
#' 
#' 
#' #' Title
#' #'
#' #' @param x 
#' #' @param lang 
#' #'
#' #' @return
#' #' @export
#' #'
#' #' @examples
#' #' y <- get_proper_nouns(x =  c('run fast', 'The Ottawa Senators are not going to win the Stanley Cup. Maybe they should recruit Justin Bieber, I hear he is better at hockey then music. Trudeau could be an enforcer as he was a boxer.', 'Who is the informant Deep Thoat? Its pseudonym of Bob Woodward!'))
#' #' get_proper_nouns(x = 'run fast')
#' # x = 'we have had 2 prime ministers named Trudeau , the father Trudeau  and the son Trudeau.'
#' #' x = "in 1812 madison was mad he was the president, you know but he thought he tell the british where they ought to go"
#' #' 
#' get_proper_nouns <- function(x, 
#'                              lang, 
#'                              ...,
#'                              a_type = "all", 
#'                              a_ent_type = ent_type_from_lang(lang)){
#'   # x = 'The MP for Ottawa West Nepean Anita Vandenbeld can be reached at anita.vandenbeld@parl.gc.ca, while the MPP can be contacted @ 613-721-8075'
#'   #x = "Les Sénateurs d'Ottawa ne vont pas gagner la Coupe Stanley. Ils devraient peut-être recruter Justin Bieber, il paraît qu'il est meilleur au hockey qu'en musique. Trudeau pourrait être un enforcer comme il était boxeur."
#'   #x <- c("We have had 2 prime ministers named Trudeau , the father Trudeau  and the son Trudeau. There is also Martin Brian Mulroney. ", 'we need to run, jump and slide', 'The Ottawa Senators are not going to win the Stanley Cup. Maybe they should recruit Justin Bieber, I hear he is better at hockey then music. Trudeau could be an enforcer as he was a boxer.', 'I live at 20 Bainbridge Ave, Ottawa, Ontario, while the Queen lives in buckingham palace, London England.')
#'   #x = 'run away'
#'   
#'   assertthat::assert_that(length(lang)==1)
#'   
#'   
#'   reload_language_model(lang = lang)
#'   
#'   spacy_dat <- 
#'     if (valid_language(lang = lang)){
#'         x |> 
#'         spacyr::spacy_extract_entity(type = a_type)
#'     }else{
#'       NULL
#'     }
#'     
#'   persons_dat <-  
#'     if (is.null(spacy_dat)){
#'         tibble::tibble(index = as.integer(),
#'                        text = as.character())
#'     }else{
#'         spacy_dat |>
#'         tibble::as_tibble() |>
#'         dplyr::filter(ent_type == a_ent_type) |> 
#'         dplyr::mutate(index = as.integer(stringr::str_replace(doc_id, '^text', '')))
#'     } |>
#'     dplyr::distinct(text, index)
#'     
#' 
#'   dplyr::tibble(text = x, index = 1:length(x)) |>
#'     dplyr::left_join(persons_dat |> rename(found:=text), by = "index") |>
#'     dplyr::mutate(hit_type = 'proper_nouns') |>
#'     dplyr::select(index, found, hit_type, text) |>
#'     dplyr::mutate(location = str_locate_all(text, found)) |>
#'     tidyr::unnest(cols = location) %>%
#'     dplyr::mutate(location = as_tibble(location)) |>
#'     tidyr::unnest_wider(location) |> 
#'     dplyr::rename(any_of(c(start = 'V1', end = 'V2'))) |>
#'     select(-text)
#' 
#'   
#'   
#'   
#'   # purrr::map(1:length(x), ~{
#'   #   text <- x[.x]
#'   #   found <- persons_dat |> dplyr::filter(index == .x) |> dplyr::pull(text)
#'   #   list(
#'   #     text = text,
#'   #     proper_nouns = purrr::map(found, \(.f){
#'   #       list(found = .f,
#'   #            location = stringr::str_locate(string = text, pattern = .f)
#'   #     )}))
#'   # })
#' }
#' 
#' 
#' 

