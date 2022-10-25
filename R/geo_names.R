
library(tools)
library(janitor)
library(readr)
library(stringr)
library(glue)
library(memoise)
#
# https://www.nrcan.gc.ca/earth-sciences/geography/download-geographical-names-data/9245
#

# https://ftp.cartes.canada.ca/pub/nrcan_rncan/vector/geobase_cgn_toponyme/prov_csv_eng/cgn_ab_csv_eng.zip





#' takes a file name and replaces the extention with a new one
#'
#' @param fn file name
#' @param new_ext new extention default 'csv'
#' @param curr_ext 
#'
#' @return
#' @export
#'
#' @examples
#'   replace_extension_fn('poop.zip')
#' 
replace_extension_fn <- function(fn, new_ext = 'csv', curr_ext = tools::file_ext(fn)){
  str_replace(fn, curr_ext, new_ext)
}




#' Given a region and language return the url to get the names from https://ftp.cartes.canada.ca/pub/nrcan_rncan/vector/geobase_cgn_toponyme/
#'
#' @param  region two letter PT code ie 'ab', 'on' or the word 'canada' for all canada or the term 'idgn' for indigious names 
#' @param lang either eng or fra 
#' @param url_base 
#'
#' @return
#' @export
#'
#' @examples
#' download_geo_names_canada_ca(region = 'ab', lang = 'english')
#' 
geo_names_canada_ca_url <- function(region = 'canada', 
                                    lang = 'en', 
                                    url_base = 'https://ftp.cartes.canada.ca/pub/nrcan_rncan/vector/geobase_cgn_toponyme/prov_csv_{lang3}/{prefix}_{region}_csv_{lang3}.zip'
                                    ){
  prefix <- ''
  lang3 <- ''
  if (lang_code(lang)  == 'en'){
    lang3 <- 'eng'
    prefix <- 'cgn'
  }
  
  if (lang_code(lang) == 'fr'){
    lang3 <- 'fra'
    prefix <- 'tc'
  }  
  
  if (str_to_lower(region) %in% c('canada', 'ca')){
    region <- 'canada'
  }  
  
  
  
  glue::glue(url_base)
}






#' Download the zip file from https://ftp.cartes.canada.ca/pub/nrcan_rncan/vector/geobase_cgn_toponyme, if it does not already exist
#'
#' @param region two letter PT code ie 'ab', 'on' or the word 'canada' for all canada or the term 'idgn' for indigious names
#' @param lang either eng or fra
#' @param url 
#' @param to_be_saved_name 
#' @param geo_data_dir 
#'
#' @return
#' @export
#'
#' @examples
#' download_geo_names_canada_ca(region = 'ab')
#' 
download_geo_names_canada_ca <- function(
    region = 'canada',
    lang = 'en',
    url = geo_names_canada_ca_url(region = region, lang = lang),
    to_be_saved_name = basename(url),
    geo_data_dir = file.path('private', 'geo_data')
    ){
  
  local_fn <- file.path(geo_data_dir, to_be_saved_name)
  
  
  if (! file.exists(local_fn)){
    download.file(url,local_fn, quiet = TRUE, cacheOK = TRUE, extra    = '--insecure')
  }
  return(local_fn)
}





#' reads a dataframe from a zipped file where there is one file inside the zip file
#'
#' @param local_fn filename with the .zip
#' @param new_ext assumes the inside 
#' @param read_func used to read the 'inside' of the zip file
#' @param ... 
#'
#' @return
#' @export
#'
#' @examples
read_zipped_file <- function(local_fn, new_ext = 'csv', read_func = readr::read_csv, ...){
  inner_fn <- replace_extension_fn(basename(local_fn), new_ext = new_ext, ...)
  read_func(unz(local_fn, inner_fn)) |>
    janitor::clean_names()
  
}
  



#' downloads place names from canada
#'
#' @param region two letter PT code ie 'ab', 'on' or the word 'canada' for all canada or the term 'idgn' for indigious names
#' @param lang either eng or fra
#' @param new_ext extension of inside file
#'
#' @return
#' @export
#'
#' @examples
#'    read_geo_names_canada_ca_data(region = 'pe', lang = 'french')
#'    read_geo_names_canada_ca_data(region = 'pe', lang = 'english')
#'    read_geo_names_canada_ca_data(region = 'pe', lang = 'en')
#'    read_geo_names_canada_ca_data(region = 'ca', lang = 'english')
#'    read_geo_names_canada_ca_data(region = 'pe', lang = 'german')
.read_geo_names_canada_ca_data <- function(region = 'canada', 
                                          lang = 'english', 
                                          new_ext = 'csv'
                                          ){
  
  eng_col_nms <- 
    c("cgndb_id", "geographical_name", "language", 
      "syllabic_form", "generic_term", "generic_category", 
      "concise_code", "toponymic_feature_id", "latitude", "longitude", 
      "location", "province_territory", "relevance_at_scale", "decision_date", "source")
  
  dat <- 
    if ( ! valid_language(lang)){
      download_geo_names_canada_ca(region = region, lang  = 'en' )  |> 
        read_zipped_file(new_ext = new_ext)  
      
    }else{
      download_geo_names_canada_ca(region = region, lang  = lang ) |> 
        read_zipped_file(new_ext = new_ext)  
    }
  
  
  
  
  

  
  
  #dat |> setNames(eng_col_nms) |> count(province_territory)
  
  dat |> 
    setNames(eng_col_nms) |> 
    #filter(str_detect(geographical_name, 'Nepean')) |>
    #sample_n(10) |>
    select(geographical_name, latitude, longitude,   concise_code) |> 
    rename(type := concise_code ) |> 
    #separate_rows(alternatenames, sep = ',') |> 
    #pivot_longer(cols = c(name, asciiname, alternatenames), names_to = 'col', values_to = 'name', values_drop_na = TRUE) |>
    #select(-col) |>
    mutate(name = str_to_lower(geographical_name)) |> 
    select(-geographical_name) |>
    distinct() |>
    mutate(geo_id = row_number()) |> 
    group_by(geo_id) |>  mutate(name_id = row_number()) |> ungroup() |>
    tidytext::unnest_tokens(word, name, drop = FALSE) |>
    group_by(geo_id, name_id) |>  mutate(word_id = row_number())|> ungroup() |>
    relocate(geo_id, name_id, 
             word_id, 
             name, 
             word, 
             type) |>
    mutate(country_code  = 'CA') |> 
    mutate(source = 'canada.ca') |>
    mutate(lang = str_sub(lang, 1,2))
}




read_geo_names_canada_ca_data <- memoise::memoise(.read_geo_names_canada_ca_data)



#' returns a vector of column names, based on this file https://download.geonames.org/export/dump/readme.txt
#'
#' @return
#' @export
#'
#' @examples
get_geom_names_col_names <- function(){
  #
  # https://download.geonames.org/export/dump/readme.txt
  #
  #
  
  
  x <- 
"geonameid         : integer id of record in geonames database
name              : name of geographical point (utf8) varchar(200)
asciiname         : name of geographical point in plain ascii characters, varchar(200)
alternatenames    : alternatenames, comma separated, ascii names automatically transliterated, convenience attribute from alternatename table, varchar(10000)
latitude          : latitude in decimal degrees (wgs84)
longitude         : longitude in decimal degrees (wgs84)
feature class     : see http://www.geonames.org/export/codes.html, char(1)
feature code      : see http://www.geonames.org/export/codes.html, varchar(10)
country code      : ISO-3166 2-letter country code, 2 characters
cc2               : alternate country codes, comma separated, ISO-3166 2-letter country code, 200 characters
admin1 code       : fipscode (subject to change to iso code), see exceptions below, see file admin1Codes.txt for display names of this code; varchar(20)
admin2 code       : code for the second administrative division, a county in the US, see file admin2Codes.txt; varchar(80) 
admin3 code       : code for third level administrative division, varchar(20)
admin4 code       : code for fourth level administrative division, varchar(20)
population        : bigint (8 byte int) 
elevation         : in meters, integer
dem               : digital elevation model, srtm3 or gtopo30, average elevation of 3''x3'' (ca 90mx90m) or 30''x30'' (ca 900mx900m) area in meters, integer. srtm processed by cgiar/ciat.
timezone          : the iana timezone id (see file timeZone.txt) varchar(40)
modification date : date of last modification in yyyy-MM-dd format"
  
  x |> 
    str_split('\n') |> magrittr::extract2(1) |>
    str_sub(1, 18) |> 
    str_trim() |>
    make_clean_names()
}
    
  
  


#' reads in 
#'
#' @param external_fn ether 'allCountries' (Default) or a two letter country code
#' @param base_url 
#' @param new_ext inner text
#' @param read_func reading inner File
#'
#' @return
#' @export
#'
#' @examples
#'  read_geo_names_org_data(external_fn = 'CA')
#'  read_geo_names_org_data(external_fn = 'CW')
#' 
.read_geo_names_org_data <- function(external_fn = 'allCountries', 
                                    base_url = 'https://download.geonames.org/export/dump/{external_fn}.zip', 
                                    new_ext = 'txt',  
                                    read_func = \(x, ...){readr::read_tsv(x, ..., col_names = get_geom_names_col_names())}
                                    ){
  dat <- 
    download_geo_names_canada_ca(url = glue::glue(base_url)) |> 
    read_zipped_file(new_ext = new_ext, read_func = read_func)
  
  dat |> 
    #sample_n(1234) |>
    select(name, asciiname, alternatenames, latitude, longitude, country_code,feature_code  ) |> 
    rename(type := feature_code) |>
    mutate(geo_id = row_number()) |>
    separate_rows(alternatenames, sep = ',') |> 
    pivot_longer(cols = c(name, asciiname, alternatenames), names_to = 'col', values_to = 'name', values_drop_na = TRUE) |>
    select(-col) |>
    mutate(name = str_to_lower(name)) |> 
    distinct() |>
    group_by(geo_id) |>  mutate(name_id = row_number()) |> ungroup() |>
    #tidytext::unnest_tokens(word, name,drop = FALSE) |> 
    #group_by(geo_id, name_id) |>  mutate(word_id = row_number())|> ungroup() |>
    relocate(geo_id, name_id, 
             #word_id, 
             name, 
             #word, 
             type) |>
    mutate(source = 'geonames.org')
  
}

read_geo_names_org_data <- memoise::memoise(.read_geo_names_org_data)





spacy_filter_geo_names <- function(spacy_entity_rollup, 
                                   lang, 
                                   a_looking_for = 'address', 
                                   a_ent_type = ent_type_from_lang(lang, a_looking_for),
                                   valid_name_df = read_geo_names_canada_ca_data(lang = lang)
                                   ){
  spacy_entity_rollup |> 
    spacy_filter_entity_and_valid(lang = lang, 
                                  a_looking_for = a_looking_for) |> 
    mutate(found = str_squish(str_to_lower(found))) |>
    inner_join(valid_name_df |> select(name, latitude, longitude)
               , by = c('found' = 'name')) 
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
#'   get_proper_nouns(x = c('I live at 20 Bainbridge Ave. Ottawa, Ontario, but Justin Trudeau lives at 24 Sussex Drive.'), lang = 'english')
#'   x = c("We have had 2 prime ministers named Trudeau , the father Trudeau  and the son Trudeau. There is also a town Trudeau, Ontario.")
#'   get_proper_nouns(c("Nous avons eu deux premiers ministres nommés Trudeau, le père Trudeau et le fils Trudeau."), lang = 'fr')
get_geo_names <- function(x, 
                             lang, 
                             a_looking_for = 'address',
                             a_ent_type = ent_type_from_lang(lang, a_looking_for)
){
  x  |> 
    get_spacy_model_hits(lang) |> 
    spacy_filter_geo_names(lang = lang, a_looking_for = a_looking_for, a_ent_type = a_ent_type) |>
    mutate(hit_type    = a_looking_for) |>
    rename(sub_type := entity) |>
    select(-sentence_id)
  
}






# 
# load_geo_names_data_CACHE
# load_geo_names_data <- function(){
#   ca_eng <- read_geo_names_canada_ca_data(lang = 'en')
#   ca_fr <- read_geo_names_canada_ca_data(lang = 'fr')
#   world_eng <- read_geo_names_org_data()
# }





# geo_dat <- read_geo_names_canada_ca_data(lang = lang)
# geo_dat <- read_geo_names_org_data()

# spacy_dat2 |> 
#   mutate(name = str_to_lower(found)) |>
#   inner_join(geo_dat |>  
#                distinct(name, type, latitude, longitude, country_code) |> 
#                rename(geo_type := type) , 
#              by = 'name')



# ca_dat |>
#   filter(name == 'trudeau')
# 
# 
# x  <- 'The simpsons live at 742 Evergreen Terrace, Springfield, USA'
# 
# x  <- 'I live at 20 Bainbridge Ave. Ottawa, Ontario, but Justin Trudeau lives at 24 Sussex Drive.'
# x <- "J'habite au 20, avenue Bainbridge. Ottawa, Ontario, mais Justin Trudeau habite au 24, promenade Sussex."
# assertthat::assert_that(length(lang)==1)
# 
# 
# reload_language_model(lang = 'english')
# 
# spacyr::spacy_finalize()
# spacyr::spacy_initialize(model = "en_core_web_md")
# spacyr::spacy_initialize(model = "fr_core_news_md")
# 
# spacy_dat <- 
#   if (valid_language(lang = 'english')){
#     x |> 
#       spacyr::spacy_extract_entity(type = 'all')
#   }else{
#     NULL
#   }
# 
# 
# of_interest_dat <-  
#   if (is.null(spacy_dat)){
#     tibble::tibble(index = as.integer(),
#                    text = as.character())
#   }else{
#     spacy_dat |>
#       tibble::as_tibble() |>
#       #dplyr::filter(ent_type == a_ent_type) |> 
#       dplyr::mutate(index = as.integer(stringr::str_replace(doc_id, '^text', '')))
#   } |>
#   dplyr::distinct(text, index) |>
#   mutate(name := str_to_lower(text)) |>
#   select(-text)
# 
# 
# 
# 
# 
# inner_join(
#   of_interest_dat,
#   ca_dat
# )
# spacy_dat <- 
#   spacy_dat |> 
#   mutate(name = str_to_lower(text)) |>
#   select(-text)
# 
# spacy_dat
# 
# ca_dat <- read_geo_names_canada_ca_data()
# 
# spacy_dat
# 
# inner_join(
#   spacy_dat,
#   ca_dat
# )
# spacy_dat
