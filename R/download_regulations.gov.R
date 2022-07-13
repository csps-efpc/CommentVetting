
library(stringdist)
library(tidytext)
library(tm)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(textreuse)
library(tibble)
library(ggplot2)
library(httr)
library(glue)
library(jsonlite)
library(stringr)
library(arrow)
library(dotenv)


max_pages = 100
max_comments = 1000
commenting_on = '09000064846eebaf'
max_requests_per_minute = 50
max_requests_per_hour = 500
data_save_dir <- file.path('private', 'regulations.gov')

PAST_REQUESTS <- tibble(time = Sys.time())









#' Title get the API results but check if they will actually come back, and wait till they will come back
#'
#' @param url 
#' @param requests_per_minute 
#' @param requests_per_hour 
#'
#' @return
#' @export
#'
#' @examples
get_gov_api <- function(url, 
                        requests_per_minute = max_requests_per_minute, 
                        requests_per_hour = max_requests_per_hour
                        ){
  
  one_hour_ago  <- Sys.time() - 3600
  one_minute_ago  <- Sys.time() - 60  
  

  if (    
  PAST_REQUESTS |>
    filter(time >= one_minute_ago) |> 
    nrow() >= requests_per_minute
  ){
    
    sleep_for <- difftime(PAST_REQUESTS |> filter(time >= one_minute_ago) |> pull(time) |> min() + 60 , Sys.time(), units = 'secs' )
      
    
    
    print(glue('{Sys.time()} Sleeping for {sleep_for} seconds as we used {PAST_REQUESTS |>filter(time >= one_minute_ago) |> nrow()} requests'))
    if (sleep_for > 0){
      Sys.sleep(sleep_for)
    }
  }
  
  if (    
    PAST_REQUESTS |>
    filter(time >= one_hour_ago) |> 
    nrow() >= requests_per_hour
  ){
    sleep_for <- difftime(PAST_REQUESTS |> filter(time >= one_hour_ago) |> pull(time) |> min() + 3600 , Sys.time(), units = 'secs' )
    
    print(glue('{Sys.time()} Sleeping for {sleep_for} seconds as we used {PAST_REQUESTS |>filter(time >= one_hour_ago) |> nrow()} requests'))
    if (sleep_for > 0){
      Sys.sleep(sleep_for)
    }
  } 
  
 
  PAST_REQUESTS <<- PAST_REQUESTS |> bind_rows(tibble(time = Sys.time()))
  print(glue('{Sys.time()} getting {url}'))
  return(GET(url))
}









#' Title
#'
#' @param url_template 
#' @param gov_api_key 
#'
#' @return
#' @export
#'
#' @examples
gov_api_get <- function(commenting_on = '09000064846eebaf',
                        url_template = "https://api.regulations.gov/v4/comments?filter[commentOnId]={commenting_on}&page[size]=250&page[number]={i_page}&sort=lastModifiedDate,documentId&api_key={gov_api_key}",
                        gov_api_key = Sys.getenv('REGULATIONS_GOVE_API_KEY'),
                        max_num_page = max_pages){
  
  i_page <- 1
  one_more_time <- TRUE
  pages_dat <- list()
  
  while (one_more_time){
    page <- 
      url_template  |> 
      glue() |>
      #get_api(api_id = gov_api_key) |>
      get_gov_api() |>
      #GET() |>
      content("text") |> 
      jsonlite::parse_json()
    
    pages_dat[[i_page]] <- 
      page$data |>
      purrr::map_dfr(~{
        as_tibble(.x[['attributes']]) |>
          mutate(links = .x[['links']][['self']],
                 id = .x[['id']],
                 type = .x[['type']]
          ) 
      })
    page$meta$hasNextPage
    if (page$meta$hasNextPage & i_page < max_num_page){
      one_more_time <<- TRUE
      i_page <- i_page + 1
      print(glue('current page = {i_page}'))
    }else {
      print(glue('last page'))
      one_more_time <- FALSE
    }
    
  }
  
  
  
  comments_index_all <- bind_rows(pages_dat)
  comments_index_all 
}

#' Title gets details of comments
#'
#' @param ids 
#' @param gov_api_key 
#' @param url_template 
#'
#' @return
#' @export
#'
#' @examples
get_coments_details <- function(ids = comments_index_all$id, 
                                gov_api_key = Sys.getenv('REGULATIONS_GOVE_API_KEY'), 
                                url_template = "https://api.regulations.gov/v4/comments/{id}?api_key={gov_api_key}" 
){
  i_id <- 1
  dat<- 
    ids |>
    #sample(20) |>
    map_dfr(\(id){
      #id <- ids[[5]] 
      i_id <<- i_id + 1
      comment <- 
        url_template  |> 
        glue() |>
        #get_api(api_id = gov_api_key) |>
        get_gov_api() |>
        #GET() |>
        content("text") |> 
        jsonlite::parse_json()
      comment$data$attributes[sapply(comment$data$attributes, is.null)] <- NULL
      print(glue('i_id = {i_id}'))
      
      as_tibble(comment$data$attributes) |>
        mutate(id = id)
      
    })
  
  dat
}
sample_n_max <- function(x, size, ...){
  n2 <- min(size, nrow(x))
  x |> sample_n(size = n2, ...)
}


sample__max <- function(x, size, ...){
  n2 <- min(size, nrow(x))
  x |> sample(size = n, ...)
}



comments_index_all <- gov_api_get(commenting_on = commenting_on, max_num_page = 501)
comments_index_all |> arrow::write_feather(file.path(data_save_dir, glue('comments_index_all_{commenting_on}.feather')))
comments <- tibble(id = as.character())

comments <- 
  comments_index_all |> 
  anti_join(comments |> select(id), by = 'id') |>
  sample_n_max(max_comments) |> 
  pull(id)  |>
  unique() |> 
  get_coments_details() |> 
  bind_rows(comments)


comments |> arrow::write_feather(file.path(data_save_dir, glue('comments_details_{commenting_on}.feather')))
comments |> write_csv(file.path(data_save_dir, glue('comments_details_{commenting_on}.csv')))
comments <- arrow::read_feather(file.path(data_save_dir, glue('comments_details_{commenting_on}.feather')))
