library(httr)
library(glue)
library(purrr)
library(dplyr)


# https://irene.rbind.io/post/iterative-testing-plumber/

#' for testing an endpoint 
#'
#' @param server 
#' @param port 
#' @param endpoint 
#' @param params 
#' @param expected_response_code 
#'
#' @return
#' @export
#'
#' @examples
test_get <- function(
    server = 'http://127.0.0.1',
    port = 9302,
    endpoint = 'check_message',
    httr_method = httr::GET,
    params = list(),
    expected_response_code  = 200
    ){
  
  url_temp = URLencode(glue('{server}:{port}/{endpoint}?'))
  url <- 
    names(params) |>
    map(\(.param){
      .value = params[[.param]]
      URLencode(glue('{.param}={.value}')) |> paste0(collapse = "&")
    }) |>
    paste0(collapse = '&') %>%
    paste0(url_temp, .)
  r <- httr_method(url)

  assertthat::assert_that(r$status_code == expected_response_code)
  httr::content(r)
}



test_letter_writing_campaign <- function(
    server = 'http://127.0.0.1',
    fn = list.files(file.path('private', 'regulations.gov'), pattern = '^comments_details_.*\\.csv$', full.names = TRUE) |> head(1),
    port = 9302,
    endpoint = 'letter_writing_campaign',
    httr_method = httr::POST,
    params = list(col_nm='comment'),
    expected_response_code  = 200
){
  
  url_temp = URLencode(glue('{server}:{port}/{endpoint}?'))
  url <- 
    names(params) |>
    map(\(.param){
      .value = params[[.param]]
      URLencode(glue('{.param}={.value}')) |> paste0(collapse = "&")
    }) |>
    paste0(collapse = '&') %>%
    paste0(url_temp, .)
  
  
  r <- httr_method(url, body = list(f = httr::upload_file(fn)))
  assertthat::assert_that(r$status_code == expected_response_code)
  httr::content(r)
}




extract_hit_2 <- function(resp, hit){
  str_sub(resp$text, start = hit$start, end = hit$end)
}
  


extract_hit <- function(resp, ih = 1){
  hit = resp$hits[[ih]]
  str_sub(resp$text, start = hit$start, end = hit$end)
}



hit_contains_property <- function(resp , val, prop){
  resp$hits |> map(~{
    .x[[prop]] == val
  }) |>
    unlist() |>
    any()
}


hit_contains_type <- function(resp , val){
  hit_contains_property(resp, val = val, prop = 'hit_type')
}



hit_contains_sub_type <- function(resp , val){
  hit_contains_property(resp, val = val, prop = 'sub_type')
}




hit_contains_types <- function(resp , vals){
  #hit_types = c('phone_number', 'profanity','proper_nouns')
  
  vals |> map(~{
    hit_contains_type(resp = resp, val = .x)
  }) |> unlist()
}


hit_contains_sub_types <- function(resp , vals){

  vals |> map(~{
    hit_contains_sub_type(resp = resp, val = .x)
  }) |> unlist()
}






str_clean_up <- function(a){
  str_extract_all(a , '[[:alnum:]]+') |> unlist() |> paste0(collapse = '') |> str_to_lower()
}




str_kinda_close <- function(a,b, clean_func = str_clean_up){
  clean_func(a) == clean_func(b)
}




hit_in_correct_location <- function(hit, resp, ...){
  str_kinda_close(hit$found, extract_hit_2(resp = resp, hit = hit,  ...))
}




hits_in_correct_location <- function(resp, ...){
  resp$hits |> 
    map(\(.h){
      hit_in_correct_location(hit = .h, resp = resp)
    }) |> unlist() |> all()
}




this_hit_is <- function(hit, hit_type, start, end){
  hit$hit_type == hit_type & hit$start == start & hit$end == end
}


hit_found_at <- function(resp, hit_type, start, end){
  resp$hits |> 
    map(~{
      this_hit_is(.x, hit_type, start, end)
    }) |> 
    unlist() |>
    any()
}




test_resp <- function(resp, hit_types, sub_types, nms , lang, required_hits = tibble(hit_type = as.character(), 
                                                                                     start =    as.integer(),
                                                                                     end =      as.integer()) ){
  
  resp$lang |> expect_equal(lang, info = 'wrong language')
  (resp |> names() == nms) |> all()   |> expect_true(info = glue('a name is missing'))
  hits_in_correct_location(resp) |> expect_true(info = glue('wrong location on a hit'))
  hit_contains_types(resp, hit_types) |> all()  |>expect_true(info = glue('type missing'))
  hit_contains_sub_types(resp,sub_types) |> all() |> expect_true(info = glue('sub type missing'))
  
  
  
  
  pmap(required_hits, \(hit_type, start, end){
    hit_found_at(resp = resp, hit_type = hit_type,start = start, end = end)
  }) |> 
    unlist() |>
    all() |> 
    expect_true(info = glue('required hit is missing, {hit_type}, {start}, {end}'))
  
  TRUE
}






test_check_message <- function(msg,hit_types, sub_types, nms, lang, required_hits = tibble(hit_type = as.character(), 
                                                                                           start =    as.integer(),
                                                                                           end =      as.integer()) ){
  r <- test_get(endpoint = 'check_message',  params = list(msg = msg))
  r |> length() |> expect_equal(length(msg))  
  test_resp(resp = r[[1]], 
            hit_types = hit_types,  
            sub_types = sub_types , 
            nms= nms, 
            lang = lang, 
            required_hits = required_hits)
}





test_that("service is up", 
{
  r <-test_get(endpoint = 'up')
  expect_equal(names(r),'msg')
})




test_that("error returned which is good", 
{
  r <-test_get(endpoint = 'does not exist', expected_response = 404)
  expect_equal(names(r),'error')
})



test_that("check_message is working", 
{
  
    
    test_check_message(msg = 'Ryan Gosling likes to eat poop, because it tastes good. What a f.u.c.k.i.n.g. Wierdo!, call and complaing at 1 (800) 622-6232.', 
                       hit_types = c('phone_number', 'profanity','proper_nouns'),  
                       sub_types = c('fucking', 'north_america') , 
                       nms= c("text", "lang", "imsg", "hits"), 
                       lang = 'en')
    
    
    test_check_message(msg = "Hello out there, we're on the air, it's 'Hockey Night' tonight. Tension grows, the whistle blows, and the puck goes down the ice.", 
                       hit_types = c(),  
                       sub_types = c() , 
                       nms= c("text", "lang", "imsg", "hits"), 
                       lang = 'en')
    
    
    
    test_check_message(msg = "The goalie jumps, and the players bump, and the fans all go insane. Someone roars, Bobby Scores!, at the good ol' Hockey Game", 
                       hit_types = c('proper_nouns'),  
                       sub_types = c() , 
                       nms= c("text", "lang", "imsg", "hits"), 
                       lang = 'en')
    
    
    test_check_message(msg = "in 1812 madison was mad he was the president, you know but he thought he tell the british where they ought to go", 
                       hit_types = c('proper_nouns'),  
                       sub_types = c() , 
                       nms= c("text", "lang", "imsg", "hits"), 
                       lang = 'en')
    
    test_check_message(msg = "My momma taught me long ago remember this one thing Surround yourself with losers so that you can be their king And if you choose to fall in love, pick someone with a curse 'Cause no matter how bad you may feel, she'll always feel much worse.", 
                       hit_types = c(),  
                       sub_types = c() , 
                       nms= c("text", "lang", "imsg", "hits"), 
                       lang = 'en')
    
    
    test_check_message(msg = "This is maybe a valid SIN 046 454 286 I found on the crap ass wikipedia founded by the Liberal fucktards Jimmy Whales and Larry Sanger as a bastion of slavery!!!!, but this is not 123 456 789 seee! asswipe!!!", 
                       hit_types = c('proper_nouns', 'sin_number'),  
                       sub_types = c() , 
                       nms= c("text", "lang", "imsg", "hits"), 
                       lang = 'en', 
                       required_hits = 
                         tibble(hit_type = c('proper_nouns', 'sin_number'), 
                                start =    c('106'         , '27'),
                                end =      c('117'         , '37'))   
    )
    
    
    test_check_message(msg = "Et pis toé, mon p'tit gars, tu l'sais pu c'que tu vas faire Dans ton p'tit trois et demi bin trop cher, frette en hiver Il te vient des envies de devenir propriétaire Et tu rêves la nuit d'avoir ton petit lopin d'terre", 
                       hit_types = c(),  
                       sub_types = c() , 
                       nms= c("text", "lang", "imsg", "hits"), 
                       lang = 'fr', 
                       
    )    
    
    test_check_message(msg = "Vive la Canadienne et ses jolis yeux doux Et ses jolis yeux doux doux doux, et ses jolis yeux doux (bis)", 
                       hit_types = c(),  
                       sub_types = c() , 
                       nms= c("text", "lang", "imsg", "hits"), 
                       lang = 'fr' 
    )    
    
    test_check_message(msg = "Mon père n'avait fille que moi Je le mène bien mon dévidoi Un jour sur la mer il m'envoit", 
                       hit_types = c(),  
                       sub_types = c() , 
                       nms= c("text", "lang", "imsg", "hits"), 
                       lang = 'fr' 
    )        
    
    
    test_check_message(msg = "Dear Assistant Director, Office of Policy, Executive Office for Immigration Review Lauren Alder Reid, I strongly oppose the proposed rule by the Homeland Security Department and the Executive Office for Immigration Review on Procedures for Asylum and Withholding of Removal; Credible Fear and Reasonable Fear Review (RIN 1125-AA94 or EOIR Docket No. 18-0002). Sincerely, Cheryl Esposito of Brooklyn, NY 11219 sagethe7th@aol.com", 
                       hit_types = c('e_mail', 'proper_nouns'),  
                       sub_types = c() , 
                       nms= c("text", "lang", "imsg", "hits"), 
                       lang = 'en',
                       required_hits = 
                         tibble(hit_type = c('proper_nouns', 'proper_nouns', 'e_mail'), 
                                start =    c('372'         , '91'          , '410'),
                                end =      c('386'         , '100'         , '427'))                        
    )     
    
    
    test_check_message(msg = "Madame la directrice adjointe du bureau des politiques de l'Executive Office for Immigration Review, Lauren Alder Reid, je m'oppose fermement à la règle proposée par le département de la sécurité intérieure et l'Executive Office for Immigration Review concernant les procédures d'asile et de suspension de l'expulsion, la crainte crédible et la crainte raisonnable (RIN 1125-AA94 ou EOIR Docket No. 18-0002). Sincèrement, Cheryl Esposito de Brooklyn, NY 11219 sagethe7th@aol.com", 
                       hit_types = c('e_mail', 'proper_nouns'),  
                       sub_types = c() , 
                       nms= c("text", "lang", "imsg", "hits"), 
                       lang = 'fr',
                       required_hits = 
                         tibble(hit_type = c('proper_nouns', 'proper_nouns', 'e_mail'), 
                                start =    c('1'         , '102'          , '461'),
                                end =      c('6'         , '118'         , '478'))                        
    )         
    
    
    test_check_message(msg = "Sehr geehrte stellvertretende Direktorin, Office of Policy, Executive Office for Immigration Review Lauren Alder Reid, ich lehne die vorgeschlagene Regelung des Homeland Security Department und des Executive Office for Immigration Review zu Verfahren für Asyl und Zurückhaltung der Abschiebung; Credible Fear und Reasonable Fear Review (RIN 1125-AA94 oder EOIR Docket No. 18-0002) entschieden ab. Mit freundlichen Grüßen, Cheryl Esposito aus Brooklyn, NY 11219 sagethe7th@aol.com", 
                       hit_types = c('e_mail'),  
                       sub_types = c() , 
                       nms= c("text", "lang", "imsg", "hits"), 
                       lang = 'de',
                       required_hits = 
                         tibble(hit_type = c('e_mail'), 
                                start =    c(462),
                                end =      c(479))                        
    )      
})





















is_group_size_correct <- function(resp_grp){
  resp_grp$comments |> length() == resp_grp$grp_size
}








test_that("letter_writing_campaign is working", 
{
  
  r <- test_letter_writing_campaign()
  length(r) |> expect_gte(10)

  r[1:(length(r)-1)] |> map(is_group_size_correct) |>
    unlist() |>
    all() |>
    expect_true(info = 'all group sizes are correct')
  
  r_comments <- 
    r |> 
      map(\(.grp){
        .grp$comments |> map(\(.comment){
          .comment$comment_id
        }) |> unlist()
      }) |> unlist()
  
  expect_equal(length(r_comments), length(unique(r_comments)), info = 'all comments are not unique')
  
  r <- test_letter_writing_campaign(fn = file.path('test_data','bigHateSpeech.csv'), 
                                    params = list(col_nm='text', max_rows = 10000 )
                                    )

  r[1:(length(r)-1)] |> map(is_group_size_correct) |>
    unlist() |>
    all() |>
    expect_true(info = 'all group sizes are correct')
  
  r_comments <- 
    r |> 
    map(\(.grp){
      .grp$comments |> map(\(.comment){
        .comment$comment_id
      }) |> unlist()
    }) |> unlist()
  
  expect_equal(length(r_comments), length(unique(r_comments)), info = 'all comments are not unique')
  
})

