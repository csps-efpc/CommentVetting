#
# This is a Plumber API. In RStudio 1.2 or newer you can run the API by
# clicking the 'Run API' button above.
#
# In RStudio 1.1 or older, see the Plumber documentation for details
# on running the API.
#
# Find out more about building APIs with Plumber here:
#
#    https://www.rplumber.io/
#

library(plumber)
#library(ggplot2)
library(glue)
source(file.path('R','proper_nouns.R'))
source(file.path('R','simple_regex_finders.R'))
source(file.path('R','utils.R'))
source(file.path('R','generate_regex.R'))
source(file.path('R','phrase_regex.R'))
source(file.path('R','language_detect.R'))
source(file.path('R','min_hash.R'))



#* @apiTitle Comments Vetting



G_FIRST_TIME <- Sys.time()
#* Echo back the input
#* @get /up
function(){
  toc <- Sys.time()
  list(msg = glue("The service is up, started at {G_FIRST_TIME}, current time'{toc}'. uptime is {round(difftime(toc, G_FIRST_TIME, 'mins'),0)} min"))
}

#http://127.0.0.1:7152/check_message?msg=Michael%20Jordan%20is%20the%20GOAT.&msg=Wayne%20Gretzky%20is%20known%20as%20the%20fucking%20great%20one.&msg=Quel%20est%20le%20trajet%20de%20l%27Ontario%20%C3%A0%20Montr%C3%A9al%20?&msg=F%C3%BCr%20eine%20gute%20Zeit%20rufen%20Sie%201800%20765%204321%20an%20und%20fragen%20Sie%20nach%20Jimmy.&msg=%E4%B8%AD%E5%9B%BD%E7%9A%84%E9%A2%86%E5%AF%BC%E4%BA%BA%E6%98%AF%E4%B9%A0%E8%BF%91%E5%B9%B3&msg=%F0%9F%92%A9%20is%20Canada%27s%20and%20Howard%27s%20favourite%20emoji!

#' # msg = "Michael Jordan is the GOAT."
#' #' # msg = "Sir I respectfully disagree with you, for the reason that you are a pimp."
#' #' #' msg = 'Lawrence Taylor was a coke head. Which explains a why he was an assssssssssho0000000le on the field, Robin William was also, that explains why he was so hyper in his comedy. Maybe we should contact Wllness Togeather Canada at 1-866-585-0445 to help them, or should we say fuckit and be asshole our self and let them be? No Really we should call 866-585-0445, nigger'
#' #' #' 
#' #' msg = c("Lawrence Taylor était accro à la coke. Ce qui explique pourquoi il était un connard sur le terrain, Robin William l'était aussi, ce qui explique pourquoi il était si hyperactif dans sa comédie. Peut-être devrions-nous contacter Wllness Togeather Canada au 1-866-585-0445 pour les aider, ou devrions-nous dire fuckit et être des connards nous-mêmes et les laisser faire ? Non vraiment, on devrait appeler le 866-585-0445, négro.", 
#'  'This is maybe a valid SIN 046 454 286 I found on the crap ass wikipedia founded by the Liberal fucktards Jimmy Whales and Larry Sanger as a bastion of slavery!!!!, but this is not 123 456 789 seee! asswipe!!!',
#'  'Das ist vielleicht eine gültige SIN 046 454 286, die ich auf der Scheiß-Wikipedia gefunden habe, die von den liberalen Arschlöchern Jimmy Whales und Larry Sanger als eine Bastion der Sklaverei gegründet wurde!!!!, aber das ist nicht 123 456 789 seee! Arschgeige!!!',
#'  'am I the asshole, no really am I the asshole', 
#' 'Run devil run devil run.',"Michael Jordan is the GOAT.", "Wayne Gretzky is known as the fucking great one.")
#' #' find_categories_from_phrase_regex(x)
#str_extract(' asdfas  123-456-789 aaaaa', '\b[:digit:]{3}([.\\s\\_\\-]*)[:digit:]{3}([.\\s\\_\\-]*)[:digit:]{3}\b' )
#* Echo back the input
#* @param msg The message to check
#* 
#* @get /check_message
#* @serializer unboxedJSON
function(msg="The main parameter of this enpoint is 'msg', you should think about passing 'msg', its a string....Try it see what happens 😉. If you don't like it the Honky Justin Trudeau will be happy to take your call at 1-800-622-6232"){
  
  txt_langs <- 
    tibble(imsg = 1:length(msg),
           text = msg) |>
    mutate(langs = get_language(text))
  
  
  dat <- 
    txt_langs$langs |> 
    unique() |> 
    map_dfr(\(lng){
      #print(lng)
      txts <- 
        txt_langs |>
        filter(langs == lng) |>
        mutate(index = row_number())
      
      bind_rows(
        get_proper_nouns(x= txts$text, lang = lng) ,
        get_regex_combined(x= txts$text, lang = lng),
        find_categories_from_phrase_regex(x= txts$text, lang = lng)
      ) |>
        right_join(txts |> select(-text, -langs), by = "index")
    }) |>
    filter(!is.na(found) & !is.na(start)) |>
    right_join(txt_langs, by = "imsg") |> 
    arrange(imsg, hit_type, sub_type, start, end, found) |>
    rename(lang := langs) |>
    mutate(lang = lang_name_to_code(lang)) |>
    select(-index)
  
  
  
  make_grouped_list(dat, grping =  'imsg',lst_attach = c('text', 'lang', 'imsg'), rest = 'hits' )
  
}






#* @plumber
function(pr) {
  register_parser("csv",
                  parser_csv,
                  fixed = c("application/csv",
                            "application/x-csv",
                            "text/csv",
                            "text/x-csv",
                            "application/vnd.ms-excel"))
}

#* @post /letter_writing_campaign
#* @parser multi
#* @parser csv
#* @param f:file
#* @serializer unboxedJSON
function(f, col_nm = 'comment') {
  #Filename
  names(f)
  # data_save_dir <- file.path('private', 'regulations.gov')
  # fn <- list.files(data_save_dir, pattern = '^comments_details_.*\\.feather$', full.names = TRUE) |> sample(1)
  # f <- list(arrow::read_feather(fn))
  dat <- f[[1]]
  print(glue('NAMES FROM FILE {names(f)}, col_nm = {col_nm}'))

  
  
  grps <- find_near_duplicats(dat[[col_nm]])
  make_grouped_list(grps, grping =  'grp',lst_attach = c('grp', 'name', 'grp_size', 'b_str'), rest = 'comments' )
  
  # 
  # #Content
  # f[[1]]
}


