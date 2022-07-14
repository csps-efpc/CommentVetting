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
#* report the uptime of the service 
#* @get /up
function(){
  toc <- Sys.time()
  list(msg = glue("The service is up, started at {G_FIRST_TIME}, current time'{toc}'. uptime is {round(difftime(toc, G_FIRST_TIME, 'mins'),0)} min"))
}



#* run some detection algorithms on the msgs
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
function(f, col_nm = 'comment', max_rows = -1) {
  #Filename
  #print('names ',names(f))
  # data_save_dir <- file.path('private', 'regulations.gov')
  # fn <- list.files(data_save_dir, pattern = '^comments_details_.*\\.feather$', full.names = TRUE) |> sample(1)
  # f <- list(arrow::read_feather(fn))
  dat <- f[[1]]
  if (max_rows > 0 & max_rows < nrow(dat)){
    dat <- head(dat, max_rows)
  }
  
  print(glue('NAMES FROM FILE {names(dat)}, col_nm = {col_nm}'))

  
  
  grps <- find_near_duplicats(dat[[col_nm]])
  make_grouped_list(grps, grping =  'grp',lst_attach = c('grp', 'name', 'grp_size', 'b_str'), rest = 'comments' )
  
  # 
  # #Content  
  # f[[1]]
}


