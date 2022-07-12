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




#* @apiTitle Comments Vetting



G_FIRST_TIME <- Sys.time()
#* Echo back the input
#* @param msg The message to echo
#* @get /up
function(){
  toc <- Sys.time()
  list(msg = glue("The service is up, started at {G_FIRST_TIME}, current time'{toc}'. uptime is {round(difftime(toc, G_FIRST_TIME, 'mins'),0)} min"))
}



#' # msg = "Michael Jordan is the GOAT."
#' # msg = "Sir I respectfully disagree with you, for the reason that you are a pimp."
#' #' msg = 'Lawrence Taylor was a coke head. Which explains a why he was an assssssssssho0000000le on the field, Robin William was also, that explains why he was so hyper in his comedy. Maybe we should contact Wllness Togeather Canada at 1-866-585-0445 to help them, or should we say fuckit and be asshole our self and let them be? No Really we should call 866-585-0445, nigger'
#' #'
#' msg = c("Lawrence Taylor était accro à la coke. Ce qui explique pourquoi il était un connard sur le terrain, Robin William l'était aussi, ce qui explique pourquoi il était si hyperactif dans sa comédie. Peut-être devrions-nous contacter Wllness Togeather Canada au 1-866-585-0445 pour les aider, ou devrions-nous dire fuckit et être des connards nous-mêmes et les laisser faire ? Non vraiment, on devrait appeler le 866-585-0445, négro.",
#' 'This is maybe a valid SIN 046 454 286 I found on the crap ass wikipedia founded by the Liberal fucktards Jimmy Whales and Larry Sanger as a bastion of slavery!!!!, but this is not 123 456 789 seee! asswipe!!!',
#' 'Das ist vielleicht eine gültige SIN 046 454 286, die ich auf der Scheiß-Wikipedia gefunden habe, die von den liberalen Arschlöchern Jimmy Whales und Larry Sanger als eine Bastion der Sklaverei gegründet wurde!!!!, aber das ist nicht 123 456 789 seee! Arschgeige!!!',
#' 'am I the asshole, no really am I the asshole',
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
    select(-index)



  dat2 <- make_grouped_list(dat, grping =  'imsg',lst_attach = c('text', 'langs', 'imsg'), rest = 'hits' )
  dat2
  #
  #
  #
  #
  # dat |>
  #   group_split(!!sym(grping))  |> # magrittr::extract2(1) ->.grp_dat
  #   map(\(.grp_dat){
  #     ret_lst <- list()
  #     lst_attach |>
  #       walk(~{
  #         ret_lst[[.x]] <<- .grp_dat[[.x]] |> unique()
  #         .grp_dat[[.x]] <<- NULL
  #         })
  #     ret_lst[['dat']] <- .grp_dat
  #     ret_lst
  #   })
  # #.msg_dat <-
  #   dat |>
  #   group_split(imsg) |> #magrittr::extract2(7)
  #   map(\(.msg_dat){
  #     .msg_dat_lang <- txt_langs |> inner_join(.msg_dat)
  #     list(text = .msg_dat_lang$text |> unique(),
  #          lang = lang_name_to_code(.msg_dat_lang$langs |> unique()),
  #          hits = .msg_dat_lang |>
  #                 select(-text, -langs) |>
  #                 group_split(hit_type) |> #magrittr::extract2(1) -> .msg_hit_dat
  #                    map(\(.msg_hit_dat){
  #                      list(
  #                         type = .msg_hit_dat$hit_type |> unique(),
  #                         sub_type = .msg_hit_dat |>
  #                                   select(-hit_type) |>
  #                                   group_split(sub_type) |> #magrittr::extract2(1) -> .msg_hit_sub_dat
  #                                       map(\(.msg_hit_sub_dat){
  #                                         list(
  #                                           type = .msg_hit_dat$hit_type |> unique(),
  #                                           sub_type = .msg_hit_dat |>
  #                                             select(-hit_type) |>
  #                                             group_split(sub_type) |> #magrittr::extract2(1) -> .msg_hit_sub_dat
  #                                         )
  #                                       })
  #                      )
  #                    })
  #
  #            )
  #   })

  # msg_dat |> group_split(imsg) %>% setNames(unique(msg_dat$imsg))
  #


  # ret_lst <-
  #   get_proper_nouns(x= msg, lang = langs) |>
  #   combine_named_lists(b = get_regex_combined(x= msg, lang = langs)) |>
  #   combine_named_lists(b = find_categories_from_phrase_regex_lst(x= msg, lang = langs))
  #
  #
  # lengths(ret_lst)
  #
  # ret_lst2 <-
  #   map2(ret_lst, langs,
  #        ~{
  #       .x[['lang']] <- .y
  #       .x
  #     })
  #
  # ret_lst2
  # #jsonlite::toJSON(ret_lst2, auto_unbox=TRUE)
}



#*
#*
#*
#* @get /test_dat
function(){
  iris |> dplyr::as_tibble()
}



#*
#*
#* @get /test_in
function(a){
  print(a)
  print(typeof(a))
  length(a)

}





#
#
# #* Plot a histogram
# #* @get /plot
# #* @serializer contentType list(type='image/png')
# function(){
#   p <-
#     iris |>
#     ggplot2::ggplot(ggplot2::aes(x =Sepal.Length, fill = Species)) +
#     ggplot2::geom_density()
#
#   fn <- 'file.svg'
#   grDevices::svg(fn, p)
#
#   ggsave(fn,p)
#   readBin(fn,'raw',n = file.info(fn)$size)
# }
#
# #* Return the sum of two numbers
# #* @param a The first number to add
# #* @param b The second number to add
# #* @post /sum
# function(a, b){
#   as.numeric(a) + as.numeric(b)
# }
#
