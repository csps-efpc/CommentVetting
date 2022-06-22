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



# msg = "Wayne Gretzky is known as the fucking great one."
# msg = "Michael Jordan is the GOAT."
# msg = "Sir I respectfully disagree with you, for the reason that you are a pimp."
#' msg = 'Lawrence Taylor was a coke head. Which explains a why he was an assssssssssho0000000le on the field, Robin William was also, that explains why he was so hyper in his comedy. Maybe we should contact Wllness Togeather Canada at 1-866-585-0445 to help them, or should we say fuckit and be asshole our self and let them be? No Really we should call 866-585-0445, nigger'
#' 
#msg = "Lawrence Taylor était accro à la coke. Ce qui explique pourquoi il était un connard sur le terrain, Robin William l'était aussi, ce qui explique pourquoi il était si hyperactif dans sa comédie. Peut-être devrions-nous contacter Wllness Togeather Canada au 1-866-585-0445 pour les aider, ou devrions-nous dire fuckit et être des connards nous-mêmes et les laisser faire ? Non vraiment, on devrait appeler le 866-585-0445, négro."


#' x = 'am I the asshole, no really am I the asshole'
#' x = 'run devil run devil run'
#' find_categories_from_phrase_regex(x)

#* Echo back the input
#* @param msg The message to check
#* 
#* @get /check_message
function(msg="The main parameter of this enpoint is 'msg', you should think about passing 'msg', its a string....Try it see what happens 😉. If you don't like it the Honky Justin Trudeau will be happy to take your call at 1-800-622-6232"){
  langs <- get_language(msg)
  ret_lst <- 
    get_proper_nouns(x= msg, langs) |> 
    combine_named_lists(b = get_regex_combined(x= msg, lang = langs)) |> 
    combine_named_lists(b = find_categories_from_phrase_regex_lst(x= msg, lang = langs))    
  
  ret_lst2 <-
    map2(ret_lst, langs,
         ~{
        .x[['lang']] <- .y
        .x
      })
  
  
}



#* 
#* 
#* 
#* @get /test_dat
function(){
  iris |> dplyr::as_tibble()
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
