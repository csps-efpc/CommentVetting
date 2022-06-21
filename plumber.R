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
#' x = 'am I the asshole, no really am I the asshole'
#' x = 'run devil run devil run'
#' find_categories_from_phrase_regex(x)

#* Echo back the input
#* @param msg The message to check
#* 
#* @get /check_message
function(msg="fb is hiring a new vice president of global policy"){
  get_proper_nouns(x= msg) |> 
    combine_named_lists(get_regex_combined(x= msg)) |> 
    combine_named_lists(b = find_categories_from_phrase_regex_lst(x= msg))
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
