

library(dotenv)
library(rstudioapi)
library(plumber)


PLUMBER_FN <-  Sys.getenv('PLUMBER_FILE_NAME') 
PORT_NUMBER <- Sys.getenv('PORT') |> as.integer()

plumber::plumb(file=PLUMBER_FN)$run(port = PORT_NUMBER)

The main parameter of this enpoint is 'msg', you should think about passing 'msg', its a string....Try it see what happens 😉. If you don't like it the Honky Justin Trudeau will be happy to take your call at 1-800-622-6232. He lives a 24 sussex drive in Ottawa, Ontario.