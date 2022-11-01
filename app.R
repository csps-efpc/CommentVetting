

library(dotenv)
library(rstudioapi)
library(plumber)


PLUMBER_FN <-  Sys.getenv('PLUMBER_FILE_NAME') 
PORT_NUMBER <- Sys.getenv('PORT') |> as.integer()

plumber::plumb(file=PLUMBER_FN)$run(port = PORT_NUMBER)

