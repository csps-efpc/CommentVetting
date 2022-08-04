
FROM rstudio/plumber

# Ensure that the undeclared igraph dependency on libglpk0 gets fulfilled.
RUN apt-get update && apt-get install -y libglpk40

# install R packages
RUN R -e "install.packages('spacyr')" \
    R -e "install.packages('tidyr')" \
    R -e "install.packages('dplyr')" \
    R -e "install.packages('stringr')" \
    R -e "install.packages('textcat')" \
    R -e "install.packages('fastText')" \
    R -e "install.packages('readr')" \
    R -e "install.packages('janitor')" \
    R -e "install.packages('readxl')" \
    R -e "install.packages('purrr')" \
    R -e "install.packages('stringi')" \
    R -e "install.packages('tidytext')" \
    R -e "install.packages('quanteda')" \
    R -e "install.packages('tibble')" \
    R -e "install.packages('numbers')" \
    R -e "install.packages('stringdist')" \
    R -e "install.packages('tm')" \
    R -e "install.packages('textreuse')" \
    R -e "install.packages('ggplot2')" \
    R -e "install.packages('httr')" \
    R -e "install.packages('jsonlite')" \
    R -e "install.packages('igraph')" \
    R -e "install.packages('tokenizers')" \
    R -e "install.packages('gtools')" \
    R -e "install.packages('glue')" \
    R -e "install.packages('arrow')" \
    R -e "install.packages('dotenv')" \
    R -e "install.packages('memoise')"

# CWD
WORKDIR /

# copy things
# excluding files like README.md in .dockerignore
COPY ./ /app/

# to launch your docker container
CMD ["/app/plumber.R"]
