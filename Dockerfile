
FROM rstudio/plumber

# install packages
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
    R -e "install.packages('glue')"

# CWD
WORKDIR /

# copy things
# excluding files like README.md in .dockerignore
COPY ./ /app/

# to launch your docker container
CMD ["/app/plumber.R"]