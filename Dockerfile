
FROM rstudio/plumber

# Ensure that the undeclared igraph dependency on libglpk0 and python gets fulfilled.
RUN apt-get update && apt-get install -y libglpk40

# intall Python packages

# RUN pip install -U pip setuptools wheel; \
#    pip install -U spacy
    
# RUN python3 -m spacy download en_core_web_sm; \
#    python3 -m spacy download fr_core_news_md

# install R packages
RUN R -e "install.packages(c(\
      'spacyr', \
      'tidyr', \
      'dplyr', \
      'stringr', \
      'textcat', \
      'fastText', \
      'readr', \
      'janitor', \
      'readxl', \
      'purrr', \
      'stringi', \
      'tidytext', \
      'quanteda', \
      'tibble', \
      'numbers', \
      'stringdist', \
      'tm', \
      'textreuse', \
      'ggplot2', \
      'httr', \
      'jsonlite', \
      'igraph', \
      'tokenizers', \
      'gtools', \
      'glue', \
      'arrow', \
      'dotenv', \
      'memoise', \
      'reticulate', \
      'testthat' ))"

# Use Reticulate to install miniconda

RUN R -e "reticulate::install_miniconda()"

# Load the two spacy models

RUN R -e "spacyr::spacy_install(prompt = FALSE, lang_models = 'en_core_web_md')"; \
    R -e "spacyr::spacy_install(prompt = FALSE, lang_models = 'fr_core_news_md')"

# Run the test harness

# RUN R -e "devtools::test()"

# CWD
WORKDIR /

# copy things
# excluding files like README.md in .dockerignore
COPY ./ /app/

# to launch your docker container
CMD ["/app/plumber.R"]
