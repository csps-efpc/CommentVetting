
FROM rstudio/plumber

RUN apt-get update
# Preemptively install libglpk. igraph has an unmanaged dependency on it.
RUN apt-get install -y libglpk40

# install Python and Spacy packages into the R environment
# Python dependency tree resolution can take a *very* long time.
RUN R -e "install.packages(c('spacyr', 'reticulate'))"

RUN R -e "reticulate::install_miniconda()"

RUN R -e "spacyr::spacy_install(prompt = FALSE, lang_models = 'en_core_web_md')"

# install the more typical R packages
RUN R -e "install.packages(c('tidyr', 'dplyr', 'stringr', 'textcat', 'fastText', 'readr', 'janitor', 'readxl', 'purrr', 'stringi', 'tidytext', 'quanteda', 'tibble', 'numbers', 'stringdist', 'tm', 'textreuse', 'ggplot2', 'httr'))" \
    R -e "install.packages(c('jsonlite', 'igraph', 'tokenizers', 'gtools', 'glue', 'arrow', 'dotenv', 'gtools', 'NLP', 'memoise'))"

# CWD
WORKDIR /

# copy things
# excluding files like README.md in .dockerignore
COPY ./ /app/

# Launch Plumber

CMD ["/app/plumber.R"]
