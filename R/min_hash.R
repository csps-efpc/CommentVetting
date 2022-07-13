
library(stringdist)
library(tidytext)
library(tm)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(textreuse)
library(tibble)
library(ggplot2)
library(httr)
library(glue)
library(jsonlite)
library(stringr)
library(igraph)
library(tokenizers)
library(janitor)
library(gtools)

#'split a vector into equal parts
#'
#' @param x vector
#' @param part_size the size of the parts to split into
#' @param ... ignored
#'
#' @return
#' list of vectors
#' @export
#'
#' @examples
#'    split_equal_parts(letters, part_size = 10)
split_equal_parts <- function(x, part_size = 20, ...){
  split(x, ceiling(seq_along(x)/part_size))
}



#'split a vector into equal parts
#'
#' @param x vector
#' @param number_parts how many parts to split into
#' @param ... passed to split_equal_parts
#'
#' @return
#' list of vectors
#' @export
#'
#' @examples
#'   split_equal_parts_2(letters, number_parts = 7)
#' 
split_equal_parts_2 <- function(x, number_parts = 20, ...){
  x |> 
    split_equal_parts(part_size = length(x)/number_parts, ...)
}





#' Return closest number in lst to x
#'
#' @param x 
#' @param lst 
#'
#' @return
#' @export
#'
#' @examples
#'    closest_number(5, lst = c(1,2,7, 9))
closest_number <- function(x, lst){
  closest_index <- 
    (lst - x) |> 
    abs() |>
    which.min()
  lst[closest_index]
}




#' Return the closest highly composite number to the given number x
#'
#' @param x  a number
#' @param lst a vector of numbers to choose from, by default the first 25 highly composite numbers 
#'
#' @return
#' @export
#'
#' @examples
#'   closest_highly_composite(15)
closest_highly_composite <- function(x,lst = c(1, 2, 4, 6, 12, 24, 36, 48, 60, 120, 180, 240, 360, 720, 840, 1260, 1680, 2520, 5040, 7560, 10080, 15120, 20160, 25200, 45360)){
  closest_number(x = x, lst = lst)
}




#' find divisors of a number
#'
#' @param x a number
#'
#' @return
#'  a vector of divisors of a number
#' @export
#'
#' @examples
#'  divisors(12) #BASE 12 is better then base 10
#'  divisors(10)
#'  divisors(97) #PRIME
#'  divisors(99)
#'  divisors(120) # HIGHLY COMPOSITE
divisors <- function(x){
  y <- seq_len( ceiling( x / 2 ) )
  y[ x%%y == 0 ]
}


#' simple wrapper around tokenizers::tokenize_ngrams so it can work with the textreuse package
#'
#' @param string a string of length one
#' @param lowercase passed to tokenizers::tokenize_ngrams
#' @param n passed to tokenizers::tokenize_ngrams
#'
#' @return
#'    vector of token shingles
#' @export
#'
#' @examples
#'     custom_tokenize_ngrams('Is this a real life, or is this just fantasy', lowercase = TRUE, n = 2)
#' 
custom_tokenize_ngrams <- function(string, lowercase = TRUE, n = 3) {
  assertthat::assert_that(length(string) == 1)
  
  
  tokenizers::tokenize_ngrams(x = string, lowercase = lowercase, n = n) |> unlist()
}




#' return the best number of hashes to use for a given vector of x strings
#'
#' @param x vector of strings
#'
#' @return
#'   an integer
#' @export
#'
#' @examples
#'    best_hashs(x = c('poop','We love you',"We passed upon the stair We spoke of was and when Although I wasn't there He said I was his friend",'Is this the real life? Is this just fantasy? Caught in a landside,No escape from reality','Wake up (Wake up) Grab a brush and put a little make-up', "I hurt myself today To see if I still feel I focus on the pain The only thing that's real"))
best_hashs <- function(x, shingle_size = 4){
  
  x_summary <- 
      x |>
        #sample(3) |>  
        unlist() |>
        map(~{
          custom_tokenize_ngrams(string = .x, n = shingle_size)
        }) |>
        lengths()|>
        summary()   

  
  closest_highly_composite(x_summary[['1st Qu.']])
}


#' Title Find the best band to use given a certain number of hashes
#'
#' @param n_hashes Positive integer
#' @param low_prob from 0 to 1 low probability value to consider. Default 0.25
#' @param high_prob  from 0 to 1 high probability value to consider . Default 0.75
#'
#' @return
#' @export
#'
#' @examples
#' best_bands(97)#PRIME
#' best_bands(99)
#' best_bands(120)# HIGHLY COMPOSITE
#' best_bands(4)
best_bands <- function(n_hashes, low_prob = 0.25, high_prob = 0.75){
  divisors(n_hashes) |> map_dfr(~{
    
    tibble(band = .x, 
           threshold = textreuse::lsh_threshold(h = n_hashes, b = .x),
           high_prob = textreuse::lsh_probability(h = n_hashes, b = .x, s = high_prob),
           low_prob = textreuse::lsh_probability(h = n_hashes, b = .x, s = low_prob))
  }) |> 
    mutate(diff = high_prob - low_prob) |>
    arrange(diff) |>
    slice(which.max(diff)) |>
    pull(band)
}



#' Title Makes an object of type list of lists into a dataframe by extracting the index and the name given in content_name
#'
#' @param corpus_chunk A list of named lists
#' @param content_name a string that is a key for each list in corpus_chunk
#'
#' @return
#' @export
#'
#' @examples
corpus_2_dat <- function(corpus_chunk, content_name = c('content', 'tokens')){
  
  content_name_dat <- 
    content_name |>
      map_dfc(\(.nm){
          1:length(corpus_chunk) |>
          map_dfr(\(.i){
            tibble(!!sym(.nm) := corpus_chunk[[.i]][[.nm]]  |> as.character() |> unlist()) |>
              nest(data = everything())
          })
        }) 
  
  content_name_dat |> 
    set_names(content_name) |>
    unnest('content') |>
    mutate(comment_id = row_number()) 
  
  
}



#' find the base string most like all other strings
#'
#' @param x vector of strings
#' @param str_dist_func function to use for measuring distace
#' @param ... passed to str_dist_func
#'
#' @return
#' @export
#'
#' @examples
#'   base_string(x = c('base string!', 'ase string', 'base string', 'base-string', 'basic string', 'base ment string'))
#' 
base_string <- function(x, 
                        max_sample = 15 ,
                        str_dist_func = stringdist::stringsim){
  
  

  
  
  x2 <- x  |> unique() |> sample(size =  min(length(unique(x)), max_sample), replace = FALSE)
  
  if (length(x2) == 1)
    return(x2)
  
  
  dat <- 
    gtools::combinations(n = length(x2), r = 2, v = x2, repeats.allowed = FALSE) |>
    as_tibble() |>
    set_names(c('a','b'))
  

  dat |>
    mutate(sim = str_dist_func(a, b)) |>
    pivot_longer(cols = c('a','b')) |>
    group_by(value) |>
    summarise(sim = sum(sim)) |>
    slice_max(sim, with_ties = FALSE) |> 
    pull(value)
}






#' Returns a sting which represent the required edits to turn B into A
#'
#' @param a 
#' @param b 
#'
#' @return
#' @export
#'
#' @examples
#' edit_required("prefix this is the same", "this is the same postfix")
edit_required <- function(a, b){
  assertthat::assert_that(length(b) == 1)
  
  attr(adist(b, a, counts = TRUE), "trafos")[,1]
}






#' Divides some comments up into groups.
#'
#' @param x a vector of texts
#' @param ... 
#' @param dat dataframe created from x
#' @param n_hashes number of hashes to use
#' @param low_prob used for calculation of default n_bands
#' @param high_prob  used for calculation of default n_bands
#' @param n_bands number of bands to use
#' @param seed random seed
#' @param minhash_func a function that generates a hash
#' @param n_word_in_name words for name
#' @param progress show a progress bar
#'
#' @return
#'    Data frame with 4 columns, 
#'      comment_id -- which is the index of the comment from x
#'      grp_size   -- which indicates the number of items in the group
#'      grp        -- an index for the group
#'      text       -- the original text of the comment from x
#'      name       -- unique string for each group that has a series of word, that distinguish this comment group from other comments.  
#'      
#' @export
#'
#' @examples
find_near_duplicats <- function(x, 
                                ...,
                                dat = 
                                  tibble(
                                    comment_id = 1:length(x),
                                    text = x) |>
                                  mutate_all(as.character),
                                n_hashes = best_hashs(x, shingle_size = 4), 
                                low_prob = 0.25,
                                high_prob = 0.75,
                                n_bands = best_bands(n_hashes, low_prob = low_prob, high_prob = high_prob),
                                seed = 123456,
                                minhash_func = textreuse::minhash_generator(n = n_hashes, seed = seed),
                                n_word_in_name = 4,
                                progress = TRUE
){
  
  print(glue('{Sys.time()} making corpus object for {nrow(dat)} documents, using {n_hashes} hashes and {n_bands} bands'))

  corpus_chunk <- 
    dat |>
    #sample_n(1) |>[]
    deframe() %>%
    #TextReuseTextDocument(
    textreuse::TextReuseCorpus(
      text = ., 
      tokenizer = custom_tokenize_ngrams, 
      minhash_func = minhash_func , 
      keep_tokens = TRUE,
      progress = progress, 
      n = 4 # This is stupid but for some reason I get an error when I move n=3 up into a parameter for this function, so for now n =4 is a magic number
    ) 
  
  print(glue('{Sys.time()} making clusters for corpus'))
  clusters <- 
    corpus_chunk |>
    textreuse::lsh(bands = n_bands, progress = progress) |> 
    textreuse::lsh_candidates() |> 
    #textreuse::lsh_compare(corpus_chunk, textreuse::jaccard_similarity, progress = progress)  |> 
    igraph::graph_from_data_frame(directed = FALSE) |>
    igraph::cluster_fast_greedy()  
  
  
  #corpus_dat <- corpus_2_dat(corpus_chunk)
  
  cluster_comments <- 
    1:length(clusters) |>
    map_dfr(~{
      cl = clusters[[.x]]
      tibble(
        comment_id = list(cl),
        grp_size = length(cl)
      )
    }) |>
    arrange(desc(grp_size)) |>
    mutate(grp = row_number()) |>
    unnest(cols = comment_id) |>
    full_join(dat, by = 'comment_id') 
    #full_join(corpus_dat |> mutate(comment_id = as.character(comment_id)), by = 'comment_id')

  
  cluster_names <- 
    cluster_comments |>
    #filter(!is.na(grp)) |>
    select(text, grp) |>
    replace_na(list(grp = 0)) |>
    tidytext::unnest_tokens('word', 'text') |>
    add_count(word, name = 'n_word') |>
    # mutate(rand_word = dense_rank(n_word)) |>
    # mutate(f_word = rand_word/max(rand_word)) |>
    #arrange(grp, f_word) |>
    #group_by(grp)|> slice_min(n_word, prop = 0.75) |> ungroup() |>
    count(grp, word, sort = TRUE) |>
    bind_tf_idf(term = word, document = grp, n = n) |>
    group_by(grp) |> 
    slice_max(n = n_word_in_name, order_by = tf_idf, with_ties = FALSE) |> 
    #arrange(desc(tf_idf)) |>
    summarise(name = paste0(word, collapse = ' '))  |>
    mutate(name = janitor::make_clean_names(name, case = "title")) |>
    filter(grp > 0)
  
  a <- 
  cluster_comments |>
    full_join(cluster_names, by = "grp")

  b <-   
    a |>
    group_split(grp)  |> # magrittr::extract2(1) ->.grp_dat
    map_dfr(\(.grp_dat){
      #.grp_dat <- a |> filter(grp == 45)
      if (is.na(unique(.grp_dat$grp))){
        return(.grp_dat)
      }
      
      print(glue('{Sys.time()} grp = {unique(.grp_dat$grp)} length(x) = {nrow(.grp_dat)}'))
      
      
      b_str <- base_string(x = .grp_dat$text)
      
      .grp_dat |>
        #sample_n(20) |>
        mutate(edits = edit_required(text, b_str)) |>
        mutate(b_str = b_str)
        #select(edits) |> head(1) |> pull(edits)
    })

  b
  #cluster_comments |> filter(grp == 3)
  
  # cluster_comments |>
  #   mutate(len = nchar(text)) |>
  #   filter(grp ==5) |>
  #   ggplot(aes(x = len, fill = as.character(grp))) + geom_density(alpha = 0.5)
    
}

# data_save_dir <- file.path('private', 'regulations.gov')
# fn <- list.files(data_save_dir, pattern = '^comments_details_.*\\.feather$', full.names = TRUE) |> sample(1)
# comments <- arrow::read_feather(fn)
# cc <- find_near_duplicats(x = comments$comment)

