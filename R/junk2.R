


x <- c('base string')
x <- c('base string!', 'ase string', 'base string', 'base string', 'base string', 'base-string', 'base-string')
tbl_x <- table(x)
library(RcppAlgos)
comboGeneral(v = names(tbl_x), m = 2, freqs = tbl_x)



edit_required <- function(a, b){
  assertthat::assert_that(length(b) == 1)

  attr(adist(a, b, counts = TRUE), "trafos")[,1]
}
x |> edit_required(base_string)


library(stringdist)

stringdist::stringsim(b, b)

base_string <- 
  expand_grid(a = x, b = x) |>
  mutate(sim = stringdist::stringsim(a, b)) |>
  group_by(a) |>
  summarise(sim = sum(sim)) |>
  slice_max(sim) |> 
  pull(a)



library(gtools)
combinations(n = length(x), r = 2, v = x, repeats.allowed = FALSE) |>
  as_tibble() |>
  set_names(c('a','b'))


