



#' Writes a csv file to a file with the name and version number of all attached packages
#'
#' @param fn file name to write to.
#'
#' @return 
#' @export
#'
#' @examples
#' write_attached_packages_to_file()
#' 
write_attached_packages_to_file <- function(fn = 'packages.csv'){
  si <- sessionInfo()
  si$R.version
  si$basePkgs
  lapply(si$otherPkgs, function(.x){
    list( package =.x$Package,
          version = .x$Version
    )
  }) |> dplyr::bind_rows() |> 
    readr::write_csv(fn)
}
write_attached_packages_to_file()


getwd()
install.packages_from_file  <- function(fn, upgrade = 'always'){
  require(remotes)
  
  
  packs <- read.csv(fn) 
  
  packs |> 
    purrr::pwalk(\(package, version){
      remotes::install_version(package = package, version = version, upgrade = upgrade,  repos = "http://cran.us.r-project.org")  
    })
    
}



#packrat::snapshot()
#packrat::restore()
