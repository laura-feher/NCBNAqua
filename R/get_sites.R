#' Print a list of the sites in a data frame of downloaded water data
#'
#' @inheritParams tidal_datums
#' 
#' @returns Prints a list of sites in the data to the console.
#' 
#' @export
#'
#' @examples
#' get_sites(wl_data = asis_set_wl)
#' 
get_sites <- function(wl_data) {
  
  wl_data %>% 
    select(Name, Identifier) %>%
    distinct()
}
