#' Print a list of the parameters in a data frame of downloaded water data
#'
#' @inheritParams tidal_datums 
#'
#' @returns Prints a list of water parameters in the data to the console.
#' @export
#'
#' @examples
#' get_parameters(wl_data = caco_dh_seagrass_wl)
#' 
get_parameters <- function(wl_data) {
  
  wl_data %>%
    select(Name, Identifier, Unit, Label) %>%
    distinct()
}
