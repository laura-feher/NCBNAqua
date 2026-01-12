#' Save NAVD88 water level data, tidal datums, and plot to an excel file
#'
#' @param wl_data (required); A data frame containing water level data obtained
#'   via the function `get_wl_data`. It must include columns for 'water_level',
#'   'date', 'Identifier', 'Name', and 'LocationIdentifier'. Note that the data
#'   needs to be filtered to single site before using this function.
#' @param folder (required); A string with the folder that you want to save to.
#'   Defaults to the current working directory unless you specify a full folder
#'   path (e.g., "C:/Users/lfeher/data").
#' @param file_name (required); A string with the name of the excel file that
#'   you want to use.
#' @param overwrite (optional); Do you want to overwrite an existing file of the
#'   same name? Defaults to FALSE.
#'
#' @returns Invisibly saves an excel workbook to the specified folder/file_name.
#'
#' @importFrom lubridate month
#' @importFrom lubridate day
#' @importFrom lubridate year
#'
#' @export
#'
#' @examples
#' save_water_level_with_datums(wl_data = asis_m8_set_wl, folder = "data", file_name = "ASIS_M8_Plotting", overwrite = TRUE)
#'   
save_water_level_with_datums <- function(wl_data, folder, file_name, overwrite = FALSE) {
  
  # 1) Create a new excel workbook with a single worksheet
  wb <- openxlsx::createWorkbook()
  openxlsx::addWorksheet(wb, sheetName = "Sheet1")
  
  # 2) Put water level data in the excel file
  asis_m8_set_wl %>%
    mutate(Date = paste0(lubridate::month(date), "/", lubridate::day(date), "/", lubridate::year(date), " ", time)) %>%
    select(Date, "Depth (m, NAVD88)"= water_level) %>%
    openxlsx::writeData(wb, sheet = 1, x = ., startCol = 1, startRow = 1)
  
  # 3) Calculate tidal datum and add them to the excel file
  tidal_datums_df <- tidal_datums(wl_data = wl_data) %>%
    ungroup() %>%
    select(MHW, MHHW, MLW, MLLW) %>%
    pivot_longer(., cols = everything())
  
  openxlsx::writeData(wb, sheet = 1, x = tidal_datums_df, colNames = FALSE, startCol = "E", startRow = 1)
  
  # 4) Create the water level plot and add it to the excel file
  print(plot_water_level_with_datums(wl_data = wl_data))
  
  openxlsx::insertPlot(wb, sheet = 1, height = 8, width = 10, startCol = 7, startRow = 3, units = "in")
  
  # 5) Save the workbook as xlsx (can overwrite or make a copy)
  openxlsx::saveWorkbook(wb, file = paste0(folder, "/", file_name, ".xlsx"), overwrite = overwrite)
}
