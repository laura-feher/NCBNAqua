#' Plot NAVD88 water level data with tidal datums
#'
#' This function uses the tidal_datums() function to calculate tidal datums and
#' then plot them on top of the water level data referenced to NAVD88.
#'
#' @param wl_data (required); A data frame containing water level data obtained
#'   via the function `get_wl_data`. It must include columns for 'water_level',
#'   'date', 'Identifier', 'Name', and 'LocationIdentifier'.
#' @param site (optional): A string specifying the name of a single site to
#'   plot.
#' @param x_right_expand (optional): Value used to expand the x-axis to the
#'   right.
#' @param include_extremes (optional): TRUE will plot MHHW and MLLW in addition
#'   to MHW and MLW. Defaults to FALSE.
#'
#' @returns A single ggplot.
#'
#' @export
#'
#' @examples
#' plot_water_level_with_datums(wl_data = asis_m8_set_wl)
#' 
plot_water_level_with_datums <- function(wl_data, site = NA, x_right_expand = 0.25, include_extremes = FALSE) {
  
  if (is.na(site)) {
    df <- wl_data
  } else {
    df <- wl_data %>%
      filter(str_detect(string = Name, pattern = site))
  }
  
  # Calculate tidal datums
  df_datums <- NCBNAqua::tidal_datums(wl_data = df)
  MHW <- df_datums$MHW
  MLW <- df_datums$MLW
  MHHW <- df_datums$MHHW
  MLLW <- df_datums$MLLW
  
  # Get x-axis range and set placement of datum labels
  x_range  <- range(df$datetime, na.rm = TRUE)
  label_x  <- x_range[2] + lubridate::days(7)
  
  # Build plot
  ggplot2::ggplot(df, ggplot2::aes(x = datetime, y = water_level)) +
    # Main time series (mapped so it appears in the legend with desired label)
    ggplot2::geom_line(
      ggplot2::aes(color = "Depth (m, NAVD88)", linetype = "Depth (m, NAVD88)"),
      linewidth = 0.3
    ) +
    
    # MHW horizontal line across the plot (mapped for combined legend)
    ggplot2::geom_line(
      data = data.frame(datetime = x_range, water_level = rep(MHW, 2)),
      ggplot2::aes(x = datetime, y = water_level, color = "MHW", linetype = "MHW"),
      linewidth = 0.8
    ) +
    
    # MLW horizontal line across the plot
    ggplot2::geom_line(
      data = data.frame(datetime = x_range, water_level = rep(MLW, 2)),
      ggplot2::aes(x = datetime, y = water_level, color = "MHW", linetype = "MHW"),
      linewidth = 0.8
    ) +
    
    # MHW label (shifted right into the expanded margin)
    ggplot2::annotate(
      "text",
      x = label_x, y = MHW,
      label = paste0("MHW = ", round(MHW, 3), " m"),
      hjust = 0, vjust = 0.5,
      color = "red", size = 4
    ) +
    
    # MLW label (shifted right into the expanded margin)
    ggplot2::annotate(
      "text",
      x = label_x, y = MLW,
      label = paste0("MLW = ", round(MLW, 3), " m"),
      hjust = 0, vjust = 0.5,
      color = "red", size = 4
    ) +
    
    # Include MHHW and MLLW if include_extremes = TRUE
    {if(include_extremes)
      ggplot2::geom_line(
        data = data.frame(datetime = x_range, water_level = rep(MHHW, 2)),
        ggplot2::aes(x = datetime, y = water_level, color = "MHW", linetype = "MHW"),
        linewidth = 0.8
      )
      } +
    {if(include_extremes)
      ggplot2::geom_line(
          data = data.frame(datetime = x_range, water_level = rep(MLLW, 2)),
          ggplot2::aes(x = datetime, y = water_level, color = "MHW", linetype = "MHW"),
          linewidth = 0.8
        )
      } +
    {if(include_extremes)
      ggplot2::annotate(
        "text",
        x = label_x, y = MHHW,
        label = paste0("MHHW = ", round(MHHW, 3), " m"),
        hjust = 0, vjust = -0.2,
        color = "red", size = 4
        )
      } +
    {if(include_extremes)
      ggplot2::annotate(
        "text",
        x = label_x, y = MLLW,
        label = paste0("MLLW = ", round(MLLW, 3), " m"),
        hjust = 0, vjust = 0.8,
        color = "red", size = 4)
      } +
        
    # Titles/axes
    ggplot2::labs(
      title    = paste(unique(df$Name), "Water Level", sep = " "),
      x        = "Date",
      y        = "Depth (m, NAVD88)",
      color    = NULL,   # combined legend, no title
      linetype = NULL
    ) +
    
    # Manual scales: keys MUST match strings used in aes() above
    ggplot2::scale_color_manual(
      values = c(
        "Depth (m, NAVD88)" = "steelblue",
        "MHW"               = "red"
      ),
      breaks = c("Depth (m, NAVD88)", "MHW"),  # legend order
      labels = c("Depth (m, NAVD88)", "Tidal Datums")
    ) +
    ggplot2::scale_linetype_manual(
      values = c(
        "Depth (m, NAVD88)" = "solid",
        "MHW"               = "dashed"
      ),
      breaks = c("Depth (m, NAVD88)", "MHW"),
      labels = c("Depth (m, NAVD88)", "Tidal Datums")
    ) +
    
    # Make legend lines thicker (plot lines remain thin)
    ggplot2::guides(
      color    = ggplot2::guide_legend(override.aes = list(linewidth = 1))
    ) +
    
    # Expand right side to make room for the label
    ggplot2::scale_x_datetime(expand = ggplot2::expansion(mult = c(0.01, x_right_expand))) +
    
    # Theme
    ggplot2::theme_minimal() +
    ggplot2::theme(
      plot.title   = ggplot2::element_text(hjust = 0.5, face = "bold"),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 20)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 20)),
      legend.position = "top"
    )
}
