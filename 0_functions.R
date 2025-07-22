## function to store


readodpair = function(path, filename) {
  # Skip first two lines
  df <- read.csv(paste0(path, filename, '.csv'), skip = 2, header = T)
  if (any(colnames(df) == 'Trips')) {
    if (typeof(df$Trips) == 'character') {
      df$Trips = as.numeric(gsub(',','',df$Trips))
    }
  }
  
  return(df)
}

readgeojson = function(path, jsonname) {
  library(sf)
  # Read the GeoJSON file
  geo_data <- st_read(paste0(path, '\\', jsonname, '.geojson'))
  return(geo_data)
}

tottrips = function(odpair, filter, o_sel, d_sel) {
  if (typeof(odpair$Trips) == 'character') {
    odpair$Trips = as.numeric(gsub(",", "", odpair$Trips))
  }
  if (filter == 0) {
    trips = sum(odpair$Trips)
  }
  else if (filter == 1) {
    trips = sum(odpair$Trips[which(odpair$Origin.Source == o_sel & odpair$Destination.Source == d_sel)])
  }
  return(trips)
}

pairwiseplot = function(data, x, y) {
  # Extract vectors
  x_vals <- as.numeric(data[[x]])
  y_vals <- as.numeric(data[[y]])
  
  # Fit linear model: y ~ 0 + x (no intercept)
  fit <- lm(y_vals ~ 0 + x_vals)
  slope <- coef(fit)[1]
  
  # Compute R²
  r_squared <- summary(fit)$r.squared
  
  # Format label text if not provided
  #if (is.null(label_text)) {
  label_text <- paste0("slope = ", round(slope, 3), "\nR² = ", round(r_squared, 3))
  #}
  
  # Build the plot
  ggplot(data, aes(x = as.numeric(.data[[x]]), y = as.numeric(.data[[y]]))) +
    geom_point(color = 'white') +
    geom_abline(slope = 1, intercept = 0, color = "gray", linetype = "dashed", size = 1) +
    geom_smooth(method = "lm", formula = y ~ 0 + x, se = FALSE, color = "blue") +
    annotate("text", x = Inf, y = -Inf, label = label_text,
             hjust = 1.1, vjust = -1.1, size = 5, color = 'white') +
    theme_minimal() +
    theme_black()  # Replace or remove if undefined
}


# convert time character to period
time_to_period = function(time_char, duration) { #duration in seconds 
  if(typeof(time_char) == 'character') {
    time_obj = hms(time_char)
    period = ceiling(as.numeric(time_obj)/duration) * duration
    minutes <- period %/% 60  # integer division to get minutes
    return(minutes)
  } else {
    return('Error: input not character')
  }
}

################################ define a high contrast plot style, use this function in ggplot ################################
# Define your custom black background theme # 👈 Use your custom theme here
theme_black <- function(base_size = 20) {
  theme_minimal(base_size = base_size) +
    theme(
      panel.background = element_rect(fill = "black", color = NA),
      plot.background = element_rect(fill = "black", color = NA),
      panel.grid.major = element_line(color = "gray30"),
      panel.grid.minor = element_line(color = "gray20"),
      axis.line = element_line(color = "white", size = 1),
      axis.ticks = element_line(color = "white"),
      
      # Larger axis tick text
      axis.text = element_text(color = "white", size = base_size + 2),
      
      # Larger, bold axis titles
      axis.title = element_text(color = "white", size = base_size + 4, face = "bold"),
      
      # Bold and large title, subtitle, and caption
      plot.title = element_text(color = "white", size = base_size + 6, face = "bold"),
      # plot.subtitle = element_text(color = "white", size = base_size + 3, face = "bold"),
      # plot.caption = element_text(color = "white", size = base_size + 2),
      
      # Legend styling
      legend.background = element_rect(fill = "black"),
      legend.key = element_rect(fill = "black"),
      legend.text = element_text(color = "white", size = base_size),
      legend.title = element_text(color = "white", face = "bold", size = base_size + 2),
      legend.position = 'top'
    )
}