####################################################################################################
###
### File:    00_source.R
### Purpose: Load required packages and functions used throughout Pattern Based Prediction results visualisation
### Authors: Gabriel Palma
### Date:    01/04/2022
###
####################################################################################################

### packages required

packages <- c('GGally', # Multiple plots 
              'ggpcp', # Parallel coordinates plots
              'tidyverse', # data-wrangling + plotting
              'lubridate', # dates and time-series
              'flexmix', # Estimation in Mixed Models
              'here' # efficient file structures
)
install.packages(setdiff(packages, rownames(installed.packages())))
lapply(packages, library, character.only = TRUE)

### functions used Pattern Based Prediction method

### plot settings

theme_new <- function(base_size = 14, base_family = "Arial"){
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      axis.text = element_text(size = 12, colour = "grey30"),
      legend.key=element_rect(colour=NA, fill =NA),
      axis.line = element_line(colour = 'black'),
      axis.ticks =         element_line(colour = "grey20"),
      plot.title.position = 'plot',
      legend.position = "bottom"
    )
}

