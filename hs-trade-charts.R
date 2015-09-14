# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(grid)

# Load exports & imports data from CSV files
# Note only certain columns are loaded, the others are set to NULL in the colClasses definition
loadTradeData = function() {
  exports <<- data.frame(month = character(0), 
                         code = character(0), 
                         country = character(0), 
                         value_fob = numeric(0), 
                         stringsAsFactors = FALSE)
  imports <<- data.frame(month = character(0), 
                         code = character(0), 
                         country = character(0), 
                         value_cif = numeric(0), 
                         stringsAsFactors = FALSE)
  
  # Helper for reading numeric data with commas
  setClass("num.with.commas")
  setAs("character", "num.with.commas", function(from) as.numeric(gsub(",", "", from)))
  
  exportSuffix = "_Exports_HS10_by_Country.csv"
  importSuffix = "_Imports_HS10_by_Country.csv"
  for (i in 2000:2014) {
    print(i)
    exportFile = paste("data/", i, exportSuffix, sep="")
    exportData = read.csv(exportFile, 
                          stringsAsFactors = FALSE, 
                          sep = ",", quote = "\"", 
                          colClasses = c("Month" = "character", 
                                         "Harmonised.System.Code" = "character", 
                                         "Harmonised.System.Description" = "NULL", 
                                         "Unit.Qty" = "NULL", 
                                         "Exports...NZD.fob." = "NULL", 
                                         "Exports.Qty" = "NULL", 
                                         "Re.exports...NZD.fob." = "NULL", 
                                         "Re.exports.Qty" = "NULL", 
                                         "Total.Exports...NZD.fob." = "num.with.commas", 
                                         "Total.Exports.Qty" = "NULL", 
                                         "Status" = "NULL"))
    names(exportData) = c("date", "code", "country", "value_fob")
    exports <<- rbind(exports, exportData)
    
    importFile = paste("data/", i, importSuffix, sep="")
    importData = read.csv(importFile, 
                          stringsAsFactors = FALSE, 
                          sep = ",", quote = "\"", 
                          colClasses = c("Month" = "character", 
                                         "Harmonised.System.Code" = "character",     # Treat as character to preserve leading zeros and to enable splitting later 
                                         "Harmonised.System.Description" = "NULL", 
                                         "Unit.Qty" = "NULL", 
                                         "Imports...NZD.vfd." = "NULL", 
                                         "Imports...NZD.cif." = "num.with.commas", 
                                         "Imports.Qty" = "NULL", 
                                         "Status" = "NULL"))
    names(importData) = c("date", "code", "country", "value_cif")
    imports <<- rbind(imports, importData)
  }
  
  # Split date into month & year, and convert back to numbers
  exports <<- exports %>% separate(date, c("year", "month"), sep = 4)
  imports <<- imports %>% separate(date, c("year", "month"), sep = 4)
  exports$year <<- as.numeric(exports$year)
  exports$month <<- as.numeric(exports$month)
  imports$year <<- as.numeric(imports$year)
  imports$month <<- as.numeric(imports$month)
}

# Custom theme for ggplot
mytheme = function(base_size = 10, base_family = "sans") {
  (
    theme_foundation(base_size = base_size, base_family = base_family) + 
      theme(
        # Plot
        plot.title = element_text(face = "bold", vjust = 3, hjust = 0), 
        plot.margin = unit(c(15, 10, 0, 10), "mm"), 
        
        # Axes
        axis.line.y = element_blank(), 
        axis.line.x = element_blank(), 
        axis.ticks = element_line(colour = "#666666"), 
        axis.ticks.y = element_blank(), 
        axis.title = element_text(face = "italic"), 
        axis.title.y = element_text(vjust = 1, hjust = 0.1), 
        axis.title.x = element_text(hjust = 1, vjust = 0.1), 
        axis.text.x = element_text(vjust = 0), 
        
        # Panel
        panel.grid.major.y = element_line(colour = "#f2f2f2"), 
        panel.grid.major.x = element_blank(), 
        panel.grid.minor = element_blank(), 
        panel.border = element_rect(colour = NA), 
        panel.background = element_rect(colour = NA), 
        panel.margin = unit(4, "lines"), 
        
        # Facets
        strip.background = element_blank()
      )
  )
}

# Example function to plot small multiples of import & export trends by country
smallMultiplesByCountry = function(num = 20) {
  # Calculate annual totals by country
  exportsByCountry = group_by(exports, country, year) %>% 
                     summarise(value_fob = sum(value_fob))
  
  importsByCountry = group_by(imports, country, year) %>%
                     summarise(value_cif = sum(value_cif))
  
  # Extract 2014 totals, this will be used for sorting
  exports2014 = filter(exportsByCountry, year == 2014) %>% 
                ungroup() %>%
                arrange(desc(value_fob))
  exports2014$year = NULL
  names(exports2014) = c("country", "total2014")
  
  imports2014 = filter(importsByCountry, year == 2014) %>% 
                ungroup() %>% 
                arrange(desc(value_cif))
  imports2014$year = NULL
  names(imports2014) = c("country", "total2014")
  
  # Pull out required list of top countries, sorted
  selExportCountries = exports2014[1:num, ] %>% 
                       arrange(desc(total2014))
  selImportCountries = imports2014[1:num, ] %>%
                       arrange(desc(total2014))
  
  # Get data for selected countries
  selExports = inner_join(exportsByCountry, selExportCountries, "country")
  selImports = inner_join(importsByCountry, selImportCountries, "country")
  
  # Create factors for sorting by 2014 totals
  selExports$country_f = factor(selExports$country, levels = selExportCountries$country)
  selImports$country_f = factor(selImports$country, levels = selImportCountries$country)
  
  # Set up graphics parameters
  panelWidth = 500    # Pixel width of individual plot
  panelHeight = 250    # Pixel height of individual plot
  panelsPerRow = 4
  
  # Draw small multiples for exports
  png("exports-small-multiples.png", width = panelWidth * panelsPerRow, height = panelHeight * ceiling(num / panelsPerRow))
  p1 = ggplot(data = selExports, aes(x = year, y = value_fob)) +
    geom_bar(stat = "identity") + 
    facet_wrap(~ country_f, ncol = panelsPerRow) + 
    mytheme(base_family = "Fira Sans", base_size = 22) + 
    xlab("") + 
    scale_y_continuous(name = "", 
                       breaks = c(0, 2e+9, 4e+9, 6e+9, 8e+9, 1e+10), 
                       labels = c("0", "2", "4", "6", "8", "10")) + 
    ggtitle("Annual exports $ billion")
  print(p1)
  dev.off()
  
  # And for imports
  png("imports-small-multiples.png", width = panelWidth * panelsPerRow, height = panelHeight * ceiling(num / panelsPerRow))
  p2 = ggplot(data = selImports, aes(x = year, y = value_cif)) +
    geom_bar(stat = "identity") + 
    facet_wrap(~ country_f, ncol = panelsPerRow) + 
    mytheme(base_family = "Fira Sans", base_size = 22) + 
    xlab("") + 
    scale_y_continuous(name = "", 
                       breaks = c(0, 2e+9, 4e+9, 6e+9, 8e+9, 1e+10), 
                       labels = c("0", "2", "4", "6", "8", "10")) + 
    ggtitle("Annual imports $ billion")
  print(p2)
  dev.off()
}

# Main code
loadTradeData()
smallMultiplesByCountry(20)
