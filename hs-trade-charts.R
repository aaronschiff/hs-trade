# Load libraries
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)

# Custom theme for ggplot
mytheme = function(base_size = 10, base_family = "sans") {
  (
    theme_foundation(base_size = base_size, base_family = base_family) + 
      theme(
        # Plot
        plot.title = element_text(face = "bold", vjust = 1), 
        plot.title = element_text(vjust = 1, size = 10), 
        
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
        
        # Facets
        strip.background = element_blank()
      )
  )
}

# Load data from CSV files
loadTradeData = function() {
  exports <<- data.frame(month = character(0), 
                         code = character(0), 
                         country = character(0), 
                         value_fob = numeric(0), 
                         stringsAsFactors = FALSE)
  imports <<- data.frame(month = character(0), 
                         code = character(0), 
                         country = character(0), 
                         value_vfd = numeric(0), 
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
                                         "Exports...NZD.fob." = "num.with.commas", 
                                         "Exports.Qty" = "NULL", 
                                         "Re.exports...NZD.fob." = "NULL", 
                                         "Re.exports.Qty" = "NULL", 
                                         "Total.Exports...NZD.fob." = "NULL", 
                                         "Total.Exports.Qty" = "NULL", 
                                         "Status" = "NULL"))
    names(exportData) = c("date", "code", "country", "value_fob")
    exports <<- rbind(exports, exportData)
    
    importFile = paste("data/", i, importSuffix, sep="")
    importData = read.csv(importFile, 
                          stringsAsFactors = FALSE, 
                          sep = ",", quote = "\"", 
                          colClasses = c("Month" = "character", 
                                         "Harmonised.System.Code" = "character", 
                                         "Harmonised.System.Description" = "NULL", 
                                         "Unit.Qty" = "NULL", 
                                         "Imports...NZD.vfd." = "num.with.commas", 
                                         "Imports...NZD.cif." = "num.with.commas", 
                                         "Imports.Qty" = "NULL", 
                                         "Status" = "NULL"))
    names(importData) = c("date", "code", "country", "value_vfd", "value_cif")
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

smallMultiplesByCountry = function() {
  # Prepare the data 
  groupedExports = group_by(exports, country, year)
  groupedImports = group_by(imports, country, year)
  
  totalExportsByCountry = summarise(groupedExports, value_fob = sum(value_fob))
  totalExportsByCountry = arrange(totalExportsByCountry, country, year)
  totalImportsByCountry = summarise(groupedImports, value_fob = sum(value_cif))
  totalImportsByCountry = arrange(totalImportsByCountry, country, year)
  
  # Set up graphics parameters
  panelWidth = 200    # Pixel width of individual plot
  panelHeight = 100    # Pixel height of individual plot
  numPanels = length(unique(groupedExports$country))  # Number of plots
  numPanelsPerRow = 6
  png("exports-small-multiples.png", width = panelWidth * numPanelsPerRow, height = panelHeight * ceiling(numPanels / numPanelsPerRow))
  
  # Draw small multiples
  p1 = ggplot(data = totalExportsByCountry, aes(x = year, y = value_fob)) +
    geom_bar(stat = "identity") + 
    facet_wrap(~ country, ncol = numPanelsPerRow) + 
    mytheme(base_family = "Fira Sans", base_size = 18)
  print(p1)
  
  # Save output
  dev.off()
}

# Main code
# loadTradeData()
smallMultiplesByCountry()
