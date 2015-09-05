# Load libraries
library(dplyr)
library(ggplot2)
library(ggthemes)

# Helper for reading numeric data with commas
setAs("character", "num.with.commas", function(from) as.numeric(gsub(",", "", from)))

# Load data
exports = data.frame(month = character(0), 
                     code = character(0), 
                     country = character(0), 
                     value_fob = numeric(0), 
                     stringsAsFactors = FALSE)
imports = data.frame(month = character(0), 
                     code = character(0), 
                     country = character(0), 
                     value_vfd = numeric(0), 
                     value_cif = numeric(0), 
                     stringsAsFactors = FALSE)
exportSuffix = "_Exports_HS10_by_Country.csv"
importSuffix = "_Imports_HS10_by_Country.csv"
for (i in 2000:2000) {
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
  names(exportData) = c("month", "code", "country", "value_fob")
  exports = rbind(exports, exportData)
  
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
  names(importData) = c("month", "code", "country", "value_vfd", "value_cif")
  imports = rbind(imports, importData)
}

rm(exportData, importData)
