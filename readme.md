# NZ harmonised trade analysis

This R code loads in harmonised trade exports and imports CSV files published by Statistics New Zealand at http://www.stats.govt.nz/browse_for_stats/industry_sectors/imports_and_exports/overseas-merchandise-trade/HS10-by-country.aspx

Exports and imports are loaded into two dataframes. Only the following fields are loaded:
- Month (which gets separated into month and year)
- Harmonised System Code (the numeric harmonised trade category)
- Total exports (VFD) and imports (CIF)

The code can be easily modified to import other fields if necessary.

As an illustration, a small multiples chart is produced of exports and imports for the top 20 countries in 2014. 
