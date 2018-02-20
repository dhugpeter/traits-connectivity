01_LoadData.R => load the abundance and trait data, select the years, aggregation, etc.
02_TraitAnalyses.R => prepare trait information, calculate diversity index, metrics from Merritt, etc.
03_PaillexIndex.R => calculate the connectivity index from Paillex
04_..... .R => calculate hydrological indices from Pierre (??)
05_Models.R => for now, perform simple regression analyses
...
99_Report_Paillex_index.RMD => call from 05_Models.R to generate a report with the relationships between all indices/metrics and the connectivity index from Paillex.
