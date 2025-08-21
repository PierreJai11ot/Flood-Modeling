# Introducing a function reformatting generated tables

tableCleaner <- function(tableString) {
  cleanedString <- gsub("\\\\toprule", "\\\\hline", tableString)
  cleanedString <- gsub("\\\\midrule", "\\\\hline", cleanedString)
  cleanedString <- gsub("\\\\bottomrule", "\\\\hline", cleanedString)
  cleanedString <- gsub("\\\\addlinespace", "", cleanedString)
  return(cleanedString)
}

