# Introducing a function to generate a table to evaluated the fitted models 

tailRiskAssessmentTableGenerator <- function(data, title) {
  
  tableData <- data %>%
    dplyr::select(Country, 
                  maxSev, VaRSev, ESSev, 
                  max2020Euros, VaR2020Euros, ES2020Euros) %>%
    rowwise() %>%
    mutate(max2020Euros = max2020Euros/1e6, 
           VaR2020Euros = VaR2020Euros/1e6,
           ES2020Euros = ES2020Euros/1e6) %>%
    mutate(
           maxSev = formatC(maxSev, format = "e", digits = 3), 
           VaRSev = formatC(VaRSev, format = "e", digits = 3), 
           ESSev = formatC(ESSev, format = "e", digits = 3), 
           max2020Euros = formatC(max2020Euros, format = "e", digits = 3), 
           VaR2020Euros = formatC(VaR2020Euros, format = "e", digits = 3), 
           ES2020Euros = formatC(ES2020Euros, format = "e", digits = 3)
           ) %>%
    ungroup() %>%
    rename(
      '$\\underset{\\text{Sev.}}{\\text{max}}$' = maxSev, 
      '$\\underset{\\text{Sev.}}{\\text{VaR}_{99.5\\%}}$' = VaRSev,
      '$\\underset{\\text{Sev.}}{\\text{ES}_{99\\%}}$' = ESSev, 
      '$\\underset{\\text{Mio €}}{\\text{max}}$' = max2020Euros, 
      '$\\underset{\\text{Mio €}}{\\text{VaR}_{99.5\\%}}$' = VaR2020Euros, 
      '$\\underset{\\text{Mio €}}{\\text{ES}_{99\\%}}$' = ES2020Euros
           )
  
  tableData <- tableData %>%
    rename(ISO = Country) %>%
    rowwise() %>%
    mutate(Country = ifelse(ISO == "All Considered Countries", "AC", 
                            relevantCountries %>% 
                              filter(Country == ISO) %>%
                              pull(CountryISO)
                            )) %>%
    dplyr::select(-ISO) %>%
    relocate(Country, .before = everything())
  
  table <- kable(tableData, format = "latex", booktabs = TRUE, caption = title, escape = FALSE)
  table <- tableCleaner(table)
  
  return(table)
}
