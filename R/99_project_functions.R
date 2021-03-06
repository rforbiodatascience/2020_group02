#Function for alligning country to Johns Hopkins data -------------------

country_translate <- function(x){
  con <- c(	"Africa"= "not in JH_clean",
            "American Samoa" = "not in cleaned JH",
            "Americas" = "not in cleaned JH",
            "Anguilla" = "not in cleaned JH", 
            "Aruba" = "not in cleaned JH",
            "Asia" = "not in cleaned JH",
            "Australia and New Zealand" = "not in cleaned JH",
            "Bermuda" = "not in cleaned JH",
            "Bolivia (Plurinational State of)"= "Bolivia",
            "Bolivia (Plurin. State of)" = "Bolivia",
            "Bonaire, St. Eustatius & Saba" = "not in cleaned JH",
            "Bosnia" = "Bosnia and Herzegovina",
            "British Virgin Islands" = "not in cleaned JH", 
            "Brunei Darussalam" = "Brunei", 
            "Cape Verde" = "Cabo Verde",
            "Caribbean" = "not in cleaned JH", 
            "Cayman Islands" = "not in cleaned JH", 
            "Cen African Rep" = "not in cleaned JH",  
            "Central America" = "not in cleaned JH", 
            "Central Asia" = "not in cleaned JH", 
            "Channel Islands" = "not in cleaned JH", 
            "China, Hong Kong SAR" = "not in cleaned JH", 
            "China, Macao SAR" = "not in cleaned JH",
            "Congo" = "Congo (Brazzaville)", 
            "Congo-Brz" = "Congo (Brazzaville)", 
            "Congo/Zaire" = "Congo (Kinshasa)",
            "Cook Islands" = "not in cleaned JH",
            "Côte d'Ivoire" = "Cote d'Ivoire", 
            "Dem. People's Rep. Korea" = "not in cleaned JH",
            "Dem. Rep. of the Congo" = "Congo (Kinshasa)",
            "Democratic People's Republic of Korea" = "not in cleaned JH", 
            "Democratic Republic of the Congo" = "Congo (Kinshasa)",
            "Dominican Rep" = "	Dominican Republic",
            "East Timor" = "not in cleaned JH",
            "Eastern Africa" = "not in cleaned JH",
            "Eastern Asia" = "not in cleaned JH",
            "Eastern Europe" = "not in cleaned JH",
            "Europe" = "not in cleaned JH",
            "Falkland Islands (Malvinas)" = "not in cleaned JH",
            "Faroe Islands" = "not in cleaned JH",
            "French Guiana" = "not in cleaned JH",
            "French Polynesia" = "not in cleaned JH",
            "Gibraltar" = "not in cleaned JH",
            "Greenland" = "not in cleaned JH",
            "Guadeloupe" = "not in cleaned JH",
            "Guam" = "not in cleaned JH",
            "Guinea Bissau" = "not in cleaned JH",
            "Hong Kong" = "not in cleaned JH",
            "Iran (Islamic Republic of)" = "Iran",
            "Isle of Man" = "not in cleaned JH",
            "Ivory Coast" = "Cote d'Ivoire",
            "Kiribati" = "not in JH", 
            "Korea North" = "not in cleaned JH",
            "Korea South" = "Korea, South", 
            "Lao People's Dem. Rep." = "Laos",
            "Lao People's Democratic Republic" = "Laos", 
            "Latin America & the Caribbean" = "not in cleaned JH",
            "Lesotho" = "not in cleaned JH", 
            "Macedonia" = "North Macedonia", 
            "Marshall Islands"= "not in cleaned JH",
            "Martinique" = "not in cleaned JH",
            "Mayotte" = "not in cleaned JH",
            "Melanesia" = "not in cleaned JH",
            "Micronesia" = "not in cleaned JH",
            "Micronesia (Fed. States of)" = "not in cleaned JH",
            "Micronesia (Federated States of)" = "not in cleaned JH",
            "Middle Africa" = "not in cleaned JH",
            "Montserrat" = "not in cleaned JH",
            "Myanmar"="Burma", 
            "Nauru" = "not in cleaned JH",
            "New Caledonia" = "not in cleaned JH",
            "Niue" = "not in cleaned JH",
            "Northern Africa" = "not in cleaned JH",
            "Northern America" = "not in cleaned JH",
            "Northern Europe" = "not in cleaned JH",
            "Northern Mariana Islands" = "not in cleaned JH",
            "Oceania" = "not in cleaned JH",
            "Other non-specified areas" = "not in cleaned JH",
            "Palau" = "not in cleaned JH",
            "Polynesia" = "not in cleaned JH",
            "Puerto Rico" = "not in cleaned JH",
            "Republic of Korea" = "Korea, South", 
            "Republic of Moldova" = "Moldova", 
            "Republic of North Macedonia" = "North Macedonia", 
            "Russian Federation"= "Russia", 
            "Samoa" = "not in cleaned JH",
            "Saint Helena" = "not in cleaned JH",
            "Saint Martin (French part)" = "not in cleaned JH",
            "Saint Pierre and Miquelon" = "not in cleaned JH",
            "Saint Vincent & Grenadines" = "Saint Vincent and the Grenadines",
            "Sint Maarten (Dutch part)" = "not in cleaned JH",
            "Solomon Islands" = "not in cleaned JH",
            "South America" = "not in cleaned JH",
            "South Korea" = "Korea, South",
            "South-central Asia" ="not in cleaned JH",
            "South-eastern Asia" = "not in cleaned JH",
            "Southern Africa" = "not in cleaned JH",
            "Southern Asia" = "not in cleaned JH", 
            "Southern Europe" = "not in cleaned JH",
            "St Kitts and Nevis" = "Saint Kitts and Nevis", 
            "St Lucia" = "Saint Lucia",
            "St Vincent" = "Saint Vincent and the Grenadines", 
            "State of Palestine" = "not in cleaned JH",
            "Sub-Saharan Africa" = "not in cleaned JH",
            "Sudan (former)" = "Sudan",
            "Swaziland" = "	Eswatini", 
            "Syrian Arab Republic" = "Syria",
            "Taiwan" = "Taiwan*",
            "The former Yugoslav Republic of Macedonia" = "North Macedonia",
            "Tokelau" = "not in cleaned JH",
            "Tonga"= "not in cleaned JH",
            "Total, all countries or areas" = "not in cleaned JH",
            "Turkmenistan" = "not in cleaned JH",
            "Turks and Caicos Islands" = "not in cleaned JH",
            "Tuvalu"="not in cleaned JH",
            "UKG" = "United Kingdom",
            "United Kingdom of Great Britain and Northern Ireland" = "United Kingdom",
            "United Rep. of Tanzania" = "Tanzania",
            "United Republic of Tanzania" = "Tanzania",
            "United States" = "US",
            "United States of America" = "US",
            "United States Virgin Islands" = "not in cleaned JH",
            "USA" = "US", 
            "Vanuatu" = "not in cleaned JH",
            "Venezuela (Bolivarian Republic of)" = "Venezuela",
            "Viet Nam" = "Vietnam",
            "Wallis and Futuna Islands" = "not in cleaned JH",
            "West Bank and Gaza Strip" = "not in cleaned JH",
            "Western Africa" = "not in cleaned JH",
            "Western Asia" = "not in cleaned JH",
            "Western Europe" = "not in cleaned JH",
            "Zanzibar" = "not in cleaned JH")
  
  return(as.character(con[x]))
}
