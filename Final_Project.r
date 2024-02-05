# install and load the necessary packages
install.packages("httr")
install.packages("rvest")

library(httr)
library(rvest)

# get data from a wikipedia page
get_wiki_covid19_page <- function() {
  wiki_base_url <- "https://en.wikipedia.org/w/index.php"
  wiki_parameter <- "title=Template:COVID-19_testing_by_country"
  full_url <- paste(wiki_base_url, "?", wiki_parameter, sep = "")
  
  response <- GET(url = full_url)
  return(response)
}

# call the function and print the response
wiki_html <- get_wiki_covid19_page()
print(wiki_html)

# get the root node from the html
parsed_html <- read_html(wiki_html)
root_node <- xml2::xml_root(parsed_html)
print(root_node)

# get the table node and convert it to a data frame
table_node <- html_nodes(root_node,"table")
data_frame <- as.data.frame(html_table(table_node[2]))

# preprocess the data frame
preprocess_covid_data_frame <- function(data_frame) {
  
  shape <- dim(data_frame)
  
  # remove the world row
  data_frame<-data_frame[!(data_frame$`Country.or.region`=="World"),]
  # Remove the last row
  data_frame <- data_frame[1:172, ]
  
  # remove units and ref columns
  data_frame["Ref."] <- NULL
  data_frame["Units.b."] <- NULL
  
  # renaming the columns
  names(data_frame) <- c("country", "date", "tested", "confirmed", "confirmed.tested.ratio", "tested.population.ratio", "confirmed.population.ratio")
  
  # convert column data types
  data_frame$country <- as.factor(data_frame$country)
  data_frame$date <- as.factor(data_frame$date)
  data_frame$tested <- as.numeric(gsub(",","",data_frame$tested))
  data_frame$confirmed <- as.numeric(gsub(",","",data_frame$confirmed))
  data_frame$'confirmed.tested.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.tested.ratio`))
  data_frame$'tested.population.ratio' <- as.numeric(gsub(",","",data_frame$`tested.population.ratio`))
  data_frame$'confirmed.population.ratio' <- as.numeric(gsub(",","",data_frame$`confirmed.population.ratio`))
  
  return(data_frame)
}
preprocess_covid_data_frame(data_frame)

# export the data frame to a .csv file
write.csv(data_frame, "covid.csv", row.names=FALSE)
wd <- getwd()
file_path <- paste(wd, sep = "", "/covid.csv")

#import the data frame from .csv file and remove final row
df_covid_full <- read.csv(file_path)
df_covid <- df_covid_full[-173, ]

# return the 5th to 10th rows with only the country and confirmed columns
df_confirmed <- df_covid[5:10, c(1, 5)]
df_confirmed

# calculate the worldwide covid testing positive ratio
confirmed_vector_raw <- as.vector(df_covid[ ,5])
confirmed_vector <- gsub(",", "", confirmed_vector_raw)
confirmed_vector_num <- as.numeric(confirmed_vector)
confirmed <- sum(confirmed_vector_num)

tested_vector_raw <- as.vector(df_covid[ ,3])
tested_vector <- gsub(",", "", tested_vector_raw)
tested_vector_num <- as.numeric(tested_vector)
tested <- sum(tested_vector_num)

ratio <- confirmed/tested
ratio

# get a country list which reported their testing data
countries_factor <- as.factor(df_covid[ ,1])
countries <- as.character(countries_factor)

# sort countries alphabetically and reverse alphabetically
countries_AtoZ <- sort(countries)
countries_AtoZ

countries_ZtoA <- sort(countries, decreasing = TRUE)
countries_ZtoA

# identify countries beginning with "united"
united_countries <- grep("United.+", countries, value = TRUE)
print(united_countries)

# pick two countries and review their testing data
belgium <- subset(df_covid[17, ])
belgium

sudan <- subset(df_covid[152, ])
sudan

#compare the two countries to find which has the larger ratio of confirmed cases to population
belgium_row <- df_covid[df_covid$Country.or.region == "Belgium", ]
sudan_row <- df_covid[df_covid$Country.or.region == "Sudan", ]

belgium_confirmed_ratio <- as.numeric(belgium_row$Confirmed..population..)
sudan_confirmed_ratio <- as.numeric(sudan_row$Confirmed..population..)

if(belgium_confirmed_ratio > sudan_confirmed_ratio) {
  print("Belgium has a greater ratio of confirmed cases to population")
} else {
  print("Sudan has a greater ratio of confirmed cases to population")
}

# find countries with confirmed to population ratio less than 1%
subset(df_covid, subset = Confirmed..population.. < 1)