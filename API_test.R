##############API test##############

#=======================================================#
#                     LOAD PACKAGES
#=======================================================#

library(tidyverse)
library(httr)
library(jsonlite)

#=======================================================#
#                     BUILD REQUEST
#=======================================================#

#Example:
##https://simfin.com/api/v1/companies/id/{companyId}

#API key:
api_key <- "gt14fI6OUddLNfBbRLzZVN3726imMqEk" #Obtained from: https://simfin.com/data/access/api.

#Define root url:
url_api_root <- "https://simfin.com/api/v1/companies/id/"

#Build request:
GET_BY_SFID <- function(sfid, type, api_key){
  #### Returns data of "type" for a given SimFin ID "sfid". ####
  
  #Define url root extensions for data return type:
  url_extn_list <- list("available_statements" = "/statements/list/",
                        "statements_orig" = "/statements/original/",
                        "statements_stan" = "/statements/standardised/",
                        "ratios" = "/ratios/",
                        "shares_agg" = "/shares/aggregated/",
                        "share_price" = "/shares/prices/",
                        "share_classes" = "/shares/classes/list/") 
  
  #Put together request URL:
  request_url <- paste0(url_api_root, sfid, url_extn_list[[type]], "?api-key=", api_key)

  #Perform request:
  cat("Performing request for SFID:", sfid, type, "...", "\n",
      "\t", "URL: ", request_url, "\n", "\n")

  start_time <- Sys.time() #Starting time.
  request <- GET(request_url) #Request.
  finish_time <- Sys.time() #Ending time.
  request_status <- status_code(request) #Request status code.

  cat("Request for", sfid, " :complete.", "\n",
        "\t", "Status code: ", request_status, "\n",
        "\t", "Request duration: ", finish_time - start_time,  "\n", "\n")

  #Return result:
  return(request)
}

#Example request:
request_1 <- GET_BY_SFID(sfid = "65735", type = "ratios", api_key)

#=======================================================#
#                     EXAMINE RESPONSE
#=======================================================#

#Examine response components:
names(request_1)

#Process API request content:
request_1_content <- content(request_1)

#Apply function across all list elements to extract the name and address of each repo:
request_1_df <- lapply(request_1_content,
                       function(x){
                         df <- data_frame(INDICATOR_ID = x$name,
                                          INDICATOR_NAME = x$html_url,
                                          VALUE = x$git_commits_url,
                                          PERIOD = x$period,
                                          F_YEAR = x$fyear,
                                          CURRENCY = x$currency,
                                          PERIOD_END_DATE = x$`period-end-date`)
                         }) %>% bind_rows()