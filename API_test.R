#===============================================================================================================#
##########==================================== API TEST ===============================================##########
#===============================================================================================================#

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

#Format API request content:
for(i in 1:length(request_1_content)){
  
  #Initialise dataframe:
  if(i == 1){ #On first loop...
    request_1_df <- data.frame(matrix(nrow = 0, ncol = length(names))) #Intialise the data frame.
    request_1_df <- request_1_df %>% mutate_all(as.character) #Change all to character, avoid errors to do with factors.
    }
  
  #Extract data from request content:
  vals <- as.character(request_1_content[[i]]) #Values corresponding to those names.
  
  #Assign current row of data to DF:
  request_1_df <- rbind(request_1_df, vals)
  request_1_df <- request_1_df %>% mutate_all(as.character) #Change all to character, avoid errors to do with factors.
  
  #Assign names to DF:
  if(i == length(request_1_content)){
    names(request_1_df) <- names(request_1_content[[i]]) #Assign names of columns.
  }
}


