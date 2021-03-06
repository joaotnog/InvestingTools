#===============================================================================================================#
##########==================================== API TEST ===============================================##########
#===============================================================================================================#

#=================================================================#
#                     LOAD PACKAGES
#=================================================================#

library(tidyverse)
library(httr)
library(jsonlite)
library(dplyr)

#=================================================================#
#                     SETUP FOR TESTING
#=================================================================#

#API key:
api_key <- "gt14fI6OUddLNfBbRLzZVN3726imMqEk" #Obtained from: https://simfin.com/data/access/api.

#Define root url:
url_api_root <- "https://simfin.com/api/v1/companies/id/"


#Example request:
eg_simfin_id <- "65735"

#=================================================================#
#                     GET SIMFIN ID BY TICKER ##WORKING
#=================================================================#

GET_SFID_BY_TICKER <- function(ticker, api_key){
  #### Finds SimFin ID associated with ticker. ####
  
  #Construct request URL:
  url_api_root <- "https://simfin.com/api/v1/info/find-id/ticker/" #Define root url for this query.
  request_url <- paste0(url_api_root, ticker, "?api-key=", api_key)
  
  #Perform request:
  cat("Performing request for TICKER:", ticker, "...", "\n",
      "\t", "URL: ", request_url, "\n", "\n")
  
  start_time <- Sys.time() #Starting time.
  response <- GET(request_url) #Request.
  finish_time <- Sys.time() #Ending time.
  request_status <- status_code(response) #Request status code.
  
  cat("Request for", ticker, ": complete.", "\n",
      "\t", "Status code: ", request_status, "\n",
      "\t", "Request took: ", finish_time - start_time,  "\n", "\n")
  
  #Extract content:
  response_content <- content(response)
  if(request_status == 400){ #Stop process if error:
    stop(paste0("Error code: 400! Bad request: ", response_content[[1]]))
  }
  
  #Format response content:
  cat("Processing response content...")
  for(i in 1:length(response_content)){
    
    #Initialise dataframe:
    if(i == 1){ #On first loop...
      response_colnames <- names(response_content[[i]]) #Extract colnames.
      response_df <- data.frame(matrix(nrow = 0, ncol = length(response_colnames))) #Intialise the data frame.
      response_df <- response_df %>% mutate_all(as.character) #Change all to character, avoid errors to do with factors.
    }
    
    #Extract data from request content:
    vals <- as.character(response_content[[i]]) #Values corresponding to those names.
    
    #Assign current row of data to DF:
    response_df <- rbind(response_df, vals)
    response_df <- response_df %>% mutate_all(as.character) #Change all to character, avoid errors to do with factors.
    
    #Final formatting:
    if(i == length(response_content)){
      names(response_df) <- names(response_content[[i]]) #Assign names of columns.
      response_df[response_df == "NULL"] <- NA
    }
  }
  cat("Processing complete.")
  
  #Return response data content:
  return(response_df)
}

#Example:
example_1 <- GET_SFID_BY_TICKER("AAPL", api_key)

#=================================================================#
#                     GET GENERAL INFO BY SIMFIN ID ##WORKING
#=================================================================#

GET_INFO_BY_SFID <- function(sfid, api_key){
  #### Finds company general info by ticker. ####
  
  #Construct request URL:
  url_api_root <- "https://simfin.com/api/v1/companies/id/" #Define root url for this query.
  request_url <- paste0(url_api_root, sfid, "?api-key=", api_key)
  
  #Perform request:
  cat("Performing request for SFID:", sfid, "...", "\n",
      "\t", "URL: ", request_url, "\n", "\n")
  
  start_time <- Sys.time() #Starting time.
  response <- GET(request_url) #Request.
  finish_time <- Sys.time() #Ending time.
  request_status <- status_code(response) #Request status code.
  
  
  cat("Request for", sfid, ": complete.", "\n",
      "\t", "Status code: ", request_status, "\n",
      "\t", "Request took: ", finish_time - start_time,  "\n", "\n")
  
  #Extract content:
  response_content <- content(response)
  if(request_status == 400){ #Stop process if error:
    stop(paste0("Error code: 400! Bad request: ", response_content[[1]]))
  }
  
  #Format response content:
  cat("Processing response content...")
  
  response_colnames <- names(response_content) #Extract colnames.
  response_df <- data.frame(matrix(nrow = 0, ncol = length(response_colnames))) #Intialise the data frame.
  response_df <- response_df %>% mutate_all(as.character) #Change all to character, avoid errors to do with factors.
  vals <- as.character(response_content) #Extract data. Values corresponding to names.
  response_df <- rbind(response_df, vals)
  response_df <- response_df %>% mutate_all(as.character) #Change all to character, avoid errors to do with factors.
  names(response_df) <- names(response_content) #Assign names of columns.
  response_df[response_df == "NULL"] <- NA #Deal with NULLs.
  
  cat("Processing complete.")
  
  #Return response data content:
  return(response_df)
}

#Example:
example_2 <- GET_INFO_BY_SFID("65735", api_key)

#=================================================================#
#                     GET GENERAL INFO BY SIMFIN ID ##WIP
#=================================================================#

GET_STATEMENTS_BY_SFID <- function(sfid, api_key, fyear,
                                  stype = c("pl", "bs", "cf"), 
                                  ptype = c("TTM", "TTM-1", "TTM-2", "Q1", "Q2", "Q3", "Q4", "H1", "H2", "9M", "FY")
                                  ){
  #### Returns company statements as DF, by SFID. ####
  
  #Evaluate argument choice:
  stype <- match.arg(stype)
  if(missing(ptype)){
    ptype <- "TTM"
  } else{
    ptype <- match.arg(ptype)
  }

  #Construct request URL:
  url_api_root <- "https://simfin.com/api/v1/companies/id/" #Define root url for this query.
  request_base_url <- paste0(url_api_root, sfid, "/statements/standardised") #Base of URL.
  query_list <- list("api-key" = api_key, "stype" = stype, "ptype" = ptype, "fyear" = fyear) #Build list of query parameters.

  #Perform request:
  cat("Performing request for SFID:", sfid, "\n",
        "\t", "Statement type:", stype, ",", "Period type:", ptype, "\n",
        "\t", "URL: ", request_url, "\n", "\n")
  
  start_time <- Sys.time() #Starting time.
  response <- GET(request_base_url, query = query_list) #Request.
  finish_time <- Sys.time() #Ending time.
  request_status <- status_code(response) #Request status code.
  
  cat("Request for", sfid, ": complete.", "\n",
      "\t", "Status code: ", request_status, "\n",
      "\t", "Request took: ", finish_time - start_time,  "\n", "\n")
  
  #Extract content:
  response_content <- content(response)
  if(request_status == 400){ #Stop process if error:
    stop(paste0("Error code: 400! Bad request: ", response_content[[1]]))
  }
  
  #Format response content:
  cat("Processing response content...")
  
  response_content_main <- response_content$values
  main_colnames <- unique(names(unlist(response_content_main))) #Extract colnames.
  response_df <- data.frame(matrix(nrow = 0, ncol = length(response_colnames))) #Intialise the data frame.
  response_df <- response_df %>% mutate_all(as.character) #Change all to character, avoid errors to do with factors.
  names(response_df) <- main_colnames
  for(i in 1:length(response_content_main)){
    vals <- as.data.frame(t(as.data.frame(unlist(response_content_main[i]))))
    response_df <- bind_rows(response_df, vals)
  }

  cat("Processing complete.")
  
  #Return response data content:
  return(response_df)
}

#Example request:
example_3 <- GET_STATEMENTS_BY_SFID(sfid = "65735", api_key, stype = "pl", ptype = "TTM", fyear = "2018")



