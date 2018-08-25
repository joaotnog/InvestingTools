#===============================================================================================================#
##########====================================VALUE INVESTING TOOLS====================================##########
#===============================================================================================================#

#=======================================================#
#                     SETTINGS & LIBRARIES
#=======================================================#

#Libaries:
library(plyr)
library(quantmod)
library(dplyr)
library(reshape2)
library(tidyr)
library(data.table)

#Settings:
set.seed(123)

#=======================================================#
#                     IMPORT & FORMAT
#=======================================================#

#Define paths:
root_path <- "C:/Users/Ste/Desktop/Investing/ValueInvestingTools/Data/" #Define path.
data_2013_2018_path <- paste0(root_path, "result_2013_to_2018_df.csv")
data_2009_2013_path <- paste0(root_path, "result_2009_to_2013_df.csv")

#Perform importing:
IMPORT_FORMAT <- function(dataset_path, use_sample){
  #### Formats names, removes rows where KPIs are all missing. ####
  
  #Import:
  dataset <- read.csv(dataset_path) #Import dataset.
  
  #Take a small sample of the data for testing purposes:
  if(use_sample == TRUE){
    dataset <- sample_n(dataset, 10000)
  }
  
  #Format col names:
  names(dataset) <- gsub("\\.", "_", toupper(names(dataset))) #Replace periods with underscores and make all upper case.
  names(dataset) <- gsub("__", "_", names(dataset)) #Replace multiple underscores with just one.
  names(dataset) <- gsub("___", "_", names(dataset)) #Replace multiple underscores with just one.
  names(dataset) <- gsub("____", "_", names(dataset)) #Replace multiple underscores with just one.
  names(dataset) <- gsub("_____", "_", names(dataset)) #Replace multiple underscores with just one.
  names(dataset) <- gsub("______", "_", names(dataset)) #Replace multiple underscores with just one.
  names(dataset)[names(dataset) == "COPANY"] <- "COMPANY" 
  names(dataset)[names(dataset) == "NET_INCOME_FROM_DISCONTINUED_OP_"] <- "NET_INCOME_FROM_DISCONTINUED_OP" #Rename.
  names_order <- names(dataset)[names(dataset) != c("COMPANY", "DATE")] #Re-ordering names - take everything except company and date.
  names_order <- append(c("COMPANY", "DATE"), names_order) #Re-ordering names - use append on names list so that company and date come first.
  dataset <- dataset[, names_order] #Perform re-ordering of DF.
  
  #Remove rows where all KPIs are NA:
  kpi_list <- names_order[!names_order %in% c("COMPANY", "DATE", "X", "SHARE_PRICE", "MARKET_CAPITALISATION", "COMMON_SHARES_OUTSTANDING")] #All KPI col names.
  all_kpis_na <- rowSums(is.na(dataset[, c(kpi_list)])) != ncol(dataset[, c(kpi_list)]) #Indices for rows with all KPIs missing.
  dataset <- dataset[all_kpis_na, ] #Filter data based on above.
  
  #Format variable classes:
  dataset$COMPANY <- as.character(as.factor(dataset$COMPANY)) #Factor to character for company.
  dataset$DATE <- as.character(as.factor(dataset$DATE)) #Factor to character for date.
  dataset[, kpi_list] <- as.data.frame(sapply(dataset[, kpi_list], as.numeric)) #KPIs should be numeric.
  
  #Create new date variables:
  dataset$DATE <- as.numeric(gsub("-", "", dataset$DATE))
  dataset$YEAR <- as.numeric(substr(dataset$DATE, 1, 4))
  dataset$MONTH <- as.numeric(substr(dataset$DATE, 5, 6))
  dataset$DAY <- as.numeric(substr(dataset$DATE, 7, 8))
  
  #Return formatted dataset:
  return(dataset)
}

data_2013_2018_df <- IMPORT_FORMAT(data_2013_2018_path, use_sample = FALSE)
data_2009_2013_df <- IMPORT_FORMAT(data_2009_2013_path, use_sample = FALSE)

#Join the two datasets:
main_df <- rbind(data_2013_2018_df, data_2009_2013_df)
rm(data_2013_2018_df, data_2009_2013_df) #Remove components of main dataset.

#=======================================================#
#                     PRELIMINARY ASSESSMENT
#=======================================================#

#Asses variable classes:
unlist(lapply(main_df, function(x){class(x)})) #Class of all variables.

#Assess NAs:
unlist(lapply(main_df, function(x){sum(is.na(x))})) #NAs of all variables.


#=======================================================#
#                     JOIN STOCK PRICES
#=======================================================#

symbols <- unique(as.character(main_df$COMPANY))

stocks.raw <- list()
for (s in symbols)
{
  print(paste0("Reading ",s,"..."))
  try({stocks.raw[[s]] <- get(getSymbols(s))})
}

stocks = lapply(stocks.raw, function(x) {
  dt <- as.data.frame(x) %>%
    select(contains("Adjusted")) %>%
    mutate(date = index(x))
  return(dt)
}) %>%
  join_all(type="full") %>%
  gather(COMPANY, price, -date) %>%
  mutate(COMPANY = gsub(".Adjusted","",COMPANY))

main_df$date = as.Date(main_df$DATE,"%Y%m%d")
  
main_df <- main_df %>%
  left_join(stocks) %>%
  select(-date)

