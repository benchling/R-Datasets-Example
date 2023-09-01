################################################################################
# OBJECTIVE: Get Dataset from Benchling and save it as an R Dataset ############
################################################################################

library(httr)
library(jsonlite)
library(base64enc)

# Get Analysis Key from Benchling tenant
# INPUT
analysis_key <- "<Replace with your analysis step key>"

# Get the analysis ID by extracting string prior to : in analysis_key
analysis_id <- gsub(":.*$", "", analysis_key)

# Get the JWT Token and decode it to extract the subdomain of the tenant
split_key <- strsplit(analysis_key, ".", fixed = TRUE)
token     <-
  jsonlite::fromJSON(rawToChar(base64decode(split_key[[1]][2])))
subdomain <- token$aud

###### Generate Bearer Token ######

# Construct url
# The resulting URL should look like: https://subdomain.benchling.com/api/v2/token
api_path <- '/api/v2/token'
url      <- paste("https://", subdomain, api_path, sep = "")

# Provide Client ID and Secret
# EDIT: I added these variables to make it easier for customers to input their own values;
#       can you double check my syntax?
# INPUT
client_id       <- "<Replace with your app's client id>"
client_secret   <- "<Replace with your app's client secret>"
request_payload <-
  paste("client_id=",client_id,"&client_secret=",client_secret,"&grant_type=client_credentials", sep = "")

# Perform a post request and get the token
token_request <- httr::POST(
  url = url,
  body = request_payload,
  httr::accept('application/json'),
  httr::content_type('application/x-www-form-urlencoded')
)

# Use the jsonLite library to read the Json body
request_body <- jsonlite::fromJSON(rawToChar(token_request$content))

# Bearer Token needs to be regenerated every 900 seconds
access_token <- request_body$access_token

#############################################################
# Use the Get analysis endpoint and retrieve the dataset id #
#############################################################

# Construct url
# The resulting URL should look like: https://subdomain.benchling.com/api/v2-beta/analysis-steps/ana_ABCD1234
api_path <- "/api/v2-beta/analysis-steps/"
url      <-
  paste("https://", subdomain, api_path, analysis_id, sep = "")

# Perform a get request
analysis_response <-
  httr::GET(url,
            add_headers(
              Accept = 'application/json',
              Authorization = paste("Bearer", access_token, sep = " ")
            ))

# Use the jsonlite library to read the JSON body
analysis_body <-
  jsonlite::fromJSON(rawToChar(analysis_response$content))

# Get the dataset ID
# Note: This assumes there is a single input dataset
dataset_id <- c(analysis_body$inputData$datasetIds)


#############################################################
# Use the Get dataset endpoint and retrieve the dataset id #
#############################################################

# Construct url
# The resulting URL should look like: https://subdomain.benchling.com/api/v2-beta/datasets/dset_bHbGo1FP79Kl)
api_path <- "/api/v2-beta/datasets/"
url <- paste("https://", subdomain, api_path, dataset_id, sep = "")

# Perform a get request
dataset_response <-
  httr::GET(url,
            add_headers(
              Accept = 'application/json',
              Authorization = paste("Bearer", access_token, sep = " ")
            ))

# Use the jsonlite library to read the JSON body
dataset_body <-
  jsonlite::fromJSON(rawToChar(dataset_response$content))

# Retrieve the url to retrieve the dataset in CSV format
dataset_url <- c(dataset_body$manifest$url)

# Read the dataset csv, save it as an R dataframe and let the R magic begin
df <- read.csv(dataset_url)
