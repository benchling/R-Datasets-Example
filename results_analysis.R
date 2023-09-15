##########################################################################################
# OBJECTIVE: Send an HTML file of an IC50 Calculation plot and a dataset
#            of IC50 statistical results to a Benchling analysis
#
# This code requires a dataframe (df) in order to work
# The dataframe should contain the columns Cell Concentration and Cell Mortality
# The code also requires values for subdomain, analysis_id, and access_token
# (see get_dataframe.R for more detail)
#
# Steps -> calculate IC50
#       -> calculate result dataset and send to Benchling S3 Bucket
#       -> create IC50 plot file and send to Benchling S3 Bucket
#       -> Patch analysis
##########################################################################################

library(httr)
library(jsonlite)
library(drc)
library(plotly)
library(htmlwidgets)
library(rstudioapi)
library(pracma)

################################################################################
# Calculate IC50
# In this example, we use the drc library and a 4 parameter log-logistics model
# to calculate a slope and IC50 value
################################################################################

model <-
  drm(
    Cell.Mortality.Mortality.24h ~ Cell.Mortality.Concentration,
    data = df,
    fct = LL.4(
      fixed = c(NA, 0, 100, NA),
      names = c("Slope", "LS Inferior", "LS Superior", "IC50")
      )
    )
summary <- summary(model)

#Create an IC50 list composed of value and slope
IC50_result <- list(
  value = round(summary$coefficients[2],2),
  slope = round(summary$coefficients[1],2))

# Create CSV file
cell            <- c(df$Cell.Line.Name[1])
hours           <- c(24)
fit_function    <- c('LL.4')
IC50            <- c(round(summary$coefficients[2],2))
std             <- c(round(summary$coefficients[4],2))
t_Value         <- c(round(summary$coefficients[6],2))
p_Value         <- summary$coefficients[8]

# Transform p-Value to scientific notation
formatC(p_Value, format = "e", digits = 4)
p_Value <-signif(p_Value, digits=3)

# create CSV dataframe
csv_df <-
  data.frame(cell, hours, fit_function, IC50, std, t_Value, p_Value)

# 4 parameter Logisitic function plot curve
logistic4 <- function(x, A, B, C, D) {
  return ((A-D) / (1.0 + ((x / C) ** B))) + D
}

##########################################################################################
# Calculate result dataset and send to Benchling S3 Bucket
##########################################################################################

# Save CSV on local path
csv_name <- "mortality.csv"
path     <- dirname(rstudioapi::getSourceEditorContext()$path)
csv_file <- paste(path, '/', csv_name, sep = "")
write.csv(csv_df, csv_file, row.names = FALSE)


############ Use the POST dataset endpoint to create a new dataset ############

# Construct url
# The resulting URL should look like: https://<subdomain>.benchling.com/api/v2-beta/datasets
api_path <- "/api/v2-beta/datasets"
url      <- paste("https://", subdomain, api_path, sep = "")

# Create a binary file
my_data <- readBin(csv_file, "raw", 10e6)

# Create nested lists providing csv file name and specifying dataset name
dataset_payload <-
  list(manifest = list(list(fileName = csv_name)), name = 'Mortality_IC50')

# Convert payload to Json format and use POST endpoint to create dataset
dataset_request <-
  httr::POST(url = url,
    body = toJSON(dataset_payload, pretty = TRUE, auto_unbox = TRUE),
    httr::accept('application/json'),httr::content_type('application/json'),
    httr::add_headers ('Authorization' = paste("Bearer", access_token)))

# Use the jsonLite library to read the Json body
dataset_body <-
  jsonlite::fromJSON(rawToChar(dataset_request$content))

# Retrieve dataset Id
result_dataset_id <- dataset_body$id

# Retrieve S3 PUT url from previous call
s3_put_url <- dataset_body$manifest$url

####### Use the PUT file endpoint to upload file in a Benchling S3 Bucket ######
# Put file in S3 bucket and add [;x-amz-server-side-encryption': 'AES256'] to
# the request headers, because we use server-side encryption
s3_file_request <-
  httr::PUT(
    url = s3_put_url,
    body = my_data,
    httr::add_headers ('x-amz-server-side-encryption' = 'AES256'))

# Change dataset status to IN PROGRESS after uploading to S3
if (s3_file_request$status_code == '200')
{
  # Construct url
  # The resulting URL should look like: https://<subdomain>.benchling.com/api/v2-beta/datasets/data_cACYhKlo)
  api_path <- "/api/v2-beta/datasets/"
  url      <-
    paste("https://", subdomain, api_path, result_dataset_id, sep = "")

  # Set Payload list as IN PROGRESS
  dataset_payload <-list(uploadStatus = "IN_PROGRESS")

  # Convert payload to Json format and use Patch endpoint to mark dataset upload as in Progress
  dataset_request  <-
    httr::PATCH(
      url = url,
      body = toJSON(dataset_payload, pretty = TRUE, auto_unbox = TRUE),
      httr::accept('application/json'),httr::content_type('application/json'),
      httr::add_headers ('Authorization' = paste("Bearer", access_token)))

  # Use the jsonLite library to read the Json body
  dataset_body <-jsonlite::fromJSON(rawToChar(dataset_request$content))

  # Print status code - 200 = Successful
  print(dataset_request$status_code)

}else {
  print('ERROR: Upload to S3 unsuccessful')
}

################################################################################
# Use plotly to create a html plot to embed in Benchling Analysis ##############
################################################################################

# Set Image Name and Path (path is set here are the local directory of the R script)
image_name <- "mortality_24h.html"

# Save html file on local path
path       <- dirname(rstudioapi::getSourceEditorContext()$path)
image_file <- paste(path, '/', image_name, sep = "")

# Use plotly to plot the IC50
plot <-
  plot_ly(
    data = df,
    x = df$Cell.Mortality.Concentration,
    y = df$Cell.Mortality.Mortality.24h,
    type = "scatter",
    mode = "markers")

plot <- layout(plot, xaxis = list(type = "log"))

xs   <-
  linspace(min(df$Cell.Mortality.Concentration),
           max(df$Cell.Mortality.Concentration),
           n=1000)

plot <-
  add_lines(plot,
            line = list(shape = "scatter"),
            x = xs,
            y = logistic4(xs, 100, IC50_result$slope, IC50_result$value, 0))

# Save html file
saveWidget(widget = plot,
           file = image_file,
           selfcontained = TRUE)

### Create a binary file
my_data <- readBin(image_file, "raw", 10e6)


############### Use the POST File endpoint to create a new File ###############

# Construct url
# The resulting URL should look like: https://<subdomain>.benchling.com/api/v2-beta/files)
api_path <- "/api/v2-beta/files"
url      <- paste("https://", subdomain, api_path, sep = "")

# Provide name of file as a payload in a list
file_payload <- list(name = image_name)

# Convert payload to Json format and use POST endpoint to create File
file_request  <-
  httr::POST(url = url,
             body = toJSON(file_payload, pretty = TRUE, auto_unbox = TRUE),
             httr::accept('application/json'),httr::content_type('application/json'),
            httr::add_headers ('Authorization' = paste("Bearer", access_token)))

# Use the jsonLite library to read the Json body
file_body <-jsonlite::fromJSON(rawToChar(file_request$content))

# Retrieve the file id
file_id <- file_body$id

# Retrieve S3 PUT url from previous call
s3_put_url <- file_request$headers$`content-location`

####### Use the PUT file endpoint to upload file in a Benchling S3 Bucket ######
# Put file in S3 bucket and add [;x-amz-server-side-encryption': 'AES256'] to the request headers, because we use server-side encryption
s3_file_request  <-
  httr::PUT(url = s3_put_url,
            body = my_data,
            httr::add_headers ('x-amz-server-side-encryption' = 'AES256'))

# Change dataset status to SUCCEEDED after uploading to S3
if (s3_file_request$status_code == '200')
{
  # Construct url
  # The resulting URL should look like: https://<subdomain>.benchling.com/api/v2-beta/files/file_cGCGhKqh)
  api_path <- "/api/v2-beta/files/"
  url <- paste("https://", subdomain, api_path, file_id, sep = "")

  # Set Payload list as SUCCEEDED
  file_payload <-list (uploadStatus = 'SUCCEEDED')

  # Convert payload to Json format and use Patch endpoint to mark file upload as completed
  file_request  <-
    httr::PATCH(url = url,
                body = toJSON(file_payload, pretty = TRUE, auto_unbox = TRUE),
                httr::accept('application/json'),httr::content_type('application/json'),
                httr::add_headers ('Authorization' = paste("Bearer", access_token)))

  # Use the jsonLite library to read the Json body
  file_body <-jsonlite::fromJSON(rawToChar(file_request$content))

  # Print status code - 200 = Successful
  print(file_request$status_code)

}else {
  print('ERROR: Upload to S3 unsuccessful')
}

################################################################################
# Use the PATCH Analysis endpoint to upload file into analysis #################
################################################################################

# Construct url
# The resulting URL should look like: https://<subdomain>.benchling.com/api/v2-beta/analysis-steps/ana_Tx98nkEm)
api_path <- "/api/v2-beta/analysis-steps/"
url      <- paste("https://", subdomain, api_path, analysis_id, sep = "")


# Create nested list containing output files and output datasets
file_data_ids <-
  list(fileIds = list(file_id),
       datasetIds = list(result_dataset_id))
outputfiles <- list(outputData = file_data_ids)

# Convert payload to Json format and use Patch endpoint to update Analyis to include output files and output datasets
analysis_request  <-
  httr::PATCH(url = url,
              body = toJSON(outputfiles, pretty = TRUE, auto_unbox = TRUE),
              httr::accept('application/json'),httr::content_type('application/json'),
              httr::add_headers ('Authorization' = paste("Bearer", access_token)))

# Use the jsonLite library to read the Json body
analysis_body <-jsonlite::fromJSON(rawToChar(analysis_request$content))

# Print status code - 200 = Successful
print(analysis_request$status_code)

# Initial status will be running and move to succeeded
# We recommend that can build a wait function to verify that it succeeded
print(analysis_body$status)
