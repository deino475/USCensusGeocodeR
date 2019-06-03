#' @title USCensusGeocodeR
#'
#' @description This package allows for individuals to easily geocode address parts using the US Census Geocoding API Services. By passing in the Street Address, City, State, and/or Zip Code, you   can find information such as the GEOID of the block group the address resides in as well as the coordinates of the block group the address resides in.
#'
#' @param street This is a string or vector of strings containing the street address.
#' @param city This is a string or vector of strings that contain the city.
#' @param state This is a string or vector of strings that contain the two-letter state abbreviations.
#' @param zip This is a string or vector of strings that contain the 5-character zip code.
#'
#' @return dataframe
#'
#' @examples geocode("1600 Pennsylvania Ave","Washington","DC","22052")
#'
#' @export geocoding_api

geocode <- function(street, city = "", state = "", zip = "") {
  #Replace Spaces with Plus Signs
  street <- gsub(" ","+", street)
  city <- gsub(" ","+",city)

  #Create Base URL
  base_url = "https://geocoding.geo.census.gov/geocoder/geographies/address?benchmark=Public_AR_Census2010&format=json&vintage=Census2010_Census2010&layers=14&"

  #Append URL param arguments
  street = paste("&street=", street, sep = "")
  city = paste("&city=", city, sep = "")
  state = paste("&state=", state, sep = "")
  zip = paste("&zip=", zip, sep = "")

  #Create vector of URLs
  req_url = paste(base_url, street, city, state, zip, sep = "")

  #Create an emptyp data frame for storing information
  return_data <- data.frame(
    "BLKGRP" = rep("", length(req_url)),
    "STATE" = rep("", length(req_url)),
    "CENTLON" = rep("", length(req_url)),
    "BLOCK" = rep("",length(req_url)),
    "BASENAME" = rep("", length(req_url)),
    "INTPLAT" = rep("", length(req_url)),
    "POP100" = rep("", length(req_url)),
    "COUNTY" = rep("", length(req_url)),
    "GEOID" = rep("", length(req_url)),
    "CENTLAT" = rep("", length(req_url)),
    "INTPLON" = rep("", length(req_url)),
    "TRACT" = rep("", length(req_url))
  )

  #Turn data frame elements to a character value
  return_data$GEOID <- as.character(return_data$GEOID)
  return_data$BLKGRP <- as.character(return_data$BLKGRP)
  return_data$STATE <- as.character(return_data$STATE)
  return_data$CENTLON <- as.character(return_data$CENTLON)
  return_data$BLOCK <- as.character(return_data$BLOCK)
  return_data$BASENAME <- as.character(return_data$BASENAME)
  return_data$POP100 <- as.character(return_data$POP100)
  return_data$COUNTY <- as.character(return_data$COUNTY)
  return_data$GEOID <- as.character(return_data$GEOID)
  return_data$CENTLAT <- as.character(return_data$CENTLAT)
  return_data$TRACT <- as.character(return_data$TRACT)

  #For each URL in the vector of urls
  for (i in 1:length(req_url)) {
    #Make the GET request
    req <- GET(req_url[[i]])
    #Retrieve the content from the GET request
    resp <- content(req)
    #Save the first match in the data frame.
    return_data[i,]$BLKGRP = resp$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$BLKGRP
    return_data[i,]$STATE = resp$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$STATE
    return_data[i,]$CENTLON = resp$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$CENTLON
    return_data[i,]$BLOCK = resp$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$BLOCK
    return_data[i,]$BASENAME = resp$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$BASENAME
    return_data[i,]$INTPLAT = resp$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$INTPLAT
    return_data[i,]$POP100 = resp$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$POP100
    return_data[i,]$COUNTY = resp$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$COUNTY
    return_data[i,]$GEOID = resp$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$GEOID
    return_data[i,]$CENTLAT = resp$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$CENTLAT
    return_data[i,]$TRACT = resp$result$addressMatches[[1]]$geographies$`Census Blocks`[[1]]$TRACT
  }
  return(return_data)
}
