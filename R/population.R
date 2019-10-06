#' Linear Regression
#' 
#' Running a Linear Multiple Reregression Model.
#' 
#' This function does not take any parameters but instead gets its data from the Kolada web-api.
#' 
#' @return Returns an object of the class 'population'. This object can be manipulated.
#' 
#' @import methods
#' @import jsonlite
#' 
#' @export population
#' @exportClass population
library(jsonlite)
population <- setRefClass(Class = "population", 
                          fields = list(
                            kpi_metadata = "list",
                            municipality_metadata = "list",
                            municipality = "character",
                            municipality_data = "data.frame",
                            municipality_name = "character"
                          ),
                          
                          methods = list(
                            initialize = function(){
                              
                              # metadata of dataset
                              kpi_metadata <<- fromJSON("http://api.kolada.se/v2/kpi")
                              municipality_metadata <<- fromJSON("http://api.kolada.se/v2/municipality")
                              
                              # id of total inhabitant data
                              kpi_id = paste(kpi_metadata$values$id[428], collapse = ",")
                              
                              
                            },
                            
                            municipalities = function(){
                              return(as.vector(municipality_metadata$values$title))
                            },
                            
                            # select only population data of a certain municipality
                            population_data = function(municipality_name){
                              municipality_id = municipality_metadata$values$id[municipality_metadata$values$title == municipality_name]
                              data_url = paste("http://api.kolada.se/v2/data/kpi/N01951", "/municipality/", municipality_id, sep = "")
                              
                              df = fromJSON(data_url)
                              
                              period = df$values$period
                              pop = c()
                              for (i in 1:length(period)){
                                pop = c(pop, df$values$values[[i]]$value[3])
                              }
                              
                              municipality_data <<- as.data.frame(cbind(period, pop))
                              
                              
                              return(municipality_data)
                            }
                            
                          ))
