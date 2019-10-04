# get data

population <- setRefClass(Class = "population", 
                          fields = list(
                            municipality = "character",
                            year = "vector",
                            pop_list = "list",
                            summary = "data.frame",
                            growth_rate = "numeric",
                            municipalities_list = "character"
                          ),
                          
                          methods = list(
                            initialize = function(municipality, year){
                              municipality <<- municipality
                              year <<- year
                              year_char = paste(year, collapse = ",")
                              
                              
                              # fetch the data
                              kolada_api_kpi = fromJSON("http://api.kolada.se/v2/kpi")
                              kolada_api_municipality = fromJSON("http://api.kolada.se/v2/municipality")
                              
                              
                              # list of municipalities
                              municipalities_list <<- kolada_api_municipality$values$title
                              
                              
                              # id of dataset and municipalities
                              kpi_id = paste(kolada_api_kpi$values$id[428], collapse = ",")
                              municipality_id = kolada_api_municipality$values$id[kolada_api_municipality$values$title == municipality]
                              pop_url = paste("http://api.kolada.se/v2/data/kpi/", kpi_id, "/municipality/", municipality_id, "/year/", year_char, sep = "")
                              pop_list <<- fromJSON(pop_url)
                              
                              pop_matrix = c()
                              for (i in (1:length(year))){
                                pop_row = c(year[i], pop_list$values$values[[i]]$value[1], pop_list$values$values[[i]]$value[2], pop_list$values$values[[i]]$value[3])
                                pop_matrix = rbind(pop_matrix, pop_row)
                              }
                              
                              rownames(pop_matrix) = NULL
                              colnames(pop_matrix) = c("Year", "Female", "Male", "Total")
                              summary <<- as.data.frame(pop_matrix)
                              
                            },
                            
                            municipalities = function(){
                              return(municipalities_list)
                            }
                          ))


pop1 = population$new("Stockholm", 1998:2018)
pop1$summary
