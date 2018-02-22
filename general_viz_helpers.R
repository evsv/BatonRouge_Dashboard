#===================================================================#
# THIS SCRIPT CONTAINS GENERAL PURPOSE HELPER METHODS USED ACROSS THE
# INDIVIDUAL VIEWS FOR GRAPHING OPERATIONS
#===================================================================#

library(ggplot2)

#-------------------------------------------------------------------#
# GroupBarGen()
#
# data:           dataframe. The dataframe containing the data to 
#                 be plotted
# xVector:        string. The string containing the name of the 
#                 x-column
# yVector:        string. The string containing the name o f the 
#                 y-column
# classVector:    string. The string containing the name of the 
#                 variable to be split into different colours
# xLabel:         string. The string containing the x-axes label
# yLabel:         string. The string containing the y-axis label
#
#
# Returns:        graph, a ggplot object which contains the 
#                 graph to to be rendered
#          
#-------------------------------------------------------------------#
GroupBarGen <- function(data, xVector, yVector, classVector,
                        xLabel, yLabel){
  
  graph <- ggplot(data) +
           geom_bar(mapping = aes_string(x = xVector, y = yVector, 
                                         fill = classVector), 
                    position = "dodge", stat = "identity") +
           labs(x = xLabel, y = yLabel)
  
  return(graph)
  
}

#-------------------------------------------------------------------#
# GroupTrendLineGen()
#
# data:           dataframe. The dataframe containing the data to 
#                 be plotted
# xVector:        string. The string containing the name of the 
#                 x-column
# yVector:        string. The string containing the name o f the 
#                 y-column
# classVector:    string. The string containing the name of the 
#                 variable to be split into different colours
# xLabel:         string. The string containing the x-axes label
# yLabel:         string. The string containing the y-axis label
#
#
# Returns:        trendPlot, a ggplot object which contains the 
#                 graph to to be rendered
#          
#-------------------------------------------------------------------#
GroupTrendLineGen <- function(data, xVector, yVector, classVector,
                              xLabel, yLabel){
  
  trendPlot <- ggplot(data) +
               geom_line(mapping = aes_string(x = xVector, y = yVector, 
                                       colour = classVector)) +
               labs(x = "xLabel", y = "yLabel")
            
  return(trendPlot)
  
}