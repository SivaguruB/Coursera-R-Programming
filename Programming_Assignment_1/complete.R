complete <- function(directory, id = 1:332) {
  
  
  dataframe = NULL  ## initializing the dataframe we want from this function   
  setwd(file.path(getwd(), directory)) ## setting the directory
  
  #Looping thru the directory's files specified in the 'id' argument 
  for (i in id)
{
    
    
      ## Due to the format of the filename, i.e 001, 010  instead of 1, 10. I became aware that the following method works 
      ##but not efficient, 
      ## but at the time of the completion of this assignment, it was the only way I knew how to do it.           
      if (i <10) { 
                         data <- read.csv(paste("0","0", as.character(i), ".csv", sep=""),  
                         ## for example, if 'id' =7, we get 007.csv
                                          header = T, 
                                          na.strings=c("NA","NaN", " "))
                 }
      
else if (i>=10 & i<100) { 
                         data <- read.csv(paste("0", as.character(i), ".csv", sep=""),  
                         ## for example, if 'id' = 17, we get 017.csv
                                          header = T, 
                                          na.strings=c("NA","NaN", " ") 
                                          )
                        }
                     
       

     else       { 
                        data <- read.csv(paste(as.character(i), ".csv", sep=""),     
                        ## Normal
                                         header = T, 
                                         na.strings=c("NA","NaN", " ") 
                                        )
                }
  
  ## getting rid of all the "NA" values and, consequently, all the non-complete ovservations 
  ##(the ones with at least one NA in row)
    data = na.omit(data) 
 ##  make it a matrix to easily fill each successive row of our dataframe
    data = as.matrix(data)
    dataframe = rbind(dataframe, c(i,nrow(data)))
    # fill each successive row of our dataframe. Each row contains the monitor ID,
    # and its total complete observed cases (no rows containg NAs)
  }
  
  setwd("..")  # reseting working directory path
  dataframe = data.frame(dataframe)  # from matix to data frame 
  names(dataframe) = c('id', 'nobs') # set the column names of the data frame
  return (dataframe) 
}
