pollutantmean <- function(directory, pollutant, id = 1:332) 
    {
      data <- data.frame();
      files <- list.files(directory, full.names = TRUE);
  
      for (index in files) 
        {
        data <- rbind(data, read.csv(index, comment.char = ""))
        }
  
      neededMonitors <- subset(data, ID %in% id);
      pollutantMean <- mean(neededMonitors[[pollutant]], na.rm = TRUE);
      pollutantMean;
    }
