## Part 1

pollutantmean <- function(directory, pollutant, id = 1:332) {
  # Get full path of the specsdata folder
  directory <- paste(getwd(), "/", directory, "/", sep="")
  
  # variables
  monitors <- list.files(directory)
  data <- NA
  #For each id
  for (i in id) {
    #Read the file,
  file_monitor <-  paste(directory, monitors[i], sep="")
    file_data <- read.csv(file_monitor)
    
    # accumulate the data
    data <- rbind(data, file_data)
  }
  # Calculate the mean and return it
  mean(data[[pollutant]], na.rm = TRUE)
}

test<-pollutantmean("specdata", "sulfate", 1:10)
test

test2<-pollutantmean("specdata", "nitrate", 70:72)
test2

test3<-pollutantmean("specdata", "nitrate", 23)
test3

## Part 2

complete<- function(directory, id = 1:332) {
  # Get directory
  directory <- paste(getwd(), "/", "specdata", "/", sep="")
  
  # variables
  monitors<-list.files(directory)
  monitor<- vector()
  cases<- vector()
  
  #For each id
  for (i in id) {
    #Read the file,
    file_monitor <- paste(directory, monitors[i], sep="")
    file_csv <- read.csv(file_monitor)
    
    # accumulate data
    monitor = c(monitor, i)
    cases = c(cases, sum(complete.cases(file_csv)))
  }

  
  #create data frame
  data.frame(id = monitor, nobs = cases)
  
}

test_complete<- complete("specdata", 1)
test_complete

test_complete2<- complete("specdata", c(2, 4, 8, 10, 12))
test_complete2

## Part 3

corr<- function(directory, threshold = 0){
  # get directory
  directory <- paste(getwd(), "/", "specdata", "/", sep="")
  
  # get observations
  observations <- complete(directory)
  filter <- subset(observations, observations$nobs > threshold)
  
  # variables
  
  monitors <- list.files(directory)
  correlation <- vector()
  
  for(i in filter$id) {
  #read file
  file_monitor <- paste(directory, monitors[i], sep="")
  file_csv <- read.csv(file_monitor)
  
  # remove na
  file_csv<- subset(file_csv, complete.cases(file_csv))
  
  # calculate correlation
   correlation<-c(correlation, cor(file_csv$nitrate, file_csv$sulfate))
  
  }
  correlation 
}
cr <- corr("specdata", 150)
head(cr)
