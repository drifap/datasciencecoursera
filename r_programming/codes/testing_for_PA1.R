complete <- function(directory, id = 1:332, pollutant) {
  ## 'directory' is a character vector of length 1 indicating
  ## the location of the CSV files
  
  ## 'id' is an integer vector indicating the monitor ID numbers
  ## to be used
  
  ## Return a data frame of the form:
  ## id nobs
  ## 1  117
  ## 2  1041
  ## ...
  ## where 'id' is the monitor ID number and 'nobs' is the
  ## number of complete cases

  make_id <- id
  make_pol <- pollutant

  data1 <- data.frame(row.names=c("id","mean"))
  
  for (i in 1:length(id)){
    while (nchar(id[i]) < 3){
      id[i] <- paste("0",id[i],sep="")
    }  
    file <- paste(directory,"/", id[i], ".csv", sep = "")
    b <- read.csv(file)
    b <- b[complete.cases(b[,pollutant]),]
    
    rows <- c(make_id[i],b[,make_pol])
    data1 <- rbind(data1, rows)
    
  }
  
  names(data1) <- c("id","sum")
  print(data1)
  
  mean(data1[,2])
  
}

