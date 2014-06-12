# Write a function named 'pollutantmean' that calculates the mean of a pollutant (sulfate or nitrate) 
# across a specified list of monitors. The function 'pollutantmean' takes three arguments: 
# 'directory', 'pollutant', and 'id'. 
# Given a vector monitor ID numbers,  'pollutantmean' reads that monitors' particulate matter data 
# from the directory specified in the 'directory' argument and returns the mean of the pollutant across
# all of the monitors, ignoring any missing values coded as NA. 

## 'directory' is a character vector of length 1 indicating the location of the CSV files
## 'pollutant' is a character vector of length 1 indicating the name of the pollutant for which we will calculate 
## the mean; either "sulfate" or "nitrate".
## 'id' is an integer vector indicating the monitor ID numbers to be used
## Return the mean of the pollutant across all monitors list in the 'id' vector (ignoring NA values)

#____________________________________________________________________________________________________#

# add the folder name to the working directory
workDirectory <- getwd()
#create a list to store the individual file result and data frame to hold all the results
resultList <- list("File" = NULL, "Pollutant" = NULL, "Mean" = 0.0)
df <- data.frame(File = character(), Pollutant = character(), Mean = numeric())
pollutantmean <- function(directory, pollutant, id = 1:332) {

  fil<-NULL
    
  for(i in 1: length(id))
       {
         # calculate the names of required files for calculating the mean   
         if(id[i]<10)
           {
            fil<- paste(workDirectory,"/", directory,"/00",id[i],".csv", sep="")
           }
         else if(id[i]<100){
           fil<- paste(workDirectory, "/",directory,"/0",id[i],".csv", sep="")
         }
         else if(id[i]<333){
           fil<- paste(workDirectory, "/",directory,"/",id[i], ".csv", sep="")
         }
         else
         {
           print("Too many files entered")
         }
         
         # call function to calculate the mean for each separate file and store the information in the
         # list for each individual list
        resultList<- calMean(fil, pollutant)
        df <- rbind(df, data.frame(File=resultList$File,Pollutant=resultList$Pollutant, Mean = resultList$Mean))
        #listMean<-mean(df$Mean, na.rm=TRUE)
        #print(listMean)
        #print(resultList$Mean)
  }
  #print(df)
  listMean<-mean(df$Mean, na.rm=TRUE)
  print(listMean)
  #return(df)
}


# function to calculate mean for given pollutant in given file (fil)
calMean<- function(fil, pollutant){
  dat<- read.csv(fil,header=TRUE)
  datMean<-dat[[pollutant]]
  dataMean<-mean(datMean, na.rm=TRUE)
  #print(dataMean)
  resultList <- list("File" = fil, "Pollutant" = pollutant, "Mean" = dataMean)
  return(resultList)
  

}