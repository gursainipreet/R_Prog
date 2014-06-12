# add the folder name to the working directory
workDirectory <- getwd()
#create a list to store the individual file result and data frame to hold all the results
resultList <- list("File" = NULL, "Pollutant" = NULL, "Mean" = 0.0)
df <- data.frame(id = character(), nobs = numeric())

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



complete <- function(directory, id = 1:332) {
      
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
            comCases<- calcCompMean(fil)
            #df <- data.frame(id = character(), nobs = numeric())
            df <- rbind(df, data.frame(id=toString(id[i]),nobs = comCases))
            #listMean<-mean(df$Mean, na.rm=TRUE)
            #print(listMean)
            #print(resultList$Mean)
      }
      print(df)
 }

# function to calculate complete cases in individual file (fil)
calcCompMean<- function(fil){
      dat<- read.csv(fil,header=TRUE)
      ok <- complete.cases(dat)
      sumOk <- sum(ok) # how many are not "ok" ?
      return(sumOk)
      
      
}