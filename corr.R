workDirectory <- getwd()
#create a list to store the individual file result and data frame to hold all the results
df <- data.frame(id = character(), nobs = numeric())
lnVec = 0
vecCor <- vector(mode="numeric", length=0)
## directory' is a character vector of length 1 indicating
## the location of the CSV files

## 'threshold' is a numeric vector of length 1 indicating the
## number of completely observed observations (on all
## variables) required to compute the correlation between
## nitrate and sulfate; the default is 0

## Return a numeric vector of correlations

corr <- function(directory, threshold = 0) {
      # complete data frame with number of completed cases in each of the files (monitor files)
      listDF<-complete(directory, 1:332)
      
      for(i in 1:332){
            
            # print(i)             print("From For")             print(listDF$nobs[i])
            #check if number of complet cases for given file is greater than tehreshold
            # then keep adding the ids to the list
            if(listDF$nobs[i]>threshold){
                  nam<- (toString(listDF$id[i]))
                  #print(nam)
                  datComp <- read.csv(nam, header = TRUE)
                  #print(dat)
                  x<- datComp[,"sulfate"]
                  y<- datComp[,"nitrate"]
                  c<- cor(x, y, use = "complete.obs")
                  #print(c)
                  vecCor <- append(vecCor, c)
            }
      }
      return (vecCor)
}



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
            df <- rbind(df, data.frame(id=fil,nobs = comCases))
         
      }
      return(df);
}

# function to calculate complete cases in individual file (fil)
calcCompMean<- function(fil){
      dat<- read.csv(fil,header=TRUE)
      ok <- complete.cases(dat)
      sumOk <- sum(ok) # how many are not "ok" ?
      return(sumOk)
      
      
}