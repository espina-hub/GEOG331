#initial code from assignment for visualization
#make a vector of tree heights in meters
heights <- c(30,41,20,22)
#convert to cm
heights_cm <- heights*100
heights_cm
#look at the first tree height
heights[1]
#get more info on the matrix function
help(matrix)
#example matrices
#set up a matrix with 2 columns and fill in by rows
#first argument is the vector of numbers to fill in the matrix
Mat<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=TRUE)
Mat
#set up a matrix that fills in by columns
#first argument is the vector of numbers to fill in the matrix
Mat.bycol<-matrix(c(1,2,3,4,5,6), ncol=2, byrow=FALSE)
#byrow determines how the numbers fill in
Mat.bycol
#subset the matrix to look at row 1, column2
Mat.bycol[1,2]
#look at all values in row 1
#opening in square bracket (blank before bracket) refers to all items
Mat.bycol[1,]
#look at all values in column 2
Mat.bycol[,2]

#read in weather station file from your data folder
#copy file path requires me to add an additional \
datW <- read.csv("Z:\\espina\\Data for Class\\noaa_weather\\2011124.csv")
#get more information about the dataframe, answer to q1
str(datW)

#specify a column with a proper date format
#note the format here dataframe$column
datW$dateF <- as.Date(datW$DATE, "%Y-%m-%d")
#create a date column by reformatting the date to only include years
#and indicating that it should be treated as numeric data
datW$year <- as.numeric(format(datW$dateF,"%Y"))
#question2 now
#character example 
char_vector <- c("red","yellow", "blue", "green", "purple")
#factor example -- convert char_vector to factor data
char_factor <- factor(char_vector)
print(char_factor)
#numeric example
numeric_vector <- c(1.0, 2.0, 3.0, 4.0, 5.0)
#integer example
int_vector <- c(1, 2, 3, 4, 5)
