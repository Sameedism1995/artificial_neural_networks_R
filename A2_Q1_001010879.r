#Assignment2_Part1
library(ggplot2)#for plotting
library(corrplot)#for visual correlation
library(tsoutliers)#for finding ouliers
library(psych)
my_data = read.csv("dataArrests_Mac.csv", header = TRUE,sep = ";")

my_data
class(my_data$Assault)
class(my_data$Traffic)
class(my_data$CarAccidents)
class(my_data$UrbanPop)
class(my_data$Murder)

my_data$Assault
my_data$Traffic
my_data$CarAccidents
my_data$UrbanPop
my_data$Murder

#Assignment2_Part2
#Plotting dependent variable murder

#Murder
ggplot(my_data,aes(x=Murder))+
  geom_histogram(fill="pink", color="yellow") +
  ggtitle("Crime: Murder")
#Murder is decreasing by count

#Assault
ggplot(my_data,aes(x=Assault))+
  geom_histogram(fill="yellow", color="pink") +
  ggtitle("Crime: Assault")
#Assault is decreasing by count

#Urbanpop
ggplot(my_data,aes(x=UrbanPop))+
  geom_histogram(fill="pink", color="red") +
  ggtitle("Crime: UrbanPop")
#urbanpop increase till middle, then start decreasing

#Traffic
ggplot(my_data,aes(x=Traffic))+
  geom_histogram(fill="yellow", color="black") +
  ggtitle("Crime: Traffic")
#Traffic increases till middle, then starts decreasing irregularly

#CarAccidents
ggplot(my_data,aes(x=CarAccidents))+
  geom_histogram(fill="orange", color="yellow") +
  ggtitle("Crime: CarAccidents")
#CarAccidents increases till middle then starts decreasing irregularly

ggplot(my_data, aes(x=UrbanPop, y=Murder)) + 
  geom_point()

#Assignment2_Part3

cor(my_data)
print(my_data)

#mostly values are null 

#Assignment2_Part4

corPlot(my_data)
#Car accidents and Traffic are strongly correlated with eachother having value 0.98
#Positive co-relation between same variables

corrplot(cor(my_data),"number")

#Assignment2_Part5: Removing explanatory variables that has high correlation from explanatory variables
depvar = my_data$Murder
exvar = my_data[,2:10]

depvar
exvar

#figuring out correlations
corrplot(cor(exvar),"number")
#removing high correlations between explanatory variables
cor_exvar = abs(cor(exvar));

diag(cor_exvar)=0 

while (max(cor_exvar)>=0.8){
  
  #Finding explanatory variables with highest absolute correlation
  
  maxvar=which(cor_exvar==max(cor_exvar), arr.ind = TRUE)
  
  #select variable with the highest average correlation
  
  maxavg = which.max(rowMeans(cor_exvar[maxvar[,1],])) 
  
  
  
  print(rownames(maxvar)[maxvar[,1]==maxvar[maxavg,1]])
  
  
  
  #removal of exploratory variables
  
  exvar=exvar[,-maxvar[maxavg,1]]
  
  cor_exvar=cor_exvard[-maxvar[maxavg,1],-maxvar[maxavg,1]]
  
}

#making our model
data_1 =cbind('Murder'=depvar,exvar)

data_1



mylinearmodel = lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber+Kidnapping+Domestic+Alcohol, data=data_1)
summary(mylinearmodel)

#Model without Drugs

mylinearmodel=lm(Murder~Assault+UrbanPop+Traffic+Cyber, data=data_1)

summary(mylinearmodel)

#Model without Domestic
mylinearmodel=lm(Murder~Assault+UrbanPop+Drug+Traffic+Cyber, data=data_1)

summary(mylinearmodel)

#Model without Traffic

mylinearmodel=lm(Murder~Assault+UrbanPop+Cyber, data=data_1)

summary(mylinearmodel)

#Model without Cyber
mylinearmodel=lm(Murder~Assault+UrbanPop, data=data_1)

summary(mylinearmodel)

#Plotting
#homoskedicity & checking whether residuals are linearly independent
ggplot(my_data,aes(x=Assault, y=Murder))+
  
  geom_point()+
  
  geom_line(aes(x=Assault,y=fitted.values(mylinearmodel)),col='pink')

#Checking all properties of OLS are fulfilled
#Finding means of residuals
mean(residuals(mylinearmodel))

#Checking correlation between residuals and independent variables
cor(residuals(mylinearmodel), my_data$Assault)

#Checking if residuals are normally distributed or not
JarqueBera.test(residuals(mylinearmodel))











































