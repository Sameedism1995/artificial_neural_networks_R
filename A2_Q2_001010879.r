library(scales)
library(NbClust)
library(purrr)
library(ggplot2)
library(dplyr)
library(corrplot)


#loading the data into variable my_data
my_data=read.csv('wholesale_Mac.csv', header=TRUE, sep=";")

#checking the structure of our data
str(my_data)

#checking for any missing values
any(is.na(my_data)) 

class(my_data)
class(my_data$Channel)

#checking whether channels are categorical variables

my_data$Channel
my_data$Region


#Plotting 
ggplot(my_data, aes(x=Grocery))+geom_bar()+ggtitle('Channel/Grocery')
ggplot(my_data, aes(x=Region))+geom_bar()+ggtitle('Region Count')


par(mfrow = c(2,3)) 

#Plotting Grocery against Frozen
plot(my_data$Frozen,
     my_data$Grocery,ylab='Grocery', xlab = 'Frozen', main = 'Grocery/Frozen', pch = 3, col = 'pink')

#Plotting Grocery against Detergents_Paper
plot(my_data$Detergents_Paper,
     my_data$Grocery,ylab='Grocery', xlab = 'Detergents_Paper', main = 'Grocery/Detergent_Paper', pch = 3, col = 'pink')

#Plotting grocery against Delicassen
plot(my_data$Delicassen,
     my_data$Grocery,ylab='Grocery', xlab = 'Delicassen', main = 'Grocery/Delicassen', pch = 3, col = 'pink')

#Plotting grocery against Fresh
plot(my_data$Fresh,
     my_data$Grocery, ylab='Grocery', xlab = 'Fresh', main = 'Grocery/Fresh',pch = 3, col = 'pink')

#Plotting grocery against Milk
plot(my_data$Milk,
     my_data$Grocery,ylab='Grocery', xlab = 'Milk', main = 'Grocery/Milk', pch = 3, col = 'pink')



#checking correlation
cor(my_data)

#plotting correlation
corrplot(cor(my_data),"number")

#Scaled the data
scaled_data=apply(my_data[,c(3,4,5,6,7,8)],2,rescale, to=c(0,1))


#Determining total clusters
num_clusters=map_dbl(1:10, function(k){
  model=kmeans(scaled_data, center=k, nstart=25)
  model$num_clusters
})

#Determining Total clusters using elbow method
plot(1:10, num_clusters, type='o', xlab='Total lusters',
     ylab='Total', panel.first = grid())

#Using silhouette method to determine total clusters
silhouette=NbClust(scaled_data,distance='euclidean', min.nc =2, max.nc = 10,
                 method = 'kmeans', index='silhouette')

#Using Gap method to determine total clusters
Gap=NbClust(scaled_data,distance='euclidean', min.nc =2, max.nc = 10,
                 method = 'kmeans', index='gap')

#Using Calinski Harabasz method to determine total clusters
CalHar=NbClust(scaled_data,distance='euclidean', min.nc =2, max.nc = 10,
                method = 'kmeans', index='ch')

#Plotting Calinski Harabasz method
plot(2:10, CalHar$All.index,type='o', xlab='Total Clusters',
     ylab='Calinski Harabasz', panel.first = grid())

#Plotting Gap Stats
plot(2:10, Gap$All.index,type='o', xlab='Total Clusters',
     ylab='Gap Stats', panel.first = grid())

#plotting Silhouette method
par(mfrow=c(1,3))
plot(2:10, silhouette$All.index,type='o', xlab='Total Clusters',
     ylab='Silhouette Method', panel.first = grid())



#Clustering
kmeans=kmeans(scaled_data, center=2, nstart=25)
par(mfrow=c(1,1))

# adding cluster membership
data_cluster=my_data.frame(scaled_data)%>%
  mutate(MembershipCluster=factor(kmeans$cluster))

#determining number of obs in each cluster
table(data_cluster$MembershipCluster)
data_cluster

#avg mean of every var in cluster
data_cluster%>%
  group_by(MembershipCluster)%>%
  summarise_all(list(avg=mean, std=sd))


my_data_new=my_data%>%
  mutate(MembershipCluster=factor(kmeans$cluster))
table(my_data_new$MembershipCluster)

my_data_new$MembershipCluster

ggplot(my_data_new, aes(x = MembershipCluster, y=Fresh, fill=MembershipCluster))+
  geom_boxplot()+
  xlab("Cluster")+
  ylab("Fresh")


ggplot(my_data_new, aes(x = MembershipCluster, y=Grocery, fill=MembershipCluster))+
  geom_boxplot()+
  xlab("Cluster")+
  ylab("Grocery")

ggplot(my_data_new, aes(x = MembershipCluster, y=Delicassen, fill=MembershipCluster))+
  geom_boxplot()+
  xlab("Cluster")+
  ylab("Delicassen")


ggplot(my_data_new, aes(x = MembershipCluster, y=Detergents_Paper, fill=MembershipCluster))+
  geom_boxplot()+
  xlab("Cluster")+
  ylab("Detergents_Paper")

ggplot(my_data_new, aes(x = MembershipCluster, y=Milk, fill=MembershipCluster))+
  geom_boxplot()+
  xlab("Cluster")+
  ylab("Milk")


ggplot(my_data_new, aes(x = MembershipCluster, y=Frozen, fill=MembershipCluster))+
  geom_boxplot()+
  xlab("Cluster")+
  ylab("Frozen")


my_data_new%>%
  group_by(MembershipCluster)%>%
  summarise_all(list(avg=mean, std=sd))

my_data_new%>%
  group_by(MembershipCluster)

ggplot(my_data_new,(aes(x=Detergents_Paper, y=Frozen, col='pink')))+
  geom_point()+
  facet_wrap(~MembershipCluster)+
  ggtitle('Income/Saving')


ggplot( y_data_new,(aes(x=Detergents_Paper, y=Milk, col= 'pink')))+
  geom_point()+
  facet_wrap(~MembershipCluster)+
  ggtitle('Income/Saving')