
# Clustering using Online Retail data from UCI website

# Link: https://archive.ics.uci.edu/ml/datasets/Online+Retail#

# Let's formulate this as a Customer Segmentation Business Problem
# So we are clustering (grouping our customers into segments), to help us with strategic marketing policies and initiatives
# Example - Differetiate High Value Customer vs Low Value Customers, New versus Existing Customers, Low versus High Volume Purchasers, etc 
# To help us better formulate marketing policies, focus on specific groups or engage in target marketing


library(tidyverse)  # data manipulation
library(cluster)    # clustering algorithms
library(factoextra) # clustering visualization
library(dendextend) # for comparing two dendrograms
library(dplyr)


# Read data into R
raw.data <- read.csv(file.choose(), header = TRUE, stringsAsFactors = FALSE, na.strings = c("NA","","#NA", "?", "??", "???")) # Use Online Retail dataset.csv

data <- raw.data # the data takes a couple of minutes to import, so keep raw.data as a backup

head(data,5)
str(data)

# Explore the dataset
# Missing values
colSums(is.na(data))

length(unique(data$StockCode))
length(unique(data$Description))
length(unique(data$CustomerID))
length(unique(data$Country))

summary(data$Quantity)
summary(data$UnitPrice)
table(data$Country)

# Convert CustomerID from Integer to Factor
data$CustomerID <- as.factor(data$CustomerID) 

# Convert InvoiceDate to Date
#data$InvoiceDate <- as.Date(data$InvoiceDate, "%m/%d/%y") #Outputs wrong dates
#str(data)

data <- transform(data, lapply({l<-list(InvoiceDate);names(l)=c('InvoiceDate');l}, 
                              function(x)do.call(rbind, strsplit(x, ' ', fixed=TRUE))), stringsAsFactors=F)

#str(data)

to_drop1 <- c("InvoiceDate","InvoiceDate.1.1", "InvoiceDate.2", "InvoiceDate.2.1")
data <- data[, !(names(data) %in% to_drop1)]

library(lubridate)
data$InvoiceDate <- mdy(data$InvoiceDate.1)

to_drop2 <- c("InvoiceDate.1")
data <- data[, !(names(data) %in% to_drop2)]

str(data)

range(data$InvoiceDate)

library(ggplot2)

#ggplot(data = data,aes(x=Quantity))+geom_histogram(binwidth=10000.5)+ylab("Frequency")

#ggplot(data, aes(UnitPrice, Quantity)) + geom_point() + 
#  scale_x_continuous("Unit Price") +
#  scale_y_continuous("Quantity")+ theme_bw()

boxplot(data$UnitPrice) # we can use ylim=c(0,100) to zoom in
boxplot(data$Quantity)

#Some invoices are missing ID numbers. We are doing the analysis at the customer level, so remove any observations with missing ID numbers.

length(unique(data$CustomerID))
sum(is.na(data$CustomerID))
data <- subset(data, !is.na(data$CustomerID))

#There's a little over one year's worth of data in the downloaded dataset. To keep things tidy, let's restrict the date range to one full year.

range(data$InvoiceDate)
#data <- subset(data, InvoiceDate >= "2010-12-09")
#range(data$InvoiceDate)

#Research indicates that customer clusters vary by geography, so let's restrict the data to one geographic unit
table(data$Country)

# Because UK has the most data (349,806), let's use United Kingdom....Wanted to use USA and Canada but they have only 291 and 151, respectively
#data <- subset(data, Country %in% ("Canada", "USA"))
data <- subset(data, Country == "United Kingdom")
length(unique(data$CustomerID))

length(unique(data$InvoiceNo))

# We now have a dataset of 19,140 unique invoices and 3,891 unique customers.

# RFM Variables

# Since this is customer level segmentation, we want to add three new features that is typically used to segment customers - 
# Recency, Frequency and Monetary Value, (i.e. RFM)
# The recency variable refers to the number of days that have elapsed since the customer last purchased something (so, 
# smaller numbers indicate more recent activity on the customer's account). 
# Frequency refers to the number of invoices with purchases during the year. 
# Monetary value is the amount that the customer spent during the year. Some customers have negative monetary values. These customers 
# probably returned something during the year that they had purchased before the year started, so I reset their monetary value to zero.

#To calculate the recency and frequency variables below, it will be necessary to distinguish invoices related to actual purchases from invoices related to returns.

# Identify returns
data$item.return <- grepl("C", data$InvoiceNo, fixed=TRUE)
data$purchase.invoice <- ifelse(data$item.return=="TRUE", 0, 1)
head(data)


#################################
# Create customer-level dataset #
#################################

# create a customer-level dataset and add recency, frequency, and monetary value data to it.

customers <- as.data.frame(unique(data$CustomerID))
names(customers) <- "CustomerID"


###########
# Recency #
###########

#data$recency <- as.Date("2011-12-10") - as.Date(data$InvoiceDate)
data$recency <- as.Date("2011-12-10") - data$InvoiceDate

# remove returns so only consider the data of most recent *purchase*
temp <- subset(data, purchase.invoice == 1)

# Obtain # of days since most recent purchase
recency <- aggregate(recency ~ CustomerID, data=temp, FUN=min, na.rm=TRUE)

# Add recency to customer data
customers <- merge(customers, recency, by="CustomerID", all=TRUE, sort=TRUE)

customers$recency <- as.numeric(customers$recency)


#############
# Frequency #
#############

customer.invoices <- subset(data, select = c("CustomerID","InvoiceNo", "purchase.invoice"))
customer.invoices <- customer.invoices[!duplicated(customer.invoices), ]
customer.invoices <- customer.invoices[order(customer.invoices$CustomerID),]
row.names(customer.invoices) <- NULL

# Number of invoices (purchases only)
annual.invoices <- aggregate(purchase.invoice ~ CustomerID, data=customer.invoices, FUN=sum, na.rm=TRUE)
names(annual.invoices)[names(annual.invoices)=="purchase.invoice"] <- "frequency"

# Add # of invoices to customers data
customers <- merge(customers, annual.invoices, by="CustomerID", all=TRUE, sort=TRUE)

range(customers$frequency)
table(customers$frequency)

# Remove customers who have not made any purchases in the past year
customers <- subset(customers, frequency > 0)


###############################
# Monetary Value of Customers #
###############################

# Total spent on each item on an invoice
data$Amount <- data$Quantity * data$UnitPrice

# Aggregated total sales to customer
annual.sales <- aggregate(Amount ~ CustomerID, data=data, FUN=sum, na.rm=TRUE)
names(annual.sales)[names(annual.sales)=="Amount"] <- "monetary"

# Add monetary value to customers dataset
customers <- merge(customers, annual.sales, by="CustomerID", all.x=TRUE, sort=TRUE)

# Identify customers with negative monetary value numbers, as they were presumably returning purchases from the preceding year
hist(customers$monetary)
customers$monetary <- ifelse(customers$monetary < 0, 0, customers$monetary) # reset negative numbers to zero

summary(customers$monetary)
summary(customers$frequency)
summary(customers$recency)

hist(customers$monetary)
hist(customers$frequency)
hist(customers$recency)

library(lattice)
#splom(customers[frequency, monetary, recency], cex = 0.1)

data_4_corr <- customers[, c("frequency", "monetary", "recency")]
str(data_4_corr)

library(corrplot)
corr_matrix <- cor(data_4_corr)
round(corr_matrix, 2)
corrplot(corr_matrix, type = "upper", order = "hclust", 
         tl.col = "black", tl.srt = 45)

#Try rcorr
library(Hmisc)
cor_mat_2 = rcorr(as.matrix(data_4_corr))
cor_mat_2

#The following custom function is used to flatten the Correlation Matrix:

# ++++++++++++++++++++++++++++
# flattenCorrMatrix
# ++++++++++++++++++++++++++++
# cormat : matrix of the correlation coefficients
# pmat : matrix of the correlation p-values

flattenCorrMatrix <- function(cormat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut]
  )
}

flattenCorrMatrix(cor_mat_2$r)

# Produce Charts along with Correlation Matrix
library("PerformanceAnalytics")
chart.Correlation(data_4_corr, histogram=TRUE, pch=5)


#Let's use the famous 80/20 Rule (also known as the Pareto Principle). It's the concept that 80% of the 
# results generally come from 20% of the causes. In this context, it implies that  about 80% of sales would be produced by 
# the top 20% of customers, approximately. These 20% represent the high-value, important customers a business would want to protect.

customers <- customers[order(-customers$monetary),]

# Apply Pareto Principle (80/20 Rule)
pareto.cutoff <- 0.8 * sum(customers$monetary)
customers$pareto <- ifelse(cumsum(customers$monetary) <= pareto.cutoff, "Top 20%", "Bottom 80%")
customers$pareto <- factor(customers$pareto, levels=c("Top 20%", "Bottom 80%"), ordered=TRUE)
levels(customers$pareto)
round(prop.table(table(customers$pareto)), 2)

# We see that in this dataset, 80% of the annual sales are produced by the top 29% of # customers, so the percentage 
# isn't quite 20%, but it's not that far off and it does illustrate that there's a smallish segment producing the bulk of the value.

customers <- customers[order(customers$CustomerID),]

# Preprocess data

# k-means clustering requires continuous variables and works best with relatively normally-distributed, standardized input 
# variables. Standardizing the input variables is quite important; otherwise, input variables with larger variances will 
# have commensurately greater influence on the results. 
# Below, we transform our three input variables to reduce positive skew and then standardize them as z-scores.

# Log-transform positively-skewed variables
customers$recency.log <- log(customers$recency)
customers$frequency.log <- log(customers$frequency)
customers$monetary.log <- customers$monetary + 0.1 # can't take log(0), so add a small value to remove zeros
customers$monetary.log <- log(customers$monetary.log)

# Z-scores
customers$recency.z <- scale(customers$recency.log, center=TRUE, scale=TRUE)
customers$frequency.z <- scale(customers$frequency.log, center=TRUE, scale=TRUE)
customers$monetary.z <- scale(customers$monetary.log, center=TRUE, scale=TRUE)


# Visualize data

# Typically, the user has to specify the number of clusters with k-means clustering. 
# Let's look at the data to get a sense for what we are dealing with and how many clusters we might have. 
# In the graphs below, the outcome we're probably most interested in, customer monetary value, is plotted on the y-axis. 
# Frequency of purchases is on the x-axis, and  the third variable, recency of purchase is used to color-code the data points. 
# Lastly, the 80/20 Rule segments is included so we could map those designations on to customer monetary value, frequency, and recency.

library(ggplot2)
library(scales)

# Original scale
scatter.1 <- ggplot(customers, aes(x = frequency, y = monetary))
scatter.1 <- scatter.1 + geom_point(aes(colour = recency, shape = pareto))
scatter.1 <- scatter.1 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.1 <- scatter.1 + scale_colour_gradient(name="Recency\n(Days since Last Purchase))")
scatter.1 <- scatter.1 + scale_y_continuous(label=dollar)
scatter.1 <- scatter.1 + xlab("Frequency (Number of Purchases)")
scatter.1 <- scatter.1 + ylab("Monetary Value of Customer (Annual Sales)")
scatter.1


# This first graph uses the variables' original metrics and is almost completely uninterpretable. 
# There's a clump of data points in the lower left-hand corner of the plot, and then a few outliers. 
# This is why we log-transformed the input variables

# Log-transformed
scatter.2 <- ggplot(customers, aes(x = frequency.log, y = monetary.log))
scatter.2 <- scatter.2 + geom_point(aes(colour = recency.log, shape = pareto))
scatter.2 <- scatter.2 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.2 <- scatter.2 + scale_colour_gradient(name="Log-transformed Recency")
scatter.2 <- scatter.2 + xlab("Log-transformed Frequency")
scatter.2 <- scatter.2 + ylab("Log-transformed Monetary Value of Customer")
scatter.2

# This is better. Now we can see a scattering of high-value, high-frequency customers in the top, 
# right-hand corner of the graph. These data points are dark, indicating that they've purchased something recently. 
# In the bottom, left-hand corner of the plot, we can see a couple of low-value, low frequency customers who haven't 
# purchased anything recently, with a range of values in between.

# Importantly, we can see that the data points are fairly continuously-distributed. 
# There really aren't clear clusters. This means that any cluster groupings we create won't exactly reflect 
# some true, underlying group membership - they'll be somewhat arbitrary (albeit reasonable) distinctions that 
# we draw for our own purposes.


#Handling outliers

# The following code investigates a little more thoroughly the customers plotted in the bottom, left-hand corner. 

# How many customers are represented by the two data points in the lower left-hand corner of the plot? 18
delete <- subset(customers, monetary.log < 0)
no.value.custs <- unique(delete$CustomerID)
delete2 <- subset(data, CustomerID %in% no.value.custs)
delete2 <- delete2[order(delete2$CustomerID, delete2$InvoiceDate),]
remove(delete, delete2, no.value.custs)

# The eighteen, no-value customers we just investigated are all customers who returned everything they bought.
# They highlight an important decision point at this juncture in the analysis. k-means clustering tends to be sensitive 
# to outliers, such that outliers will sometimes end up being clustered together in their own tiny group. This is often 
# cited as a reason to exclude them from the analysis. In this type of customer segmentation, however, the outliers may be 
# the most important customers to understand. 
# In the top, right-hand corner, we also have customers who are outliers in terms of being extraordinarily high-value, high-frequency shoppers. 
# These data points are all represented with little triangles, indicating that they're in the top 80/20 category. 
# These are important customers to understand, because they're the customers we most want. 
# At the other end of the continuum, we have the no-value customers in the bottom, left-hand corner. These customers, too, may be 
# important to model and understand - they're the customers we want to minimize. 
# So let's include these outliers in this analysis.

# Scaled variables
scatter.3 <- ggplot(customers, aes(x = frequency.z, y = monetary.z))
scatter.3 <- scatter.3 + geom_point(aes(colour = recency.z, shape = pareto))
scatter.3 <- scatter.3 + scale_shape_manual(name = "80/20 Designation", values=c(17, 16))
scatter.3 <- scatter.3 + scale_colour_gradient(name="Z-scored Recency")
scatter.3 <- scatter.3 + xlab("Z-scored Frequency")
scatter.3 <- scatter.3 + ylab("Z-scored Monetary Value of Customer")
scatter.3

remove(scatter.1, scatter.2, scatter.3)

# This third scatterplot is basically identical to the second - it illustrates that even though we've changed the scaling 
# for the analysis, the shape of the distributions and the relationships among the variables remain the same.


# Clustering Techniques 

#Hierarchical Clustering

#Scale dataset
#scaled_df <- scale(df)
#head(scaled_df)

# Dissimilarity matrix
#d <- dist(scaled_df, method = "euclidean")
# Hierarchical clustering using Complete Linkage
#hc1 <- hclust(d, method = "complete" ) # hclust is a type of agglomerative hierarchical clustering 
# Plot the obtained dendrogram
#plot(hc1, cex = 0.6, hang = -1)

scaled_customers_1 = customers[,9:11]
scaled_customers_2 <- customers[, c("CustomerID", "frequency.z", "monetary.z", "recency.z")]
#head(scaled_customers_2)
d <- dist(scaled_customers_1, method = "euclidean") #dissimilarity (distance) matrix
h_clust <- hclust(d, method = "ward.D2") #clustering using Ward's minimum variance method
h_clust

#Plot dendogram
plot(h_clust, cex = 0.6, hang = -1, labels = customers$CustomerID) 
rect.hclust(h_clust,k=5)

#extract clusters
groups <- cutree(h_clust,k=5)
table(groups)

# methods to assess
m <- c( "average", "single", "complete", "ward")
names(m) <- c( "average", "single", "complete", "ward")

# function to compute coefficient
ac <- function(x) {
  agnes(scaled_customers_2, method = x)$ac
}

map_dbl(m, ac)

# Add the output of the cutree to the "original" customers data so we can make a scatter plot showing the clusters
#head(customers)
names_hc <- c("CustomerID", "recency.log", "frequency.log", "monetary.log")
customers_for_hc <- customers[, (names(customers) %in% names_hc)]
#customers_for_hc <- na.omit(customers_for_hc)

hc_customers<- customers_for_hc %>%
  mutate(cluster_grp = groups)
  
#View(hc_customers)

str(hc_customers)
#hc_customers$cluster <- as.numeric(hc_customers$cluster)

#use the fviz_cluster function from the factoextra package to visualize the result in a scatter plot.
fviz_cluster(list(data=hc_customers, cluster_grp = cluster)) # na.rm = TRUE)

# Create new column filled with default colour
#colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')
#hc_customers$Colour="black"
# Set new column values to appropriate colours
#hc_customers$Colour[hc_customers$cluster_grp==5]="red"
#hc_customers$Colour[hc_customers$cluster_grp==4]="green"
#hc_customers$Colour[hc_customers$cluster_grp==3]="orange"
#hc_customers$Colour[hc_customers$cluster_grp==2]="blue"

# Plot all points at once, using newly generated colours
#plot(hc_customers$frequency.log, hc_customers$monetary.log, col=hc_customers$Colour)

hc_customers$cluster_grp <- as.factor(hc_customers$cluster_grp)
ggplot(hc_customers) + geom_point(mapping = aes(x = frequency.log, y = monetary.log, color= cluster_grp))
ggplot(hc_customers) + geom_point(mapping = aes(x = recency.log, y = monetary.log, color= cluster_grp))


#Determining Optimal Clusters
# Use Elbow method
fviz_nbclust(hc_customers, FUN = hcut, method = "wss")

#Average Silhouette Method
fviz_nbclust(hc_customers, FUN = hcut, method = "silhouette")

#Gap Statistic Method
gap_stat <- clusGap(hc_customers, FUN = hcut, nstart = 25, K.max = 10, B = 50)
fviz_gap_stat(gap_stat)


# k-means clustering

# Let's create a for loop to run the k-means analysis with increasing numbers of clusters, each time 
# generating a graph of the clusters, the cluster centers for each model, and information about the variance explained. 
# All of this can assist in selecting the optimal number of clusters.

preprocessed <- customers[,9:11]
j <- 10 # specify the maximum number of clusters you want to try out

models <- data.frame(k=integer(),
                     tot.withinss=numeric(),
                     betweenss=numeric(),
                     totss=numeric(),
                     rsquared=numeric())

for (k in 1:j ) {
  
  print(k)
  
  # Run kmeans
  # nstart = number of initial configurations; the best one is used
  # $iter will return the iteration used for the final model
  output <- kmeans(preprocessed, centers = k, nstart = 20)
  
  # Add cluster membership to customers dataset
  var.name <- paste("cluster", k, sep="_")
  customers[,(var.name)] <- output$cluster
  customers[,(var.name)] <- factor(customers[,(var.name)], levels = c(1:k))
  
  # Graph clusters
  cluster_graph <- ggplot(customers, aes(x = frequency.log, y = monetary.log))
  cluster_graph <- cluster_graph + geom_point(aes(colour = customers[,(var.name)]))
  colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')
  cluster_graph <- cluster_graph + scale_colour_manual(name = "Cluster Group", values=colors)
  cluster_graph <- cluster_graph + xlab("Log-transformed Frequency")
  cluster_graph <- cluster_graph + ylab("Log-transformed Monetary Value of Customer")
  title <- paste("k-means Solution with", k, sep=" ")
  title <- paste(title, "Clusters", sep=" ")
  cluster_graph <- cluster_graph + ggtitle(title)
  print(cluster_graph)
  
  # Cluster centers in original metrics
  library(plyr)
  print(title)
  cluster_centers <- ddply(customers, .(customers[,(var.name)]), summarize,
                           monetary=round(median(monetary),2),# use median b/c this is the raw, heavily-skewed data
                           frequency=round(median(frequency),1),
                           recency=round(median(recency), 0))
  names(cluster_centers)[names(cluster_centers)=="customers[, (var.name)]"] <- "Cluster"
  print(cluster_centers)
  cat("\n")
  cat("\n")
  
  # Collect model information
  models[k,("k")] <- k
  models[k,("tot.withinss")] <- output$tot.withinss # the sum of all within sum of squares
  models[k,("betweenss")] <- output$betweenss
  models[k,("totss")] <- output$totss # betweenss + tot.withinss
  models[k,("rsquared")] <- round(output$betweenss/output$totss, 3) # percentage of variance explained by cluster membership
  assign("models", models, envir = .GlobalEnv)
  
  remove(output, var.name, cluster_graph, cluster_centers, title, colors)
  
}

# Somewhere from 2-5 clusters seems most interpretable.

# A 2-cluster solution produces one group of high-value (median = $1,797.78), high-frequency (median = 5 purchases) customers who 
# have purchased recently (median = 17 days since their most recent purchase), and one group of lower value (median = $327.50), 
# low frequency (median = 1 purchase) customers for whom it's been a median of 96 days since their last purchase. 
# Although these two clusters are clear and interpretable, this may be simplifying customer behavior too much.

# As we add clusters, we gain insight into increasingly subtle distinctions between customers. 

# Let's settle on the 5-cluster solution. It gives us: a high-value, high-frequency, recent purchase group (cluster 5), 
# a medium-value, medium-frequency, relatively-recent purchase group (cluster 2), two clusters of low-value, 
# low-frequency customers broken down by whether their last purchase was recent or much earlier in the year (clusters 3 and 1, 
# respectively), and lastly a no-value cluster whose median value to the business is $0.00 (cluster 4).

# As we move beyond 5 clusters, the graphs become increasingly hard to interpret visually, and the cluster centers start 
# to make distinctions that may not be that helpful (e.g., low-value-with-1-purchase vs. low-value-with-2-purchases customers).

# There are a couple of other things we can check to assist us in selecting the optimal solution. The code below 
# generates graphs of the variance explained and within-cluster variance by number of cluster. These graphs are two different
# ways of visualizing the same information - in both cases, we're looking for an "elbow" or bend in the graph beyond which 
# additional clusters add little additional explanatory power. Both graphs look to have elbows at around 2 clusters, 
# but a 2-cluster solution explains only 49% of the variance and, once again, a 2-cluster solution may be too much of a 
# simplification to really help the business with targeted marketing. The 5-cluster solution explains ~73% of the variance, 
# but there are no clear elbows in the graph at this point.


library(ggplot2)
library(scales)

# Graph variance explained by number of clusters
r2_graph <- ggplot(models, aes(x = k, y = rsquared))
r2_graph <- r2_graph + geom_point() + geom_line()
r2_graph <- r2_graph + scale_y_continuous(labels = scales::percent)
r2_graph <- r2_graph + scale_x_continuous(breaks = 1:j)
r2_graph <- r2_graph + xlab("k (Number of Clusters)")
r2_graph <- r2_graph + ylab("Variance Explained")
r2_graph

# Graph within sums of squares by number of clusters
# Look for a "bend" in the graph, as with a scree plot
ss_graph <- ggplot(models, aes(x = k, y = tot.withinss))
ss_graph <- ss_graph + geom_point() + geom_line()
ss_graph <- ss_graph + scale_x_continuous(breaks = 1:j)
ss_graph <- ss_graph + scale_y_continuous(labels = scales::comma)
ss_graph <- ss_graph + xlab("k (Number of Clusters)")
ss_graph <- ss_graph + ylab("Total Within SS")
ss_graph


#########################################################
# Using NbClust metrics to determine number of clusters #
#########################################################
# Lastly, we use an R package that will look at a host of different fit indices and, using majority rule, suggest the number of 
# clusters that most indices recommend. 

library(NbClust)
set.seed(1)
nc <- NbClust(preprocessed, min.nc=2, max.nc=7, method="kmeans")
table(nc$Best.n[1,])

nc$All.index # estimates for each number of clusters on 26 different metrics of model fit

barplot(table(nc$Best.n[1,]),
        xlab="Number of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by Criteria")


# The greatest number of indices recommend the 2-cluster solution. 
# The Dindex plot recommends a 6-cluster solution, based on gain in the second difference plot...Reference below
# file:///C:/Users/joegy/AppData/Local/Temp/v61i06.pdf   (see page 14)


# How do we proceed at this point? At this juncture, it makes sense to show interested stakeholders 
# the cluster solutions and get their input. The decision should be based upon how the business plans to use the results, and 
# the level of granularity they want to see in the clusters.

# If the business wants to use the results to understand a range of customer behavior from high-to-low value customers, 
# we'd probably recommend the 5-cluster solution. It distinguishes the no-value group of customers, whom the business 
# probably wants to eliminate as much as possible, and also separates low-value, low-frequency customers 
# who have purchased recently from those who have not. It may be easier to encourage recently-active customers to re-engage 
# with the business and possibly develop into medium-value customers. That said, there isn't one, correct decision here.

# Three-Dimensional Representation of Clusters

# This code produces a 3D rendering of our clusters, which is helpful in our case as we're working with three input 
# variables, an RFM analysis in this case. The first graph features planes through each cluster that help the us 
# understand the monetary value associated with the cluster. The second features ellipses that highlight the 
# location of each cluster in three-dimensional space. In both graphs, the data points are color-coded to correspond 
# to their associated cluster.

colors <- c('red','orange','green3','deepskyblue','blue','darkorchid4','violet','pink1','tan3','black')

library(car)
library(rgl)

scatter3d(x = customers$frequency.log,
          y = customers$monetary.log,
          z = customers$recency.log,
          groups = customers$cluster_5,
          xlab = "Frequency (Log-transformed)",
          ylab = "Monetary Value (log-transformed)",
          zlab = "Recency (Log-transformed)",
          surface.col = colors,
          axis.scales = FALSE,
          surface = TRUE, # produces the horizonal planes through the graph at each level of monetary value
          fit = "smooth",
          # ellipsoid = TRUE, # to graph ellipses uses this command and comment out "surface = TRUE"
          grid = TRUE,
          axis.col = c("black", "black", "black"))


