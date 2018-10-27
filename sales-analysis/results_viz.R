# Adam Hendel
# DS740 - Data Mining
# Sales Analysis
# Visualization of customer customers

require(ggplot2)
options(scipen=999)
# read in the results produced by python output
# 'X' is the cluster number
results <- read.csv('Academics/Data Science/DS740_Data_Mining/sales-analysis/pca_results.csv')
results$Cluster <- as.factor(results$Cluster)
# names(results) <- c('Cluster', 'PC1', 'PC2')

# load in the summary stats from python output
summ <- read.csv('Academics/Data Science/DS740_Data_Mining/sales-analysis/summary.csv')
summ <- round(summ,0)
summ$cluster <- as.factor(summ$cluster)


# build x-y coordinates for some annotations based on centroids of clusters
centroids <- read.csv('Academics/Data Science/DS740_Data_Mining/sales-analysis/pca_centroids.csv')

# some summary statistics one the raw data
d <- read.csv('Academics/Data Science/DS740_Data_Mining/sales-analysis/Online_Retail.csv')


# a theme to re-use in all plots
mytheme <- theme(
  axis.text=element_text(size=18),
  axis.title=element_text(size=18,face="bold"),
  legend.text = element_text(size=18),
  plot.title = element_text(hjust = 0.5),
  plot.subtitle = element_text(hjust = 0.5))


# PCA with cluster plot
ggplot(results, aes(x=PC1,y=PC2,col=Cluster)) +
  geom_point(size=4) + 
  ggtitle('Customer Clusters ', subtitle = 'Principle Components 1 and 2') +
  scale_color_brewer(palette = 'Set1') +
  mytheme
  

ggplot(summ, aes(x=mean_total_rev,y=mean_num_invoices,col=cluster, size=mean_num_unique_prods)) +
  geom_point(size=4) +
  annotate('text',
           x=summ$mean_total_rev,
           y=summ$mean_num_invoices+3,
           label=summ$num_members) +
  ggtitle(label='Characteristics of Customer Clusters',
          subtitle = 'Size = mean number of unique products purchased\nAnnotation = Number of customers in cluster') +
  scale_x_continuous(name='Mean Total Revenue (pounds)') +
  scale_y_continuous(name='Mean Number of Invoices') +
  scale_size_continuous(guide=F) +
  scale_color_brewer(palette = 'Set1') +
  mytheme

# elbow plot
elbow <- read.csv('Academics/Data Science/DS740_Data_Mining/sales-analysis/elbow_plot.csv')
names(elbow) <- 'error'
elbow$error <- as.numeric(elbow$error)
elbow$cluster <- 1:nrow(elbow)

ggplot(elbow, aes(x=cluster, y=error)) +
  geom_line() +
  geom_point(x=6,y=elbow[6,'error'], size=10, shape=21, color='red') +
  mytheme
