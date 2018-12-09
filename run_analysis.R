# First, read the data
x_train <- read.table("train/X_train.txt")
y_train <- read.table("train/y_train.txt")
x_test <- read.table("test/x_test.txt")
y_test <- read.table("test/y_test.txt")

# Join the data into one table
x_all <- rbind(x_train,x_test)
y_all <- rbind(y_train,y_test)
all_data <- merge(y_train,x_train,by="row.names")

# rename the rows
t <- read.table("activity_labels.txt")
getname <- function(id) {as.character(t$V2)[id]}
labled_data<-mutate(all_data, V1.x = getname(V1.x))
#remove useless first column
labled_data<-labled_data[,-1]

# get means and standard deviations
measurement_set <- labled_data[,-1]
labled_means_stdevs <- 
  data.frame(ID=labled_data$V1.x,
             Means = rowMeans(measurement_set),
             sds = apply(measurement_set,1,sd))

#group by activity
Grouped_means_devs <- labled_means_stdevs %>%
        group_by(ID) %>%
        summarise(mean = mean(Means), sd_mean = mean(sds))