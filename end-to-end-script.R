#######################################################################################
################################# Prepare Environment #################################
#######################################################################################
#Install or load packages
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('lubridate')) install.packages('lubridate'); library('lubridate')
if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')
if (!require('geosphere')) install.packages('geosphere'); library('geosphere')
if (!require('caret')) install.packages('caret'); library('caret')
if (!require('randomForest')) install.packages('randomForest'); library('randomForest')
if (!require('knitr')) install.packages('knitr'); library('knitr')


#######################################################################################
#################################### Data wrangling ###################################
#######################################################################################

####################### Data wrangling for products data set

# read the data from source and remove irrelevant columns
products_dataset <- read.csv(file.path('data','olist_products_dataset.csv'),header=TRUE,stringsAsFactors = FALSE) %>%
  select(-product_name_lenght,-product_description_lenght,-product_photos_qty)

# check how they are structured
products_dataset %>% head()
products_dataset %>% str()

# load the product category name translations
product_category_name_translation <- read.csv(file.path('data','product_category_name_translation.csv'),header=TRUE,stringsAsFactors = FALSE)

product_category_name_translation %>% head()

# convert product_name to english form wherever possible. If not available, existing product category name is retained
products_dataset <- products_dataset %>% 
  left_join(product_category_name_translation,by='product_category_name') %>% 
  mutate(product_category_name=if_else(is.na(product_category_name_english),
                                       product_category_name,
                                       product_category_name_english)) %>% 
  select(-product_category_name_english) 

# further remove products for which data is not available i.e is NA
products_dataset <- products_dataset %>% 
  filter(!is.na(product_weight_g) & !is.na(product_length_cm) & !is.na(product_height_cm) & !is.na(product_width_cm))

# assign default category name of UNCATEGORIZED where-ever product_category_name is blank.
products_dataset <- products_dataset %>%
  mutate(product_category_name=if_else(product_category_name=='','UNCATEGORIZED',product_category_name))


#Data wrangling result-products data set
products_dataset %>% head(2) %>% as.matrix()  %>% t() %>% 
  knitr::kable(col.names=c('Example Value1','Example Value2'), caption='Product data set')


####################### Data wrangling for orders data set
# order data set with various delivery related times
# note each customer_id is unique per order
# order_purchase_timestamp -> Shows the purchase timestamp.
# order_approved_at/payment_approved_at -> Shows the payment approval timestamp.
# order_delivered_carrier_date/handed_over_to_carrier_date -> Shows the order posting timestamp. When it was handled to the logistic partner.
# order_delivered_customer_date --> Shows the actual order delivery date to the customer.
# order_estimated_delivery_date --> Shows the estimated delivery date that was informed to customer at the purchase moment.

orders_dataset <- read.csv(file.path('data','olist_orders_dataset.csv'),header=TRUE,stringsAsFactors = FALSE) %>% 
  rename(payment_approved_at=order_approved_at,
         handed_over_to_carrier_date=order_delivered_carrier_date)  %>% 
  mutate(order_status=as.factor(order_status),
         order_purchase_timestamp=as_datetime(order_purchase_timestamp),
         payment_approved_at=as_datetime(payment_approved_at),
         handed_over_to_carrier_date=as_datetime(handed_over_to_carrier_date),
         order_delivered_customer_date=as_datetime(order_delivered_customer_date),
         order_estimated_delivery_date=as_datetime(order_estimated_delivery_date))

# Filtered out any orders where delivery was not made 
# Also filtered out orders where delivery was made but required delivery time columns were not available.
orders_dataset <- orders_dataset %>% 
  filter(order_status=='delivered') %>% 
  filter( !is.na(order_estimated_delivery_date ) & !is.na(order_delivered_customer_date ) & !is.na(handed_over_to_carrier_date) & !is.na(payment_approved_at) & !is.na(order_purchase_timestamp))

# check how they are structured
orders_dataset %>% head()
orders_dataset %>% str()

# Data wrangling result-orders dataset
orders_dataset %>% head(2) %>% as.matrix() %>% t() %>% 
  knitr::kable(col.names=c('Example Value1','Example Value2'), caption='Orders data set')

####################### Data wrangling for orders items data set
# orders data with item related seller info
# order_item_id --->sequential number identifying number of items included in the same order.
# so if there are three items they will be numbered 1 , 2, 3 for the given order
# shipping_limit_date ---> Shows the seller shipping limit date for handling the order over to the logistic partner.
# freight_value --> item freight value item (if an order has more than one item the freight value is split between items)
order_items_dataset <- read.csv(file.path('data','olist_order_items_dataset.csv'),header=TRUE,stringsAsFactors = FALSE) %>% mutate(shipping_limit_date=as_datetime(shipping_limit_date))  %>% select(-order_item_id)

# check how they are structured
order_items_dataset %>% str()
order_items_dataset %>% head()

# Data wrangling result-orders items dataset
order_items_dataset %>% head(2) %>% as.matrix() %>% t() %>% 
  knitr::kable(col.names=c('Example Value1','Example Value2'), caption='Order Items\' data set')

####################### Data wrangling for geolocation data set
# geo location details
# In brazil zip code is 8 digits. So first 5 digits zip code is good enough to identify an area but it does not uniquely identify customer location.
# So because of this, there are multiple rows for same geolocation_zip_code_prefix
# refer: https://codigo-postal.org/en-us/brazil/
#
# The first five digits represent the 1) region, 2) sub-region, 3) sector, 4) subsector, 5) subsector divider;
geolocation_dataset <- read.csv(file.path('data','olist_geolocation_dataset.csv'),
                                header=TRUE,stringsAsFactors = FALSE) %>%
  select(-geolocation_city,-geolocation_state) 

# check how they are structured
geolocation_dataset %>% head()
geolocation_dataset %>% str()

# check count of rows per geolocation_zip_code_prefix
geolocation_dataset %>% dplyr::count(geolocation_zip_code_prefix) %>% nrow()

# as each geolocation_zip_code_prefix can have many data points, we will limit these to one by taking mean for lat, long location
geolocation_dataset <- geolocation_dataset %>% group_by(geolocation_zip_code_prefix) %>% 
  summarise(geolocation_lat=mean(geolocation_lat),geolocation_lng=mean(geolocation_lng)) %>%
  ungroup()


# Data wrangling result-geolocation dataset
geolocation_dataset %>% head(2) %>% summarise_all(as.character) %>% t() %>% 
  knitr::kable(col.names=c('Example Value1','Example Value2'), caption='Geolocation data set')

#######################  Data wrangling for customers and sellers data set
# Data wrangling-customers data set
# customers data set  with zip code prefix and can be used to uniquely identify a customer
# customer_id ----> key to the orders dataset. Each order has a unique customer_id.
# customer_zip_code_prefix --> first five digits of customer zip code
customers_dataset <- read.csv(file.path('data','olist_customers_dataset.csv'),header=TRUE,stringsAsFactors = FALSE) 

# check how they are structured
customers_dataset %>% head()
customers_dataset %>% str()

# seller data set with zip code prefix
# seller_zip_code_prefix --> first 5 digits of seller zip code
sellers_dataset <- read.csv(file.path('data','olist_sellers_dataset.csv'),header=TRUE,stringsAsFactors = FALSE)

# check how they are structured
sellers_dataset %>% head()
sellers_dataset %>% str()


# Refine sellers dataset
# Filter out data where sellers location details are not available
# we noticed some seller_zip_code_prefix are not present in geolocation dataset. So we will ignore them
sellers_to_ignore <- sellers_dataset %>% anti_join(geolocation_dataset,by=c('seller_zip_code_prefix'='geolocation_zip_code_prefix')) 

# while there are small number of order items for the concerned sellers, we will ignore them as we will still have sufficient data for analysis.
order_items_dataset %>% inner_join(sellers_to_ignore,by='seller_id') %>% nrow()

# let's use the one of interest
sellers_dataset <- sellers_dataset %>% 
  inner_join(geolocation_dataset,by=c('seller_zip_code_prefix'='geolocation_zip_code_prefix')) %>% 
  rename(geolocation_lat_seller=geolocation_lat,geolocation_lng_seller=geolocation_lng)

# Refined sellers dataset result
sellers_dataset %>% head(2) %>% as.matrix()  %>% t() %>% 
  knitr::kable(col.names=c('Example Value1','Example Value2'), caption='Sellers data set')

# Refine customers data set
# similarly let see customers for whom we don't have data in geolocation_dataset
customers_to_ignore <- customers_dataset %>% anti_join(geolocation_dataset,by=c('customer_zip_code_prefix'='geolocation_zip_code_prefix')) 

# while there are small number of orders for these customers, we will ignore them as we will still have sufficient data for analysis.
orders_dataset %>% inner_join(customers_to_ignore,by='customer_id') %>% nrow()

# let's use customers of interest
customers_dataset <- customers_dataset %>% 
  inner_join(geolocation_dataset,by=c('customer_zip_code_prefix'='geolocation_zip_code_prefix')) %>% 
  rename(geolocation_lat_customer=geolocation_lat,geolocation_lng_customer=geolocation_lng)

# Refined customers dataset result
customers_dataset %>% head(2) %>% as.matrix()  %>% t() %>% 
  knitr::kable(col.names=c('Example Value1','Example Value2'), caption='Customers data set')

####################### Combining data sets
# so raw data after joining datasets of interest and converting them to usable values.
# in Brazil first digit of the five code zip code prefix can be 0. So we explicitly add 0 to it.
raw_data <- orders_dataset %>% 
  inner_join(order_items_dataset,by='order_id') %>%
  inner_join(sellers_dataset,by='seller_id') %>% 
  inner_join(customers_dataset,by='customer_id') %>%  
  inner_join(products_dataset,by='product_id') %>%
  mutate(order_purchase_timestamp=as.numeric(order_purchase_timestamp),
         product_id=as.factor(product_id), 
         seller_id=as.factor(seller_id),
         seller_zip_code_prefix=as.factor(seller_zip_code_prefix),
         customer_unique_id=as.factor(customer_unique_id),
         product_category_name=as.factor(product_category_name),
         customer_zip_code_prefix=str_pad(customer_zip_code_prefix, c(5),pad=c('0'),side='left')) %>% 
  select(-customer_id,-order_status,-seller_city,-seller_state,-customer_city,-order_id,-product_id,-seller_zip_code_prefix,-customer_unique_id)


# Prepare final wrangled data set
# we will predict number of hours to deliver based on various predictors. 
# This seems more appropriate. Though as a result of this, we won't be able to use night, day time 
# data of purchase which may influence delivery time (say to carrier it is informed only next day)
wrangled_data <- raw_data %>%
  mutate(order_delivered_customer_date=as.numeric(order_delivered_customer_date)) %>%
  mutate(number_of_hours=(order_delivered_customer_date-order_purchase_timestamp)/3600) %>%
  select(-order_delivered_customer_date,-order_estimated_delivery_date)

# Final wrangled data
wrangled_data %>% head(2) %>% as.matrix()  %>% t() %>% 
  knitr::kable(col.names=c('Example Value1','Example Value2'), caption='Final wrangled data set')


# Performance metric
RMSE <- function(true_outcomes, predicted_outcomes) {
  sqrt(mean((true_outcomes - predicted_outcomes)^2))
}

####################### Current RMSE calculation
# current RMSE can be calculated by finding number of hours for predicted delivery time 
# order_estimated_delivery_date vs actual outcome/number of hours give by order_delivered_customer_date
# However, note that order_estimated_delivery_date has no timestamp, so we will calculate RMSE 
# for both cases i.e. delivery date was taken as previous date or next date. 
# For this, from order_delivered_customer_date we will remove timestamp component, round date 
# using floor_date, which means any delivery done during the day say at 2017-10-10 21:25:13 is treated to be done on 2017-10-10
# Similarly we will round upwards using ceiling_date.

# For this, first we convert it to desired date
# then we covert date to numeric i.e. to convert it to seconds
# then we calculate time_taken_from_order_to_delivery (min and max as shared above), estimated_time_from_order_to_delivery in seconds
data_for_rmse <- raw_data %>%
  select(order_purchase_timestamp,order_delivered_customer_date,order_estimated_delivery_date) %>%
  mutate(order_delivered_customer_date_low=floor_date(order_delivered_customer_date,unit="day"),
         order_delivered_customer_date_high=ceiling_date(order_delivered_customer_date,unit="day")) %>% 
  mutate(order_delivered_customer_date_low=as.numeric(order_delivered_customer_date_low),
         order_delivered_customer_date_high=as.numeric(order_delivered_customer_date_high),
         order_estimated_delivery_date=as.numeric(order_estimated_delivery_date))  %>%
  mutate(time_taken_from_order_to_delivery_rangemax=order_delivered_customer_date_high-order_purchase_timestamp,
         time_taken_from_order_to_delivery_rangemin=order_delivered_customer_date_low-order_purchase_timestamp,
         estimated_time_from_order_to_delivery=order_estimated_delivery_date-order_purchase_timestamp) %>% 
  mutate(estimated_time_from_order_to_delivery=estimated_time_from_order_to_delivery/3600,
         time_taken_from_order_to_delivery_rangemax = time_taken_from_order_to_delivery_rangemax/3600,
         time_taken_from_order_to_delivery_rangemin = time_taken_from_order_to_delivery_rangemin/3600) %>%
  select(estimated_time_from_order_to_delivery,time_taken_from_order_to_delivery_rangemax,time_taken_from_order_to_delivery_rangemin)


RMSE_value_based_on_max <- RMSE(data_for_rmse$time_taken_from_order_to_delivery_rangemax,data_for_rmse$estimated_time_from_order_to_delivery)
# 359.9466

RMSE_value_based_on_min <- RMSE(data_for_rmse$time_taken_from_order_to_delivery_rangemin,data_for_rmse$estimated_time_from_order_to_delivery)
# 377.9515

rm(data_for_rmse)

# Range of current rmse
performance_metric <- data.frame(type=character(),value=numeric()) %>% 
  add_row(type='Current RMSE Max',value=RMSE_value_based_on_max) %>% 
  add_row(type='Current RMSE Min',value=RMSE_value_based_on_min) 

performance_metric %>% knitr::kable(caption='Performance Metric(RMSE) Summary')

# Cleanup and save required dataset
save(wrangled_data,file='rda/wrangled_data.rda')
rm(geolocation_dataset, order_items_dataset, orders_dataset, product_category_name_translation, 
   products_dataset, sellers_dataset, sellers_to_ignore, customers_dataset, 
   customers_to_ignore, raw_data)



#######################################################################################
################################## Helper functions ###################################
#######################################################################################
# Method to derive distance data
# we will use distGeo from geosphere package to compute distance in km between seller and customer. 
# considered to be highly accurate. 
extract_distance_between_seller_customer <- function(df) { 
  df %>% mutate(distance_between_two_points=round(distGeo(
    matrix(c(geolocation_lng_seller,geolocation_lat_seller),ncol=2),
    matrix(c(geolocation_lng_customer,geolocation_lat_customer),ncol=2)
  )/1000,2
  )) %>% select(-geolocation_lat_customer,-geolocation_lng_customer,-geolocation_lat_seller,-geolocation_lng_seller)
}

# Method to reduce levels in customer_zip_code_prefix
# trimming zip code to three digits to be able to extract zip code component which represents region, sub-region and sector data.
compute_rough_customer_location <- function(df) {
  df %>% mutate(customer_loc = as.numeric(str_extract(customer_zip_code_prefix,"\\d\\d\\d"))) 
}

# Method to derive delay in handover to carrier
# calculated in hours
get_delay_in_handover_to_carrier_per_seller_id <- function(df) {
  df %>% mutate(delay= (as.numeric(handed_over_to_carrier_date) -as.numeric(shipping_limit_date))/3600)  %>% 
    group_by(seller_id) %>% 
    summarise(mean_delay_in_handover=mean(delay))
}

# Get delay in payment approval based on customer zip code prefix
# calculated in hours
# can be due to fraud check etc.
get_delay_in_payment_approval_per_customer_zip_code <- function(df) {
  df %>% mutate(payment_approved_at=as.numeric(payment_approved_at)) %>%
    mutate(time_to_approve_payment_hours=(payment_approved_at-order_purchase_timestamp)/3600) %>% 
    group_by(customer_zip_code_prefix) %>% 
    summarise(mean_delay_in_payment_approval=mean(time_to_approve_payment_hours))
}

# Method to compute volume from available fields
# compute volume
compute_volume_using_product_info <- function(df) {
  df %>% mutate(volume=product_length_cm*product_height_cm*product_width_cm) %>%
    select(-product_length_cm,-product_height_cm,-product_width_cm)
}

#######################################################################################
################### Partitioning dataset for training and validation ##################
#######################################################################################
# Split data into training and validation set, where validation set will be used later
set.seed(1,sample.kind = 'Rounding')
test_indices <- createDataPartition(wrangled_data$number_of_hours,times=1,p=0.2,list=FALSE)

training_dataset <- wrangled_data[-test_indices,]
validation_dataset <- wrangled_data[test_indices,]

# ensure validation_dataset has products only for concerned product categories and seller_ids 
# and customer_zip_code_prefix which are in training_dataset 
validation_dataset <- validation_dataset %>%
  semi_join(training_dataset,by='product_category_name') %>%
  semi_join(training_dataset,by='seller_id') %>% 
  semi_join(training_dataset,by="customer_zip_code_prefix")

rm(test_indices)


# The partitioned data were named training_dataset and validation_dataset, where training_dataset 
# was our training data set and validation_dataset was our validation data set, which we kept 
# aside as planned earlier. 
data.frame(data_set=c("Training data set","Validation data set"),
           name=c('training_dataset','validation_dataset'), 
           count=c(nrow(training_dataset),nrow(validation_dataset))) %>% 
  knitr::kable()

####################### Partition training_dataset further
# since we will be using only training_dataset and use it for feature analysis, model training,
# tuning and evaluation and to identify weights for target encoding of categorical variables. 
# We use training_dataset and split into into train and test set. 
# Let's split training_dataset into two parts
set.seed(1,sample.kind = 'Rounding')
training_dataset <- training_dataset %>% compute_volume_using_product_info() %>% extract_distance_between_seller_customer() %>% compute_rough_customer_location()

test_indices <- createDataPartition(training_dataset$number_of_hours,times=1,p=0.2,list=FALSE)

train_subset <- training_dataset[-test_indices,]
test_subset <- training_dataset[test_indices,]

# ensure test_subset has products only for concerned product categories and seller_ids which are in train_subset
test_subset <- test_subset %>% semi_join(train_subset,by='product_category_name') %>%
  semi_join(train_subset,by='seller_id') %>% 
  semi_join(train_subset,by="customer_zip_code_prefix") %>% 
  semi_join(train_subset,by="customer_loc")

rm(test_indices)


#######################################################################################
############ Helper methods for preparing derived ands categorical features ###########
#######################################################################################

# Dataframe to store target encoding weights
target_encoding_weights <- data.frame(effect=character(0),weight=double(0)) 

# Prepare delay in handover to carried data
# prepare delay_in_handover_to_carrier_data based on regularisation weight we have identified
prepare_delay_in_handover_to_carrier <- function(train_df, weight_to_use) {
  global_mean <- mean(train_df %>% 
                        get_delay_in_handover_to_carrier_per_seller_id() %>% 
                        pull(mean_delay_in_handover))
  
  set.seed(1,sample.kind='Rounding')
  test_weight_indices <- createDataPartition(train_df$number_of_hours,p=0.2,times=10,list=FALSE)
  
  delay_in_handover_to_carrier_data_list <- lapply(1:10, function(index) {
    train_weight_set <- train_df[-test_weight_indices[,index],]
    
    delay_in_handover_to_carrier_data <- train_weight_set %>% 
      get_delay_in_handover_to_carrier_per_seller_id()
    
    delay_in_handover_to_carrier_data
  }
  )
  
  dhc <- delay_in_handover_to_carrier_data_list[[1]]
  for (index in 2:10) {
    dhc <- dhc %>% full_join(delay_in_handover_to_carrier_data_list[[index]],by='seller_id', suffix=c("1",index))
  }
  
  # wherever value is NA, we have used global mean
  dhc <- dhc %>% mutate(across(-seller_id,~if_else(is.na(.x),global_mean,.x)))
  
  # as different partitions may not have exactly same data set combined the results
  delay_in_handover_to_carrier_data <- tibble(dhc[,1],
                                              mean_delay_in_handover=rowMeans(dhc[,-1],
                                                                              na.rm = TRUE))
  
  #delay_in_handover_to_carrier_data is ready
  delay_in_handover_to_carrier_data
}

# Prepare delay in payment approval
# prepare delay_in_payment_approval based on regularisation weight we have identified
prepare_delay_in_payment_approval <- function(train_df, weight_to_use) {
  global_mean <- mean(train_df %>% 
                        get_delay_in_payment_approval_per_customer_zip_code() %>% 
                        pull(mean_delay_in_payment_approval))
  
  set.seed(1,sample.kind='Rounding')
  test_weight_indices <- createDataPartition(train_df$number_of_hours,p=0.2,times=10,list=FALSE)
  
  delay_in_payment_approval_data_list <- lapply(1:10, function(index) {
    train_weight_set <- train_df[-test_weight_indices[,index],]
    
    delay_in_payment_approval_data <- train_weight_set %>% 
      get_delay_in_payment_approval_per_customer_zip_code()
    
    delay_in_payment_approval_data
  }
  )
  
  dpa <- delay_in_payment_approval_data_list[[1]]
  for (index in 2:10) {
    dpa <- dpa %>% full_join(delay_in_payment_approval_data_list[[index]],by='customer_zip_code_prefix', suffix=c("1",index))
  }
  
  # wherever value is NA, we have used global mean
  dpa <- dpa %>% mutate(across(-customer_zip_code_prefix,~if_else(is.na(.x),global_mean,.x)))
  
  # as different partitions may not have exactly same data set combined the results
  delay_in_payment_approval_data <- tibble(dpa[,1],
                                           mean_delay_in_payment_approval=rowMeans(dpa[,-1],
                                                                                   na.rm = TRUE))
  
  #delay_in_payment_approval_data is ready
  delay_in_payment_approval_data
}

# Prepare product_category effect based on target encoding
# prepare product_category_effect based on regularisation weight we have identified
prepare_product_category_effect <- function(train_df, weight_to_use) {
  global_mean <- mean(train_df$number_of_hours)
  
  set.seed(1,sample.kind='Rounding')
  test_weight_indices <- createDataPartition(train_df$number_of_hours,p=0.2,times=10,list=FALSE)
  
  product_category_effects_list <- lapply(1:10, function(index) {
    train_weight_set <- train_df[-test_weight_indices[,index],]
    
    product_category_effects <- train_weight_set %>% 
      group_by(product_category_name) %>%
      summarise(mean=mean(number_of_hours),n=n()) %>%
      mutate(mean=(n*mean+weight_to_use*global_mean)/(n+weight_to_use)) %>% 
      select(-n) %>% rename()
    
    product_category_effects
  }
  )
  
  pce <- product_category_effects_list[[1]]
  for (index in 2:10) {
    pce <- pce %>% full_join(product_category_effects_list[[index]],by='product_category_name', suffix=c("1",index))
  }
  
  # wherever value is NA, we have used global mean
  pce <- pce %>% mutate(across(-product_category_name,~if_else(is.na(.x),global_mean,.x)))
  
  # as different partitions may not have exactly same data set combined the results
  product_category_effects <- tibble(pce[,1], product_category_effect=rowMeans(pce[,-1],na.rm = TRUE))
  
  #product_category effects are ready
  product_category_effects
}

# Prepare seller_id effect based on target encoding
# prepare seller_id_effect based on regularisation weight we have identified
prepare_seller_id_effect <- function(train_df, weight_to_use) {
  global_mean <- mean(train_df$number_of_hours)
  
  set.seed(1,sample.kind='Rounding')
  test_weight_indices <- createDataPartition(train_df$number_of_hours, p=0.2,times=10,list=FALSE)
  
  seller_id_effects_list <- lapply(1:10, function(index) {
    train_weight_set <- train_df[-test_weight_indices[,index],]
    
    seller_id_effects <- train_weight_set %>% 
      group_by(seller_id) %>%
      summarise(mean=mean(number_of_hours),n=n()) %>%
      mutate(mean=(n*mean+weight_to_use*global_mean)/(n+weight_to_use)) %>% 
      select(-n) %>% rename()
    
    seller_id_effects
  })
  
  
  sie <- seller_id_effects_list[[1]]
  for (index in 2:10) {
    sie <- sie %>% 
      full_join(seller_id_effects_list[[index]],by='seller_id', suffix=c("1",index))
  }
  
  # wherever value is NA, we have used global mean
  sie <- sie %>% mutate(across(-seller_id,~if_else(is.na(.x),global_mean,.x)))
  
  # as different partitions may not have exactly same data set combined the results
  seller_id_effects <- tibble(sie[,1], seller_id_effect=rowMeans(sie[,-1],na.rm = TRUE))
  
  #seller id effects are ready
  seller_id_effects
}

# Prepare customer_state effect based on target encoding
# prepare consumer_state_effect based on regularisation weight we have identified
prepare_customer_state_effect <- function(train_df, weight_to_use) {
  global_mean <- mean(train_df$number_of_hours)
  
  set.seed(1,sample.kind='Rounding')
  test_weight_indices <- createDataPartition(train_df$number_of_hours,p=0.2,times=10,list=FALSE)
  
  customer_state_effects_list <- lapply(1:10, function(index) {
    train_weight_set <- train_df[-test_weight_indices[,index],]
    
    customer_state_effects <- train_weight_set %>% 
      group_by(customer_state) %>%
      summarise(mean=mean(number_of_hours),n=n()) %>%
      mutate(mean=(n*mean+weight_to_use*global_mean)/(n+weight_to_use)) %>% 
      select(-n) %>% rename()
    
    customer_state_effects
  }
  )
  
  cse <- customer_state_effects_list[[1]]
  for (index in 2:10) {
    cse <- cse %>% 
      full_join(customer_state_effects_list[[index]],by='customer_state', suffix=c("1",index))
  }
  
  # wherever value is NA, we have used global mean
  cse <- cse %>% mutate(across(-customer_state,~if_else(is.na(.x),global_mean,.x)))
  
  # as different partitions may not have exactly same data set combined the results
  customer_state_effects <- tibble(cse[,1], customer_state_effect=rowMeans(cse[,-1],na.rm = TRUE))
  
  #customer state effects are ready
  customer_state_effects
}

# Prepare customer_loc effect based on target encoding
# prepare customer_loc_effect based on regularisation weight we have identified
prepare_customer_loc_effect <- function(train_df, weight_to_use) {
  global_mean <- mean(train_df$number_of_hours)
  
  set.seed(1,sample.kind='Rounding')
  test_weight_indices <- createDataPartition(train_df$number_of_hours,p=0.2,times=10,list=FALSE)
  
  customer_loc_effects_list <- lapply(1:10, function(index) {
    train_weight_set <- train_df[-test_weight_indices[,index],]
    
    customer_loc_effects <- train_weight_set %>% 
      group_by(customer_loc) %>%
      summarise(mean=mean(number_of_hours),n=n()) %>%
      mutate(mean=(n*mean+weight_to_use*global_mean)/(n+weight_to_use)) %>% 
      select(-n) %>% rename()
    
    customer_loc_effects
  }
  )
  
  cle <- customer_loc_effects_list[[1]]
  for (index in 2:10) {
    cle <- cle %>% full_join(customer_loc_effects_list[[index]],by='customer_loc', suffix=c("1",index))
  }
  
  # where-ever value is NA, we have used global mean
  cle <- cle %>% mutate(across(-customer_loc,~if_else(is.na(.x),global_mean,.x)))
  
  # as different partitions may not have exactly same data set combined the results
  customer_loc_effects <- tibble(cle[,1], customer_loc_effect=rowMeans(cle[,-1],na.rm = TRUE))
  
  #customer loc effects are ready
  customer_loc_effects
}

# Enhance training and test dataset for delay in handover to carrier
enhance_datasets_using_delay_in_handover_to_carrier <- function(mean_delay_in_handover_to_carrier_data,train_df,test_df,cleanup) {
  #we will use mean_delay_in_handover_to_carrier_data computed using training data in test data.
  train_df <- train_df %>% left_join(mean_delay_in_handover_to_carrier_data,by='seller_id') 
  test_df <- test_df %>% left_join(mean_delay_in_handover_to_carrier_data,by='seller_id')
  
  #cleanup
  if (cleanup) {
    train_df <- train_df %>% select(-handed_over_to_carrier_date,-shipping_limit_date)
    test_df <- test_df %>% select(-handed_over_to_carrier_date,-shipping_limit_date)
  }
  
  list(train_df,test_df)
}

# Enhance training and test dataset for delay in payment approval
enhance_datasets_using_delay_in_payment_approval <- function(delay_in_payment_approval_data,train_df,test_df,cleanup) {
  #we will use delay_in_payment_approval_data computed using training data in test data.  
  train_df <- train_df %>%
    left_join(delay_in_payment_approval_data,by='customer_zip_code_prefix')
  
  test_df <- test_df %>% left_join(delay_in_payment_approval_data,by='customer_zip_code_prefix')
  
  #cleanup
  if (cleanup) {
    train_df <- train_df %>%
      select(-customer_zip_code_prefix,-order_purchase_timestamp,-payment_approved_at)
    test_df <- test_df %>% 
      select(-customer_zip_code_prefix,-order_purchase_timestamp,-payment_approved_at)
  }
  
  list(train_df,test_df)
}

# Enhance training and test dataset for product_category effect based on target encoding
enhance_datasets_using_product_category_effect <- function(product_category_effects,train_df,test_df,cleanup) {
  global_mean <- mean(train_df$number_of_hours)
  
  #we will use product category effects computed using training data in test data.
  train_df <- train_df %>% left_join(product_category_effects,by='product_category_name') %>%
    mutate(product_category_effect=if_else(is.na(product_category_effect),global_mean,product_category_effect))
  
  test_df <- test_df %>% left_join(product_category_effects,by='product_category_name')
  
  # cleanup
  if (cleanup) {
    train_df <- train_df %>% select(-product_category_name)
    test_df <- test_df %>% select(-product_category_name)
  }
  
  list(train_df,test_df)
}

# Enhance training and test dataset for seller_id effect based on target encoding
enhance_datasets_using_seller_id_effect <- function(seller_id_effects,train_df,test_df,cleanup) {
  global_mean <- mean(train_df$number_of_hours)
  
  #we will use seller id effects computed using training data in test data.
  train_df <- train_df %>% left_join(seller_id_effects,by='seller_id') %>%
    mutate(seller_id_effect=if_else(is.na(seller_id_effect),global_mean,seller_id_effect))
  
  test_df <- test_df %>% left_join(seller_id_effects,by='seller_id')
  
  # cleanup
  if (cleanup) {
    train_df <- train_df %>% select(-seller_id)
    test_df <- test_df %>% select(-seller_id)
  }
  
  list(train_df,test_df)
}

# Enhance training and test dataset for customer_state effect based on target encoding
enhance_datasets_using_customer_state_effect <- function(customer_state_effects,train_df,test_df,cleanup) {
  global_mean <- mean(train_df$number_of_hours)  
  
  #we will use customer state effects computed using training data in test data.
  train_df <- train_df %>% left_join(customer_state_effects,by='customer_state') %>%
    mutate(customer_state_effect=if_else(is.na(customer_state_effect),
                                         global_mean,
                                         customer_state_effect))
  
  test_df <- test_df %>% left_join(customer_state_effects,by='customer_state') 
  
  # cleanup
  if (cleanup) {
    train_df <- train_df %>% select(-customer_state)
    test_df <- test_df %>% select(-customer_state)
  }
  
  list(train_df,test_df)
}

# Enhance training and test dataset for customer_loc effect based on target encoding
enhance_datasets_using_customer_loc_effect <- function(customer_loc_effects,train_df,test_df,cleanup) {
  global_mean <- mean(train_df$number_of_hours)
  
  #we will use customer loc effects computed using training data in test data.
  train_df <- train_df %>% 
    left_join(customer_loc_effects,by='customer_loc') %>%
    mutate(customer_loc_effect=if_else(is.na(customer_loc_effect),global_mean,customer_loc_effect))
  
  test_df <- test_df %>% left_join(customer_loc_effects,by='customer_loc')
  
  # cleanup
  if (cleanup) {
    train_df <- train_df %>% select(-customer_loc, -customer_zip_code_prefix)
    test_df <- test_df %>% select(-customer_loc, -customer_zip_code_prefix)
  }
  
  list(train_df,test_df)
}

# Enhance training and testing datasets with derived features
# uses training data to derive features and then enhances training and test set with those features
enhance_datasets_with_derived_features <- function(train_ds,test_ds,cleanup) {
  print('Adding column for delay in handover')
  
  delay_in_handover_to_carrier_data <- prepare_delay_in_handover_to_carrier(train_ds,0)
  
  dataset_list <- enhance_datasets_using_delay_in_handover_to_carrier(delay_in_handover_to_carrier_data,train_ds,test_ds,cleanup)
  train_ds <- dataset_list[[1]]
  test_ds <- dataset_list[[2]]
  
  print('Adding column for delay in payment approval')
  delay_in_payment_approval_data <- prepare_delay_in_payment_approval(train_ds,0)
  
  #we will use delay_in_payment_approval computed using training data in test data.
  dataset_list <- enhance_datasets_using_delay_in_payment_approval(delay_in_payment_approval_data,train_ds,test_ds,cleanup)
  
  dataset_list
}  

# Prepare data set for training
# prepares data for training and testing
# returns list of data frames. First list has training dataframe. Second list has test data frame
prepare_data_for_training_testing <- function(train_ds, test_ds, cleanup) {
  test_ds <- test_ds %>%
    semi_join(train_ds,by='product_category_name') %>%
    semi_join(train_ds,by='seller_id') %>% 
    semi_join(train_ds,by='customer_zip_code_prefix') %>% 
    semi_join(train_ds,by='customer_state') %>% 
    semi_join(train_ds,by='customer_loc')
  
  #adding derived features and target encoded categorical features
  print('Adding derived features')
  dataset_list <- enhance_datasets_with_derived_features(train_ds,test_ds,cleanup)
  train_ds <- dataset_list[[1]]
  test_ds <- dataset_list[[2]]
  
  print('Enhancing data for product category effects')
  weight_to_use <- target_encoding_weights %>% 
    filter(effect=='product_category') %>% 
    pull(weight)
  product_category_effects <- prepare_product_category_effect(train_ds,weight_to_use)
  dataset_list <- enhance_datasets_using_product_category_effect(product_category_effects,train_ds,test_ds,cleanup)
  train_ds <- dataset_list[[1]]
  test_ds <- dataset_list[[2]]
  
  print('Enhancing data for seller id effects')
  weight_to_use <- target_encoding_weights %>% filter(effect=='seller_id') %>% pull(weight)
  seller_id_effects <- prepare_seller_id_effect(train_ds,weight_to_use)
  dataset_list <- enhance_datasets_using_seller_id_effect(seller_id_effects,train_ds,test_ds,cleanup)
  train_ds <- dataset_list[[1]]
  test_ds <- dataset_list[[2]]
  
  print('Enhancing data for customer state effects')
  weight_to_use <- target_encoding_weights %>% filter(effect=='customer_state') %>% pull(weight)
  customer_state_effects <- prepare_customer_state_effect(train_ds,weight_to_use)
  dataset_list <- enhance_datasets_using_customer_state_effect(customer_state_effects,train_ds,test_ds,cleanup)
  train_ds <- dataset_list[[1]]
  test_ds <- dataset_list[[2]]
  
  print('Enhancing data for customer loc effects')
  weight_to_use <- target_encoding_weights %>% filter(effect=='customer_loc') %>% pull(weight)
  customer_loc_effects <- prepare_customer_loc_effect(train_ds,weight_to_use)
  dataset_list <- enhance_datasets_using_customer_loc_effect(customer_loc_effects,train_ds,test_ds,cleanup)
  dataset_list
}


#######################################################################################
########################### Feature Analysis and Selection ############################
#######################################################################################

####################### Distance between seller and customer feature

# Explore distance_between_two_points relation with number_of_hours
training_dataset  %>% ggplot(aes(distance_between_two_points,number_of_hours)) + 
  geom_point() + geom_smooth() + xlab('Distance between seller and the customer') + 
  ylab('Number of hours taken for delivery')

# correlation analysis for distance_between_two_points with number of hours

analysis_output_rows <- sapply(seq(0,2000,1000), function(offset) {  
  list(delay_limit=str_c('>=',offset,' & <',offset+1000),
       correlation=cor(training_dataset %>% 
                         filter(distance_between_two_points>=offset & distance_between_two_points< offset+1000) %>% 
                         pull(distance_between_two_points), 
                       training_dataset %>% 
                         filter(distance_between_two_points>=offset & distance_between_two_points< offset+1000) %>% 
                         pull(number_of_hours))) 
},simplify=TRUE)

analysis_output_rows %>% t() %>% rbind( c(str_c('>=',3000),
                                          round(cor(training_dataset %>% 
                                                      filter(distance_between_two_points>=3000) %>%
                                                      pull(distance_between_two_points), 
                                                    training_dataset %>% 
                                                      filter(distance_between_two_points>=3000) %>%
                                                      pull(number_of_hours)),7))
) %>% as.data.frame() %>% knitr::kable(caption='Correlation analysis between distance between seller & carrier and number of hours taken for delivery')

# cleanup
rm(analysis_output_rows)

####################### Delay in handover of product from seller to carrier feature

# Explore delay_in_handover_to_carrier relation with number_of_hours
delay_in_handover_to_carrier_analysis_dataset <- training_dataset %>%
  get_delay_in_handover_to_carrier_per_seller_id()

analysis_joined_data <- training_dataset %>%
  left_join(delay_in_handover_to_carrier_analysis_dataset,by='seller_id')

analysis_joined_data %>% ggplot(aes(mean_delay_in_handover,number_of_hours)) + geom_point() + 
  geom_smooth() + xlab('Delay in handover of product to carrier') + 
  ylab('Number of hours taken for delivery')


# correlation analysis for mean_delay_in_handover with number of hours
data.frame(delay_limit=character(0),correlation=numeric(0)) %>% 
  add_row(delay_limit='<= 0',correlation=cor(analysis_joined_data %>% 
                                               filter(mean_delay_in_handover<= 0) %>% 
                                               pull(mean_delay_in_handover), 
                                             analysis_joined_data %>% 
                                               filter(mean_delay_in_handover<= 0) %>% 
                                               pull(number_of_hours))
  ) %>% 
  add_row(delay_limit='> 0',correlation=cor(analysis_joined_data %>% 
                                              filter(mean_delay_in_handover> 0) %>% 
                                              pull(mean_delay_in_handover), 
                                            analysis_joined_data %>% 
                                              filter(mean_delay_in_handover> 0) %>% 
                                              pull(number_of_hours))) %>% 
  knitr::kable(caption='Correlation analysis between delay in handover to carrier and number of hours taken for delivery')


# cleanup
rm(delay_in_handover_to_carrier_analysis_dataset,analysis_joined_data)


####################### Delay in payment approval feature
# Explore delay_in_payment_approval relation with number_of_hours
delay_in_payment_approval_analysis_dataset <- training_dataset %>%
  get_delay_in_payment_approval_per_customer_zip_code()

analysis_joined_data <- training_dataset %>%
  left_join(delay_in_payment_approval_analysis_dataset,by='customer_zip_code_prefix')

analysis_joined_data %>% ggplot(aes(mean_delay_in_payment_approval,log10(number_of_hours))) + 
  geom_point() + geom_smooth() + xlab('Delay in payment approval') + 
  ylab('Number of hours taken for delivery [log10(n) scale]')


# correlation analysis for mean_delay_in_payment_approval with number of hours
analysis_output_rows <- sapply(seq(0,125,25), function(offset) {  
  list(delay_in_approval=str_c('>=',offset,' & <',offset+25),
       correlation=cor(analysis_joined_data %>% 
                         filter(mean_delay_in_payment_approval>=offset & mean_delay_in_payment_approval< offset+25) %>% 
                         pull(mean_delay_in_payment_approval), 
                       analysis_joined_data %>% 
                         filter(mean_delay_in_payment_approval>=offset & mean_delay_in_payment_approval< offset+25) %>% 
                         pull(number_of_hours))) 
},simplify=TRUE)

analysis_output_rows %>% t() %>% rbind( c(str_c('>=',150),
                                          round(cor(analysis_joined_data %>% 
                                                      filter(mean_delay_in_payment_approval>=150) %>%
                                                      pull(mean_delay_in_payment_approval), 
                                                    analysis_joined_data %>% 
                                                      filter(mean_delay_in_payment_approval>=150) %>%
                                                      pull(number_of_hours)),7))
) %>% as.data.frame() %>% knitr::kable(caption='Correlation analysis between delay in payment approval and number of hours taken for delivery')

# cleanup
rm(analysis_output_rows)
rm(delay_in_payment_approval_analysis_dataset,analysis_joined_data)

####################### Volume feature
# Explore volume relation with number_of_hours
training_dataset  %>% ggplot(aes(volume,log10(number_of_hours))) + geom_point() + geom_smooth() + 
  xlab('Volume of ordered product') + ylab('Number of hours taken for delivery [log10(n) scale]')

# correlation analysis for volume with number of hours

analysis_output_rows <- sapply(seq(0,210000,70000), function(offset) {  
  list(delay_limit=str_c('>=',offset,' & <',offset+70000),
       correlation=cor(training_dataset %>% 
                         filter(volume>=offset & volume< offset+70000) %>% 
                         pull(volume), 
                       training_dataset %>% 
                         filter(volume>=offset & volume< offset+70000) %>% 
                         pull(number_of_hours))) 
},simplify=TRUE)

analysis_output_rows %>% t() %>% rbind( c(str_c('>=',280000),
                                          round(cor(training_dataset %>% 
                                                      filter(volume>=280000) %>%
                                                      pull(volume), 
                                                    training_dataset %>% 
                                                      filter(volume>=280000) %>%
                                                      pull(number_of_hours)),7))
) %>% as.data.frame() %>% knitr::kable(caption='Correlation analysis between volume and number of hours taken for delivery')

rm(analysis_output_rows)

####################### Product category effects
# Explore product_category_name relation with number_of_hours
training_dataset %>% group_by(product_category_name) %>%
  summarise(mean=mean(number_of_hours),se=sd(number_of_hours)/sqrt(n())) %>% 
  mutate(product_category_name=reorder(product_category_name,mean)) %>% 
  ggplot(aes(x=product_category_name,y=mean,ymax=mean+2*se,ymin=mean-2*se)) +
  geom_point()+ geom_errorbar() + theme(axis.text.x = element_text(angle=90,size=8)) + 
  xlab('Product Categories') + ylab('Avg number of hours')


# Apply Target encoding for product_category_name
# here we will try to identify the regularization weight/smoothing factor to use by carrying out target encoding 
# for different datasets set of weights, selecting single weight per dataset based on 
# performance metric i.e. RMSE and then we will take a mean of weights identified across the datasets.
train_subset_for_encoding <- train_subset[1:40000,] %>% select(-customer_loc, -customer_state)

global_mean <- mean(train_subset_for_encoding$number_of_hours)

set.seed(1,sample.kind='Rounding')
test_weight_indices <- createDataPartition(train_subset_for_encoding$number_of_hours,p=0.2,times=3,list=FALSE)

weights <- seq(0,10,5)

weight_to_use_per_partition <- sapply(1:3, function(index) {
  train_weight_set <- train_subset_for_encoding[-test_weight_indices[,index],]
  test_weight_set <- train_subset_for_encoding[test_weight_indices[,index],]
  
  test_weight_set <- test_weight_set %>%
    semi_join(train_weight_set,by='product_category_name') %>%
    semi_join(train_weight_set,by='seller_id') %>%
    semi_join(train_weight_set,by='customer_zip_code_prefix')
  
  dataset_list <- enhance_datasets_with_derived_features(train_weight_set,test_weight_set,TRUE)
  train_weight_set <- dataset_list[[1]] %>% select(-seller_id)
  test_weight_set <- dataset_list[[2]] %>% select(-seller_id)
  
  # calculating rmse for each weight
  weight_rmses <- sapply(weights, function(weight) {
    product_category_effects <- train_weight_set %>% 
      group_by(product_category_name) %>% 
      summarise(mean=mean(number_of_hours),n=n()) %>%
      mutate(mean=(n*mean+weight*global_mean)/(n+weight)) %>% 
      select(-n)
    
    train_weight_set <- train_weight_set %>% left_join(product_category_effects,by='product_category_name') %>% rename(product_category_effect=mean) %>% select(-product_category_name)
    
    test_weight_set <- test_weight_set %>% left_join(product_category_effects,by='product_category_name') %>% rename(product_category_effect=mean) %>% select(-product_category_name)
    
    set.seed(seed=1,sample.kind = 'Rounding')
    preProcessScalingValues <- preProcess(train_weight_set %>% select(-number_of_hours), method = c("center", "scale"))
    
    scaled_train_subset <- predict(preProcessScalingValues, train_weight_set)
    scaled_test_subset <- predict(preProcessScalingValues, test_weight_set)
    
    tuningControl=trainControl(method = "cv", number = 3)
    knn_fit_for_weight <- train(number_of_hours~., scaled_train_subset, method='knn',tuneGrid=data.frame(k=c(41,47,53)),trControl=tuningControl)
    
    knn_fit_for_weight
    print(knn_fit_for_weight)
    
    predictions <- predict(knn_fit_for_weight,scaled_test_subset %>% select(-number_of_hours))
    
    RMSE(scaled_test_subset$number_of_hours,predictions) 
  })
  
  # selecting weight leading to lowest RMSE
  weight_to_use <- weights[which.min(weight_rmses)]
  paste('Weight to use: ', weight_to_use, ', corresponding RMSE: ', weight_rmses[which.min(weight_rmses)])
  
  print(weight_rmses)
  weight_to_use
})

weight_to_use_per_partition

weight_to_use <- mean(weight_to_use_per_partition)  
# 5

target_encoding_weights <- target_encoding_weights %>% 
  add_row(effect='product_category',weight=weight_to_use)


####################### Seller id effects
# Explore seller_id relation with number_of_hours
# Since there were lot many sellers, we sampled 300 of them to represent their influence 
# effectively in the graph. 

set.seed(1, sample.kind = 'Rounding')

training_dataset %>% group_by(seller_id) %>%
  summarise(mean=mean(number_of_hours),se=sd(number_of_hours)/sqrt(n())) %>%
  mutate(seller_id=reorder(seller_id,mean)) %>% 
  filter(seller_id %in% sample(seller_id,300,replace=FALSE)) %>%
  ggplot(aes(x=seller_id,y=mean,ymax=mean+2*se,ymin=mean-2*se)) +
  geom_point()+ geom_errorbar() + theme(axis.text.x = element_blank()) + xlab('Seller Ids') + 
  ylab('Avg number of hours')

# Apply Target encoding for seller_id
# here we will try to identify the regularization weight/smoothing factor to use by carrying out target encoding
train_subset_for_encoding <- train_subset[1:40000,] %>% select(-customer_loc, -customer_state)

global_mean <- mean(train_subset_for_encoding$number_of_hours)

set.seed(1,sample.kind='Rounding')
test_weight_indices <- createDataPartition(train_subset_for_encoding$number_of_hours,p=0.2,times=3,list=FALSE)

weights <- seq(0,9,3)

weight_to_use_per_partition <- sapply(1:3, function(index) {
  train_weight_set <- train_subset_for_encoding[-test_weight_indices[,index],]
  test_weight_set <- train_subset_for_encoding[test_weight_indices[,index],]
  
  test_weight_set <- test_weight_set %>%
    semi_join(train_weight_set,by='product_category_name') %>%
    semi_join(train_weight_set,by='seller_id') %>% 
    semi_join(train_weight_set,by='customer_zip_code_prefix')
  
  ## adding derived features and target encoded categorical features
  dataset_list <- enhance_datasets_with_derived_features(train_weight_set,test_weight_set,FALSE)
  train_weight_set <- dataset_list[[1]]
  test_weight_set <- dataset_list[[2]]
  
  weight_to_use <- target_encoding_weights %>% 
    filter(effect=='product_category') %>% pull(weight)
  
  product_category_effects <- prepare_product_category_effect(train_weight_set,weight_to_use)
  dataset_list <- enhance_datasets_using_product_category_effect(product_category_effects,train_weight_set,test_weight_set,FALSE)
  
  train_weight_set <- dataset_list[[1]] %>% select(-order_purchase_timestamp,-handed_over_to_carrier_date,-shipping_limit_date,-customer_zip_code_prefix,-payment_approved_at,-product_category_name)
  test_weight_set <- dataset_list[[2]] %>% select(-order_purchase_timestamp,-handed_over_to_carrier_date,-shipping_limit_date,-customer_zip_code_prefix,-payment_approved_at,-product_category_name)
  ##
  
  # calculating rmse for each weight
  weight_rmses <- sapply(weights, function(weight) {
    seller_id_effects <- train_weight_set %>% 
      group_by(seller_id) %>% 
      summarise(mean=mean(number_of_hours),n=n()) %>%
      mutate(mean=(n*mean+weight*global_mean)/(n+weight)) %>% 
      select(-n)
    
    train_weight_set <- train_weight_set %>% left_join(seller_id_effects,by='seller_id') %>% rename(seller_id_effect=mean) %>% select(-seller_id)
    
    test_weight_set <- test_weight_set %>% left_join(seller_id_effects,by='seller_id') %>% rename(seller_id_effect=mean) %>% select(-seller_id)
    
    set.seed(seed=1,sample.kind = 'Rounding')
    preProcessScalingValues <- preProcess(train_weight_set %>% select(-number_of_hours), method = c("center", "scale"))
    
    scaled_train_subset <- predict(preProcessScalingValues, train_weight_set)
    scaled_test_subset <- predict(preProcessScalingValues, test_weight_set)
    
    tuningControl=trainControl(method = "cv", number = 3)
    knn_fit_for_weight <- train(number_of_hours~., scaled_train_subset, method='knn',tuneGrid=data.frame(k=c(41,47,53)),trControl=tuningControl)
    
    knn_fit_for_weight
    
    print(knn_fit_for_weight)
    
    predictions <- predict(knn_fit_for_weight,scaled_test_subset %>% select(-number_of_hours))
    
    RMSE(scaled_test_subset$number_of_hours,predictions) 
  })
  
  # selecting weight leading to lowest RMSE
  weight_to_use <- weights[which.min(weight_rmses)]
  paste('Weight to use: ', weight_to_use, ', corresponding RMSE: ', weight_rmses[which.min(weight_rmses)])
  
  print(weight_rmses)
  weight_to_use
})

weight_to_use_per_partition

weight_to_use <- mean(weight_to_use_per_partition)  
# 8

target_encoding_weights <- target_encoding_weights %>% 
  add_row(effect='seller_id',weight=weight_to_use)


####################### Customer state effects
# Explore customer_state relation with number_of_hours
set.seed(1, sample.kind = 'Rounding')
training_dataset %>% group_by(customer_state) %>%
  summarise(mean=mean(number_of_hours),se=sd(number_of_hours)/sqrt(n())) %>%
  mutate(customer_state=reorder(customer_state,mean)) %>% 
  ggplot(aes(x=customer_state,y=mean,ymax=mean+2*se,ymin=mean-2*se)) +
  geom_point()+ geom_errorbar() + theme(axis.text.x = element_text(angle=90,size=8)) + 
  xlab('Customer States') + ylab('Avg number of hours')

# Apply Target encoding for customer_state
# here we will try to identify the regularization weight/smoothing factor to use by carrying out target encoding 
train_subset_for_encoding <- train_subset[1:40000,] %>% select(-customer_loc)
global_mean <- mean(train_subset_for_encoding$number_of_hours)

set.seed(1,sample.kind='Rounding')
test_weight_indices <- createDataPartition(train_subset_for_encoding$number_of_hours,p=0.2,times=3,list=FALSE)

weights <- seq(0,30,15)

weight_to_use_per_partition <- sapply(1:3, function(index) {
  train_weight_set <- train_subset_for_encoding[-test_weight_indices[,index],]
  test_weight_set <- train_subset_for_encoding[test_weight_indices[,index],]
  
  test_weight_set <- test_weight_set %>% semi_join(train_weight_set,by='customer_state') 
  
  test_weight_set <- test_weight_set %>%
    semi_join(train_weight_set,by='product_category_name') %>%
    semi_join(train_weight_set,by='seller_id') %>% 
    semi_join(train_weight_set,by='customer_zip_code_prefix') %>% 
    semi_join(train_weight_set,by='customer_state')
  
  ## adding derived features and target encoded categorical features
  dataset_list <- enhance_datasets_with_derived_features(train_weight_set,test_weight_set,FALSE)
  train_weight_set <- dataset_list[[1]]
  test_weight_set <- dataset_list[[2]]
  
  weight_to_use <- target_encoding_weights %>% 
    filter(effect=='product_category') %>% pull(weight)
  
  product_category_effects <- prepare_product_category_effect(train_weight_set,weight_to_use)
  dataset_list <- enhance_datasets_using_product_category_effect(product_category_effects,train_weight_set,test_weight_set,FALSE)
  
  train_weight_set <- dataset_list[[1]] %>% select(-order_purchase_timestamp,-handed_over_to_carrier_date,-shipping_limit_date,-customer_zip_code_prefix,-payment_approved_at,-product_category_name)
  test_weight_set <- dataset_list[[2]] %>% select(-order_purchase_timestamp,-handed_over_to_carrier_date,-shipping_limit_date,-customer_zip_code_prefix,-payment_approved_at,-product_category_name)
  
  weight_to_use <- target_encoding_weights %>% filter(effect=='seller_id') %>% pull(weight)
  
  seller_id_effects <- prepare_seller_id_effect(train_weight_set,weight_to_use)
  dataset_list <- enhance_datasets_using_seller_id_effect(seller_id_effects,train_weight_set,test_weight_set,FALSE)
  
  train_weight_set <- dataset_list[[1]] %>% select(-seller_id)
  test_weight_set <- dataset_list[[2]] %>% select(-seller_id)
  ##
  
  # calculating rmse for each weight
  weight_rmses <- sapply(weights, function(weight) {
    customer_state_effects <- train_weight_set %>% 
      group_by(customer_state) %>% 
      summarise(mean=mean(number_of_hours),n=n()) %>%
      mutate(mean=(n*mean+weight*global_mean)/(n+weight)) %>% 
      select(-n)
    
    train_weight_set <- train_weight_set %>% left_join(customer_state_effects,by='customer_state') %>% rename(customer_state_effect=mean) %>% select(-customer_state) #-product_category_name
    
    test_weight_set <- test_weight_set %>% left_join(customer_state_effects,by='customer_state') %>% rename(customer_state_effect=mean) %>% select(-customer_state) # -product_category_name,
    
    set.seed(seed=1,sample.kind = 'Rounding')
    preProcessScalingValues <- preProcess(train_weight_set %>% select(-number_of_hours), method = c("center", "scale"))
    
    scaled_train_subset <- predict(preProcessScalingValues, train_weight_set)
    scaled_test_subset <- predict(preProcessScalingValues, test_weight_set)
    
    tuningControl=trainControl(method = "cv", number = 3)
    knn_fit_for_weight <- train(number_of_hours~., scaled_train_subset, method='knn',tuneGrid=data.frame(k=c(34,41,48)),trControl=tuningControl)
    
    knn_fit_for_weight
    
    print(knn_fit_for_weight)
    
    predictions <- predict(knn_fit_for_weight,scaled_test_subset %>% select(-number_of_hours))
    
    RMSE(scaled_test_subset$number_of_hours,predictions) 
  })
  
  # selecting weight leading to lowest RMSE
  weight_to_use <- weights[which.min(weight_rmses)]
  paste('Weight to use: ', weight_to_use, ', corresponding RMSE: ', weight_rmses[which.min(weight_rmses)])
  
  print(weight_rmses)
  weight_to_use
})

weight_to_use_per_partition

weight_to_use <- mean(weight_to_use_per_partition) 
# 5

target_encoding_weights <- target_encoding_weights %>%
  add_row(effect='customer_state',weight=weight_to_use)


####################### Customer location(loc) effects
# Explore customer_loc relation with number_of_hours
set.seed(1, sample.kind = 'Rounding')
training_dataset %>% group_by(customer_loc) %>%
  summarise(mean=mean(number_of_hours),se=sd(number_of_hours)/sqrt(n())) %>%
  mutate(customer_loc=reorder(customer_loc,mean)) %>% 
  ggplot(aes(x=customer_loc,y=mean,ymax=mean+2*se,ymin=mean-2*se)) +
  geom_point()+ geom_errorbar() + theme(axis.text.x = element_blank()) + 
  xlab('Customer Locations') + ylab('Avg number of hours')

# Apply Target encoding for customer_loc
# here we will try to identify the regularization weight/smoothing factor to use by carrying out target encoding 
train_subset_for_encoding <- train_subset[1:40000,]
global_mean <- mean(train_subset_for_encoding$number_of_hours)

set.seed(1,sample.kind='Rounding')
test_weight_indices <- createDataPartition(train_subset_for_encoding$number_of_hours,p=0.2,times=3,list=FALSE)

weights <- seq(0,10,5)

weight_to_use_per_partition <- sapply(1:3, function(index) {
  train_weight_set <- train_subset_for_encoding[-test_weight_indices[,index],]
  test_weight_set <- train_subset_for_encoding[test_weight_indices[,index],]
  
  test_weight_set <- test_weight_set %>%
    semi_join(train_weight_set,by='product_category_name') %>%
    semi_join(train_weight_set,by='seller_id') %>% 
    semi_join(train_weight_set,by='customer_zip_code_prefix') %>% 
    semi_join(train_weight_set,by='customer_state') %>% 
    semi_join(train_weight_set,by='customer_loc')
  
  ## adding derived features and target encoded categorical features
  dataset_list <- enhance_datasets_with_derived_features(train_weight_set,test_weight_set,FALSE)
  train_weight_set <- dataset_list[[1]]
  test_weight_set <- dataset_list[[2]]
  
  weight_to_use <- target_encoding_weights %>% 
    filter(effect=='product_category') %>% pull(weight)
  
  product_category_effects <- prepare_product_category_effect(train_weight_set,weight_to_use)
  dataset_list <- enhance_datasets_using_product_category_effect(product_category_effects,train_weight_set,test_weight_set,FALSE)
  
  train_weight_set <- dataset_list[[1]] %>% select(-order_purchase_timestamp,-handed_over_to_carrier_date,-shipping_limit_date,-customer_zip_code_prefix,-payment_approved_at,-product_category_name)
  test_weight_set <- dataset_list[[2]] %>% select(-order_purchase_timestamp,-handed_over_to_carrier_date,-shipping_limit_date,-customer_zip_code_prefix,-payment_approved_at,-product_category_name)
  
  weight_to_use <- target_encoding_weights %>% filter(effect=='seller_id') %>% pull(weight)
  
  seller_id_effects <- prepare_seller_id_effect(train_weight_set,weight_to_use)
  dataset_list <- enhance_datasets_using_seller_id_effect(seller_id_effects,train_weight_set,test_weight_set,FALSE)
  
  train_weight_set <- dataset_list[[1]] %>% select(-seller_id)
  test_weight_set <- dataset_list[[2]] %>% select(-seller_id)
  
  weight_to_use <- target_encoding_weights %>% filter(effect=='customer_state') %>% pull(weight)
  
  customer_state_effects <- prepare_customer_state_effect(train_weight_set,weight_to_use)
  dataset_list <- enhance_datasets_using_customer_state_effect(customer_state_effects,train_weight_set,test_weight_set,TRUE)
  
  train_weight_set <- dataset_list[[1]]
  test_weight_set <- dataset_list[[2]]
  ##
  
  # calculating rmse for each weight
  weight_rmses <- sapply(weights, function(weight) {
    customer_loc_effects <- train_weight_set %>% 
      group_by(customer_loc) %>% 
      summarise(mean=mean(number_of_hours),n=n()) %>%
      mutate(mean=(n*mean+weight*global_mean)/(n+weight)) %>% 
      select(-n) %>% rename()
    
    train_weight_set <- train_weight_set %>% left_join(customer_loc_effects,by='customer_loc') %>% rename(customer_loc_effect=mean) %>% select(-customer_loc) #-product_category_name
    
    test_weight_set <- test_weight_set %>% left_join(customer_loc_effects,by='customer_loc') %>% rename(customer_loc_effect=mean) %>% select(-customer_loc) # -product_category_name,
    
    set.seed(seed=1,sample.kind = 'Rounding')
    preProcessScalingValues <- preProcess(train_weight_set %>% select(-number_of_hours), method = c("center", "scale"))
    
    scaled_train_subset <- predict(preProcessScalingValues, train_weight_set)
    scaled_test_subset <- predict(preProcessScalingValues, test_weight_set)
    
    tuningControl=trainControl(method = "cv", number = 3)
    knn_fit_for_weight <- train(number_of_hours~., scaled_train_subset, method='knn',tuneGrid=data.frame(k=c(35,43,49)),trControl=tuningControl)
    
    knn_fit_for_weight
    
    print(knn_fit_for_weight)
    
    predictions <- predict(knn_fit_for_weight,scaled_test_subset %>% select(-number_of_hours))
    
    RMSE(scaled_test_subset$number_of_hours,predictions) 
  })
  
  # selecting weight leading to lowest RMSE
  weight_to_use <- weights[which.min(weight_rmses)]
  paste('Weight to use: ', weight_to_use, ', corresponding RMSE: ', weight_rmses[which.min(weight_rmses)])
  
  print(weight_rmses)
  weight_to_use
})

weight_to_use_per_partition

weight_to_use <- mean(weight_to_use_per_partition) 
# 20/3

target_encoding_weights <- target_encoding_weights %>% 
  add_row(effect='customer_loc',weight=weight_to_use)

# save target encoding weights
save(target_encoding_weights,file='rda/target_encoding_weights.rda')

# required only to generate report or resume modeling quickly
load('rda/target_encoding_weights.rda')


####################### Price feature
# Explore price relation with number_of_hours
training_dataset  %>% ggplot(aes(price,number_of_hours)) + geom_smooth() + 
  xlab('Price of ordered item') + ylab('Number of hours taken for delivery')

#######################  Product weight feature
# Explore product weight relation with number_of_hours
training_dataset  %>% ggplot(aes(product_weight_g,log10(number_of_hours))) + geom_point() + 
  geom_smooth() + xlab('Weight of ordered product in grams') + 
  ylab('Number of hours taken for delivery [log10(n) scale]')

#######################  Freight value feature
# Explore freight value relation with number_of_hours
training_dataset  %>% ggplot(aes(freight_value,log10(number_of_hours))) + geom_point() + 
  geom_smooth() + xlab('Freight value of ordered product') + 
  ylab('Number of hours taken for delivery [log10(n) scale]')


#######################################################################################
################################## Model Selection ####################################
#######################################################################################

# Prepare training and test data for model tuning and development
dataset_list <- prepare_data_for_training_testing(train_subset,test_subset,FALSE)
train_subset <- dataset_list[[1]]
test_subset <- dataset_list[[2]]

# cleanup
train_subset <- train_subset %>%
  select(-handed_over_to_carrier_date,-shipping_limit_date,-product_category_name,
         -seller_id,-customer_state,-customer_loc,-customer_zip_code_prefix,
         -order_purchase_timestamp,-payment_approved_at)
test_subset <- test_subset %>%
  select(-handed_over_to_carrier_date,-shipping_limit_date,-product_category_name,
         -seller_id,-customer_state,-customer_loc,-customer_zip_code_prefix,
         -order_purchase_timestamp,-payment_approved_at)

# Final set of features identified
train_subset %>% colnames() %>% knitr::kable(col.names=c('Feature'),caption='Features List')


####################### k-nearest neighbors(knn) model
# knn model with data scaled under test
set.seed(seed=1,sample.kind = 'Rounding')
preProcessScalingValues <- preProcess(train_subset %>% select(-number_of_hours), method = c("center", "scale"))

scaled_train_subset <- predict(preProcessScalingValues, train_subset) 
scaled_test_subset <- predict(preProcessScalingValues, test_subset) 

tuningControl=trainControl(method = "cv", number = 5)
knn_fit <- train(number_of_hours~., scaled_train_subset, method='knn',tuneGrid=data.frame(k=c(45,71,77,83)),trControl=tuningControl)

save(knn_fit,file='rda/knn_model.rda')

# trained knn model based on train_subset
load('rda/knn_model.rda')
knn_fit

knn_fit %>% ggplot()

# trained knn model validation
predictions <- predict(knn_fit,scaled_test_subset %>% select(-number_of_hours))

performance_metric <- performance_metric %>% 
  add_row(type='Knn model(partial dataset)',
          value=RMSE((scaled_test_subset %>% pull(number_of_hours)),predictions))

performance_metric
#183.0028 for k=77

save(performance_metric,file='rda/performance_metric.rda')

# performance metric for knn model based on test data
load('rda/performance_metric.rda')
performance_metric[1:3,] %>% knitr::kable(caption='Performance Metric(RMSE) Summary')


####################### Random forest model
# rf model with train_subset data
set.seed(seed=1,sample.kind = 'Rounding')
tuningControl=trainControl(method = "cv", number = 4)
rf_fit <- train(number_of_hours~., train_subset, method='rf',tuneGrid = data.frame(mtry = c(1,2,3,4,5)),trControl=tuningControl,ntree=200)

save(rf_fit,file='rda/rf_model.rda')

# trained random forest model based on train_subset
load('rda/rf_model.rda')
rf_fit

rf_fit %>% ggplot()

# trained random forest model validation
predictions <- predict(rf_fit,test_subset %>% select(-number_of_hours))

performance_metric <- performance_metric %>% 
  add_row(type='Random forest model(partial dataset)',
          value=RMSE((test_subset %>% pull(number_of_hours)),predictions))

performance_metric
# 173.3481 for mtry 2. There are total of 11 predictors.

# performance metric for random forest model based on test data
load('rda/performance_metric.rda')
performance_metric[1:4,] %>% knitr::kable(caption='Performance Metric(RMSE) Summary')


#######################  Average based ensemble model based on knn and random forest models
# Average based Ensemble of knn and rf model using partial data
load('rda/knn_model.rda')
load('rda/rf_model.rda')
set.seed(1, sample.kind = 'Rounding')

predictions_knn <- predict(knn_fit,scaled_test_subset %>% select(-number_of_hours))
predictions_rf <- predict(rf_fit,test_subset %>% select(-number_of_hours))

predictions_df <- data.frame(predictions_knn,predictions_rf)

# mean approach
performance_metric <- performance_metric %>% 
  add_row(type='Avg based ensemble model(partial dataset)',
          value=RMSE(rowMeans(predictions_df),(test_subset %>% pull(number_of_hours) )))
#175.7183


save(performance_metric,file='rda/performance_metric.rda')

# performance metric for ensemble model based on test data
load('rda/performance_metric.rda')
performance_metric[1:5,] %>% knitr::kable(caption='Performance Metric(RMSE) Summary')

####################### Weighted average based ensemble model
# weighted average ensemble approach with partial dataset
# weighted approach
rmses_weighted_ensemble_model <- sapply(seq(0,1,0.15),function(a) {
  RMSE((test_subset %>% pull(number_of_hours)),
       predictions_df %>% mutate(value=predictions_knn*a+predictions_rf*(1-a)) %>% 
         pull(value)
  )
})
save(rmses_weighted_ensemble_model,file='rda/rmses_weighted_ensemble_model.rda')

# Summary of performance metric for weighted ensemble approach
load('rda/rmses_weighted_ensemble_model.rda')
weights_for_ensemble_model <- seq(0,1,0.15)

data.frame(weight_for_knn_pred=weights_for_ensemble_model,rmse=rmses_weighted_ensemble_model) %>%
  ggplot(aes(weight_for_knn_pred,rmse)) + geom_line() + geom_point() + 
  xlab('Weight used(for knn prediction)') + ylab('Resultant RMSE')

#######################################################################################
##################################### Final Model #####################################
#######################################################################################

# Tuning parameters to use for final random forest
load('rda/rf_model.rda')

data.frame(tuning_parameter='mtry', value=rf_fit$finalModel$mtry) %>% 
  add_row(tuning_parameter='number_of_trees', value=500) %>% 
  knitr::kable(caption='Random forest tuning parameters')

# Prepare dataset for final model training, include = FALSE}
set.seed(seed=1,sample.kind = 'Rounding')

validation_dataset <- validation_dataset %>% 
  compute_volume_using_product_info() %>% 
  extract_distance_between_seller_customer() %>% 
  compute_rough_customer_location()

dataset_list <- prepare_data_for_training_testing(training_dataset,validation_dataset,FALSE)
training_dataset <- dataset_list[[1]]
validation_dataset <- dataset_list[[2]]

# cleanup
training_dataset <- training_dataset %>%
  select(-handed_over_to_carrier_date,-shipping_limit_date,-product_category_name,-seller_id,
         -customer_state,-customer_loc,-customer_zip_code_prefix,-order_purchase_timestamp,
         -payment_approved_at)
validation_dataset <- validation_dataset %>%
  select(-handed_over_to_carrier_date,-shipping_limit_date,-product_category_name,-seller_id,
         -customer_state,-customer_loc,-customer_zip_code_prefix,-order_purchase_timestamp,
         -payment_approved_at)

# Final Model Training
set.seed(seed=1,sample.kind = 'Rounding')

mtry_to_use <- rf_fit$finalModel$mtry
ntree_to_use <- 500 

final_model <- randomForest(number_of_hours~., training_dataset,mtry= mtry_to_use, ntree=ntree_to_use)

save(final_model,file='rda/final_model.rda')


# trained final random forest model based on training_dataset
load('rda/final_model.rda')
#final_model

plot(final_model,main='Random forest model error against number of trees')

# Final model validation
predictions <- predict(final_model,validation_dataset %>% select(-number_of_hours))

performance_metric <- performance_metric %>% 
  add_row(type='Final model',
          value=RMSE((validation_dataset %>% pull(number_of_hours) ),predictions))
# 188.5793 for mtry=2, ntree=500

performance_metric


# Performance metric post final model validation
performance_metric %>% knitr::kable(caption='Performance Metric(RMSE) Summary')



