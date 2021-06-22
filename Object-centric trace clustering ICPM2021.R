library(rjson)
library(tidyverse)
library(tidyjson)
library(reprex)
library(proxy)
library(bupaR)
library(lubridate)
library(cluster)
library(expm)

##### Stage 1: Obtain marginal event logs #####

#Load the original file, which follows the ocel standard
ocel <- fromJSON(file="running-example.jsonocel")
# Extract the event information & cast it as a tibble. Because the unnest command that will be used next requires a tibble column, we need this casting, as well as adding an ID column
events.lst<- ocel[["ocel:events"]]
events.tbl<- tibble(eID = names(events.lst), json = events.lst)

# Tranforming the ocel format into a table (tidy) format. Unnest_wider creates new columns and unnest_longer created new rows
log.events<- events.tbl %>% 
  unnest_wider(json) %>% 
  unnest_wider(`ocel:vmap`) %>% 
  unnest_longer(`ocel:omap`) %>%
  rename_all(make.names)

#Extract the objects information and transform it to a tidy table
obj.lst<- ocel[["ocel:objects"]]
obj.tbl<- tibble(oID = names(obj.lst), json = obj.lst)
obj.info <- obj.tbl %>% 
  unnest_wider(json)  %>% 
  unnest_wider(`ocel:ovmap`) %>%
  rename_all(make.names)

# Join the information of the two tibbles
log<- log.events %>% 
  left_join(obj.info, by = c("ocel.omap"= "oID"))

# set the number of objects
n=5

# Create the dataset for the object customers
log.customers<- log %>% filter(ocel.type =="customers") %>% select(-cost, -producer, -ocel.type)

# Create the dataset for the object items
log.items<- log %>% filter(ocel.type =="items") %>% select( -cost, -producer, -age, -bankaccount, -ocel.type)

# Create the dataset for the object orders
log.orders<- log %>% filter(ocel.type=="orders") %>% select( -cost, -producer, -age, -bankaccount, -ocel.type) %>%
  mutate(timestamp = ymd_hms(ocel.timestamp))

# Create the dataset for the object packages
log.packages<- log %>% filter(ocel.type =="packages") %>% select( -cost, -producer, -age, -bankaccount, -ocel.type)

# Create the dataset for the object products
log.products<- log %>% filter(ocel.type =="products")%>% select( -price, -weight, -age, -bankaccount, -ocel.type)

##### Stage 2: Similarity matrices ####
### CUSTOMERS ###
# Create the feature matrix for the object customers
profile.customers <- log.customers %>%
  group_by(ocel.omap) %>%
  summarise(num.events = n(),
            num.orders = sum(ocel.activity =="place order"),
            avg.weight = mean(unique(weight)),
            avg.price = mean(unique(price)),
            age = first(age)) 

# Create the similarity matrix for the object customers
simil.customers<- profile.customers %>%
  remove_rownames() %>%
  column_to_rownames(var = "ocel.omap") %>%
  simil() %>% # The actual command to construct the similarity matrix (proxy packg)
  as.matrix(diag=1)

### ITEMS ####
# Create the feature matrix for the object items
profile.items<- log.items %>%
  group_by(eID) %>%
  mutate(num.Items = n()) %>% ungroup() %>% 
  group_by(ocel.omap) %>%
  summarise(num.events = n(),
            with.items = first(num.Items[ocel.activity =="place order"]),
            weight = first(weight[ocel.activity=="pick item"]),
            price = first(price[ocel.activity=="pick item"]))

# Create the similarity matrix for the object items
simil.items<- profile.items %>%
  remove_rownames() %>%
  column_to_rownames(var = "ocel.omap") %>%
  simil() %>% 
  as.matrix(diag=1)  

### ORDERS ####

# Create an event log for BupaR
event.log.orders<- log.orders %>%
  mutate(Status = "complete",activity_instance = 1:nrow(.), resource = NA ) %>%
  eventlog(
    case_id = "ocel.omap",
    activity_id = "ocel.activity",
    activity_instance_id = "activity_instance",
    lifecycle_id = "Status",
    timestamp = "timestamp",
    resource_id = "resource"
  )

# To create the similarity matrix for order we follow the approach of "P.  Delias,  M.  Doumpos,  E.  Grigoroudis,  and  N.  Mat-satsinis,  “A  non-compensatory  approach  for  trace  clus-tering,”International  Transactions  in  Operational  Re-search, vol. 26, no. 5, pp. 1828–1846, feb 2017."

## Calculate the criteria ##
#--- Common Activities --#
activities.tbl<- event.log.orders %>% 
  group_by(ocel.omap, ocel.activity)%>% 
  summarise(N=n())%>% # Get the frequency of every activity per case
  pivot_wider(names_from = ocel.activity, values_from = N, values_fill = 0) %>%#Transform in wide format (activities be the columns)
  ungroup()

#Cosine Similarity for Activities
library(qlcMatrix)
activitiesT<- activities.tbl %>%
  select(-ocel.omap)%>%
  t()

#Coerce as a sparse Matrix
aSim<- activitiesT %>%
  as("dgCMatrix") %>% # Sparse activitiesT
  cosSparse() %>%    # Cosine similarity in sparse matrix
  `rownames<-`(activities.tbl$ocel.omap)

summary(as.vector(aSim))
hist(as.vector(aSim),breaks=10)
quantile(as.vector(aSim),0.15)

#--- Common Transitions --#

#? IF CODES ARE NEEDED ?#
# Create an additional column to replace event names with string codes (because event names are long)
event.Classes<-as.character(unique(log.orders$ocel.activity))
library(stringi)
set.seed(101)
codes<-stri_rand_strings(length(event.Classes), 1,pattern = "[A-Z]")

#Check if there are any duplicates in codes
sum(stri_duplicated(codes))>0

log.orders<- log.orders %>%
  mutate(Code = recode(ocel.activity, !!! setNames(codes, event.Classes)))

traces <- log.orders %>%
  arrange(ocel.omap, timestamp) %>%
  group_by(ocel.omap)%>%
  summarise(Trace = paste(Code, collapse=""))

# Edit Distance #
library(stringdist)
#Calculate edit distance using The Optimal String Alignment distance (osa) is like the 
#Levenshtein distance but also allows transposition of adjacent characters. 
Levenshtein<-stringdistmatrix(traces$Trace,traces$Trace, method="osa")
#just put names on rows/cols
rownames(Levenshtein)<-traces$ocel.omap
colnames(Levenshtein)<-traces$ocel.omap

# similarity  (distance) of prices #
log.orders.cases<- log.orders %>%
  group_by(ocel.omap)%>% 
  slice(1)

#--- Additional criteria: Objects attributes --#
price.dist<- log.orders.cases %>%
  transmute(priceBin = cut(price, breaks = c(-Inf,500,1000,3000,5000,Inf), labels =c(1,2,3,4,5)))%>% # Discretize price
  pull(priceBin) %>% # tbl to vector
  as.numeric() %>%
  dist(method="manhattan") %>%
  as.matrix() %>%
  `rownames<-`(log.orders.cases$ocel.omap) %>%
  `colnames<-`(log.orders.cases$ocel.omap)

# similarity  (distance) of weight #
weight.dist<- log.orders.cases %>%
  transmute(weightBin = cut(weight, breaks = c(-Inf,0.5,1,3,5,Inf), labels =c(1,2,3,4,5)))%>% # Discretize weight
  pull(weightBin) %>% # tbl to vector
  as.numeric() %>%
  dist(method="manhattan") %>%
  as.matrix() %>%
  `rownames<-`(log.orders.cases$ocel.omap) %>%
  `colnames<-`(log.orders.cases$ocel.omap)


## partial indices for criteria ##

Activities.concordance<- apply(aSim,1:2, function(x) concordance.partial(x,0.9,0.95))
Activities.discordance<-apply(aSim,1:2, function(x) discordance.partial(x,0.9,0.7))

Transitions.concordance<- apply(Levenshtein,1:2, function(x) concordance.partial(x,10,7,increasing = F))
Transitions.discordance<-apply(aSim,1:2, function(x) discordance.partial(x,10,15, increasing = F))

Price.concordance<-apply(price.dist,1:2,function(x) concordance.partial(x,2,1, increasing = F))
Price.discordance<-apply(price.dist,1:2, function(x) discordance.partial(x,2,3, increasing = F))

Weight.concordance<-apply(weight.dist,1:2,function(x) concordance.partial(x,2,1, increasing = F))
Weight.discordance<-apply(weight.dist,1:2, function(x) discordance.partial(x,2,3, increasing = F))

## Aggregation ##
#---Concordance---#
concordance.Weighted<-list(0.3*Activities.concordance,0.3*Transitions.concordance,0.2*Price.concordance,0.2*Weight.concordance)

Concordance <-Reduce('+',concordance.Weighted)

#---Discordance---#
#For each criterion calculate the (1-d)/(1-c) matrix
Activities.Div <- (1-Activities.discordance)/(1-Activities.concordance)
Transitions.Div <- (1-Transitions.discordance)/(1-Transitions.concordance)
Price.Div <- (1-Price.discordance)/(1-Price.concordance)
Weight.Div <- (1-Weight.discordance)/(1-Weight.concordance)

#Check if discordance is greater that concordance for every criterion
#If not, put 1 in the Div matrix, to not affect the multiplication
Activities.IsDisc<- Activities.discordance-Activities.concordance
Activities.Div[Activities.IsDisc<=0]<-1

Transitions.IsDisc<- Transitions.discordance-Transitions.concordance
Transitions.Div[Transitions.IsDisc<=0]<-1

Price.IsDisc<- Price.discordance-Price.concordance
Price.Div[Price.IsDisc<=0]<-1

Weight.IsDisc<- Weight.discordance-Weight.concordance
Weight.Div[Weight.IsDisc<=0]<-1

discordance.aggregation <- list(Activities.Div,Transitions.Div,Price.Div,Weight.Div)
Discordance <- 1- Reduce('*',discordance.aggregation)

#---final aggregation---#
S <- pmin(Concordance,1-Discordance)

### PACKAGES ####
# Create the feature matrix for the object packages
profile.packages<- log.packages %>%
  group_by(ocel.omap) %>%
  summarise(num.events = n(),
            contains.fail = sum(ocel.activity =="failed delivery"),
            weight = first(weight),
            price = first(price))

# Create the similarity matrix for the object items
simil.packages<- profile.packages %>%
  remove_rownames() %>%
  column_to_rownames(var = "ocel.omap") %>%
  simil() %>% 
  as.matrix(diag=1)  

### PRODUCTS ####
# Create the feature matrix for the object products
profile.products<- log.products %>%
  group_by(eID) %>%
  mutate(num.Products = n()) %>%  
  mutate(num.Producers = n_distinct(producer)) %>% ungroup() %>% 
  group_by(ocel.omap) %>%
  summarise(avg.with.products = mean(num.Products[ocel.activity =="place order"]),
            avg.with.producers = mean(num.Producers[ocel.activity =="place order"]),
            in.orders = sum(ocel.activity =="place order"),
            #not.confirmed = in.orders - sum(ocel.activity =="confirm order"), # All 0
            cost = first(cost),
            producer = first(producer)) %>%
  mutate(avg.with.products = normalize(avg.with.products),
         avg.with.producers = normalize(avg.with.producers),
         in.orders = normalize(in.orders),
         cost = normalize(cost),
         producer = as.factor(producer)) %>%
  remove_rownames() %>%
  column_to_rownames(var="ocel.omap")


similarity.products = 1 - as.matrix(daisy(profile.products, metric="gower"))

lst.similarities<- list(Orders = S, Customers = simil.customers, Items = simil.items, Packages = simil.packages, Products=similarity.products)




#### Stege 3: Cross-object dependency matrices  ####

cross.orders.items<- log.items %>%
  select(- price, -weight, -ocel.activity, -ocel.timestamp) %>%
  pivot_wider(names_from = ocel.omap, values_from = ocel.omap, values_fn = list(ocel.omap = ~1), values_fill = list(ocel.omap = 0)) %>%
  right_join(log.orders.cases, by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -timestamp,-Code,-eID,) %>% # -priceBin, -weightBin, ) %>%
  column_to_rownames(var="ocel.omap")

cross.orders.products<- log.products %>%
  select(-ocel.activity, -ocel.timestamp, -cost, -producer) %>%
  pivot_wider(names_from = ocel.omap, values_from = ocel.omap, values_fn = list(ocel.omap = ~1), values_fill = list(ocel.omap = 0)) %>%
  right_join(log.orders.cases, by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -timestamp,-Code,-eID,) %>% # -priceBin, -weightBin, ) %>%
  column_to_rownames(var="ocel.omap")

cross.orders.packages<- log.packages %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price) %>%
  left_join(distinct(log.orders, eID, ocel.omap, .keep_all=T) , by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -timestamp, -Code, -eID) %>%
  rename(packageID = ocel.omap.x, orderID = ocel.omap.y) %>%
  distinct(packageID, orderID) %>%
  pivot_wider(names_from = packageID, values_from = packageID, values_fn = list(packageID = ~1), values_fill = list(packageID = 0)) %>%
  column_to_rownames(var="orderID")

cross.orders.customers<- log.customers %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -age, -bankaccount) %>%
  left_join(distinct(log.orders, eID, ocel.omap, .keep_all=T) , by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -timestamp, -Code, -eID) %>%
  rename(customerID = ocel.omap.x, orderID = ocel.omap.y) %>%
  distinct(customerID, orderID) %>%
  pivot_wider(names_from = customerID, values_from = customerID, values_fn = list(customerID = ~1), values_fill = list(customerID = 0)) %>%
  column_to_rownames(var="orderID")

cross.items.customers<- log.customers %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -age, -bankaccount)%>%
  left_join(distinct(log.items, eID, ocel.omap, .keep_all=T) , by= "eID")%>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price,  -eID) %>%
  rename(customerID = ocel.omap.x, itemID = ocel.omap.y)%>%
  distinct(customerID, itemID)%>%
  pivot_wider(names_from = customerID, values_from = customerID, values_fn = list(customerID = ~1), values_fill = list(customerID = 0))%>%
  column_to_rownames(var="itemID")

cross.items.packages<- log.packages %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price)%>%
  left_join(distinct(log.items, eID, ocel.omap, .keep_all=T) , by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -eID) %>%
  rename(packageID = ocel.omap.x, itemID = ocel.omap.y) %>%
  distinct(packageID, itemID) %>%
  pivot_wider(names_from = packageID, values_from = packageID, values_fn = list(packageID = ~1), values_fill = list(packageID = 0)) %>%
  column_to_rownames(var="itemID")

cross.items.products <- log.products %>%
  select(-ocel.activity, -ocel.timestamp, -cost, -producer) %>%
  left_join(distinct(log.items, eID, ocel.omap, .keep_all=T) , by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -eID) %>%
  rename(productID = ocel.omap.x, itemID = ocel.omap.y) %>%
  distinct(productID, itemID) %>%
  pivot_wider(names_from = productID, values_from = productID, values_fn = list(productID = ~1), values_fill = list(productID = 0)) %>%
  column_to_rownames(var="itemID")

cross.products.packages<- log.packages %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price)%>%
  left_join(distinct(log.products, eID, ocel.omap, .keep_all=T) , by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -cost, -producer,-eID) %>%
  rename(packageID = ocel.omap.x, productID = ocel.omap.y) %>%
  distinct(packageID, productID) %>%
  pivot_wider(names_from = packageID, values_from = packageID, values_fn = list(packageID = ~1), values_fill = list(packageID = 0)) %>%
  column_to_rownames(var="productID")

cross.products.customers<- log.customers %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -age, -bankaccount)%>%
  left_join(distinct(log.products, eID, ocel.omap, .keep_all=T) , by= "eID")%>%
  select(-ocel.activity, -ocel.timestamp, -cost, -producer,  -eID) %>%
  rename(customerID = ocel.omap.x, productID = ocel.omap.y)%>%
  distinct(customerID, productID)%>%
  pivot_wider(names_from = customerID, values_from = customerID, values_fn = list(customerID = ~1), values_fill = list(customerID = 0))%>%
  column_to_rownames(var="productID")

cross.packages.customers<- log.packages %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price) %>%
  distinct (ocel.omap, .keep_all = T ) %>%
  left_join(log.customers, by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -age, -bankaccount, -eID)%>%
  rename(packageID = ocel.omap.x, customerID = ocel.omap.y) %>%
  pivot_wider(names_from = customerID, values_from = customerID, values_fn = list(customerID = ~1), values_fill = list(customerID = 0))%>%
  column_to_rownames(var="packageID")


#### Stage 4: Get embeddings #####
library(rstiefel)

# Initialize embeddings with an Orthonormal matrix
init.embedding.orders<- rustiefel(nrow(cross.orders.customers),5) # n_i * d_i
init.embedding.customers<- rustiefel(ncol(cross.orders.customers),5)
init.embedding.items<- rustiefel(nrow(cross.items.customers),5)
init.embedding.packages<- rustiefel(nrow(cross.packages.customers),5)
init.embedding.products<- rustiefel(nrow(cross.products.customers),5)

lst.init.embeddings<- list(init.embedding.orders,
                           init.embedding.customers,
                           init.embedding.items,
                           init.embedding.packages,
                           init.embedding.products)


## STEP 0
objective<- calculate.objective(lst.init.embeddings)

## STEP 1
new.embeddings <- calculate.new.embeddings(lst.init.embeddings,lst.similarities)
objective<- append(objective, calculate.objective(new.embeddings))
convergence<- tail(objective/lag(objective),1)

## NEXT STEPS
new.embeddings <- calculate.new.embeddings(new.embeddings,lst.similarities)
objective<- append(objective, calculate.objective(new.embeddings))
convergence<- tail(objective/lag(objective),1)


### Calculate the layer-layer embedding interaction matrices K_ij = M'i * R_ij * Mj
# To create all the combinations, we will apply purrr:cross3(). So, we need to create the input lists, f
# for the time being, manually...
# R_ij in a list - Input per column
lst.cross.dependencies<- list( ##ORDERS
  matrix(rep(1,nrow(cross.orders.customers)^2), nrow=nrow(cross.orders.customers)),
  unname(t(as.matrix(cross.orders.customers)),force = T),
  unname(t(as.matrix(cross.orders.items)),force = T),
  unname(t(as.matrix(cross.orders.packages)),force = T),
  unname(t(as.matrix(cross.orders.products)),force = T),
  ## CUSTOMERS
  unname(as.matrix(cross.orders.customers),force = T),
  matrix(rep(1,ncol(cross.orders.customers)^2), nrow=ncol(cross.orders.customers)),
  unname(as.matrix(cross.items.customers),force = T),
  unname(as.matrix(cross.packages.customers),force = T),
  unname(as.matrix(cross.products.customers),force = T),
  ## ITEMS
  unname(as.matrix(cross.orders.items),force = T),
  unname(t(as.matrix(cross.items.customers)),force = T),
  matrix(rep(1,ncol(cross.orders.items)^2), nrow=ncol(cross.orders.items)),
  unname(t(as.matrix(cross.items.packages)),force = T),
  unname(t(as.matrix(cross.items.products)),force = T),
  ## PACKAGES
  unname(as.matrix(cross.orders.packages),force = T),
  unname(t(as.matrix(cross.packages.customers)),force = T),
  unname(as.matrix(cross.items.packages),force = T),
  matrix(rep(1,ncol(cross.orders.packages)^2), nrow=ncol(cross.orders.packages)),
  unname(as.matrix(cross.products.packages),force = T),
  ## PRODUCTS
  unname(as.matrix(cross.orders.products),force = T), 
  unname(t(as.matrix(cross.products.customers)),force = T),
  unname(as.matrix(cross.items.products),force = T),
  unname(t(as.matrix(cross.products.packages)),force = T),
  matrix(rep(1,ncol(cross.orders.products)^2), nrow=ncol(cross.orders.products))
)

# If n is the number of objects, then the positions in the list after the cross is 
#(0:(n-1)*(n * n^2 + n*n)+ 1 or (0:(n-1)*(n^3+n^2)+1) for the first row, and + n+1 for each subsequent row
positions<- c((0:(n-1)*(n^3+n^2)+1),
              (0:(n-1)*(n^3+n^2)+n+2),
              (0:(n-1)*(n^3+n^2)+(2*n)+3),
              (0:(n-1)*(n^3+n^2)+(3*n)+4),
              (0:(n-1)*(n^3+n^2)+(4*n)+5))


##### Stage 5: Clustering ####
emb.orders<- as_tibble(new.embeddings[[1]], .name_repair="unique")

clustering.orders<- kmeans(emb.orders, 6)
log.orders.cases$cluster<- clustering.orders$cluster

##### -- VALIDATION --  #####

#Initialize table to store the results
results<- tibble(
  Object = character(),
  Entropy = numeric(),
  Approach = character(),
  Method = character(),
  k = integer())

## Baseline - Perform hierarchical clustering on the similarity matrices for all objects

lst.clusters<- lst.similarities %>%
  map(~as.dist(1-.)) %>%
  map(hclust,method="centroid") %>%
  map(cutree, k=3) %>%
  map(stack) %>% # To keep the row names
  map(rename, ocel.omap=ind)

# Create a tibble where the clustering membership for both objects exists
## ORDERS - CUSTOMERS
cross.clusters.orders.customers<- log.customers %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -age, -bankaccount) %>%
  left_join(distinct(log.orders, eID, ocel.omap, .keep_all=T) , by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -timestamp, -Code, -eID) %>%
  rename(customerID = ocel.omap.x, orderID = ocel.omap.y) %>%
  distinct(customerID, orderID) %>%
  left_join(lst.clusters$Customers,by=c("customerID"="ocel.omap") ) %>%
  rename(customer.cl = values) %>%
  left_join(log.orders.cases, by=c("orderID"="ocel.omap")) %>%
  select(-(eID:Code)) %>%
  group_by(cluster, customer.cl)  %>% # We group to prepare for entropy
  count() %>%
  group_by(cluster)

## ORDERS - ITEMS
cross.clusters.orders.items<- log.items %>%
  select(- price, -weight, -ocel.activity, -ocel.timestamp) %>%
  right_join(log.orders.cases, by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -timestamp,-Code,-eID,) %>%
  rename(itemID=ocel.omap.x, orderID=ocel.omap.y) %>%
  left_join(lst.clusters$Items,by=c("itemID"="ocel.omap") ) %>%
  rename(item.cl = values)%>%
  group_by(cluster, item.cl) %>%
  count() %>%
  group_by(cluster)

## ORDERS - PACKAGES
cross.clusters.orders.packages<- log.packages %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price) %>%
  left_join(distinct(log.orders, eID, ocel.omap, .keep_all=T) , by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -timestamp, -Code, -eID) %>%
  rename(packageID = ocel.omap.x, orderID = ocel.omap.y) %>%
  distinct(packageID, orderID) %>%
  left_join(lst.clusters$Packages,by=c("packageID"="ocel.omap") ) %>%
  rename(package.cl = values) %>%
  left_join(log.orders.cases, by=c("orderID"="ocel.omap")) %>%
  select(-(eID:Code)) %>%
  group_by(cluster, package.cl) %>%
  count() %>%
  group_by(cluster)

## ORDERS - PRODUCTS
cross.clusters.orders.products<- log.products %>%
  select(-ocel.activity, -ocel.timestamp, -cost, -producer) %>%
  right_join(log.orders.cases, by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -timestamp,-Code,-eID,) %>% 
  rename(productID=ocel.omap.x, orderID=ocel.omap.y) %>%
  left_join(lst.clusters$Products,by=c("productID"="ocel.omap") ) %>%
  rename(product.cl = values)%>%
  group_by(cluster, product.cl) %>%
  count() %>%
  group_by(cluster)

## CROSS CLUSTER LIST
lst.cross.clusters.orders<- list(Customers = cross.clusters.orders.customers,
                                 Items = cross.clusters.orders.items,
                                 Packages = cross.clusters.orders.packages,
                                 Products = cross.clusters.orders.products)

results<- bind_rows(results,
                    map(lst.cross.clusters.orders,entropy,k=3)%>% 
                      map_dfr(~ .x %>% as_tibble(), .id = "name") %>% 
                      rename(Object = 1, Entropy = 2) %>%
                      mutate(Approach = "OC", Method="centroid", k=3))


# BASELINE
baseline.orders<- as.dist(1-S) %>% 
  hclust(method="centroid" ) %>%
  cutree(k=6) %>%
  stack %>%
  rename(ocel.omap=ind)

## ORDERS - CUSTOMERS
baseline.orders.customers<- log.customers %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -age, -bankaccount) %>%
  left_join(distinct(log.orders, eID, ocel.omap, .keep_all=T) , by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -timestamp, -Code, -eID) %>%
  rename(customerID = ocel.omap.x, orderID = ocel.omap.y) %>%
  distinct(customerID, orderID) %>%
  left_join(lst.clusters$Customers,by=c("customerID"="ocel.omap") ) %>%
  rename(customer.cl = values) %>%
  left_join(baseline.orders, by = c("orderID"="ocel.omap")) %>%
  rename(cluster=values) %>%
  group_by(cluster, customer.cl) %>%
  count() %>%
  group_by(cluster)

## ORDER -ITEMS  
baseline.orders.items<- log.items %>%
  select(- price, -weight, -ocel.activity, -ocel.timestamp) %>%
  right_join(log.orders.cases, by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -timestamp,-Code,-eID,-cluster) %>%
  rename(itemID=ocel.omap.x, orderID=ocel.omap.y) %>%
  left_join(lst.clusters$Items,by=c("itemID"="ocel.omap") ) %>%
  rename(item.cl = values) %>%
  left_join(baseline.orders, by = c("orderID"="ocel.omap")) %>%
  rename(cluster=values) %>%
  group_by(cluster, item.cl) %>%
  count() %>%
  group_by(cluster)

## ORDERS - PACKAGES
baseline.orders.packages<- log.packages %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price) %>%
  left_join(distinct(log.orders, eID, ocel.omap, .keep_all=T) , by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -timestamp, -Code, -eID) %>%
  rename(packageID = ocel.omap.x, orderID = ocel.omap.y) %>%
  distinct(packageID, orderID) %>%
  left_join(lst.clusters$Packages,by=c("packageID"="ocel.omap") ) %>%
  rename(package.cl = values) %>%
  left_join(baseline.orders, by = c("orderID"="ocel.omap")) %>%
  rename(cluster=values) %>%
  group_by(cluster, package.cl) %>%
  count() %>%
  group_by(cluster)

## ORDERS - PRODUCTS
baseline.orders.products<- log.products %>%
  select(-ocel.activity, -ocel.timestamp, -cost, -producer) %>%
  right_join(log.orders.cases, by= "eID") %>%
  select(-ocel.activity, -ocel.timestamp, -weight, -price, -timestamp,-Code,-eID,-cluster) %>% 
  rename(productID=ocel.omap.x, orderID=ocel.omap.y) %>%
  left_join(lst.clusters$Products,by=c("productID"="ocel.omap") ) %>%
  rename(product.cl = values)%>%
  left_join(baseline.orders, by = c("orderID"="ocel.omap")) %>%
  rename(cluster=values) %>%
  group_by(cluster, product.cl) %>%
  count() %>%
  group_by(cluster)


## CROSS CLUSTER LIST - BASELINE
lst.baseline.clusters.orders<- list(Customers = baseline.orders.customers,
                                    Items = baseline.orders.items,
                                    Packages = baseline.orders.packages,
                                    Products = baseline.orders.products)



results<- bind_rows(results,
                    map(lst.baseline.clusters.orders,entropy,k=3) %>%
                      map_dfr(~ .x %>% as_tibble(), .id = "name")%>%
                      rename(Object=1, Entropy = 2) %>%
                      mutate(Approach = "Baseline", Method="centroid", k=3))


##### Visualization results ####
library(ggthemes)
res<- results


## LOLLIPOP - DIFF of entropy
res %>%
  group_by(Approach, Method, k) %>%
  summarise(total_entr = sum(Entropy)) %>%
  pivot_wider(names_from=Approach, values_from = total_entr) %>%
  ungroup() %>%
  mutate(`Baseline - OC` = Baseline-OC, test = row_number()) %>%
  ggplot(aes(y=`Baseline - OC`, x=test)) +
  geom_segment( aes(x=test, xend=test, y=0, yend=`Baseline - OC`, colour = Method)) +
  geom_point(aes(colour=factor(k)), size=4)+
  guides(colour=guide_legend(ncol=2, title="Clusters      Method")) +
  theme_light()+ 
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank(),
        legend.title=element_text(size=9))+
  scale_color_brewer(palette = "Paired")



###### HELP FUNCTIONS ########
create.bupar.log<- function(log) {
  log %>%
    mutate(status = "complete",
           activity_instance = 1:nrow(.),
           resource = NA) %>%
    eventlog(
      case_id = "ocel.omap",
      activity_id = "ocel.activity",
      activity_instance_id = "activity_instance",
      lifecycle_id = "status",
      timestamp = "ocel.timestamp",
      resource_id = "resource"
    )
}

entropy <- function(target,k){
  # Hyunsoo Kim, Haesun Park, Sparse non-negative matrix factorizations via alternating non-negativity-constrained least squares for microarray data analysis, Bioinformatics, Volume 23, Issue 12, 15 June 2007, Pages 1495–1502, https://doi.org/10.1093/bioinformatics/btm134
  N = sum(target$n) # N = the number of objects in the central object type (Orders)
  target %>%        # n = the number of objects of the target object type in the marginal cluster
    mutate(cl.size = sum(n),             # cl.size = the cluster size for the central object type, 
           #freq= n/cl.size,
           entr = n*log2(n/cl.size)) %>%
    summarize(marg = sum(entr))%>%
    summarize(-sum(marg)/(N*log2(k)))
}


mat.multi.multiply <- function (...) Reduce("%*%", list(...)) # Help function

normalize<- function(x) {(x - min(x)) / (max(x) - min(x))}
exponent <- function(a, pow) (abs(a)^pow)*sign(a)

calculate.norm.adjacency<- function(Adj) {
  D<- diag(rowSums(Adj))
  Dminus<- exponent(D, -0.5)
  Dminus[is.nan(Dminus)]=0
  norm.Adjacency<- Dminus %*% Adj %*% Dminus
}

get.Embeddings.Orders<- function(lst.embeddings, lst.sim){
  Adj =lst.sim$Orders
  ## Calculate the normalized Adjacency MAtrix L = D^(-1/2) * Adjacency * D^(-1/2)
  norm.Adjacency<- calculate.norm.adjacency(Adj)
  
  ## We create a list for the cross dependencies matrices. This is special for the object "Orders"
  lst_crossings<- list(matrix(rep(1,nrow(cross.orders.customers)^2), nrow=nrow(cross.orders.customers)),
                       as.matrix(cross.orders.customers),
                       as.matrix(cross.orders.items),
                       as.matrix(cross.orders.packages),
                       as.matrix(cross.orders.products))
  
  ## The list of embeddings is an input argument
  
  ## We create the transposes
  list_t_embeddings<- map(lst.embeddings,t) 
  list_t_crossings<- map(lst_crossings,t)
  
  # We want to multiply  (algebraic matrix multiplication) the elements of the four lists
  lst_cross.products <-mapply(mat.multi.multiply, lst_crossings,
                              lst.embeddings, 
                              list_t_embeddings,
                              list_t_crossings, 
                              SIMPLIFY = FALSE)
  
  new.embeddings<- Reduce(`+`,lst_cross.products) %>%   # We want to sum the result of the cross products
    magrittr:: multiply_by(1) %>% # Assume a=1
    magrittr::add(norm.Adjacency) %>%
    eigen() %$% # Mind the different pipe operator
    vectors %>%
    .[,1:5]
}

get.Embeddings.Customers<- function(lst.embeddings, lst.sim){
  Adj = lst.sim$Customers
  
  ## Calculate the normalized Adjacency MAtrix L = D^(-1/2) * Adjacency * D^(-1/2)
  norm.Adjacency<- calculate.norm.adjacency(Adj)
  
  ## We create a list for the cross dependencies matrices. This is special for the object "Customers"
  lst_crossings<- list(t(as.matrix(cross.orders.customers)),
                       matrix(rep(1,ncol(cross.orders.customers)^2), nrow=ncol(cross.orders.customers)),
                       t(as.matrix(cross.items.customers)),
                       t(as.matrix(cross.packages.customers)),
                       t(as.matrix(cross.products.customers)))
  
  ## The list of embeddings is an input argument
  
  ## We create the transposes
  list_t_embeddings<- map(lst.embeddings,t) 
  list_t_crossings<- map(lst_crossings,t)
  
  # We want to multiply  (algebraic matrix multiplication) the elements of the four lists
  lst_cross.products <-mapply(mat.multi.multiply, lst_crossings,
                              lst.embeddings, 
                              list_t_embeddings,
                              list_t_crossings, 
                              SIMPLIFY = FALSE)
  
  new.embeddings<- Reduce(`+`,lst_cross.products) %>%   # We want to sum the result of the cross products
    magrittr:: multiply_by(1) %>% # Assume a=1
    magrittr::add(norm.Adjacency) %>%
    eigen() %$% # Mind the different pipe operator
    vectors %>%
    .[,1:5]
}

get.Embeddings.Items<- function(lst.embeddings, lst.sim){
  Adj =lst.sim$Items
  ## Calculate the normalized Adjacency MAtrix L = D^(-1/2) * Adjacency * D^(-1/2)
  norm.Adjacency<- calculate.norm.adjacency(Adj)
  print("norm.Adjacency done!")
  ## We create a list for the cross dependencies matrices. This is special for the object "Items"
  lst_crossings<- list(t(as.matrix(cross.orders.items)),
                       as.matrix(cross.items.customers),
                       matrix(rep(1,ncol(cross.orders.items)^2), nrow=ncol(cross.orders.items)),
                       as.matrix(cross.items.packages),
                       as.matrix(cross.items.products))
  
  print("lst_crossings done!")
  ## The list of embeddings is an input argument
  
  ## We create the transposes
  list_t_embeddings<- map(lst.embeddings,t) 
  list_t_crossings<- map(lst_crossings,t)
  
  # We want to multiply  (algebraic matrix multiplication) the elements of the four lists
  lst_cross.products <-mapply(mat.multi.multiply, lst_crossings,
                              lst.embeddings, 
                              list_t_embeddings,
                              list_t_crossings, 
                              SIMPLIFY = FALSE)
  
  print("lst_cross.products done!")
  
  new.embeddings<- Reduce(`+`,lst_cross.products) %>%   # We want to sum the result of the cross products
    magrittr:: multiply_by(1) %>% # Assume a=1
    magrittr::add(norm.Adjacency) %>%
    eigen() %$% # Mind the different pipe operator
    vectors %>%
    .[,1:5]
}

get.Embeddings.Packages<- function(lst.embeddings, lst.sim){
  Adj =lst.sim$Packages
  ## Calculate the normalized Adjacency MAtrix L = D^(-1/2) * Adjacency * D^(-1/2)
  norm.Adjacency<- calculate.norm.adjacency(Adj)
  
  ## We create a list for the cross dependencies matrices. This is special for the object "Packages"
  lst_crossings<- list(t(as.matrix(cross.orders.packages)),
                       as.matrix(cross.packages.customers),
                       t(as.matrix(cross.items.packages)),
                       matrix(rep(1,ncol(cross.orders.packages)^2), nrow=ncol(cross.orders.packages)),
                       t(as.matrix(cross.products.packages)))
  
  ## The list of embeddings is an input argument
  
  ## We create the transposes
  list_t_embeddings<- map(lst.embeddings,t) 
  list_t_crossings<- map(lst_crossings,t)
  
  # We want to multiply  (algebraic matrix multiplication) the elements of the four lists
  lst_cross.products <-mapply(mat.multi.multiply, lst_crossings,
                              lst.embeddings, 
                              list_t_embeddings,
                              list_t_crossings, 
                              SIMPLIFY = FALSE)
  
  new.embeddings<- Reduce(`+`,lst_cross.products) %>%   # We want to sum the result of the cross products
    magrittr:: multiply_by(1) %>% # Assume a=1
    magrittr::add(norm.Adjacency) %>%
    eigen() %$% # Mind the different pipe operator
    vectors %>%
    .[,1:5]
}

get.Embeddings.Products<- function(lst.embeddings, lst.sim){
  Adj =lst.sim$Products
  ## Calculate the normalized Adjacency MAtrix L = D^(-1/2) * Adjacency * D^(-1/2)
  norm.Adjacency<- calculate.norm.adjacency(Adj)
  
  ## We create a list for the cross dependencies matrices. This is special for the object "Products"
  lst_crossings<- list(t(as.matrix(cross.orders.products)),
                       as.matrix(cross.products.customers),
                       t(as.matrix(cross.items.products)),
                       as.matrix(cross.products.packages),
                       matrix(rep(1,ncol(cross.orders.products)^2), nrow=ncol(cross.orders.products)))
  
  ## The list of embeddings is an input argument
  
  ## We create the transposes
  list_t_embeddings<- map(lst.embeddings,t) 
  list_t_crossings<- map(lst_crossings,t)
  
  # We want to multiply  (algebraic matrix multiplication) the elements of the four lists
  lst_cross.products <-mapply(mat.multi.multiply, lst_crossings,
                              lst.embeddings, 
                              list_t_embeddings,
                              list_t_crossings, 
                              SIMPLIFY = FALSE)
  
  new.embeddings<- Reduce(`+`,lst_cross.products) %>%   # We want to sum the result of the cross products
    magrittr:: multiply_by(1) %>% # Assume a=1
    magrittr::add(norm.Adjacency) %>%
    eigen() %$% # Mind the different pipe operator
    vectors %>%
    .[,1:5]
}

calculate.objective<- function(lst.embeddings){
  ## Calculate the objective function
  
  #First, update the Kij matrix
  lst.cross.embeddings<- cross3(map(lst.embeddings,t),lst.cross.dependencies,lst.embeddings)%>% `[`(positions) %>% lapply(function(x) Reduce("%*%",x)) # for some reason map doesn't work
  
  # The second part - Frobenius norms
  MKMt<- cross3(lst.embeddings,lst.cross.embeddings,map(lst.embeddings,t))%>% `[`(positions) %>% lapply(function(x) Reduce("%*%",x))
  
  Frobenius<- map2(map(lst.cross.dependencies,t),MKMt,`-`) %>% # To avoid re-creating the cross.dependencies, we transpose it
    map(norm, type="F") %>%
    map(`%^%`,k=2)%>%
    reduce(`+`) %>% 
    magrittr:: multiply_by(1)  # Assume a=1
  

  # and the first part (sum of traces)
  tr.MLM<-  mapply(mat.multi.multiply, map(lst.embeddings,t),
                   lst.similarities, 
                   lst.embeddings,
                   SIMPLIFY = FALSE) %>%
    map(~sum(diag(.))) %>% # Calculate the traces
    reduce(`+`)
  
  objective<- tr.MLM + Frobenius
}

calculate.new.embeddings<- function(old.embeddings, similarities){
  emb.orders<- get.Embeddings.Orders(old.embeddings, similarities)
  print("Orders done!")
  emb.customers<- get.Embeddings.Customers(old.embeddings,similarities)
  print("Customers done!")
  emb.items<- get.Embeddings.Items(old.embeddings,similarities)
  print("Items done!")
  emb.packages<- get.Embeddings.Packages(old.embeddings,similarities)
  print("Packages done!")
  emb.products<- get.Embeddings.Products(old.embeddings,similarities)
  print("Products done!")
  
  new.ebeddings<- list(Orders = emb.orders,
                       Customers = emb.customers,
                       Items = emb.items,
                       Packages = emb.packages,
                       Products = emb.products)
}


##### -----Outranking------ ####
#returns partial concordance index
concordance.partial<-function(val,indifference,similarity, increasing=TRUE){
  if(increasing){
    if(val>similarity){
      ret<-1
    }else if(val>=indifference){
      ret<-(val-indifference)/(similarity-indifference)
    } else (ret<-0)}
  else
  {
    if(val<similarity){
      ret<-1
    }else if(val<=indifference){
      ret<-(val-indifference)/(similarity-indifference)
    } else (ret<-0)
  }
  return(ret)
}
vec.concordance.partial<-Vectorize(concordance.partial,vectorize.args="val")
#returns partial discordance index
discordance.partial<-function(val,indifference,veto, increasing=TRUE){
  if(increasing){
    if(val<=veto){
      ret<-1
    }else if(val<=indifference){
      ret<-(indifference-val)/(indifference-veto)
    } else (ret<-0)}
  else
  {
    if(val>=veto){
      ret<-1
    }else if(val>=indifference){
      ret<-(indifference-val)/(indifference-veto)
    } else (ret<-0)
  }
  return(ret)
}

