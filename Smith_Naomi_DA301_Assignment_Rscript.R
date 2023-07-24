## LSE Data Analytics Online Career Accelerator 

# DA301:  Advanced Analytics for Organisational Impact


###############################################################################

# 1. Load and explore the data

# Install and import Tidyverse.
library('tidyverse')
# There is an error message here because of conflicts with another package, 
# but the conflicts aren't an issue so please ignore the message.

# Set up the pathway to the file
getwd()
# Change the file pathway as appropriate
setwd("C:\\Users\\User\\Desktop\\LSE_DA301_Module_4_files")

# Import the data set.
sales <- read.csv("turtle_sales.csv", header=T)
# Print the data frame

as_tibble(sales)
glimpse(sales)

# Create a new data frame from a subset of the sales data frame.


# Remove unnecessary columns. 
# Determine if any columns are unnecessary
publishers <- unique(sales$Publisher)
print(publishers)
# This column does contain several unique values, so it will remain. At this stage I do not believe any of the columns are reduntant so they will all remain. 
# Convert the Publisher column to factor. 
sales$Publisher <- factor(sales$Publisher)
as_tibble(sales)

# Change Product datatype to factor
sales$Product <- factor(sales$Product)
as_tibble(sales)

# Change Genre to factor? 
genres <- unique(sales$Genre)
print(genres)
# There are only 12 categories, and additional statistical functions may be available if these are changes to factor datatypes.
sales$Genre <- factor(sales$Genre)

as_tibble(sales)

# Change the Platform column to factor
sales$Platform <- factor(sales$Platform)
as_tibble(sales)


# View the descriptive statistics.
summary(sales)
# Metadata tells use that the sales figures are units sold in milions. 
# Global is total sales (US plus EU plus other)
# Create a new column which is Global Sales minus US and EU sales
sales$Other_Sales <- round(sales$Global_Sales - (sales$NA_Sales + sales$EU_Sales),2)
View(sales)

# Make a new column with all those Publishers not in the graph above, marked 'other'
high_sales <- sales
view(high_sales)
high_sales$Publisher <-as.character(high_sales$Publisher)
high_sales$High_Sales_Publishers <- ifelse(high_sales$Publisher%in% c("Activision", "Microsoft Game Studios", "Nintendo", "Sony Computer Entertainment", "Take-Two Interactive"), high_sales$Publisher, "other") 

################################################################################

# 2. Review plots to determine insights into the data set.
# EDA principles determine the following order: categorical, then numerical, then combinations of those, looking for patterns and anomalies.

# Categorical Explorations: 

# Which products have higher rankings? And how does Year influence these factors? 
## 2a) Scatterplots
# Create scatterplots.
qplot(Ranking,
      Year,
      colour=Global_Sales,
      data=sales)


# What's the relationship between ranking and sales? 
qplot(Ranking,
      Global_Sales,
      color= Global_Sales,
      data=sales )
# Clear relationship between ranging of product, and sales. 
sales2 <- filter(sales, Ranking <150)
qplot(Ranking,
      Global_Sales,
      color= Global_Sales,
      data=sales2 )

qplot(Ranking,
      Global_Sales,
      color= Year,
      data=sales2 )

qplot(Ranking,
      Global_Sales,
      color= Publisher,
      data=sales2 )
sales10 <- filter(sales, Ranking <50)
qplot(Ranking,
      Global_Sales,
      color= Publisher,
      data=sales10 )
# I think we've already shown this but this could neatly show the need to focus on Nintendo. 


qplot(Ranking,
      Year,
      colour=Genre,
      data=sales10)
# No strong patterns easily discernible here. 
# Tried with variuos dfs


# What about other factors within this high ranking set of games? 
qplot(Platform,
      Ranking,
      colour=Publisher,
      data=sales) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# This is good - so far it's just too busy. 
# It's good because it shows which Platforms have a good ranking, AND that these
# are predominantly Nintendo. 
# Graph again using he high_sales df
qplot(Platform,
      Ranking,
      colour=High_Sales_Publishers,
      data=high_sales) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
### UH OH!!! THERE IS SOMETHING VERY WRONG HERE!!! Why haven't I showed that PC is important? I've been emphasising Nintendo!

qplot(Product,
      Platform,
      colour=Ranking,
      data=sales2) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Lower numbered products seem generally to have higher rating (not consistently though). 
# Lok into this 

# Graph the same but with the whole dataset. 
qplot(Product,
      Platform,
      colour=Ranking,
      data=sales) + theme(axis.text.x = element_text(angle = 90, hjust = 1))
# This one shows very cleary the relationship between sales and ranking. Those products with lower 
# rankings are barley sold, comparatively. 


# What about other factors within this high ranking set of games? 
qplot(Product,
      Platform,
      colour=Genre,
      data=sales2)
# Some products are more than one genre.

qplot(Genre,
      Global_Sales,
      colour=Publisher,
      data=sales2, geom = c('point', 'jitter'))
# Show not just that certain kinds of games dominate the market, but suggests also 
# that these are predominantly made by a select number of Publishers. To check this,
# make a new dataframe which is a filtered version of the above, with only the most popular punblishers. 

# One way to do this may be to filter for only global sales over 10 million. 
sales7 <- filter(sales, Global_Sales >10)
qplot(Genre,
      Global_Sales,
      colour=Publisher,
      data=sales7, geom = c('point', 'jitter')) + 
  scale_colour_manual(values = c("red", "lightblue", "black", "green", "orange"))
# That's not attractive but it does show clearly that Nintendo is the leader across all genres
# except action, for global sales above 10 million. 

# Too compicated to introduce the idea that Nintendo is dominant. Simplify.
qplot(Global_Sales,
      Publisher,
      data=sales7, geom = c('point', 'jitter')) + 
  scale_colour_manual(values = c("red", "lightblue", "black", "green", "orange"))
# Better as a barplot. 


# Make a new column with all those Publishers not in the graph above, marked 'other'
high_sales <- sales
view(high_sales)
high_sales$Publisher <-as.character(high_sales$Publisher)
high_sales$High_Sales_Publishers <- ifelse(high_sales$Publisher%in% c("Activision", "Microsoft Game Studios", "Nintendo", "Sony Computer Entertainment", "Take-Two Interactive"), high_sales$Publisher, "other") 

str(high_sales)

# Graph the whole dataset again but with only the top global sales publishers, coloured. 
qplot(Genre,
      Global_Sales,
      colour=High_Sales_Publishers,
      data=high_sales, geom = c('point', 'jitter')) + 
  scale_colour_manual(values = c("red", "blue", "purple", "black", "orange", "green"))
# It's ugly but it does make the points of the graph above, more clearly.  

publisher_sales <- aggregate(Global_Sales ~ Publisher, data = sales, FUN = sum)
View(publisher_sales)

publisher_sales <- publisher_sales[order(publisher_sales$Global_Sales, decreasing = TRUE),]
View(publisher_sales)

publisher_sales$Publisher <- factor(publisher_sales$Publisher, levels = rev(publisher_sales$Publisher))
ggplot(data = publisher_sales, aes(x = Publisher, y = Global_Sales)) + geom_bar(stat = "identity", fill = "magenta") +
  labs(x = "Publisher", y = "Total Sales (million)", title = "Total Sales by Publisher") + theme(axis.text.x = element_text(angle = 45, hjust = 1))



qplot(Genre,
      Global_Sales,
      colour=Publisher,
      data=sales, geom = c('point', 'jitter'))
# Too many Publishers for colour to work clearly here. Filter for Global Sales above 15

sales4 = filter(sales, Global_Sales > 15)
qplot(Genre,
      Global_Sales,
      colour=Publisher,
      data=sales4, geom = c('point', 'jitter'))
# Nintendo's sales figures are very impressive, across nearly all genres. Their products are by far the highest selling globally. 
# But does this really show anything other than we've seen already? 

# What about product impact on sales in in different regions? 
# North America
qplot(Genre,
      NA_Sales,
      colour=Publisher,
      data=sales4, geom = c('point', 'jitter'))
# EU
qplot(Genre,
      EU_Sales,
      colour=Publisher,
      data=sales4, geom = c('point', 'jitter'))
# Other
qplot(Genre,
      Other_Sales,
      colour=Publisher,
      data=sales4, geom = c('point', 'jitter'))
# Nintendo's market leadership for sales, is visible across all markets. 

# What do wee see when we plot all the regions on the one graph? (scaling may be an issue)
ggplot(sales) + geom_line(aes(y = Product, x = NA_Sales, color = "North America")) + 
  geom_line(aes(y=Product, x = EU_Sales, color = "E.U.")) + 
  geom_line(aes(y = Product, x = Other_Sales, color = "Other")) +
  labs(y = "Product", x = "Sales", color = "Sales Location") +
  scale_color_manual(values = c("North America" = "blue", "E.U." = "red", 
                                "Other" = "gray"))
# This is a good example of why EDA is so important - I assumed that "Other" was much less signifanct
# than the graph shows. I want to go back and perform descriptve statistics to ensure what I'm looking 
# at is correct. Also need to remove the very low sales figures as there is not as much point in showing them 
# if their numbers are too small to show up on this graph. 

# Consider what current overall trends are and how these relate to Turtle Games sales. 
# eg rankings - latest, and trends. Why? Because sales are so closely tied to rankings, and, this 
# dataset doesn't show what new kinds of games  / publisher popularity etc data is emerging. NB
# Nintndo supremacy v unikey to be affected regardless as it's so strong. 

# Concern that some of the bars dont start until more than sero, while some start at zero. Why is that? 

#  Regraph with the sales over 1 million. 
# New df: sales over 1 million (Global will do)
sales9 <- filter (sales, Other_Sales > 1) 
View(sales9)
dim(sales)
dim(sales9)
# looks right
ggplot(sales9) + geom_line(aes(y = Product, x = NA_Sales, color = "North America")) + 
  geom_line(aes(y=Product, x = EU_Sales, color = "E.U.")) + 
  geom_line(aes(y = Product, x = Other_Sales, color = "Other")) +
  labs(y = "Product", x = "Sales", color = "Sales Location") +
  scale_color_manual(values = c("North America" = "blue", "E.U." = "red", 
                                "Other" = "gray"))

## THE FOLLOWING IS GOOD FOR WEEK 4
# This is good but it might be better for later in the presentation, when we disucss 
# regional sales. 


# That is not at all what I expected: seek help. 
# Meantime, try a bar plot (stacked)
# Reshape the data frame to long format
sales10 <- select(sales, -Ranking, -Platform, -Year, -Global_Sales, -Genre, -Publisher, -Platform)

sales_long <-tidyr::gather(sales10, key = "region", value = "sales", -Product)
sales_long

# Reorder the sales_long column, product, by global sales
sales_long_ordered <- sales_long
sales_long$Product <- with(sales_long_ordered, reorder(Product, NA_Sales, FUN = median))

# Plot the data
ggplot(sales_long_ordered, aes(x = Product, y = sales, fill = region)) + geom_bar(stat = "Identity") + labs(x = "Product", y = "Sales", fill = "Sales Location")



# Order the bars.
sales_long <- merge(sales_long, sales[c("Product", "Global_Sales")], by = "Product")


sales_long <- sales_long[order(-sales_long$Global_sales),]
View(sales_long)

# Plot the result.
ggplot(sales_long, aes(x = Product, y = sales, fill = region)) +
  geom_bar(stat = "identity") + labs(x = "Product", y = "Sales", fill = "Region")



qplot(Year,
      Global_Sales,
      colour=Publisher,
      data=sales4)
# Furthermore, Nintendo's very high selling products come from a range of years.
# We'll leave out the Year data, because the Publisher data overrides it. 

qplot(EU_Sales,
      NA_Sales, 
      colour=Product,
      data=sales4)
# Products seem to be higher in one or the other market, after a certain point.

qplot(EU_Sales,
      NA_Sales,
      data=sales, geom = c('point', 'jitter'))
# Most NA Sales are under 5 million, except for about 50 products. Of those, thre are three which do much bettern in the 
# NA market than in the EU. Mos of the really top sellers in the EU also do have a positive correllation
# with sales in the NA market too, but it is much more subdued in NA compared to the EU. 


platforms<- unique(sales$Platform)
View(platforms)


# Plot the Dfs to check for anomalies and patterns
 qplot(EU_Sales, 
       NA_Sales,
       data = SNES)
 # TO  COMPLETE
 # Anomalies / Grps: 
 # A: WiiU, GC
 # B: PC
 # C: PS
 # D (normal positive correllation): N64, PS4, GBA, SNES (one outlier very good sales in both)
 
 # Investigate the 3DS issue
 
 
 ## 2b) Histograms
 # Create histograms.
 
 # Sales:
qplot(NA_Sales, data=sales)
qplot(EU_Sales, data = sales) 
qplot(Other_Sales, data = sales)

# Platform
qplot(Platform, data = sales)
# This is a good one. 

# Year
qplot(Year, data = sales)
# Good - 2009?
# MORE recent games generally sell better, so don't invest too heavily in the much older games. 
# Offer specific years. 
# Look into the anomalies


# Genre

qplot (Genre, Global_Sales, data = sales)
# Useful - clearly shows which genres (Shooter, Action, Sports) are most sold.
# But better, or in addition, it would be good to show the total number of sales per genre. 
ggplot(data = sales, aes(x = Genre)) + geom_bar(fill = "gold") +
  labs(x = "Genre", y = "Total Sales (million)", title = "Total Sales by Genre")



# Publisher
qplot(Publisher, data = sales) + theme(axis.text.x = element_text(angle = 45, hjust = 1))
# This is good - if I use it in presentation will need to order the publisehrs by sales. 

Platform
qplot(Platform, data = sales)
# Present this as a stacked bar chart as specific Publishers make specific platforms. 
qplot(Platform, fill=Publisher, data=sales, geom='bar')
# Doesn't work because there are too many Publishers, but would work if I just had the most popular publishers and the rest were 'other'
# Publishers with sales over 5


## 2c) Boxplots
 # Create boxplots.
 
# How do the reviews data and the sales data relate? 


# Concatenate sales and reviews - correlations between eg Income and Publisher.
getwd()
reviews <- read.csv("turtle_reviews.csv", header=T)
View(reviews)
# Change "product" to "Product in the reviews df
colnames(reviews)[colnames(reviews) == "product"] <- "Product"
View(reviews)

# Merge
reviews_sales <- merge(sales, reviews, by = "Product", all.x=TRUE)

str(reviews_sales)
# Perform EDA
qplot(Ranking,
      remuneration..k..,
      color= Global_Sales,
      data=reviews_sales )
# Nothing much to see there. 
# Would need to scale? 


qplot(age,
      Global_Sales,
      data=reviews_sales, 
      geom = c("point", "jitter"))
# I don't THINK there's anything here except there are those same data anomalies re: gaps in age, and some heavy age reps. 

qplot(age,
      Global_Sales,
      data=reviews_sales, 
      colour = Ranking, 
      geom = c("point", "jitter"))
# For the first time a visualisation that clarly shows the relationship between income and products
# Make three new datasets which are subsets of the above, organised into income brakcets 
# based on the products they are buying
income_low <- reviews_sales[reviews_sales$remuneration..k.. <= 53,]
income_mid <- reviews_sales[reviews_sales$remuneration..k.. > 53 & reviews_sales$remuneration..k.. <= 77,]
income_high <- reviews_sales[reviews_sales$remuneration..k.. > 77,]
# Nothing wrong with a bit of overlap between these dataframes : we want to see that
# the data is definiltey included and that is more important than having exclusive data.


# Replot to check the data is captured correctly  
# low income
qplot(Product, 
      remuneration..k..,,
      data = income_low)

# mid income
qplot(Product, 
      remuneration..k..,,
      data = income_mid)
# high income
qplot(Product, 
      remuneration..k..,,
      data = income_high)


# Visualise the products that are popular with these groups, looking for patterns 
# eg in terms of publishers they favour, or genres, etc. Sadly we can't identify unique 
# platforms. 
ggplot(data = income_low, aes(x = Genre)) + geom_bar(fill = "gold") +
  labs(x = "Genre", y = "Total Sales (million)", title = "Total Sales by Genre")

ggplot(data = income_mid, aes(x = Genre)) + geom_bar(fill = "gold") +
  labs(x = "Genre", y = "Total Sales (million)", title = "Total Sales by Genre")

ggplot(data = income_high, aes(x = Genre)) + geom_bar(fill = "gold") +
  labs(x = "Genre", y = "Total Sales (million)", title = "Total Sales by Genre")
# Although the products are different between mid and high the genres aren't that different


# Same but for ranking, publisher, etc
ggplot(data = income_low, aes(x = Publisher)) + geom_bar(fill = "skyblue") +
  labs(x = "Publisher", y = "Total Sales (million)", title = "Total Sales by Genre")

ggplot(data = income_mid, aes(x = Publisher)) + geom_bar(fill = "skyblue") +
  labs(x = "Publisher", y = "Total Sales (million)", title = "Total Sales by Genre")

ggplot(data = income_high, aes(x = Publisher)) + geom_bar(fill = "skyblue") +
  labs(x = "Publisher", y = "Total Sales (million)", title = "Total Sales by Genre")
# Although the products are different between mid and high the genres aren't that different




glimpse(reviews_sales)


qplot(Product, 
      remuneration..k..,,
      data = reviews_sales)


# Let's look (again at remuneration and ranking as well. )
qplot(Product, 
      remuneration..k..,
      colour = Ranking,
      data = reviews_sales)


# Let's do the same with platforms and Genres, and age and genre.
qplot(Product, 
      remuneration..k..,
      colour = Platform,
      data = reviews_sales)

qplot(Product, 
      remuneration..k..,
      colour = Genre,
      data = reviews_sales)
# And in stacked bar charts.

ggplot(reviews_sales, aes(x = Publisher, y = Global_Sales, fill = remuneration..k..)) +
  geom_bar(stat = "identity") +
  labs(x = "Publisher", y = "Global Sales", fill = "Income") +
  theme_minimal()


# Need to complete the ploarity and sentiment analysis and then add a new column in reviews for that, and then redo all this, otherwise it is meaningless. 
# Import the concatenated csv of reviews with sales data AND NLP data. 
reviews.products <- read.csv("reviews_products.csv", header = T)
str(reviews_products)
# Drop the unnecessary columns. 
reviews.products <- subset(reviews_products, select = -c(sum_rev,summary_tokens,review_tokens,combined_tokens))
View(reviews.products)

###############################################################################

# 3. Observations and insights

## Your observations and insights here ......




###############################################################################
###############################################################################


# Week 5 assignment: Cleaning and manipulating data using R

## Utilising R, you will explore, prepare and explain the normality of the data
## set based on plots, Skewness, Kurtosis, and a Shapiro-Wilk test. Note that
## you will use this data set in future modules as well and it is, therefore, 
## strongly encouraged to first clean the data as per provided guidelines and 
## then save a copy of the clean data for future use.

## Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 4 assignment. 
##  - View the data frame to sense-check the data set.
##  - Determine the `min`, `max` and `mean` values of all the sales data.
##  - Create a summary of the data frame.
# 2. Determine the impact on sales per product_id.
##  - Use the group_by and aggregate functions to sum the values grouped by
##      product.
##  - Create a summary of the new data frame.
# 3. Create plots to review and determine insights into the data set.
##  - Create scatterplots, histograms, and boxplots to gain insights into 
##     the Sales data.
##  - Note your observations and diagrams that could be used to provide 
##     insights to the business.
# 4. Determine the normality of the data set.
##  - Create and explore Q-Q plots for all sales data.
##  - Perform a Shapiro-Wilk test on all the sales data.
##  - Determine the Skewness and Kurtosis of all the sales data.
##  - Determine if there is any correlation between the sales data columns.
# 5. Create plots to gain insights into the sales data.
##  - Compare all the sales data (columns) for any correlation(s).
##  - Add a trend line to the plots for ease of interpretation.
# 6. Include your insights and observations.

################################################################################

# 1. Load and explore the data




# # View the data frame to sense-check the data set.
str(sales)

# View the descriptive statistics.
# Create a summary of the data frame to determine the min, max and mean values of all the sales data (three columns).
summary(sales)
# 



###############################################################################

# 2. Determine the impact on sales per product_id.
# The sales info for each products is already readily available in the dataframe. 
# Determine the impact on sales of the subsets of the data that each product is
# part of. 




# 2a) Use the group_by and aggregate functions.
# Group data based on Product and determine the sum per Product.
grouped_sales_1 <- sales %>% group_by(Product, Publisher)
View(grouped_sales_1)



sales_product  <- aggregate(cbind(Global_Sales, EU_Sales, NA_Sales, Other_Sales) ~ Product, data=grouped_sales_1, sum)
sales_publisher <- aggregate(cbind(Global_Sales, EU_Sales, NA_Sales, Other_Sales) ~ Publisher, data=grouped_sales_1, sum)
sales_genre <- aggregate(cbind(Global_Sales, EU_Sales, NA_Sales, Other_Sales) ~ Genre, data=grouped_sales_1, sum)
sales_year <- aggregate(cbind(Global_Sales, EU_Sales, NA_Sales, Other_Sales) ~ Year, data=grouped_sales_1, sum)
sales_platform <- aggregate(cbind(Global_Sales, EU_Sales, NA_Sales, Other_Sales) ~ Platform, data=grouped_sales_1, sum)


# View the data frames.
View(sales_publisher)



# Explore the data frames.
summary(sales_product)
summary(sales_publisher)
summary(sales_genre)
summary(sales_year)
summary(sales_platform)

## 2b) Determine which plot is the best to compare game sales.
# Create scatterplots.

# Global Sales per product
# Reorder the df baed on global sales. 
sales_product$Product <- factor(sales_product$Product, levels = sales_product$Product[order(sales_product$Global_Sales, decreasing = TRUE)])
View(sales_product)
# Plot
ggplot(data = sales_product, aes(y= Global_Sales, x= Product)) + 
  geom_point() + labs(x = "Product", y = "Global Sales") + ggtitle("Global Sales per Product") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# US Sales per product
ggplot(data = sales_product, aes(y= NA_Sales, x= Product)) + 
  geom_point() + labs(x = "Product", y = "North American Sales") + ggtitle("North American Sales per Product") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# There is only a correlation between these two - they are certainly not the same
# Order and THEN replot (potentially can show they are not the same by leaving in previous order first)
sales_product$Product <- factor(sales_product$Product, levels = sales_product$Product[order(sales_product$NA_Sales, decreasing = TRUE)])
ggplot(data = sales_product, aes(y= NA_Sales, x= Product)) + 
  geom_point() + labs(x = "Product", y = "North American Sales") + ggtitle("North American Sales per Product") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# EU Sales per product
ggplot(data = sales_product, aes(y= EU_Sales, x= Product)) + 
  geom_point() + labs(x = "Product", y = "E.U. Sales") + ggtitle("E.U. Sales per Product") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Very different from NA sales. Therefore will be different from global sales (as US dominates)
# Reorder and replot
sales_product$Product <- factor(sales_product$Product, levels = sales_product$Product[order(sales_product$EU_Sales, decreasing = TRUE)])
ggplot(data = sales_product, aes(y= EU_Sales, x= Product)) + 
  geom_point() + labs(x = "Product", y = "E.U. Sales") + ggtitle("E.U. Sales per Product") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Other Sales (Gobal - (EU + NA))
ggplot(data = sales_product, aes(y= Other_Sales, x= Product)) + 
  geom_point() + labs(x = "Product", y = "'Other' Sales") + ggtitle("'Other' Sales per Product") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
# Different to both EU and US
# Order and replot. 
sales_product$Product <- factor(sales_product$Product, levels = sales_product$Product[order(sales_product$Other_Sales, decreasing = TRUE)])
ggplot(data = sales_product, aes(y= Other_Sales, x= Product)) + 
  geom_point() + labs(x = "Product", y = "'Other' Sales") + ggtitle("'Other' Sales per Product") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Changing the language of the platform: will this make a difference (assuming it's only english, as the reviews somewhat suggest).
# Come back to the differently aggregated data? 


# Create histograms.
hist(sales$Global_Sales)
# Remove the outliers (but only in a copy of the df) to see how distribution is affected .
# Make a new df which excludes Global Sales valus over 30 million (NB: replotting continued right down to 15 mill, as shown in the code below) 
sales_without_outliers <-sales %>% filter(Global_Sales <=40)
hist(sales_without_outliers$Global_Sales)
# Tried the above with various values for the filter, and the graph is consistently right tailed. 

# Create boxplots.
boxplot(sales$Global_Sales)


ggplot(sales, aes(x = Genre, y = Global_Sales)) + geom_boxplot()
ggplot(sales_without_outliers, aes(x = Genre, y = Global_Sales)) + geom_boxplot()

ggplot(sales, aes(x = Publisher, y = Global_Sales)) + geom_boxplot()
# Probably the same outlier. Replot without it.
ggplot(sales_without_outliers, aes(x = Publisher, y = Global_Sales)) + geom_boxplot()



# Platform
ggplot(sales, aes(x = Platform, y = Global_Sales)) + geom_boxplot()
# Platform again but add hue for Publisher
ggplot(sales, aes(x = Platform, y = Global_Sales, fill = Publisher)) + geom_boxplot()
# That's interesting - I assumed that each platform had only one publisher. This shows that that isn't always the case. 
# There are too many publishers for the colours to be legible - could 'other' the lower sales publishers and replot.
ggplot(sales_without_outliers, aes(x = Platform, y = Global_Sales, fill = Publisher)) + geom_boxplot()

# Create a copy dataframe which renames the publisher 'other' if their total global sales are less than 500m.
sales_summary <- sales %>% group_by(Publisher) %>% summarise(Total_Global_Sales = sum(Global_Sales))
# Add a new column to a copy of the original df
sales_totals <- sales %>% left_join(sales_summary, by = "Publisher") %>% mutate(Publisher_Over_5 = ifelse(Total_Global_Sales >500, as.character(Publisher), NA))
View(sales_totals)
# Replot
ggplot(sales_totals, aes(x = Platform, y = Global_Sales, fill = Publisher_Over_5)) + geom_boxplot()
# That's really good at showing Nintendo is the only one with gloabl sales over 500m. 
# But we might want something showing more range.
sales_totals_over_200m <- sales %>% left_join(sales_summary, by = "Publisher") %>% mutate(Publisher_Over_2 = ifelse(Total_Global_Sales >200, as.character(Publisher), NA))
ggplot(sales_totals_over_200m, aes(x = Platform, y = Global_Sales, fill = Publisher_Over_2)) + geom_boxplot()
# Still need to cut off lower to include more (while still being legible ie not too many publishers)
sales_totals_over_100m <- sales %>% left_join(sales_summary, by = "Publisher") %>% mutate(Publisher_Over_1 = ifelse(Total_Global_Sales >100, as.character(Publisher), NA))
ggplot(sales_totals_over_100m, aes(x = Platform, y = Global_Sales, fill = Publisher_Over_1)) + geom_boxplot()
# Little bit messy (thogh it is legible) so back up at try 150m
sales_totals_over_150m <- sales_without_outlier %>% left_join(sales_summary, by = "Publisher") %>% mutate(Publisher_Over_150m = ifelse(Total_Global_Sales >150, as.character(Publisher), NA))
# Without outlier
ggplot(sales_totals_over_150m, aes(x = Platform, y = Global_Sales, fill = Publisher_Over_150m)) + geom_boxplot()
# I think that's about the right balance: legible, tells a story, informative (With a focus).
# With outlier
sales_totals_over_150m <- sales %>% left_join(sales_summary, by = "Publisher") %>% mutate(Publisher_Over_150m = ifelse(Total_Global_Sales >150, as.character(Publisher), NA))
ggplot(sales_totals_over_150m, aes(x = Platform, y = Global_Sales, fill = Publisher_Over_150m)) + geom_boxplot()
# Much easier to see the data without the outlier. 



# Show the same data in a histogram
ggplot(sales_product_without_outliers, aes(x = Global_Sales)) + geom_histogram(binwidth = .5, fill = 'purple', color = 'darkgreen')+
  labs(title = "Global Sales per Product", x = "Sales")
# And the same again, with outliers (remembering that the objective here is to check the reliability of the data).
ggplot(sales_product, aes(x = Global_Sales)) + geom_histogram(binwidth = .5, fill = 'purple', color = 'darkgreen')+
  labs(title = "Global Sales per Product", x = "Sales")



###############################################################################


# 3. Determine the normality of the data set.

var(sales$Global_Sales)
var(sales$NA_Sales)
var(sales$EU_Sales)
var(sales$Other_Sales)
# The var)value is progressively smaller.

sd(sales$Global_Sales)
sd(sales$NA_Sales)
sd(sales$EU_Sales)
sd(sales$Other_Sales)
# As is, of course, the standard deviation. 



## 3a) Create Q-Q Plots
# Create Q-Q Plots.
# Global Sales
qqnorm(sales$Global_Sales)
qqline(sales$Global_Sales)
qqnorm(sales_without_outliers$Global_Sales)
qqline(sales_without_outliers$Global_Sales)

# NA Sales
qqnorm(sales$NA_Sales)
qqline(sales$NA_Sales)
qqnorm(sales_without_outliers$NA_Sales)
qqline(sales_without_outliers$NA_Sales)


# EU Sales
qqnorm(sales$EU_Sales)
qqline(sales$EU_Sales)
qqnorm(sales_without_outliers$EU_Sales)
qqline(sales_without_outliers$EU_Sales)


# Other Sales (non US / EU)
qqnorm(sales$Other_Sales)
qqline(sales$Other_Sales)
qqnorm(sales_without_outliers$Other_Sales)
qqline(sales_without_outliers$Other_Sales)


# The data is far more spread out than the normal distribution, but bares some similarity. 
# but while the right tail is more extreme (than normal dist) the left tail is somewhat less extreme than norm dist.
# This is largely because sales can't be in negative numbers. 
# There are a lot of products which sell nearly at 0. 
# TO DO: What are the reviews for these? ie the review score? 
# Suggest displaying the score. 

# Explore correllation between Ranking and Sales again.  


## 3b) Perform Shapiro-Wilk test
# Install and import Moments.
library(moments)

# Perform Shapiro-Wilk test.
shapiro.test(sales$Global_Sales)
# W-statistic: The data deviates substantially from a normal distribution.
# p-value: very small, indicating this is not a normal distribution.
# However as this is a large dataset, we will rely more on the QQline.
shapiro.test(sales_without_outliers$Global_Sales)
# Even without the outlier this test indicates the data is not normally distrubuted. 
# What about EU sales (or another subset)
shapiro.test(sales$EU_Sales)

## 3c) Determine Skewness and Kurtosis
# Skewness and Kurtosis.
skewness(sales$Global_Sales)
# A large positive value, indicating a heavy right tail. 
# In other words, the data is highly assymetrical.
#Likely this is because the dataset is being pulled right by eg Nintendo Wii!
skewness(sales_without_outliers$Global_Sales)
# Removing that one value halves the skewness. 


# Explore the statistics of this dataset without Nintendo data.
# Try removing all Nintendo sales and see what the skewness is. 
sales_no_nintendo <-subset(sales, Publisher != "Nintendo")
skewness(sales_no_nintendo$ Global_Sales)
# Still positive (and right skewed), but much less so. 
shapiro.test(sales_no_nintendo$ Global_Sales)
# Still not normally distributed. 
qqline(sales_no_nintendo$Global_Sales)
# There is both a lin of quality AND a line of bestfit. 
hist(sales_no_nintendo$Global_Sales)







## 3d) Determine correlation
# Determine correlation.
cor_sales <-cor(sales$Global_Sales,sales$Ranking)
print(cor_sales)
# There is a weak negative correlation. 



# What is the impact on sales per product? 
# Order the Global_sales data.
sales_1 <-sales
sales_1$Product <- reorder(sales_1$Product, -sales_1$Global_Sales)
view(sales_1)
# Change datatype of 'product'
sales_1$product <- as.character(sales_1$Product)
sales_1$product <- reorder(sales_1$product, -sales_1$Global_Sales)
ggplot(sales_1, aes(x= product, y= Global_Sales)) +
  geom_bar(stat = 'identity', fill = 'blue')







###############################################################################

# 4. Plot the data
# Create plots to gain insights into data.
# Choose the type of plot you think best suits the data set and what you want 
# to investigate. Explain your answer in your report.


###############################################################################

# 5. Observations and insights
# Your observations and insights here...



###############################################################################
###############################################################################

# Week 6 assignment: Making recommendations to the business using R

## The sales department wants to better understand if there is any relationship
## between North America, Europe, and global sales. Therefore, you need to
## investigate any possible relationship(s) in the sales data by creating a 
## simple and multiple linear regression model. Based on the models and your
## previous analysis (Weeks 1-5), you will then provide recommendations to 
## Turtle Games based on:
##   - Do you have confidence in the models based on goodness of fit and
##        accuracy of predictions?
##   - What would your suggestions and recommendations be to the business?
##   - If needed, how would you improve the model(s)?
##   - Explain your answers.

# Instructions
# 1. Load and explore the data.
##  - Continue to use the data frame that you prepared in the Week 5 assignment. 
# 2. Create a simple linear regression model.
##  - Determine the correlation between the sales columns.
##  - View the output.
##  - Create plots to view the linear regression.
# 3. Create a multiple linear regression model
##  - Select only the numeric columns.
##  - Determine the correlation between the sales columns.
##  - View the output.
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.
# 5. Include your insights and observations.

###############################################################################
# 1. Load and explore the data
# View data frame created in Week 5.
# Determine a summary of the data frame.
summary(sales)
head(sales)
str(sales)

# Add an Other Sales column
sales$Other_Sales <- round(sales$Global_Sales - (sales$NA_Sales + sales$EU_Sales),2)
View(sales)
# Make a copy of the df
sales_1 <- sales
# Remove the two outliers - note this in tech report, and return to it in Pres.
# sales_1 = sales_1[sales_1$Global_Sales < 30,]


# 1. Load and explore the data
# View data frame created in Week 5.
# Determine a summary of the data frame.
summary(sales)
head(sales)
str(sales)


# Add an Other Sales column
sales$Other_Sales <- round(sales$Global_Sales - (sales$NA_Sales + sales$EU_Sales),2)
View(sales)
 # Make a copy of the df
sales_1 <- sales
# Remove the two outliers - note this in tech report, and return to it in Pres.
# sales_1 = sales_1[sales_1$Global_Sales < 60,]

# 2. Create a simple linear regression model
## 2a) Determine the correlation between columns
cor(sales_1$Global_Sales, sales_1$Other_Sales)
# 82
cor(sales_1$Global_Sales, sales_1$NA_Sales)
# 93
cor(sales_1$Global_Sales, sales_1$EU_Sales)
# 87
cor(sales_1$EU_Sales, sales_1$Other_Sales)
# only 67
cor(sales_1$EU_Sales, sales_1$NA_Sales)
# only 70
cor(sales_1$NA_Sales, sales_1$Other_Sales)
# only 64


## 2b) Create plots (simple linear regression)
# Basic visualisations.
plot(sales_1$Global_Sales, sales_1$Other_Sales)
# more difference between them, as the sales figures increase

plot(sales_1$Global_Sales, sales_1$NA_Sales)
# as above, but less extreme

plot(sales_1$Global_Sales, sales_1$EU_Sales)
# as above but Global sales very much higher

plot(sales_1$EU_Sales, sales_1$Other_Sales)
# very scattered indicating not at all a strong relationship

plot(sales_1$EU_Sales, sales_1$NA_Sales)
# a few products much higher in EU than NA and vice versa

plot(sales_1$NA_Sales, sales_1$Other_Sales)
# very scattered but still a strong relationship

# Add new columns
# Add a new column which is the sqrt of the other_sales data
sales_1$ExpOther_Sales = sales_1$Other_Sales^(1/8)


# Create the models
# model1 <-lm(Global_Sales~Other_Sales, data = sales_1)
model1 <-lm(Global_Sales~Other_Sales, data = sales_1)
model2 <-lm(Global_Sales~NA_Sales, data = sales_1)
model3 <-lm(Global_Sales~EU_Sales, data = sales_1)
model4 <-lm(Other_Sales~Global_Sales, data = sales_1)
model5 <-lm(Other_Sales~EU_Sales, data = sales_1)
model6 <-lm(Other_Sales~NA_Sales, data = sales_1)
model7 <-lm(EU_Sales~Global_Sales, data = sales_1)
model8 <-lm(EU_Sales~Other_Sales, data = sales_1)
model9 <-lm(EU_Sales~NA_Sales, data = sales_1)
model10 <-lm(NA_Sales~Global_Sales, data = sales_1)
model11 <-lm(NA_Sales~Other_Sales, data = sales_1)
model12 <-lm(NA_Sales~EU_Sales, data = sales_1)

# View the models, their summaries and residuals
model1
summary(model1)
# p-value under 0.05, t-value is 26.9, and Multiple R-squared is .67
# together, indicating other sales are reasonable predictors of global sales
plot(model1$residuals)
# clear indications of heteroskedasticity (cone shaped) residuals
# Plot the relationship with base R graphics.
plot(sales_1$Other_Sales, sales_1$Global_Sales)
# Add line-of-best-fit.
abline(coefficients(model1))
# only two significant outliers, both on the global sales side

model2
summary(model2)
# p-value under 0.05, t-value is 49.3, and Multiple R-squared is high at .87
# together, indicating other sales are good predictors of global sales
plot(model2$residuals)
# clear indications of heteroskedasticity (cone shaped) residuals
# Plot the relationship with base R graphics.
plot(sales_1$NA_Sales, sales_1$Global_Sales)
# Add line-of-best-fit.
abline(coefficients(model2))
# best yet

model3
summary(model3)
# p-value under 0.05, t-value is 34, and Multiple R-squared is quite high at .77
# together, indicating other sales are good predictors of global sales
plot(model3$residuals)
# clear indications of heteroskedasticity (cone shaped) residuals
# Plot the relationship with base R graphics.
plot(sales_1$EU_Sales, sales_1$Global_Sales)
# Add line-of-best-fit.
abline(coefficients(model3))
# outliers: about half a dozen that don't fit the model


model4
summary(model4)
# p-value under 0.05, t-value is 26, and Multiple R-squared is .67
# together, indicating other sales are good predictors of global sales
plot(model4$residuals)
# clear indications of heteroskedasticity (cone shaped) residuals
# Plot the relationship with base R graphics.
plot(sales_1$Global_Sales, sales_1$Other_Sales)
# Add line-of-best-fit.
abline(coefficients(model4))
# some outliers


model5
summary(model5)
# p-value under 0.05, t-value is 17, and Multiple R-squared is only .45
# together, indicating other sales are only reasonable predictors of eu sales
plot(model5$residuals)
# clear indications of heteroskedasticity (cone shaped) residuals
# Plot the relationship with base R graphics.
plot(sales_1$EU_Sales, sales_1$Other_Sales)
# Add line-of-best-fit.
abline(coefficients(model5))
# quite a few outliers

model6
summary(model6)
# p-value under 0.05, t-value is 15, and Multiple R-squared is only .41
# together, indicating other sales are not great predictors of na sales
plot(model5$residuals)
# clear indications of heteroskedasticity (cone shaped) residuals
# Plot the relationship with base R graphics.
plot(sales_1$NA_Sales, sales_1$Other_Sales)
# Add line-of-best-fit.
abline(coefficients(model6))
# couldn't be confident about this without transforming the data first. 


model7 # Global as predictor of EU
summary(model7)
# p-value under 0.05, t-value is 49, and Multiple R-squared is whopping 91
# together, indicating Global sales is a very good predictor of eu sales
plot(model7$residuals)
# clear indications of heteroskedasticity (cone shaped) residuals
# Plot the relationship with base R graphics.
plot(sales_1$Global_Sales, sales_1$EU_Sales)
# Add line-of-best-fit.
abline(coefficients(model7))
# there are only a few outliers, on the global sales 
# (proportionally not as popular in EU as in Global Market)
# But otherwise this appears at this stage to be quite a solid model. 

model8 # Other as predictor of EU
summary(model8)
# p-value under 0.05, t-value is 17, and Multiple R-squared is just .45
# together, indicating Other Sales is not very good predictor of eu sales
plot(model8$residuals)
# clear indications of heteroskedasticity (cone shaped) residuals
# Plot the relationship with base R graphics.
plot(sales_1$Other_Sales, sales_1$EU_Sales)
# Add line-of-best-fit.
abline(coefficients(model8))
# there are only a few outliers, on the other sales side 
# (proportionally not as popular in EU as in Other Market)

 
model9 # NA as predictor of EU
summary(model9)
# p-value under 0.05, t-value is 19, and Multiple R-squared is .50
# together, indicating Other Sales is an okay predictor of eu sales
plot(model9$residuals)
# clear indications of heteroskedasticity (cone shaped) residuals
# Strong outliers may be why the model didn't score well on the summary data.
# Plot the relationship with base R graphics.
plot(sales_1$NA_Sales, sales_1$EU_Sales)
# Add line-of-best-fit.
abline(coefficients(model9))
# there are only a 3 outliers on the NA sales side 
# (proportionally not as popular in EU as in NA)


model10 # Global  as predictor of NA
summary(model10)
# p-value under 0.05, t-value is 49, and Multiple R-squared is a considerable .87
# together, indicating Global Sales is a good predictor of NA Sales
plot(model10$residuals)
# clear indications of heteroskedasticity (cone shaped) residuals
# Strong outliers may be why the model didn't score well on the summary data.
# Plot the relationship with base R graphics.
plot(sales_1$Global_Sales, sales_1$NA_Sales)
# Add line-of-best-fit.
abline(coefficients(model10))
# there are only a 3 outliers on the NA sales side 
# (proportionally not as popular in EU as in NA)
# A model to have confidence in!

model11 # Other  as predictor of NA
summary(model11)
# p-value under 0.05, t-value is 15, and Multiple R-squared is only .41
# together, indicating Global Sales is not a good predictor of NA Sales
plot(model11$residuals)
# clear indications of heteroskedasticity (cone shaped) residuals
# Strong outliers may be why the model didn't score well on the summary data.
# Plot the relationship with base R graphics.
plot(sales_1$Other_Sales, sales_1$NA_Sales)
# Add line-of-best-fit.
abline(coefficients(model11))
# Quite a few outliers, and many of them are at the low-sales end of the line, 
# where mismatch between data and model is proportionally more significant 
# Not a model to be confident in. 

model12 # EU  as predictor of NA
summary(model12)
# p-value under 0.05, t-value is 18, and Multiple R-squared is .49
# together, indicating EU Sales is not a good predictor of NA Sales
plot(model12$residuals)
# clear indications of heteroskedasticity (cone shaped) residuals
# Strong outliers may be why the model didn't score well on the summary data.
# Plot the relationship with base R graphics.
plot(sales_1$EU_Sales, sales_1$NA_Sales)
# Add line-of-best-fit.
abline(coefficients(model12))
# there are only a 3 outliers on the NA sales side 
# (proportionally not as popular in EU as in NA)
# Otherwise a model to have confidence in


  ###############################################################################

# 3. Create a multiple linear regression model

# Install and import the psych package
install.packages('psych')
# Detach the ggplot package (mask issue)
detach(package:ggplot2,unload = TRUE)
# TO LOAD ggplot2 again: 
# library(ggplot2)
library(psych)

sales_2 <- sales_1
summary(sales_2)
# Create a new column which is the reverse of the Ranking column.
sales_2$Reverse_Ranking <- 17000 - sales_2$Ranking
# sales_2$Reverse_Ranking <- max(sales_2$Ranking) - sales_2$Ranking + min(sales_2$Ranking)
# Convert Reverse_Ranking from into to num
sales_2$Reverse_Ranking <- as.numeric(sales_2$Reverse_Ranking)
str(sales_2)
# Select only numeric columns from the original data frame.
sales_2 <- subset(sales_2, select = -c(Platform, Publisher, Genre, Ranking,
                                       Year, Sqrt_Other_Sales, SqrtOther_Sales,
                                       Exp_Other_Sales, ExpOther_Sales))


# Ranking is correlated with global sales, even if it's not a simple linear
# (it behaves like two subsets: high and low ranking products).
# Create new columns which are the ranking of all the products in their various markets
# because this may 


# view correlation of variables to inform how model is built.
cor(sales_2)
corPlot(sales_2, cex=2)
# Obsevations: 
# - Overall, Global Sales has the highest correlation with other variables
# - Product has a negative correlation with everything. Reverse it and recalculate
sales_2$Reverse_Product <- 9080 - sales_2$Product
sales_2 <- subset(sales_2, select = -c(Product))
# view correlation of variables to inform how model is built.
cor(sales_2)
corPlot(sales_2, cex=2)
# Obsevations: 



# Multiple linear regression model.
# Predicting NA Sales
modela1 <- lm(NA_Sales~Global_Sales + EU_Sales +  Other_Sales + Reverse_Ranking,
             data = sales_2)
summary(modela1)
# modela1's adjusted R-squared value is 1.
# all the variables are significant.

modela2 <- lm(NA_Sales~Global_Sales + EU_Sales, data = sales_2)
summary(modela2)
# modela2's adjusted R squared is 0.9312
# both the variables are significant


# Predicting EU Sales
modelb1 <- lm(EU_Sales~Global_Sales + NA_Sales +  Other_Sales + Reverse_Ranking,
             data = sales_2)
summary(modelb1)
# modelb1's adjusted R-squared value is 1
# all the variables are significant.

modelb2 <- lm(EU_Sales~Other_Sales + EU_Sales, data = sales_2)
summary(modelb2)
# modelb's adjusted R squared is 0.45
# both the variables are significant


# Predicting Other Sales
modelb1 <- lm(Other_Sales~Global_Sales + NA_Sales +  EU_Sales + Reverse_Ranking,
              data = sales_2)
summary(modelb1)
# modelb1's adjusted R-squared value is 1
# all the variables are significant.

modelb2 <- lm(Other_Sales~EU_Sales + EU_Sales, data = sales_2)
summary(modelb2)
# modelb's adjusted R squared is 0.45
# both the variables are significant

# Predicting NA Sales
modelc1 <- lm(NA_Sales~Global_Sales + EU_Sales +  Other_Sales + Reverse_Ranking,
              data = sales_2)
summary(modelc1)
# modelc1's adjusted R-squared value is 1
# all the variables are significant.

modelc2 <- lm(NA_Sales~Other_Sales + EU_Sales, data = sales_2)
summary(modelc2)
# modelc2's adjusted R squared is 0.5469
# both the variables are significant


# Predicting EU Sales
modele1 <- lm(EU_Sales~ NA_Sales +  Other_Sales + Reverse_Ranking,
              data = sales_2)
summary(modele1)
# modeld1's adjusted R-squared value is only .58 
# All variables but Reverse Ranking were significant. 

modele2 <- lm(EU_Sales~Other_Sales + NA_Sales, data = sales_2)
summary(modele2)
# modeld2's adjusted R squared withotu reverse ranking is slightly less at .577
# We can't predict EU sales confidently without Global Sales figures


# Predicting Global Sales
modeld1 <- lm(Global_Sales~ EU_Sales +  Other_Sales + Reverse_Ranking,
              data = sales_2)
summary(modeld1)
# modeld1's adjusted R-squared value is quite high at .8666 
# All variables but Reverse Ranking were significant. 
# This model predicts Global Sales without NA data

modeld2 <- lm(Global_Sales~Other_Sales + NA_Sales + Reverse_Ranking, data = sales_2)
summary(modeld2)
# modeld2's adjusted R squared even without NA and Ranking is 0.9563 - very high!
# all the variables are significant


modeld3 <- lm(Global_Sales~ NA_Sales +  Other_Sales + Reverse_Ranking, 
              Reverse_Product, data = sales_2)
summary(modeld3)
# modeld3's adjusted R-squared value is now 1
# This model predicts Global Sales without EU data, and is the closest. 

modeld4 <- lm(Global_Sales~ NA_Sales +  EU_Sales + Reverse_Ranking + 
                Reverse_Product, data = sales_2)
summary(modeld4)
# Adding reverse ranking only and reverse age takes the Adjusted R-squared of the model from four
# points to 9693, and increases the t-value slightly for the two sales columns  
# but with big numbers at stake, every little bit helps.  
# Unfortunately the predicted values for this assignment do not include Ranking 
# data so we'll adapt the model to exclude that variable. However, we know from 
# our models that with ranking included we can reasnablt 
modeld5 <- lm(Global_Sales~ NA_Sales +  EU_Sales,
              data = sales_2)
summary(modeld5)
# Modeld5, which is the one we will use, still has a very high adjusted R squared
# value (0.9685), and both variables have high t-values of 47 and 32 
###############################################################################


# 4. Predictions based on given values
# 4. Predict global sales based on provided values. Compare your prediction to
#      the observed value(s).
##  - NA_Sales_sum of 34.02 and EU_Sales_sum of 23.80.
##  - NA_Sales_sum of 3.93 and EU_Sales_sum of 1.56.
##  - NA_Sales_sum of 2.73 and EU_Sales_sum of 0.65.
##  - NA_Sales_sum of 2.26 and EU_Sales_sum of 0.97.
##  - NA_Sales_sum of 22.08 and EU_Sales_sum of 0.52.


# Create a new dataframe with the provided values. 
salesForecast <-data.frame(
  NA_Sales = c(34.02, 3.93, 2.73, 2.26, 22.08),
  EU_Sales = c(23.80, 1.56, 0.65, 0.97, 0.52))
salesForecast


salesForecast$Global_Sales <-predict(modeld5,
        newdata=salesForecast)
salesForecast$Other_Sales <- salesForecast$Global_Sales -(salesForecast$NA_Sales
                                                          +salesForecast$EU_Sales)
view(salesForecast)



# 5. Observations and insights
# Your observations and insights here...
# - Global sales can be predicted best when ranking data is included. 
# - Global sales prediction is more dependent on NA than any other variable, 
# which is probably because the NA market is so large (max figure of 43 compared 
# to 23, which is the max of the largest market, EU)
# - including ranking and product (reversed) helps determine the sales, so I would offer that 
# as an option with the model, for when these factors are known. 

###############################################################################
###############################################################################
# points
# How well can we predict loyalty points based on score and income? 
# (the relationship has been shown in the jupiter notebook)

# view correlation of variables to inform how model is built.
# make a copy of the reviews df which has only the numeric columns. 
str(reviews)
reviews_2 <- subset(reviews, select = -c(gender, education, language, 
                                           platform, review, summary))
# Change column names
colnames(reviews_2) <- c("age", "income", "score", "points", "product")

# Convert remaining int columns to num
reviews_2$age <- as.numeric(reviews_2$age)
reviews_2$score <- as.numeric(reviews_2$score)
reviews_2$points <- as.numeric(reviews_2$points)
reviews_2$product <- as.numeric(reviews_2$product)

# View correlation
cor(reviews_2)
corPlot(reviews_2, cex=2)
# Create a new columns for age which is reverse age
reviews_2$age <-  120 - reviews_2$age

# View correlation
str(reviews_2)

cor(reviews_2)
corPlot(reviews_2, cex=2)


# age does have an impact on score
# Try adding reverse product
# reviews_2$Reverse_Product <- 9080 - reviews_2$product
cor(reviews_2)
corPlot(reviews_2, cex=2)
# That has a negative correlation with everyhthing and a 0 correlation with score. 

modelf1 <- lm(points~age + income + score + product + age, data = reviews_2)
summary(modelf1)
# modeld2's adjusted R squared even without NA and Ranking is 0.8399 -  high!
# all the variables are highly significant
# But this is not a perfect predictor of loyalty points. 

modelf2 <- lm(points~ income + score, data = reviews_2)
summary(modelf2)
# modeld2's adjusted R squared even without NA and Ranking is 0.8267 -  high!
# all the variables are highly significant
# we can reasonably predict points based on score and ponits, but the other 
# variables do make a difference

# predict points without score
modelf3 <- lm(points~ age + income +  product + age, data = reviews_2)
summary(modelf3)
# without score we can't predict points




# Dummies

# A/ Can we predict score based on (dummies of?) other columns 
# (ie the non numeric ones, and of course excluding points from the numeric variables
# as it is a dependent variable based on score and income)
# B/ Can we predict global sales values using dummies?

# A/ Predict sales with all the other variables. 
modelg1 <- lm(score~ age + income +  product, data = reviews_2)
summary(modelg1)
str(reviews_3)
# Change column names
reviews_3 <- reviews

colnames(reviews_3) <- c("gender", "age", "income", "score", "points", "education", "language", "platform", "product", "review", "summary")
reviews_3 <- subset(reviews_3, select = -c(language, review, summary))
# Create a df with dummies, dummy values based on mean score of each category
reviews_3 <- reviews_3 %>% mutate(educationD = case_when(
  education =="diploma" ~ 1, 
  education =="PhD" ~ 100,
  education =="postgraduate" ~ 200, 
  education =="graduate" ~ 300,
  education =="Basic" ~ 400, 
  TRUE ~ NA_real_))
reviews_3 <- subset(reviews_3, select = -c(education, platform, gender))
str(reviews_3)
# PS What is the real distribution of education levels in the various populations? 

# Create new values for age which is reverse age
reviews_3$age <-  120 - reviews_3$age
# View correlation
corPlot(reviews_3, cex=2)
modelg2 <- lm(score~ age + income +  product + educationD, data = reviews_3)
summary(modelg1)
# unfortunately it is still very unlikely that we can predict score. 

# B/ predict global sales using other columns as dummies
str(sales)
sales_5 <-sales

# Alter columns values to enable correlation
sales_5$Reverse_Product <- 9080 - sales_5$Product
sales_5$Global_Ranking <- 17000 - sales_5$Ranking
sales_5$Year <- 3000 - sales_5$Year
sales_5$EU_Ranking <- sales_5$EU_Sales
sales_5$NA_RAnking <- sales_5$NA_Sales
sales_5$Other_Ranking <- sales_5$Other_Sales
str(sales_5)
# Determine dummies for publisher values based on mean score (in jupyter notebook:
#                                see evidence / notes in technical report)



#Publisher
#LucasArts                                 39.000000
#Warner Bros. Interactive Entertainment    41.600000
#RedOctane                                 41.800000
#505 Games                                 42.350000
#GT Interactive                            44.700000
#Ubisoft                                   47.536842
#Bethesda Softworks                        48.250000
#Konami Digital Entertainment              48.525000
#Sony Computer Entertainment               49.179894
#Nintendo                                  49.720824
#Activision                                49.725379
#Universal Interactive                     50.000000




sales_5 <- sales_5 %>% mutate(publisherD = case_when(
  Publisher == "LucasArts" ~39.000000,
  Publisher == "Warner Bros. Interactive Entertainment" ~ 41.600000,
  Publisher == "RedOctane" ~ 41.800000,
  Publisher == "505 Games" ~ 42.350000,
  Publisher == "GT Interactive" ~ 44.700000,
  Publisher == "Ubisoft" ~ 47.536842,
  Publisher == "Bethesda Softworks" ~ 48.250000,
  Publisher == "Konami Digital Entertainment" ~ 48.525000,
  Publisher == "Sony Computer Entertainment" ~ 49.179894,
  Publisher == "Nintendo" ~ 49.720824,
  Publisher == "Activision"  ~  49.725379,
  Publisher == "Universal Interactive" ~ 50.000000,
  TRUE ~ NA_real_))
sales_5 <- subset(sales_5, select = -c(Publisher))
str(sales_5)



# As above, for genre and platform
# Genre


sales_5 <- sales_5 %>% mutate(genreD = case_when(
  Genre == "Adventure" ~ 42.275000,
  Genre == "Simulation" ~ 48.508333,
  Genre == "Puzzle" ~ 48.750000,
  Genre == "Racing" ~ 49.759036,
  Genre == "Platform" ~ 49.809524,
  Genre == "Role-Playing" ~ 50.068460,
  Genre == "Sports" ~ 50.240964,
  Genre == "Shooter" ~ 50.339833,
  Genre == "Action" ~ 50.580460,
  Genre == "Strategy" ~ 51.400000,
  Genre == "Fighting"~ 52.289157,
  TRUE ~ NA_real_))
sales_5 <- subset(sales_5, select = -c(Genre))

str(sales_5)

sales_5$'Platfrom'

# Sort platform based on global_sales, using the sales dataset
sales_5 <- sales_5 %>% mutate(platformD = case_when(
  Platform == "PSV" ~ 0.835000,
  Platform == "PC" ~ 1.077000,
  Platform == "WiiU" ~ 1.641000,
  Platform == "GC" ~  1.969091,
  Platform == "XB" ~  2.353333,
  Platform == "PSP" ~ 2.380000,
  Platform == "XOne" ~  2.792500,
  Platform == "GBA" ~ 4.281818,
  Platform == "3DS" ~ 4.575000,
  Platform == "GEN" ~ 4.940000,
  Platform == "PS3" ~ 5.161220,
  Platform == "3DS" ~ 4.575000,
  TRUE ~ NA_real_))
sales_5 <- subset(sales_5, select = -c(Platform, platformD, Product, Ranking))
str(sales_5)
sales_5$genreD<- 100 - sales_5$genreD
# View correlation
cor(sales_5)
corPlot(sales_5, cex=.7)

# Create a model
# GLOBAL SALES
modelh <- lm(Global_Sales ~  Reverse_Product + Year + Global_Ranking
            + publisherD  + EU_Ranking + NA_RAnking + Other_Ranking 
             + genreD, data = sales_5)
modelh
summary(modelh)
$ The aadjusted R squared is one, and the p-value is less than 0.05
# Ranking and PublisherD and GenreD are not significant, but reverse product really is.
# Adjust the model to exclude them
modelh1 <- lm(Global_Sales ~ Year + Reverse_Product+ EU_Ranking + NA_RAnking
              + Other_Ranking, data = sales_5)
modelh1
summary(modelh1)
# The adjusted R-squared value is still near perfect (1)
# All the variables in this model are highly significant. 

# This is an excellent modely for predicting Global Sales
# What about other sales (markets)

# EU SALES
modelh2 <- lm(EU_Sales ~ Year + Reverse_Product+ EU_Ranking + NA_RAnking
              + Other_Ranking, data = sales_5)
modelh2
summary(modelh2)
# WE are warned that there is too perfect a fit (because we have used eu sales 
# to determine eu ranking, which we are in turn using to predict eu sales)
# I believe it's still valid to propose this model, but let's adjust the model 
# to exclude EU Ranking. 
# Reverse product and eu ranking are the most significant variables
modelh3 <- lm(EU_Sales ~ Year + Reverse_Product+ NA_RAnking
              + Other_Ranking, data = sales_5)
modelh3
summary(modelh3)
# now reverse product is not significant, and the forecast reliability drops to 
# .64. Let's adjust again by adding in Genre etc .
modelh4 <- lm(EU_Sales ~ Year + Reverse_Product+ NA_RAnking
              + Other_Ranking + publisherD + genreD, data = sales_5)

modelh4
summary(modelh4)
# Adding those values raises it to .68 which is pretty good but not what we'd hope
# for when looking for something to provide confidence, especially when we have a model that
# at the current time provides strong accuracy. 
# We'd have to recommend that EU_Ranking values are updated regularly based on overall figures 
# Aren't we just saying though, that we can base a product's sales figures on its previous ones? 
# What cautions need to be conveyed along with this model? 






Reverse_Product ##############################################################################
# Add the ranking data to the original exercise for this week to see how ranking 
# can inform correlation in the model. 



# Can I use reverse product to predict score? 


