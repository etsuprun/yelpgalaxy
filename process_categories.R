# This script aggregates information about review stars, dates, and votes by category.
# It could take an hour or more to run.

require(magrittr)
require(plyr)

# Get data from JSON

businesses <- jsonlite::stream_in(file("yelp_dataset_challenge_academic_dataset//yelp_academic_dataset_business.json"))
reviews <- jsonlite::stream_in(file("yelp_dataset_challenge_academic_dataset//yelp_academic_dataset_review.json"))


# Associate reviews with categories.

reviews <- join(reviews, businesses[,c("business_id","categories")], by="business_id",match="first")

# Name dataframe columns and allocate memory space for the large matrix

category.size <- reviews$categories %>% unlist %>% length
reviews.long <- cbind(
  category = rep(NA, times = category.size),
  business_id = rep(NA, times = category.size),
  stars = rep(NA, times = category.size),
  date = rep(NA, times = category.size),
  votes.funny = rep(NA, times = category.size),
  votes.useful = rep(NA, times = category.size),
  votes.cool = rep(NA, times = category.size)
) %>% as.matrix


# Create a dataframe where each row is a review/category pair

j <- 1
for (i in 1:nrow(reviews)) {
  sapply(unlist(reviews$categories[i]), function (x) {
    reviews.long[j,1] <<- x
    reviews.long[j,2] <<- reviews$business_id[i]
    reviews.long[j,3] <<- reviews$stars[i]
    reviews.long[j,4] <<- reviews$date[i]
    reviews.long[j,5] <<- reviews$votes$funny[i]
    reviews.long[j,6] <<- reviews$votes$useful[i]
    reviews.long[j,7] <<- reviews$votes$cool[i]
    j <<- j + 1
  })
}


# Convert to dataframe

reviews.long <- data.frame(reviews.long)

# Create a dataframe with categories

categories <- businesses$categories %>%
  unlist %>%
  table %>%
  as.data.frame %>%
  setNames(c("category","business.count"))

# Remove the categories with fewer than 10 businesses

categories <- categories[categories$business.count >= 10, ]
categories$category <- categories$category %>% as.character

# Calculate star distribution by categories

categories$stars.1 <- sapply(categories$category, function (x) {
  reviews.long$stars[reviews.long$category==x & reviews.long$stars==1] %>% length  
})

categories$stars.2 <- sapply(categories$category, function (x) {
  reviews.long$stars[reviews.long$category==x & reviews.long$stars==2] %>% length  
})

categories$stars.3 <- sapply(categories$category, function (x) {
  reviews.long$stars[reviews.long$category==x & reviews.long$stars==3] %>% length  
})

categories$stars.4 <- sapply(categories$category, function (x) {
  reviews.long$stars[reviews.long$category==x & reviews.long$stars==4] %>% length  
})

categories$stars.5 <- sapply(categories$category, function (x) {
  reviews.long$stars[reviews.long$category==x & reviews.long$stars==5] %>% length  
})

categories$mean.stars.reviews <- (categories$stars.1 + categories$stars.2*2 + categories$stars.3*3 +
                                    categories$stars.4*4 + categories$stars.5*5) /
  (categories$stars.1+categories$stars.2+categories$stars.3+categories$stars.4+categories$stars.5)

categories$review.count <- categories$stars.1 + categories$stars.2 + categories$stars.3 + categories$stars.4 + categories$stars.5

# Remove categories with few reviews

categories <- categories[categories$review.count>=10, ]

# Calculate percentages of reviews by star

categories$stars.1.percent <- (categories$stars.1 / categories$review.count *100) %>% round(2)
categories$stars.2.percent <- (categories$stars.2 / categories$review.count *100) %>% round(2)
categories$stars.3.percent <- (categories$stars.3 / categories$review.count *100) %>% round(2)
categories$stars.4.percent <- (categories$stars.4 / categories$review.count *100) %>% round(2)
categories$stars.5.percent <- (categories$stars.5 / categories$review.count * 100) %>% round(2)

# Sum up review attributes

categories$review.votes.funny <- sapply(categories$category, function (x) {
  reviews.long$votes.funny[reviews.long$category==x] %>% as.numeric %>% sum
})

categories$review.votes.funny.ratio <- categories$review.votes.funny / categories$review.count

categories$review.votes.useful <- sapply(categories$category, function (x) {
  reviews.long$votes.useful[reviews.long$category==x] %>% as.numeric %>% sum
})

categories$review.votes.useful.ratio <- categories$review.votes.useful / categories$review.count

categories$review.votes.cool <- sapply(categories$category, function (x) {
  reviews.long$votes.cool[reviews.long$category==x] %>% as.numeric %>% sum
})

categories$review.votes.cool.ratio <- categories$review.votes.cool / categories$review.count

# Create a dataframe showing relationships between categories. Let's say there's a business belonging
# to three categories: Restaurants, Fast Food, and Pizza. We will add 3 rows to reflect the 3 associations:
# Restaurants<->Fast Food, Restaurants <-> Pizza, and Pizza <-> Fast Food. Then, we will count up the number
# of unique associations.

category.association <- sapply(businesses$categories, function (x) {
  if (length(x) > 1) {
    combn (x, 2) %>% t
  }
})
category.association <- do.call("rbind", category.association) %>%
  apply(MARGIN = 1, FUN = sort) %>%
  t %>%
  as.data.frame %>%
  plyr::count(c("V1", "V2")) %>%
  setNames(c("category1","category2","count"))

# Create an abbreviated version of the categories dataframe

abbreviated.categories <- categories[, c(1, 2, 8:14)]
abbreviated.categories$mean.stars.reviews <- abbreviated.categories$mean.stars.reviews %>% round(2)


save.image("categories.Rdata")
save("abbreviated.categories", file = "abbreviated.categories.Rdata")
