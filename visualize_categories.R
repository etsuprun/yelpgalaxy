# This script creates an interactive network visualization of Yelp categories using the ndtv-d3 package.

require(ndtv)
require(magrittr)
load("categories.Rdata")
require(dplyr)
require(RColorBrewer)

# Loading 3 data objects here: 
# businesses -- All our yelp businesses, straight from the Yelp JSOn
# categories -- This has the categories, their star rankings, and other stuff. Proccessed by categories.R.
# category.associations -- Associations of categories across the 61184 businesses. Category 1, category 2, and frequency of association.

# Create a subset of nodes and links
category.association.subset <- category.association[category.association$count >= 20, ]

# Only leave the nodes that are relevant
category.subset <- categories[categories$category %in%
                                (rbind(category.association.subset$category1 %>% as.character,category.association.subset$category2 %>% as.character) %>% as.vector %>% unique), 
                              c("category",
                                "business.count",
                                "mean.stars.reviews",
                                "review.votes.funny.ratio",
                                "review.votes.useful.ratio",
                                "review.votes.cool.ratio"
                                )]

# Create a network where nodes are categories and edges are category associations.
# Color = normalized review score (using the RdYlGn color brewer pallette)
# Size = size of category (number of businesses)

net3 <- network(x = category.association.subset[,c (1,2), ],
                vertex.attr = category.subset,
                directed = F)
colors <- brewer.pal(11, "RdYlGn") 
set.edge.attribute(net3, attrname = "count", value = category.association.subset$count)
set.edge.attribute(net3, attrname = "category1", value = category.association.subset$category1 %>% as.character)
set.edge.attribute(net3, attrname = "category2", value = category.association.subset$category2 %>% as.character)

# Create the visualization and save as an HTML file

verbal.description <- function (x) {
  if (x <= .05) return ("very low")
  else if (x >= .95) return ("very high")
  else if (x <= .25) return ("low")
  else if (x >= .75) return ("high")
  else return ("average")
}


render.d3movie(net3,
   usearrows = F,
   displaylabels = F,
   bg="#000000",
   vertex.border= colors[((percent_rank(category.subset$mean.stars.reviews)) %>% round(1) * 10+1)],
   vertex.col = colors[((percent_rank(category.subset$mean.stars.reviews)) %>% round(1) * 10+1)],
   edge.lwd = net3 %e% "count" %>%  divide_by(3000),
   edge.size = 10,
   vertex.cex = sqrt(net3 %v% "business.count")/150,
   vertex.lwd = .05,
   edge.col="#999999",
   vertex.tooltip = paste0("<b>",
     net3 %v% "category","</b><br><br>",
     round(net3 %v% "business.count" / nrow(businesses) * 100,2),"% of businesses on Yelp<br>",
     "Average Yelp rating: ", 
     round(net3 %v% "mean.stars.reviews",2)," out of 5",

    '<span style="display: block; width: 65px; height: 13px; background: url(star-rating-sprite.png) 0 0;">
       <span style="display: block; width:',
    round(net3 %v% "mean.stars.reviews",2) * 20
    ,'%; height: 13px; background: url(star-rating-sprite.png) 0 -13px;"></span>
    </span><br>',
    "Funniness of reviews: ",sapply(percent_rank(category.subset$review.votes.funny.ratio), verbal.description),"<br>",
    "Usefulness of reviews: ",sapply(percent_rank(category.subset$review.votes.useful.ratio), verbal.description),"<br>",
    "Coolness of reviews: ",sapply(percent_rank(category.subset$review.votes.cool.ratio), verbal.description),"<br>"
    ),
   output.mode = "JSON",
   filename = "vis.json",
   edge.tooltip = paste0(
     round(net3 %e% "count" / sapply(net3 %e% "category1", function (x) category.subset[category.subset$category == x,"business.count"]) * 100,0)
     
     , "% of <b>", net3 %e% "category1", "</b> are <b>", net3 %e% "category2","</b><br>",
     round(net3 %e% "count" / sapply(net3 %e% "category2", function (x) category.subset[category.subset$category == x,"business.count"]) * 100,0)
     , "% of <b>", net3 %e% "category2", "</b> are <b>", net3 %e% "category1","</b><br>"
     
   )
)

