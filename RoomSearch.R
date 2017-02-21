install.packages("Rfacebook") 
library("Rfacebook")

my_oauth <- fbOAuth(app_id="xx", app_secret="xx")
save(my_oauth, file = "my_oauth")

#load("my_oauth")

# Get 20 posts from a public group
postsGroup <- getGroup("xx", token = my_oauth, n = 20)

# Keep all posts regarding studios
positionsStudio <- grep('studio', postsRoom$message, ignore.case = TRUE)
postsStudio <- postsGroup[positionsStudio,]

# Function for filtering out posts with certain keywords
filterOutPostsWithSentence <- function(filterSentence, posts) {
  positionsOfPostsWithSentence <- grep(filterSentence, posts$message, ignore.case = TRUE)
  if (length(positionsOfPostsWithSentence) != 0) {
    posts <- posts[-positionsOfPostsWithSentence,]
  }
  return(posts)
}

# Filter out girls and students only
posts <- filterOutPostsWithSentence("girls only", postsStudio)
posts <- filterOutPostsWithSentence("only for girls", posts)
posts <- filterOutPostsWithSentence("students only", posts)
posts <- filterOutPostsWithSentence("only for students", posts)

# Remove posts with no fb link
positionsNoLink <- is.na(posts2$link)
posts <- posts[!positionsNoLink,]

# Show the most recent posts on top
posts <- posts[order(posts$created_time, decreasing = TRUE),]

# Remove unnecessary information
posts <- posts[,c(-1,-2,-5,-7:-9,-11)]

# My best attempt at finding the rent price using a regular expression
price <- lapply(posts$message, function (x) grep("€?[0-9]{3}\\,?[0-9]{0,2}", unlist(strsplit(x, " |\\n")), value = TRUE)[1])
posts <- cbind(posts, Price = unlist(price))