# devtools::install_github("mkearney/rtweet")
library(rtweet)

appname <- "test"

key <- "mtkB3uos1T7Oadwo2n81sziaS"
secret <- "wDA9pAknxlBdGW9m42Ip9xVkO49WlJmhkeXdqLFDqH9nOc8Urr"

twitter_token <- rtweet::create_token(app = appname,
                                       consumer_key = key,
                                       consumer_secret = secret)

result <- rtweet::search_tweets("trump", n = 1000, token = twitter_token)
result <- rtweet::get_timeline(user = "realDonaldTrump", n = 1000, token = twitter_token)

for (i in 1:nrow(result)) {
    i = 1
    if (!is.na(result$media_url[i])) {
        download.file(result$media_url[i],
                      destfile = paste("./images/image_", i, ".jpg", sep = ""),
                      quiet = T)
    }
}
