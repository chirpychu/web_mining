install.packages("remotes")
remotes::install_github("benjaminguinaudeau/tiktokr")
install.packages("git2r")

library(tiktokr)
library(reticulate)
library(git2r)
use_python(py_config()$python)

tk_install()

tk_auth(cookie = "<s_v_web_id=verify_kucolin3_mWXEvXY3_xzuT_4InM_8aP5_G3thnSQwOtfv; msToken=Jnqe90iDhFMKGRF8qxZQO2AOoZ0lGle1F8gh2WfRfQAOzltjOJStFit6OiMY8BkQY0lJ5ypdAAO4WW1WnV0uhij4yL1xWufF_OUwWKifgkjKuv6hSI5sPpH0jwAlFQeELtrLqKc=>")

tk_auth(docker = T)

tk_init()

if(stringr::str_length(get_docker_signature("")) > 16){
  message("Signature successful. Your Docker container is working.")
} else {
  message("Unable to get signature")
}


#Note: Hashtags query only provides 2k hits, which are not drawn randomly or based on the most recent post date but rather some mix of recent and popular TikToks.
lipstick_hash_post <- tk_posts(scope = "hashtag", query = "lipstick", n = 2000)
makeup_hash_post <- tk_posts(scope = "hashtag", query = "makeup", n = 2000)
cosmetic_hash_post <- tk_posts(scope = "hashtag", query = "cosmetic", n = 2000)
beauty_hash_post <- tk_posts(scope = "hashtag", query = "beauty", n = 2000)
trend2021_hash_post <- tk_posts(scope = "hashtag", query = "trend2021", n = 2000)
trend_hash_post <- tk_posts(scope = "hashtag", query = "trend", n = 2000)
trendmakeup_hash_post <- tk_posts(scope = "hashtag", query = "trendmakeup", n = 2000)
makeup2021_hash_post <- tk_posts(scope = "hashtag", query = "2021makeup", n = 2000)


#
#beauty_hash_post2 <- beauty_hash_post[which(beauty_hash_post$createTime <= 1626910607),]

#cosmetic_test <- tk_posts(scope = "hashtag", query = "cosmetic", n = 5000, start_date =1626910607)

colnames(trendmakeup_hash_post)
colnames(makeup2021_hash_post)

lipstick_hash_post[,62:83] <- NULL
makeup_hash_post[,62:81] <- NULL
cosmetic_hash_post[,62:83] <- NULL
beauty_hash_post[,62:83] <- NULL

trend2021_hash_post[,62:82] <- NULL
trend_hash_post[,62:82] <- NULL
trendmakeup_hash_post[,62:83] <- NULL
makeup2021_hash_post[,62:80] <- NULL


write.csv(lipstick_hash_post, "lipsticktiktok.csv")
write.csv(makeup_hash_post, "makeuptiktok.csv")
write.csv(beauty_hash_post, "beautytiktok.csv")
write.csv(cosmetic_hash_post, "cosmetictiktok.csv")
write.csv(trend2021_hash_post, "trend2021tiktok.csv")
write.csv(trend_hash_post, "trendtiktok.csv")
write.csv(trendmakeup_hash_post, "trendmakeuptiktok.csv")
write.csv(makeup2021_hash_post, "makeup2021tiktok.csv")

#arules mining
data <- character()
files <- list.files("./")
for (file in files){
  if (endsWith(file, ".csv")){
    print(file)
    d <- read.csv(file)
    data <- rbind(data, d)
  }
}

write.csv(data, "tiktok.csv")

tiktok <- read.csv("~/NUS/y3sem1/web mining/projdata/tiktok.csv")
View(tiktok)

tkdesc <- tiktok$desc
#should probably use gsub() to remove urls
tkdesc <- gsub("?(f|ht)(tp)(s?)(://)(.*)[.|/](.*)", "", tkdesc)



#create the DTM
library(textmineR)
tkwordDf <- data.frame(text=c(tkdesc))
tkwordDf_unprocessed <- CreateDtm(doc_vec=tkwordDf$text,doc_names=rownames(tkwordDf), stopword_vec = NULL, lower = F)
dtm <- CreateDtm(doc_vec=tkwordDf$text, doc_names=rownames(tkwordDf))
dtm_mat <- as(dtm, "matrix")
View(dtm_mat)
dtm_mat[dtm_mat > 1] = 1
View(dtm_mat)

colnames(tiktok)
my_data <- tiktok[, c(47:57,60)]
cor(my_data)

library(arules)
dtm <- as.matrix(dtm_mat)
dtm <- as(dtm,"transactions")

summary(dtm)
rules<-apriori(dtm,parameter = list(supp = 0.03, conf = 0.5,minlen = 2))
rules2<-apriori(dtm,parameter = list(supp = 0.03, conf = 0.5,minlen = 2),appearance = list(none = c("??", "?","?")))
summary(rules)
summary(rules2)
liftrules <- sort(rules2, by = "lift")
inspect(liftrules)

countrules <- sort(rules2, by = "count")
inspect(countrules)

class(countrules)
countrules2<-DATAFRAME(countrules) 


library(arulesViz)
plot(rules2,  method = "grouped matrix", engine = "interactive")
plot(countrules,  method = "grouped matrix", engine = "interactive")
plot(rules2, engine = "htmlwidget")

library(dplyr)
#extract purely hashtag words
hashtagetime<- tiktok %>% select(desc,timeconvert)
hashtagetime$desc <- stringr::str_extract_all(hashtagetime$desc, '#\\w+')



lipstickrules <- subset(rules2, items %in% "lipstick")
inspect(lipstickrules)

#I make use of %pin% to used for partial matching. I can see that fyp is being used 2 times more. Also this is very useful to know that when people types about foryoupage and makeup, they will also 2 times more likely to type with fyp. 
fyprules <- subset(rules2, items %pin% "fyp")
inspect(sort(fyprules, by="lift"))

colnames(tiktok)
nrow(tiktok)
# split data
trainingset <- tiktok[1:3932,] # 30%
testingset <- tiktok[3933:19663,] #70% 

#diggcount means the total number of people tht like the video
linearRegmodel <- lm(stats_shareCount~stats_playCount+stats_diggCount+video_duration+stats_commentCount+authorStats_followerCount+authorStats_videoCount+authorStats_heartCount+authorStats_diggCount, data = trainingset)

linearRegmodel.res = resid(linearRegmodel)
plot(linearRegmodel.res)
plot(fitted(linearRegmodel), linearRegmodel.res)
abline(0,0)
summary(linearRegmodel)

model <- glm(stats_shareCount~stats_playCount+stats_diggCount+video_duration+stats_commentCount+authorStats_followerCount+authorStats_videoCount+authorStats_heartCount+authorStats_diggCount,family=binomial(link='logit'),data=trainingset)

mse <- sum(((testingset$stats_shareCount - predict(linearRegmodel, data = testingset))^2))/3923
mse

rmse<- sqrt(mean(linearRegmodel$residuals^2)/3923)
rmse


linearRegmodel2 <- lm(stats_shareCount~stats_diggCount+stats_commentCount+authorStats_followerCount+authorStats_videoCount, data = trainingset)

summary(linearRegmodel2)
tiktok$author_verified<-as.numeric(tiktok$author_verified)
barplot(tiktok$author_verified)
counts <- table(tiktok$author_verified)
barplot(counts, main="Number of Verified/ Non-Verified Users")



