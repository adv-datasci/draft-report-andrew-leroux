rm(list=ls())
## need to change this obviously
load("~/Desktop/repos.Rdata")


udat <- data[!duplicated(data$repo),]

uid <- unique(data$repo)

nlines <- ncomments <- nblank <- ncharacters_code <- assignemnt <- c()
for(i in uid){
        tmp       <- subset(data, repo == i)
        
        nlines    <- c(nlines,nrow(tmp))
        ncomments <- c(ncomments,sum(tmp$comment_line))
        nblank    <- c(nblank, sum(tmp$blank_line))
        ncharacters_code <- c(ncharacters_code, sum(tmp$n_characters[!(tmp$blank_line | tmp$comment_line)]))
}

par(mfrow=c(2,2))
plot(ncharacters_code, nlines)
hist(ncharacters_code)
hist(nblank)
hist(nlines)
