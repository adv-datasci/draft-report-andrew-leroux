rm(list=ls())
## need to change this obviously
# load("~/Desktop/repos.Rdata")
load("../repos.Rdata")

udat <- data[!duplicated(data$repo),]

uid <- unique(data$repo)

nlines <- ncomments <- nblank <- ncharacters_code <- nassign <- nuassign <- nfunc <- nufunc <- nsubset <- rep(NA, length(uid))
for(i in 1:length(uid)){
        tmp       <- subset(data, repo == uid[i])
        
        nlines[i]    <- nrow(tmp)
        ncomments[i] <- sum(tmp$comment_line)
        nblank[i]    <- sum(tmp$blank_line)
        ncharacters_code[i] <- sum(tmp$n_characters[!(tmp$blank_line | tmp$comment_line)])
        
        nassign[i] <- sum(tmp$assignment, na.rm=TRUE)
        nuassign[i] <- length(na.omit(unique(tmp$assignment_name)))
        nsubset[i] <- sum(!is.na(tmp$subset_functions), na.rm=TRUE)
        nfunc[i]   <- sum(!is.na(tmp$named_functions), na.rm=TRUE)
        nufunc[i]  <- length(na.omit(unique(tmp$named_functions)))
}

# scores_cur <- score[1:length(uid)]

par(mfrow=c(2,2))
plot(ncharacters_code, nlines)
hist(ncharacters_code)
hist(nblank)

hist(ncomments)
plot(ncharacters_code, scores_cur)

png("../pairs.png",height=1200,width=1200)
pairs(cbind(scores_cur,nassign,nuassign, nsubset, nfunc, nufunc, ncharacters_code, nlines, ncomments, nblank),col=rgb(0,0,0,0.2))
dev.off()




sum(grepl("BrandonChiazza/getdata-016_courseproject",repo))

which(repo == "BrandonChiazza/getdata-016_courseproject")


