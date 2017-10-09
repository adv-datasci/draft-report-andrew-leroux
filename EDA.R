rm(list=ls())
## need to change this obviously
# load("~/Desktop/repos.Rdata")
load("../repos.Rdata")

## need to load libraries to find certain functions
library(plyr); library(dplyr); library(knitr); library(markdown); library(readr); library(data.table);


vec_named <-as.vector(data$named_functions)
packages  <- rep(NA_character_, length(vec_named))
inx <- which(!is.na(vec_named))
for(i in inx){
        tmp <- find(vec_named[i], mode="function")
        package_cur <- substring(tmp, first=9)
        if(length(tmp) ==0) {
                # print(vec_named[i])
                packages[i] <- "MISSING"
                next
        } 
        # if(length(tmp) > 1){
                # print("multiple matches");print(package_cur)
        # }
        packages[i] <- package_cur
        
        
}
data$packages <- I(matrix(packages, byrow=FALSE, ncol= ncol(data$named_functions), nrow = nrow(data$named_functions)))

udat <- data[!duplicated(data$repo),]
uid <- unique(data$repo)

nlines <- ncomments <- nblank <- ncharacters_code <- nassign <- nuassign <- nfunc <- nufunc <- nsubset <- npackage <- rep(NA, length(uid))
for(i in 1:length(uid)){
        tmp       <- subset(data, repo == uid[i])
        
        nlines[i]    <- nrow(tmp)
        ncomments[i] <- sum(tmp$comment_line)
        nblank[i]    <- sum(tmp$blank_line)
        ncharacters_code[i] <- sum(tmp$n_characters[!(tmp$blank_line | tmp$comment_line)])
        
        npackage[i] <- length(na.omit(unique(tmp$packages)))
        nassign[i] <- sum(tmp$assignment, na.rm=TRUE)
        nuassign[i] <- length(na.omit(unique(tmp$assignment_name)))
        nsubset[i] <- sum(!is.na(tmp$subset_functions), na.rm=TRUE)
        nfunc[i]   <- sum(!is.na(tmp$named_functions), na.rm=TRUE)
        nufunc[i]  <- length(na.omit(unique(tmp$named_functions)))
        
        if(i %% 100 == 0) print(i)
}

# scores_cur <- score[1:length(uid)]

# scores_inx <- 


## basic pairs plot
png("../pairs.png",height=1200,width=1200)
pairs(cbind(scores_cur,nassign,nuassign, nsubset, nfunc, nufunc, ncharacters_code, nlines, ncomments, nblank),col=rgb(0,0,0,0.2))
dev.off()



## biplot for data
round(cumsum(pca$sdev^2/sum(pca$sdev^2)),3)

X <- cbind(nlines, ncomments, nblank, ncharacters_code, npackage, nassign, nuassign, nsubset, nfunc, nufunc)
pca <- prcomp(X, center=TRUE,scale=TRUE)

X_rm <- X[-1382,]
pca_rm <- prcomp(X_rm, center=TRUE,scale=TRUE)

matplot(pca$rotation,type='l')
biplot(pca, choices=1:2)

png("../biplots.png",height=1200,width=1200)
kmax <- 4
par(mfrow=c(kmax-1,kmax-1),las=1, cex=1.15)
for(k in 1:(kmax-1)){
        nblank_panels <- k-1
        if(nblank_panels > 0) for(m in 1:nblank_panels) frame()
        for(j in (k+1):kmax){
                biplot(pca, choices=c(k,j))
        }
}
dev.off()



summary(lm(scores~))

