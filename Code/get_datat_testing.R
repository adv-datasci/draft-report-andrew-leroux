rm(list=ls())
library(dplyr)
library(gh)
library(lubridate)
### For help navigating the github API: https://developer.github.com/v3/search/


token <- readLines("../AdvDataScience_Project1/github_token.txt")[1]

### Only 100 results per page (the max). 
### Search by range of dates created -- 1 week periods. Should be able to grab them all


date_start <- ymd("2015-01-01")        ## start date
day_inc    <- 14                       ## increment days by 14 at a time
dates <- c(); i <- 1
while(date_start < Sys.Date() - (day_inc+1)) {
        dates[[i]] <- c(rep(date_start,2) %m+% c(days(-1),days(day_inc+1)))
        date_start <- date_start + days(day_inc + 1)
        i <- i + 1
}
rm(list=c("date_start","i","day_inc"))


print(length(dates))

repos <- c()
for(i in 1:length(dates)){
        page <- 1
        x <- c()
        gh_date <- paste("created:", paste(dates[[i]], collapse=".."), sep="")
        ## get all repo names
        for(k in 1:10){
                gh_get <- paste("GET /search/repositories?q=getting+and+cleaning+data+",gh_date, "&per_page=100", sep="")
                x     <- try(gh(gh_get, page=k, .token=token))
                
                if("try-error" %in% class(x)) break
                
                repos <- c(repos, vapply(x[[3]], "[[", character(1), "full_name"))
                
        }
        Sys.sleep(60)
        print(i)
}

## loop over recovered repos to get run_analysis.R
for(i in repos){
        repo <- repos[i]
        string <- paste0("GET /search/code?q=repo:", repo,"+extension:r")
        res <- gh::gh(string, .token=token)
        
        path <- try(res[[3]][[1]]$path)
        
        if("try-error" %in% class(path)) next
        code.url <- file.path("https://raw.githubusercontent.com",repo, "master", path)
        
        code <- code.url %>% readLines()
        head(code)
        
}





### This assumes there will only be a single .R file (run_analysis.R).
### You will run into trouble if the user has multiple .R files
### Some users (like the most popular in repos[1]) have their work in an .Rmd that
### is sourced by run_analysis.R. Think how you would handle that too.
path <- res[[3]][[1]]$path
code.url <- file.path("https://raw.githubusercontent.com",repo, "master", path)

code <- code.url %>% readLines()
head(code)

### number of commented lines
sum(grepl("^#", code))

execode <- code[!grepl("^#", code) & code != ""]
### lines of executable code
length(execode)

### What libraries are used?
execode[grep("library\\(", execode)]



## Step 1: Get all the .R files
##         Count number of individuals who this fails for
##         

### Create a function which will
## 1) Extract librarys used (using either library or require)







