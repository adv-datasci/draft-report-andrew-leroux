rm(list=ls())
library(dplyr)
library(gh)
### For help navigating the github API: https://developer.github.com/v3/search/


token <- readLines("../AdvDataScience_Project1/github_token.txt")[1]

### Only 100 results per page (the max). Change page=1 parameter to get all the repositories.

page <- 1
x <- repos <- c()
## get all repo names
while(!"try-error" %in% class(x)){
        x     <- gh("GET /search/repositories?q=getting+and+cleaning+data&per_page=100",page=page, .token=token)
        repos <- c(repos, vapply(x[[3]], "[[", character(1), "full_name"))
        page <- page + 1
        
        Sys.sleep(5)
}


## loop over recovered repos to get run_analysis.R
for(i in repos){
        repo <- repos[i]
        string <- paste0("GET /search/code?q=repo:", repo,"+extension:r")
        res <- gh::gh(string, .token=token)
        
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







