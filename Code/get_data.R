library("gh")
my_repos <- gh("/users/andrew-leroux/repos", type = "public")
vapply(my_repos, "[[", "", "name")


#install_github("r-lib/gh")
library(dplyr)
library(gh)
### For help navigating the github API: https://developer.github.com/v3/search/

### Only 100 results per page (the max). Change page=1 parameter to get all the repositories.
x <- gh("GET /search/repositories?q=getting+and+cleaning+data&per_page=100", page=1)
repos <- sapply(x[[3]], "[[", "full_name")

### For an example, I will only load in one repo
repo <- repos[5]

### NOTE: Need to create a personal access authentication token for using GET /seach/code!!!
### Do this here: https://github.com/settings/tokens

### !If saving the token to a file like this, make sure it's in your .gitignore!
token <- readLines(".githubtoken")
string <- paste0("GET /search/code?q=repo:", repo,"+extension:r")
res <- gh::gh(string, .token=token)

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





