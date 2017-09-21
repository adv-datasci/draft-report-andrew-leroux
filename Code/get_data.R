rm(list=ls())
library(dplyr)
library(gh)
library(lubridate)
### For help navigating the github API: https://developer.github.com/v3/search/


token <- readLines("../AdvDataScience_Project1/github_token.txt")[1]

### Only 100 results per page (the max). 
### Search by range of dates created -- 1 week periods. Should be able to grab them all
date_start <- ymd("2016-01-01")        ## start date
day_inc    <- 14                       ## increment days by 14 at a time
dates <- c(); i <- 1
while(date_start < Sys.Date() - (day_inc+1)) {
        dates[[i]] <- c(rep(date_start,2) %m+% c(days(-1),days(day_inc+1)))
        date_start <- date_start + days(day_inc + 1)
        i <- i + 1
}
rm(list=c("date_start","i","day_inc"))

dates <- dates[c(1:3)]

repos <- date_repo <- c()
for(i in 1:length(dates)){
        gh_date        <- paste("created:", paste(dates[[i]], collapse=".."), sep="")
        repo_len_start <- length(repos)
        
        ## get all repo names
        for(k in 1:10){
                gh_get <- paste("GET /search/repositories?q=getting+and+cleaning+data+",gh_date, "&per_page=100", sep="")
                x     <- try(gh(gh_get, page=k, .token=token))
                
                if("try-error" %in% class(x)) break
                
                repos <- c(repos, vapply(x[[3]], "[[", character(1), "full_name"))
                
        }
        delta_repos <- length(repos) - repo_len_start
        date_repo   <- c(date_repo, rep(i, delta_repos))
        
        Sys.sleep(60)
        print(i)
}
rm(list=c("delta_repos","gh_get","x","repo_len_start","gh_date","i"))


## create empty list to hold data
code <- list()
for(i in 1:length(repos)){
        code[[i]] <- NA
}


## loop over recovered repos to get run_analysis.R
for(i in 1:length(repos)){
        repo <- repos[i]
        string <- paste0("GET /search/code?q=repo:", repo,"+extension:r")
        res <- gh::gh(string, .token=token)
        
        ## loop over this -- look for some variant of run_analysis.R
        path <- try(res[[3]][[1]]$path)
        
        if("try-error" %in% class(path)) next
        
        for(k in 1:length(res[[3]])){
                path_cur  <- res[[3]][[k]]$path
                file_name_inx <- c(gregexpr("/[aA-zZ]+?.[rR]",path_cur)[[1]][1], nchar(path_cur))
                file_name <- substr(path_cur, file_name_inx[1]+1, file_name_inx[2])
                
                if(tolower(file_name) != "run_analysis.r") next
                
                has_code <- TRUE
                
        }
        
        if(!has_code) next
        
        code.url <- file.path("https://raw.githubusercontent.com",repo, "master", path_cur)
        code[[i]] <- gsub("\\\\", "", trimws(readLines(code.url)))
        
        
        Sys.sleep(5)
        print(i)
}





get_data <- function(x){
        if(is.na(x)) return(NA)
        
        ## number of lines of code + number of lines of comments only + number of blank lines
        n_lines         <- length(x)
        comment_line    <- grepl("^#", x)
        blank_line      <- (nchar(x)==0)
        n_char_tot      <- vapply(x,nchar,numeric(1))
        
        is_assign <- grepl("^[a-zA-Z]+[0-9]* *<- *", x) | grepl("^[a-zA-Z]+[0-9]* *= *", x)
        ## need to extract only the first bit
        assign_name_loc <-  gregexpr("^[a-zA-Z]+[0-9]*(\\([a-zA-Z]+[0-9]*\\))? *([<-]|[=])",x)
        assign_names <- vapply(1:n_lines, function(y){
                tmp <- assign_name_loc[y][1]
                ## match fail if length is greater than 1
                if(!(tmp[1] %in% 1) | length(tmp[1]) > 1) return(NA_character_)
                
                # name_only <- gregexpr("^[a-zA-Z]+[0-9]*",x[y])
                name_only <- gregexpr("^[a-zA-Z]+[0-9]*(\\([a-zA-Z]+[0-9]*\\))?",x[y])
                trimws(substr(x[y], 1, attributes(name_only[[1]])$match.length))
        },character(1))        
        
        assignment_object <- ifelse(grepl("\\(",assign_names),0,1)
        assignment_object[is.na(assign_names)] <- NA

        ###########################################
        ## get names of top-level functions used ##
        ##   note: this will not capture fns     ##
        ##     used in side apply type fns       ##   
        ###########################################
        
        ## create a duplicate of "x" which replaces [ with white space
        ## note this will grab things like mean() which we dont want! try to fix later
        x_wo_sub <- gsub("\\[", "       ", x)
        # named_fn_loc <- gregexpr("[A-Za-z]+[1-9]*.{0,1}[A-Za-z]+[1-9]*\\(",x_wo_sub)
        
        named_fn_loc <- gregexpr("\\[*[A-Za-z]+[1-9]*.{0,1}[A-Za-z]+[1-9]*\\(",x_wo_sub)
        ## subsetting includes $ and [!
        subset_fn_loc <- gregexpr("( *\\[+ *[A-Za-z]+[1-9]*)|(\\$[A-Za-z]+)", x)
        
        max_named_func <- max(vapply(named_fn_loc,length, numeric(1)))
        max_sub_func   <- max(vapply(subset_fn_loc,length, numeric(1)))
        
        mat_named_func <- matrix(NA, ncol=max_named_func, nrow=n_lines)
        mat_sub_func   <- matrix(NA, ncol=max_sub_func, nrow=n_lines)
        
        for(n in 1:n_lines){
                tmp     <- named_fn_loc[n][[1]]
                tmp_sub <- subset_fn_loc[n][[1]]
                
                if(tmp[1] != -1){
                        for(k in 1:length(tmp)){
                                mat_named_func[n,k] <- substr(x_wo_sub[n], tmp[k], 
                                                              tmp[k]+attributes(tmp)$match.length[k]-2)
                        }
                }
                if(tmp_sub[1] != -1){
                        for(k in 1:length(tmp_sub)){
                                mat_sub_func[n,k] <- substr(x[n], tmp_sub[k], tmp_sub[k])
                        }
                }
        }
        
        ret <- data.frame("line" = c(1:n_lines),
                          "blank_line" = blank_line,
                          "comment_line" = comment_line,
                          "n_characters" = n_char_tot,
                          "assignment" = is_assign,
                          "assignment_name" = assign_names,
                          "assignment_object" = assignment_object,
                          "named_functions" = I(mat_named_func),
                          "subset_functions" = I(mat_sub_func))     
        
        ret
}



test <- sapply(code, get_data)

test <- c()

for(i in 1:length(code)){
        test[[i]] <- get_data(code[[i]])
}

## get number of lines of code
## get number of commented lines of code
## get number of total characters
## get number of characters in commented code
## get assignment operators
##    - distinguish between <- and = 
## get unique functions
## get unique variable names
##    - capture number of times write to the same name
## get number of 








