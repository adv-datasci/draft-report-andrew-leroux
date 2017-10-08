rm(list=ls())
library(dplyr)
library(gh)
library(lubridate)
### For help navigating the github API: https://developer.github.com/v3/search/


token <- readLines("../AdvDataScience_Project1/github_token.txt")[1]

### Only 1000 results per page (the max). 
### Search by range of dates created -- 1 week periods. Should be able to grab them all
date_start <- ymd("2007-12-01")        ## start date
day_inc    <- 14                       ## increment days by 14 at a time
dates <- c(); i <- 1
while(date_start < Sys.Date() - (day_inc+1)) {
        dates[[i]] <- c(rep(date_start,2) %m+% c(days(-1),days(day_inc+1)))
        date_start <- date_start + days(day_inc + 1)
        i <- i + 1
}
rm(list=c("date_start","i","day_inc"))


repos <- branch <- score <- date_repo <- c()
for(i in 1:length(dates)){
        gh_date        <- paste("created:", paste(dates[[i]], collapse=".."), sep="")
        repo_len_start <- length(repos)
        
        ## get all repo names
        for(k in 1:10){
                gh_get <- paste("GET /search/repositories?q=getting+and+cleaning+data+",gh_date, "&per_page=100", sep="")
                x     <- try(gh(gh_get, page=k, .token=token))
                
                if("try-error" %in% class(x)) break
                
                repos  <- c(repos, vapply(x[[3]], "[[", character(1), "full_name"))
                branch <- c(branch, vapply(x[[3]], "[[", character(1), "default_branch"))
                score  <- c(score, vapply(x[[3]], "[[", numeric(1), "score"))
                
        }
        delta_repos <- length(repos) - repo_len_start
        date_repo   <- c(date_repo, rep(i, delta_repos))
        
        Sys.sleep(60)
        print(dates[i]);print(length(repos))
}
rm(list=c("delta_repos","gh_get","x","repo_len_start","gh_date","i"))

# save.image("../repos.Rdata")


## create empty list to hold data
code <- list()
for(i in 1:length(repos)){
        code[[i]] <- NA
}


## loop over recovered repos to get run_analysis.R
## issue with "jamescooksley/GettingandCleaningDataCourseProject" repo number 5349
for(i in 19050:length(repos)){
        repo <- repos[i]
        string <- paste0("GET /search/code?q=repo:", repo,"+extension:r")
        res <- try(gh::gh(string, .token=token))
        
        if("try-error" %in% class(res)) next
        
        ## loop over this -- look for some variant of run_analysis.R
        path <- try(res[[3]][[1]]$path)
        
        if("try-error" %in% class(path)) next
        
        has_code <- FALSE
        for(k in 1:length(res[[3]])){
                path_cur  <- res[[3]][[k]]$path
                file_name_inx <- c(gregexpr("/[aA-zZ]+?.[rR]",path_cur)[[1]][1], nchar(path_cur))
                file_name <- substr(path_cur, file_name_inx[1]+1, file_name_inx[2])
                
                
                
                if(tolower(file_name) != "run_analysis.r") next
                
                has_code <- TRUE
                break
                
        }
        
        if(!has_code) next
        
        ## THIS IS A PROBLEM IF SOMEONE DECIDES TO CHANGE THE NAME OF THEIR MASTER REPO
        code.url <- file.path("https://raw.githubusercontent.com",repo, branch[i], path_cur)
        
        code.url <- gsub(" ", "%20", code.url)
        code[[i]] <- try(gsub("\\\\", "", trimws(readLines(code.url))))
        
        
        Sys.sleep(sample(4:10,size=1))
        print(i)
}





## BAD:::  https://github.com/uanyanti/gettingandcleaningdata/
## try on nchar() is to exclude bad encoding files -- limitaiton



get_data <- function(x){
        if(length(x) == 1 & !is.finite(x[1])) return(NA)
        if(length(x) == 0) return(NA)
        
        ## number of lines of code + number of lines of comments only + number of blank lines
        n_lines         <- length(x)
        comment_line    <- grepl("^#", x)
        blank_line      <- try(nchar(x)==0)
        
        if("try-error" %in% class(blank_line)) return(NA)
        
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
        
        ## this works almost, but we really want to capture the quotations
        ## can't do this explicitl, use gsub "hack"
        # named_fn_loc <- gregexpr("\\[*[A-Za-z]+[1-9]*.{0,1}[A-Za-z]+[1-9]*\\([^)]",x_wo_sub) 
        
        fn_patt <- "[A-Za-z]+[0-9]*.{0,1}[A-Za-z]+[0-9]*\\("
        named_fn_loc <- gregexpr(paste("(^",fn_patt,")","|([,|=|-| |^]{1} *[^\"]\\[*", fn_patt,")", sep=""),x_wo_sub)
        # named_fn_loc <- gregexpr("[,|=|-| |^]{1} *[^\"]\\[*[A-Za-z]+[0-9]*.{0,1}[A-Za-z]+[0-9]*\\([^)]",x_wo_sub)
        
        ## subsetting includes $ and [
        subset_fn_loc <- gregexpr("( *\\[+ *[A-Za-z]+[0-9]*)|(\\$[A-Za-z]+)", x)
        
        
        
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
        # str(mat_named_func)
        
        
        ret <- data.frame("line" = c(1:n_lines),
                          "blank_line" = blank_line,
                          "comment_line" = comment_line,
                          "n_characters" = n_char_tot,
                          "assignment" = is_assign,
                          "assignment_name" = assign_names,
                          "assignment_object" = assignment_object,
                          "named_functions" = I(mat_named_func),
                          "subset_functions" = I(mat_sub_func),
                          stringsAsFactors = FALSE)     
        
        ret
}


## get features of the data from raw code
processed_list <- sapply(code, get_data)

## get max number of columns for named and subset funcitons, respectively 
max_fns        <- vapply(seq_along(processed_list), function(x){
        if(!is.finite(processed_list[[x]][[1]])) return(c(NA,NA))
        c(ncol(processed_list[[x]]$named_functions),ncol(processed_list[[x]]$subset_functions))
}, numeric(2))
max_named <- max(max_fns[1,], na.rm=TRUE)
max_subset<- max(max_fns[2,], na.rm=TRUE)



## create our final, tidy data matrix
data       <- c()
named_mat  <- c()
subset_mat <- c()

for(i in 1:length(code)){
        if(is.na(code[[i]][1]) | is.na(processed_list[[i]])[1]) next
        
        tmp  <- data.frame("repo"=repos[i], 
                           "date"=date_repo[i],
                           "score"=score[i],
                           processed_list[[i]], 
                           stringsAsFactors = FALSE)
        data <- rbind.data.frame(data, tmp[,-c(11:12)])
        
        
        d_named <- max_named - ncol(tmp[,11])
        d_subset <- max_subset - ncol(tmp[,12])
        if(d_named > 0) for(j in 1:d_named) tmp[,11] <- cbind(tmp[,11], NA)
        if(d_subset > 0) for(j in 1:d_subset) tmp[,12] <- cbind(tmp[,12], NA)


        named_mat <- rbind.data.frame(named_mat, unclass(tmp[,c(11)]),stringsAsFactors = FALSE)
        subset_mat <- rbind.data.frame(subset_mat, unclass(tmp[,c(12)]),stringsAsFactors = FALSE)
        
        print(i)
}

data$named_functions <- I(as.matrix(named_mat))
data$subset_functions <- I(as.matrix(subset_mat))




