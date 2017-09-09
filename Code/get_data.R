library("gh")
my_repos <- gh("/users/andrew-leroux/repos", type = "public")
vapply(my_repos, "[[", "", "name")

