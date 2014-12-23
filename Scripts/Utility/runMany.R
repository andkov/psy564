rm(list=ls(all=TRUE))
########## Production of reports from .Rmd files ###

pathReport1 <- base::file.path("./LabLog/03-10-2014-Dementia-Personality-OCTO-Part2.Rmd")
#  Define groups of reports 
allReports<- c(pathReport1)
# Place report paths HERE ###########
pathFilesToBuild <- c(allReports) ##########


testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the reports
for( pathFile in pathFilesToBuild ) {
  #   pathMd <- base::gsub(pattern=".Rmd$", replacement=".md", x=pathRmd)
  rmarkdown::render(input = pathFile, 
                    output_format=c(
                      #                        "pdf_document"
                      #                       ,"md_document"
                      "html_document"
                    ),
                    clean=TRUE)
}

# To generate the static website from the htmls that have been printed in the code above
# 1) Select the "gh-pages" branch of your project's repository in GitHub client
# 2) Open command line terminal and change directory to the root folder of your repository that you've cloned onto your hard drive using GitHub client (type "cmd" in the address line of the File Explorer opened on root folder of your repository's clone)
# 3) type "bundle install" to install Bundler if you're creating the website for the first time
# 4) type "bundle exec jekyll serve" to build site and serve to localhost:4000