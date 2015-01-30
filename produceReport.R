rm(list=ls(all=TRUE))
########## Production of reports from .Rmd files ###

pathFilesToBuild <- base::file.path("./4.Rmd")






testit::assert("The knitr Rmd files should exist.", base::file.exists(pathFilesToBuild))
# Build the report
for( pathFile in pathFilesToBuild ) {
  #   pathMd <- base::gsub(pattern=".Rmd$", replacement=".md", x=pathRmd)
  rmarkdown::render(input = pathFile, 
                    output_format=c(
                      #                        "pdf_document"
                      #                       ,"md_document"
                                            "html_document"
#                       "ioslides_presentation"
                    ),
                    clean=TRUE)
}
