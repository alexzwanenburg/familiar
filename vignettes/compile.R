# This file is used to compile vignettes prior to release.

# Renders vignettes to the docs_github folder in GitHub-flavoured markdown.
rmarkdown::render('vignettes/introduction.Rmd', output_format="rmarkdown::github_document", output_file='../docs_github/introduction.md')
rmarkdown::render('vignettes/feature_selection.Rmd', output_format="rmarkdown::github_document", output_file='../docs_github/feature_selection.md')
