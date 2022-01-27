# This file is used to compile vignettes prior to release.

# Render RMD.orig-files to RMD. This is done for compute-heavy data.
# See https://ropensci.org/blog/2019/12/08/precompute-vignettes/
knitr::knit("vignettes/evaluation_and_explanation.Rmd", output = "vignettes/evaluation_and_explanation_precompiled.Rmd")
knitr::knit("vignettes/prospective_use.Rmd", output = "vignettes/prospective_use_precompiled.Rmd")

# Restart the session to render figures to the correct directories.
rstudioapi::restartSession()

# Renders vignettes to the docs_github folder in GitHub-flavoured markdown.
rmarkdown::render('vignettes/introduction.Rmd', output_format="rmarkdown::github_document", output_file='../docs_github/introduction.md')
rmarkdown::render('vignettes/feature_selection.Rmd', output_format="rmarkdown::github_document", output_file='../docs_github/feature_selection.md')
rmarkdown::render('vignettes/learners.Rmd', output_format="rmarkdown::github_document", output_file='../docs_github/learners.md')
rmarkdown::render('vignettes/performance_metrics.Rmd', output_format="rmarkdown::github_document", output_file='../docs_github/performance_metrics.md')
rmarkdown::render("vignettes/prospective_use.Rmd", output_format="rmarkdown::github_document", output_file='../docs_github/prospective_use.md')
rmarkdown::render("vignettes/evaluation_and_explanation.Rmd", output_format="rmarkdown::github_document", output_file='../docs_github/evaluation_and_explanation.md')
