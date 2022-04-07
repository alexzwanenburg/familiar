# This file is used to compile vignettes prior to release.

knitr::opts_knit$set(root.dir = getwd())
knitr::opts_knit$set(base.dir = "vignettes")
knitr::opts_chunk$set(fig.path = "eval_and_explain/")

# Render RMD.orig-files to RMD. This is done for compute-heavy data.
# See https://ropensci.org/blog/2019/12/08/precompute-vignettes/
knitr::knit("vignettes/original/evaluation_and_explanation.Rmd", output = "vignettes/evaluation_and_explanation_precompiled.Rmd")



# Restart the session to flush the temporary directories.
knitr::opts_knit$set(root.dir = getwd())
knitr::opts_knit$set(base.dir = "vignettes")
knitr::opts_chunk$set(fig.path = "prospective_use/")

knitr::knit("vignettes/original/prospective_use.Rmd", output = "vignettes/prospective_use_precompiled.Rmd")

knitr::knit("vignettes/original/introduction.Rmd", output = "vignettes/introduction_precompiled.Rmd")
knitr::knit("vignettes/original/feature_selection.Rmd", output = "vignettes/feature_selection_precompiled.Rmd")
knitr::knit("vignettes/original/learners.Rmd", output = "vignettes/learners_precompiled.Rmd")
knitr::knit("vignettes/original/performance_metrics.Rmd", output = "vignettes/performance_metrics_precompiled.Rmd")

# # Renders vignettes to the docs_github folder in GitHub-flavoured markdown.
# # This is no longer necessary as README.md now points to the vignettes on CRAN.
#
# rmarkdown::render('vignettes/introduction_precompiled.Rmd', output_format="rmarkdown::github_document", output_file='../docs_github/github_introduction.md')
# rmarkdown::render('vignettes/feature_selection_precompiled.Rmd', output_format="rmarkdown::github_document", output_file='../docs_github/github_feature_selection.md')
# rmarkdown::render('vignettes/learners_precompiled.Rmd', output_format="rmarkdown::github_document", output_file='../docs_github/github_learners.md')
# rmarkdown::render('vignettes/performance_metrics_precompiled.Rmd', output_format="rmarkdown::github_document", output_file='../docs_github/github_performance_metrics.md')
# rmarkdown::render("vignettes/prospective_use_precompiled.Rmd", output_format="rmarkdown::github_document", output_file='../docs_github/github_prospective_use.md')
# rmarkdown::render("vignettes/evaluation_and_explanation_precompiled.Rmd", output_format="rmarkdown::github_document", output_file='../docs_github/github_evaluation_and_explanation.md')
# 
# # Copy image files to docs_github
# file.copy("vignettes/eval_and_explain", "docs_github", recursive=TRUE)
# file.copy("vignettes/prospective_use", "docs_github", recursive=TRUE)
