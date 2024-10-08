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
