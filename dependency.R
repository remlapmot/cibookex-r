if (Sys.info()[['sysname']] == "Windows") options(pkgType = "win.binary")

options(repos = "https://cloud.r-project.org")

if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_dev_deps()

if (!tinytex::is_tinytex()) tinytex::install_tinytex()
