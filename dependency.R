if (Sys.info()[['sysname']] == "Windows") options(pkgType = "win.binary")

options(repos = "https://cloud.r-project.org")

if (!requireNamespace("pak", quietly = TRUE)) install.packages("pak")
#devtools::install_dev_deps()
pak::local_install_dev_deps()

if (!tinytex::is_tinytex()) tinytex::install_tinytex()
