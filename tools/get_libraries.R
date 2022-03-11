# R repository
repository <- 'http://cran.us.r-project.org'
# Local lib
platform      <- R.Version()$'platform'
version       <- paste(R.Version()[c('major', 'minor')], collapse='.')
local_library <- paste('~/R/', platform, '-library/', version, sep='')
if (!file.exists(local_library)) {
  dir.create(local_library, showWarnings=TRUE, recursive=TRUE)
}
# Install packages
install.packages('this.path', lib=local_library, repos=repository)
install.packages('foreach', lib=local_library, repos=repository)
install.packages('doParallel', lib=local_library, repos=repository)
install.packages('extrafont', lib=local_library, repos=repository)
install.packages('stringr', lib=local_library, repos=repository)
install.packages('boot', lib=local_library, repos=repository)
install.packages('data.table', lib=local_library, repos=repository)
install.packages('ggplot2', lib=local_library, repos=repository)
install.packages('RColorBrewer', lib=local_library, repos=repository)
install.packages('reshape2', lib=local_library, repos=repository)
install.packages('hexbin', lib=local_library, repos=repository)
# Load libraries (aka runtime sanity check)
library('this.path', lib.loc=local_library)
library('foreach', lib.loc=local_library)
library('doParallel', lib.loc=local_library)
library('extrafont', lib.loc=local_library)
library('stringr', lib.loc=local_library)
library('boot', lib.loc=local_library)
library('data.table', lib.loc=local_library)
library('ggplot2', lib.loc=local_library)
library('RColorBrewer', lib.loc=local_library)
library('reshape2', lib.loc=local_library)
library('hexbin', lib.loc=local_library)
# Exit
quit(save='no', status=0)
