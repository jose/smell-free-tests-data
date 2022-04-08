# ------------------------------------------------------------------------------
# Given a path to one directory or several directories (separated by ','), this
# script recursively searches for *all* 'statistics.csv' files in any given
# directory and aggregates the content of each one in a single zip file.
#
# Usage:
# Rscript get_data.R <directories (separated by ',')> <output file>
#
# ------------------------------------------------------------------------------

library('data.table', lib.loc='../../tools/R/x86_64-pc-linux-gnu-library/3.6.3') # FIXME

args = commandArgs(trailingOnly=TRUE)
if (length(args) < 2) {
  stop("USAGE: get-data.R <directories (separated by ',')> <output file>")
}

# Input
FILE_PATTERN <- "statistics.csv"
DIRECTORIES  <- strsplit(args[1], ",")
# Output
OUTPUT_FILE  <- args[2]

cat("Loading data... ", date(), "\n", sep="")

dataTables <- list(); i <- 1
for (directory in DIRECTORIES) {
  for (table in list.files(directory, recursive=TRUE, full.names=TRUE, pattern=FILE_PATTERN)) {
    cat("Reading: ", table, "\n", sep="")
    dataTables[[i]] <- tryCatch({
      read.csv(table, header=TRUE, stringsAsFactors=FALSE)
    }, error = function(err) {
      return(read.csv(table, header=TRUE, stringsAsFactors=FALSE))
    })
    if (nrow(dataTables[[i]]) != 1) {
      cat(table, " has more/less data rows than expected! Note: only one is expected.\n", sep="")
      quit(save="no", status=1)
    }
    i <- i + 1
  }
}

# Make one data.table from a list of many data.tables
table <- rbindlist(dataTables, use.names=TRUE, fill=TRUE)

cat("Data is loaded. Starting compressing it... ", date(), "\n", sep="")
write.table(table, file=gzfile(OUTPUT_FILE))
cat("Data is compressed and saved. Starting reading it back... ", date(), "\n", sep="")

table <- read.table(gzfile(OUTPUT_FILE), header=TRUE, stringsAsFactors=FALSE)
cat("Data read back. DONE! ", date(), "\n", sep="")

quit(status=0)

# EOF
