# tests/APImemTables/memtables_r.R
#
# Check FVS memory mode against the normal SQLite file output, from R.
#
# Covers fvsSetMemoryTables and the fvsTable* API in dbsqlite/dbstables.f,
# called directly with .Fortran(), passing table names and buffers as raw
# vectors. No rFVS functions or .C() shims are used for the table routines.
# memtables_ctypes.py makes the same checks from Python.
#
# Runs tests/FVSie/DBReportTest.key twice, each in its own R process (FVS
# keeps global state, so a process runs only one mode):
#
# - file: memory mode off; FVS writes DBReportTest.db as usual.
# - mem: memory mode on, stopping at every cycle (stop point 6) and at the
#   end of each stand; at each stop every table is read through the API and
#   then cleared.
#
# The checks, one function each:
#
# - checkTablesMatch: every table has the same columns, declared types and
#   rows as the file (ignoring CaseID, the FVS_Cases run time and row order).
# - checkOneCasePerStand: each stand gets its own case.
# - checkNoDbFiles: memory mode writes no .db file.
# - checkOutUnchanged: the .out file is the same in both modes.
#
# Usage, from any directory (needs R with RSQLite):
#   Rscript tests/APImemTables/memtables_r.R [path/to/FVSie.so]
# The library defaults to bin/FVSie.so. Exits non-zero on any failure.

suppressPackageStartupMessages(library(RSQLite))

script <- normalizePath(sub("^--file=", "",
  grep("^--file=", commandArgs(FALSE), value = TRUE)))
root <- normalizePath(file.path(dirname(script), "..", ".."))
keydir <- file.path(root, "tests", "FVSie")
key <- "DBReportTest.key"
ignoreCols <- c("CaseID", "RunDateTime")

# ------------------------------------------------------------------- checks

#' Check that memory mode produces the same tables as the file output.
#'
#' Every table must exist in both, with the same columns, the same declared
#' types (from fvsTableColumns) and the same rows. CaseID and the FVS_Cases
#' run time differ between runs and are ignored, as is row order. NULL is
#' compared equal to NaN and to empty text, which is how the API returns it.
#'
#' @param fileDb Database FVS wrote with memory mode off.
#' @param memDb The tables read through the API in memory mode, as written by
#'   .writeDb.
#' @return One message per difference; empty if the tables match.
checkTablesMatch <- function(fileDb, memDb) {
  fcon <- dbConnect(SQLite(), fileDb)
  mcon <- dbConnect(SQLite(), memDb)
  on.exit({dbDisconnect(fcon); dbDisconnect(mcon)})
  fnames <- dbListTables(fcon)
  mnames <- dbListTables(mcon)
  errors <- character(0)
  if (!setequal(fnames, mnames)) {
    errors <- c(errors, sprintf("tables differ: only file [%s], only mem [%s]",
      toString(setdiff(fnames, mnames)), toString(setdiff(mnames, fnames))))
  }
  for (name in sort(intersect(fnames, mnames))) {
    fcols <- .columnTypes(fcon, name)
    mcols <- .columnTypes(mcon, name)
    if (!setequal(names(fcols), names(mcols))) {
      errors <- c(errors, sprintf("%s: columns differ [%s]", name,
        toString(union(setdiff(names(fcols), names(mcols)),
                       setdiff(names(mcols), names(fcols))))))
      next
    }
    if (!identical(fcols[names(mcols)], mcols)) {
      errors <- c(errors, sprintf("%s: declared types differ", name))
      next
    }
    keep <- setdiff(names(fcols), ignoreCols)
    frows <- .rows(fcon, name, keep)
    mrows <- .rows(mcon, name, keep)
    if (!identical(frows, mrows)) {
      errors <- c(errors, sprintf("%s: rows differ (%d file, %d mem)", name,
                                  length(frows), length(mrows)))
    } else {
      cat(sprintf("ok  %s: %d rows\n", name, length(frows)))
    }
  }
  errors
}

#' Check that memory mode gives each stand its own case.
#'
#' The in-memory database stays open for the whole run, so cases from all
#' stands accumulate in it; each must have a distinct CaseID.
#'
#' @param memDb The tables read through the API in memory mode.
#' @return A message if the number of distinct CaseIDs in FVS_Cases differs
#'   from the number of stands in the keyword file; otherwise empty.
checkOneCasePerStand <- function(memDb) {
  con <- dbConnect(SQLite(), memDb)
  on.exit(dbDisconnect(con))
  ncases <- dbGetQuery(con, 'SELECT count(DISTINCT CaseID) FROM "FVS_Cases"')[[1]]
  nstands <- sum(toupper(trimws(readLines(file.path(keydir, key)))) == "PROCESS")
  if (ncases == nstands) character(0)
  else sprintf("FVS_Cases: %d cases for %d stands", ncases, nstands)
}

#' Check that memory mode writes no output database file.
#'
#' DBReportTest.key names an output database with DSNOut for every stand;
#' in memory mode none of them may be created.
#'
#' @param memDir Work directory of the memory-mode run.
#' @return A message naming any .db file other than the input database and
#'   mem_tables.db; otherwise empty.
checkNoDbFiles <- function(memDir) {
  stray <- setdiff(list.files(memDir, pattern = "\\.db$"),
                   c("FVS_Data.db", "mem_tables.db"))
  if (length(stray)) sprintf("memory mode wrote database files: %s", toString(stray))
  else character(0)
}

#' Check that memory mode doesn't change the main output (.out) file.
#'
#' @param fileOut .out file from the file-mode run.
#' @param memOut .out file from the memory-mode run.
#' @return A message if the files differ, ignoring dates and times;
#'   otherwise empty.
checkOutUnchanged <- function(fileOut, memOut) {
  strip <- function(p) gsub("[0-9]{2}[-/:][0-9]{2}[-/:][0-9]{2,4}", "",
                            readLines(p, warn = FALSE), useBytes = TRUE)
  if (identical(strip(fileOut), strip(memOut))) character(0)
  else ".out files differ"
}

# ------------------------------------------------------- running FVS (private helpers)

#' Run DBReportTest.key in the current directory in "file" or "mem" mode.
#'
#' In "mem" mode, read and clear every table at each stop, then write the
#' rows to mem_tables.db with the declared types from fvsTableColumns.
.run <- function(lib, mode) {
  dyn.load(lib)
  if (mode == "mem") {
    stopifnot(.Fortran("fvsSetMemoryTables", 1L, rtnCode = 0L)$rtnCode == 0)
  }
  cmd <- paste0("--keywordfile=", key)
  stopifnot(.C("CfvsSetCmdLine", cmd, nchar(cmd), 0L)[[3]] == 0)
  if (mode == "mem") {  # after fvsSetCmdLine, which resets the stop points
    invisible(.Fortran("fvsSetStoppointCodes", 6L, -1L))
  }
  tables <- list()
  repeat {
    if (.Fortran("fvs", 0L)[[1]] != 0) break
    if (mode == "mem") tables <- .drain(tables)
  }
  if (mode == "mem") .writeDb(.drain(tables), "mem_tables.db")
}

#' Append every table's column types and rows to tables, then clear it in FVS.
.drain <- function(tables) {
  for (name in .tableList()) {
    t <- .readTable(name)
    old <- tables[[name]]
    # FVS_Compute can gain columns between stands
    types <- c(old$types, t$types[setdiff(names(t$types), names(old$types))])
    tables[[name]] <- list(types = types, rows = c(old$rows, list(t$rows)))
    rc <- .Fortran("fvsClearTable", charToRaw(name), nchar(name),
                   rtnCode = 0L)$rtnCode
    stopifnot(rc == 0)
  }
  tables
}

#' Table names from fvsTableList.
.tableList <- function() {
  r <- .Fortran("fvsTableList", raw(4096), 4096L, ntables = 0L, rtnCode = 0L)
  stopifnot(r$rtnCode == 0)
  .split0(r[[1]], r[[2]])
}

#' Declared type of each column (numeric columns first) and every row of a
#' table, as list(types, rows); rows is a data.frame in the same column order.
.readTable <- function(name) {
  nm <- charToRaw(name)
  nch <- nchar(name)
  d <- .Fortran("fvsTableDims", nm, nch, nrow = 0L, nnum = 0L, ntxt = 0L,
                rtnCode = 0L)
  stopifnot(d$rtnCode == 0)
  cols <- .Fortran("fvsTableColumns", nm, nch, raw(4096), 4096L, raw(4096),
                   4096L, raw(4096), 4096L, raw(4096), 4096L, rtnCode = 0L)
  stopifnot(cols$rtnCode == 0)
  numcols <- .split0(cols[[3]], cols[[4]])
  txtcols <- .split0(cols[[5]], cols[[6]])
  types <- setNames(c(.split0(cols[[7]], cols[[8]]), .split0(cols[[9]], cols[[10]])),
                    c(numcols, txtcols))
  stopifnot(length(numcols) == d$nnum, length(txtcols) == d$ntxt)

  num <- .Fortran("fvsTableNum", nm, nch, 1L, nrows = d$nrow, d$nnum,
                  values = double(max(1, d$nnum * d$nrow)), rtnCode = 0L)
  stopifnot(num$rtnCode == 0, num$nrows == d$nrow)
  size <- 1L
  repeat {
    txt <- .Fortran("fvsTableTxt", nm, nch, 1L, nrows = d$nrow, d$ntxt,
                    text = raw(size), ntextch = size, rtnCode = 0L)
    if (txt$rtnCode != 2) break
    size <- txt$ntextch
  }
  stopifnot(txt$rtnCode == 0, txt$nrows == d$nrow)

  rows <- as.data.frame(matrix(num$values[seq_len(d$nnum * d$nrow)],
                               ncol = d$nnum, byrow = TRUE,
                               dimnames = list(NULL, numcols)))
  if (d$ntxt > 0) {
    tv <- matrix(.split0(txt$text, txt$ntextch), ncol = d$ntxt, byrow = TRUE,
                 dimnames = list(NULL, txtcols))
    rows <- cbind(rows, as.data.frame(tv, stringsAsFactors = FALSE))
  }
  list(types = types, rows = rows)
}

#' Entries of a char(0)-separated buffer of which n bytes are used.
.split0 <- function(buf, n) {
  x <- buf[seq_len(n)]
  readBin(x, character(), n = sum(x == as.raw(0)))
}

#' Write tables read through the API to a SQLite file for the parent to compare.
.writeDb <- function(tables, path) {
  con <- dbConnect(SQLite(), path)
  on.exit(dbDisconnect(con))
  for (name in names(tables)) {
    t <- tables[[name]]
    dbCreateTable(con, name, t$types)
    for (rows in t$rows) {
      if (nrow(rows)) dbAppendTable(con, name, rows)
    }
  }
}

# ------------------------------------------------------ comparison (private helpers)

#' Declared type of each column of a table, upper case, in table order.
.columnTypes <- function(con, name) {
  info <- dbGetQuery(con, sprintf('PRAGMA table_info("%s")', name))
  setNames(toupper(info$type), info$name)
}

#' Sorted rows of a table over cols, one string per row. NULL, NaN and ""
#' all become "NULL"; numbers are compared at full precision.
.rows <- function(con, name, cols) {
  df <- dbGetQuery(con, sprintf("SELECT %s FROM \"%s\"",
                                paste0('"', cols, '"', collapse = ","), name))
  if (!nrow(df)) return(character(0))
  norm <- lapply(df, function(x) {
    s <- if (is.numeric(x)) sprintf("%.17g", x) else as.character(x)
    s[is.na(x) | s == ""] <- "NULL"
    s
  })
  sort(do.call(paste, c(norm, sep = "\x1f")))
}

# --------------------------------------------------------------------- main

args <- commandArgs(trailingOnly = TRUE)
if (length(args) == 3) {  # child process: run one mode in the work directory
  setwd(args[2])
  .run(args[1], args[3])
  quit(status = 0)
}
lib <- normalizePath(if (length(args)) args[1] else file.path(root, "bin", "FVSie.so"))
tmp <- tempfile("fvsmem_r_")
for (mode in c("file", "mem")) {
  d <- file.path(tmp, mode)
  dir.create(d, recursive = TRUE)
  invisible(file.copy(file.path(keydir, c(key, "FVS_Data.db")), d))
  status <- system2(file.path(R.home("bin"), "Rscript"), c(script, lib, d, mode))
  if (status != 0) stop(sprintf("%s mode run failed (status %d)", mode, status))
}

fileDir <- file.path(tmp, "file")
memDir <- file.path(tmp, "mem")
errors <- c(
  checkTablesMatch(file.path(fileDir, "DBReportTest.db"), file.path(memDir, "mem_tables.db")),
  checkOneCasePerStand(file.path(memDir, "mem_tables.db")),
  checkNoDbFiles(memDir),
  checkOutUnchanged(file.path(fileDir, "DBReportTest.out"), file.path(memDir, "DBReportTest.out"))
)
for (e in errors) cat("FAIL", e, "\n")
if (length(errors)) {
  cat(length(errors), "failure(s); outputs in", tmp, "\n")
  quit(status = 1)
}
cat("PASS\n")
unlink(tmp, recursive = TRUE)
