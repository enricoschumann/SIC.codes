## ## SEC

## library("htmltab")
## codes <- htmltab("https://www.sec.gov/info/edgar/siccodes.htm")

library("textutils")

ans <- list(
    data.frame(code = character(2000),
               description = character(2000),
               stringsAsFactors = FALSE),
    data.frame(code = character(2000),
               description = character(2000),
               stringsAsFactors = FALSE),
    data.frame(code = character(2000),
               description = character(2000),
               stringsAsFactors = FALSE),
    data.frame(code = character(2000),
               description = character(2000),
               stringsAsFactors = FALSE))
names(ans) <- c("industries",
                "industry_groups",
                "major_groups",
                "divisions")

patterns <- c(
    "<h.>Description for ([0-9]+): +([^<]+)<.*",
    ".*Industry Group ([0-9]+): ([^<]+).*",
    ".*Major Group ([0-9]+): ([^<]+).*",
    ".*>Division ([A-M]): +([^<]+).*"
)

row <- 0

for (i in 1:9999) {
    message(i, "...", appendLF = FALSE)
    flush.console()
    u <- url(paste0("https://www.osha.gov/sic-manual/", sprintf("%04d", i)))
    txt <- try(readLines(u), silent = TRUE)
    close(u)
    Sys.sleep(0.5)
    if (inherits(txt, "try-error")) {
        message("[skip]")
        next
    }

    row <- row + 1

    for (j in 1:4) {
        p <- patterns[j]
        t <- grep(p, txt)
        ans[[j]]$code[row] <- gsub(p, "\\1", txt[t])
        ans[[j]]$description[row] <- gsub(p, "\\2", txt[t])
    }
    message("[OK]")
}

## keep only filled rows
for (j in 1:4)
    ans[[j]] <- ans[[j]][1:row, ]

ans[[4]] <- data.frame(code = ans[[3]][[1]],
                       description = paste(ans[[4]][[1]], ans[[4]][[2]]),
                       stringsAsFactors = FALSE)

## uniquify, sort
for (j in 1:4) {
    ans[[j]]$code <- trimws(ans[[j]]$code)
    ans[[j]]$description <- trimws(ans[[j]]$description)
    ans[[j]] <- ans[[j]][match(unique(ans[[j]]$code), ans[[j]]$code), ]
    ans[[j]] <- ans[[j]][order(ans[[j]]$code), ]
    row.names(ans[[j]]) <- NULL
}

for (j in 1:4)
    ans[[j]][["description"]] <- textutils::HTMLdecode(ans[[j]][["description"]])

.division <- ans[[4]]
.major_group <- ans[[3]]
.industry_group <- ans[[2]]
.industry <- ans[[1]]


dump(c(".division", ".major_group", ".industry_group", ".industry"),
     "~/Packages/SIC.codes/R/SIC-codes.R")

library("formatR")
formatR::tidy_file("~/Packages/SIC.codes/R/SIC-codes.R", width.cutoff = 30)

