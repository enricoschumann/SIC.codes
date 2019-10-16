## ## SEC

## library("htmltab")
## codes <- htmltab("https://www.sec.gov/info/edgar/siccodes.htm")



## OSHA
## industries <- industry_groups <- major_groups <- divisions <-

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
    "<h2>Description for ([0-9]+): +([^<]+)<.*",
    ".*Industry Group ([0-9]+): ([^<]+).*",
    ".*Major Group ([0-9]+): ([^<]+).*",
    ".*>Division ([A-M]): +([^<]+).*"
)

row <- 0

for (i in 1:1100) {
    message(i, "...", appendLF = FALSE)
    flush.console()

    txt <- readLines(paste0("https://www.osha.gov/pls/imis/",
                            "sic_manual.display?id=", i,
                            "&tab=description"))

    if (any(grepl("SIC Division Structure", txt))) {
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


## uniquify, sort
for (j in 1:4) {
    ans[[j]]$code <- trimws(ans[[j]]$code)
    ans[[j]]$description <- trimws(ans[[j]]$description)
    ans[[j]] <- ans[[j]][match(unique(ans[[j]]$code), ans[[j]]$code), ]
    ans[[j]] <- ans[[j]][order(ans[[j]]$code), ]
    row.names(ans[[j]]) <- NULL
}

.SIC1 <- ans[[4]]
.SIC2 <- ans[[3]]
.SIC3 <- ans[[2]]
.SIC4 <- ans[[1]]

dump(c(".SIC1", ".SIC2", ".SIC3", ".SIC4"),
     "~/Packages/SIC.codes/R/SIC-codes.R")

## txt <- readLines("https://www.osha.gov/pls/imis/sic_manual.html")
## ii <- grep("Major Group", txt)

## major_groups <- data.frame(group = gsub(".*>Major Group ([0-9]+): (.*)<.*", "\\1", txt[ii]),
##                            desc = gsub(".*>Major Group ([0-9]+): ([^<]+).*", "\\2", txt[ii]))

## links <- paste0("https://www.osha.gov/pls/imis", "/",
##                  gsub(".*href=\"(.*)\" title.*", "\\1", txt[ii]))

## for (i in seq_along(major_groups)) {
##     txt1 <- readLines(links[i])
## }

