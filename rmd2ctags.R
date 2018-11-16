#!/usr/bin/env Rscript

# install missing required packages
required <- c("data.table", "stringr")
missed <- setdiff(required, rownames(installed.packages()))
if (length(missed)) install.packages(missed)

args <- commandArgs(trailingOnly=TRUE)

if (!length(args)) {
    stop("Missing input file path")
}

filename <- args[1L]

if (Sys.info()[['sysname']] != 'Linux') {
    filename <- gsub("\\\\", "/", filename)
}

# read_lines {{{
read_lines <- function (filename) {
    dt <- data.table::fread(input = filename, sep = NULL, header = FALSE, col.names = "string", encoding = "UTF-8")
    data.table::set(dt, j = "line", value = seq_along(dt[["string"]]))
}
# }}}

# regexs {{{
regexs <- function () {
    list(
        header = "^(#+)([^{]+)(\\{[^}]+\\})?$",
        chunk_start = "^```\\{r(\\s*)([^,]+)?(,[^}]*)?\\}$",
        chunk_end = "^```\\s*$",
        yaml = "^---()\\s*$",
        yaml_opt = "^([^#].*):.*$",
        func = "^[\t ]*([^ ]+) *<- *function.*$",
        variable1 = "^[\t ]*([^ ]+) *<-.*$",
        variable2 = "^.*-> *(.+)$"
    )
}
# }}}

# sep_lines_yaml {{{
sep_lines_yaml <- function (l) {
    l_yaml <- l$left[stringr::str_detect(string, regexs()$yaml), line]

    l_yaml_s <- l_yaml[seq_along(l_yaml) %% 2]
    l_yaml_e <- l_yaml[!seq_along(l_yaml) %% 2]

    # yaml incomplete
    if (length(l_yaml_e) == 0L) return(l)

    # only use first complete yaml
    yaml <- l$left[line >= l_yaml_s[1L] & line <= l_yaml_e[1L]]
    l$left <- l$left[!yaml, on = .(line)]

    # get options
    yaml <- sep_by_regex(yaml, regexs()$yaml_opt, "yaml_opt")

    yaml <- yaml[!is.na(yaml_opt) | line %in% c(l_yaml_s[1L], l_yaml_e)]

    yaml[1L, yaml_opt := "YAML"]
    yaml[-1L, parent := "YAML"]

    return(c(l, list(yaml = yaml[, .(string, line, yaml_opt, parent)])))
}
# }}}

# sep_lines_chunk {{{
sep_lines_chunk <- function (l) {
    l$left <- sep_by_regex(l$left, regexs()$chunk_start, c("is_chunk", "chunk", "opt"), trim = TRUE)

    l$left[!is.na(is_chunk), index_chunk := .I]
    l$left[, `:=`(index_chunk = index_chunk[1L]), by = cumsum(!is.na(is_chunk))]

    s <- l$left[!is.na(is_chunk), .(start = line, index_chunk)]
    e <- l$left[stringr::str_detect(string, regexs()$chunk_end), .(end = line, index_chunk)]
    if (!nrow(s)) {
        data.table::set(l$left, NULL, c("is_chunk", "chunk", "opt", "index_chunk"), NULL)
        return(l)
    }

    # ignore incomplete
    se <- merge(s, e, by = "index_chunk", all = TRUE)
    se <- na.omit(se)

    chunks <- se[l$left, on = .(index_chunk, start <= line, end >= line), nomatch = 0L]
    data.table::setnames(chunks, "start", "line")
    data.table::set(chunks, NULL, "end", NULL)

    # name unnamed
    chunks[, index := .I]
    chunks[index %in% chunks[, index[1L], by = .(index_chunk)]$V1 & is.na(chunk),
        chunk := paste0("unnamed-chunk-", .I)]

    l$left <- l$left[!chunks, on = .(line)][, c("is_chunk", "chunk", "opt", "index_chunk") := NULL]
    return(c(l, list(chunks = chunks[, .(string, line, chunk, opt, index_chunk)])))
}
# }}}

# sep_lines_header {{{
sep_lines_header <- function (l, max_level = 3L, sep_level = "&&&") {
    l$left <- sep_by_regex(l$left, regexs()$header, c("depth", "header", "tag"), trim = TRUE)

    headers <- l$left[!is.na(depth)]
    data.table::set(l$left, NULL, c("depth", "header", "tag"), NULL)

    if (!nrow(headers)) return(l)

    headers[, depth := ifelse(is.na(depth), 0L, nchar(depth))]
    headers[!is.na(tag), tag := paste0("(", stringr::str_trim(stringr::str_sub(tag, 2L, -2L)), ")")]

    max_depth <- max(headers$depth)

    data.table::set(headers, NULL, c("parent", "self"), NA_character_)

    if (max_depth == 1L) {
        return(c(l, list(headers = headers[, .(string, line, depth, header, tag, parent)])))
    }

    get_parent <- function (dt, depth, sep = "&&&", max = max_depth) {
        d <- depth
        if (d == 1L) {
            dt[, self := header[1L], by = .(cumsum(depth == 1L))]
            dt[depth == 2L, parent := self]
        } else if (d < max){
            dt[depth > d - 1L, self := paste0(self[1L],"&&&", header[1L]), by = .(self, cumsum(depth == d))]
            dt[depth == d + 1L, parent := self]
        }
        dt
    }

    lapply(seq_len(max_level), get_parent, dt = headers, sep = sep_level)
    data.table::set(headers, NULL, "self", NULL)

    return(c(l, list(headers = headers[, .(string, line, depth, header, tag, parent)])))
}
# }}}

# sep_lines {{{
sep_lines <- function (dt) {
    sep <- sep_lines_yaml(list(left = dt))
    sep <- sep_lines_chunk(sep)
    sep <- sep_lines_header(sep)
    sep
}
# }}}

# sep_by_regex {{{
sep_by_regex <- function (dt, regex, new_names, trim = TRUE) {
    # nms <- names(data.table::copy(dt))
    m <- stringr::str_match(dt[["string"]], regex)

    i <- seq_along(new_names) + 1L

    data.table::set(dt, NULL, new_names,
        lapply(i,
            function (x, i) {
                if (trim) {
                    stringr::str_trim(x[, i])
                } else {
                    x[, i]
                }
            }, x = m)
    )

    # list(matched = dt[!is.na(get(new_names[1L]))],
    #      left = dt[is.na(get(new_names[1L])), ..nms]
    # )
}
# }}}

# basic_out {{{
basic_out <- function (dt, col, key) {
    dt[, out := paste0(dt[[col]], "\t", filename, "\t/^", string, "$/;\"\t",key,"\tline:", line)]
}
# }}}

# append_tag {{{
append_tag <- function (dt, col = "tag") {
    dt[, out := paste0(out, ifelse(is.na(get(col)), "", paste0("\tsignature:", get(col))))]
}
# }}}

# create_tags_yaml {{{
create_tags_yaml <- function (l) {
    if (is.null(l$yaml)) return(NULL)
    yaml <- data.table::copy(l$yaml)

    yaml[1L, yaml_opt := "YAML"]

    yaml <- basic_out(yaml, "yaml_opt", "y")

    yaml[-1L, out := paste0(out, "\tyaml:", parent)]

    yaml[-.N]$out
}
# }}}

# create_tags_header {{{
create_tags_header <- function (l) {
    if (is.null(l$headers)) return(NULL)
    headers <- data.table::copy(l$headers)

    basic_out(headers, "header", "h")
    append_tag(headers, "tag")
    headers[depth > 1L, out := paste0(out, "\theader:", parent)]

    headers$out
}
# }}}

# create_tags_chunk {{{
create_tags_chunk <- function (l) {
    if (is.null(l$chunks)) return(NULL)
    chunks <- data.table::copy(l$chunks)[!is.na(chunk)]
    chunks[, chunk := paste0("[CHK]:", chunk)]
    chunks[!is.na(opt), opt := paste0(" (", stringr::str_trim(stringr::str_sub(opt, 2L)), ")")]

    chunks <- basic_out(chunks, "chunk", "c")

    if (is.null(l$headers)) {
        append_tag(chunks, "opt")
        return(chunks$out)
    }

    nms <- names(chunks)

    hc <- data.table::rbindlist(list(
        l$headers[, .(line, parent, depth)],
        chunks
    ), fill = TRUE)

    data.table::setorderv(hc, "line")

    hc[, parent := parent[1L], by = .(cumsum(!is.na(depth)))]
    chunks <- hc[is.na(depth)][!is.na(parent), out := paste0(out, "\theader:", parent)][, ..nms]

    append_tag(chunks, "opt")

    chunks$out
}
# }}}

# create_tags {{{
create_tags <- function (l) {
    c(
        create_tags_header(l),
        create_tags_chunk(l),
        create_tags_yaml(l)
    )
}
# }}}

# cat_tags {{{
cat_tags <- function (tags) {
    if (length(tags)) {
        opts = options(encoding = 'native.enc'); on.exit(options(opts), add = TRUE)
        writeLines(enc2utf8(tags), stdout(), useBytes = TRUE)
    }
}
# }}}

rmd <- read_lines(filename)
sep <- sep_lines(rmd)
tags <- create_tags(sep)
cat_tags(tags)
