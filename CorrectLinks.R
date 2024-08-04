# function to solve the linking problem
# https://stackoverflow.com/questions/76487947/babelquarto-rendering-a-multilingual-quarto-book

# use as: create_abs_lang_link("_book", base_link = "https://<user-name>.github.io/<repo-name>")

library(rvest)
library(fs)

add_base_url <- function(file_path, base_link) {
  html_content <- rvest::read_html(file_path)
  lang_links <- html_content %>% 
    rvest::html_elements('a[id*="language-link"]') %>% 
    rvest::html_attr("href")
  lang_link_with_baseurl <- paste0(base_link, lang_links)
  raw_html <- paste(readLines(file_path), collapse="\n")
  for (i in seq_along(lang_links)) {
    raw_html <- sub(lang_links[i], lang_link_with_baseurl[i], raw_html)
  }
  return(raw_html)
}

create_abs_lang_link <- function(path, base_link) {
  html_files_path <- fs::dir_ls(path, recurse = TRUE, glob="*.html")
  for (file_path in html_files_path) {
    edited_raw_html <- add_base_url(file_path, base_link)
    writeLines(edited_raw_html, file_path)
  }
}
