library(rcrossref)
library(readxl)
library(curl)
library(readr)

get_publications <- function(dois){
  results <- cr_works(dois)
  
  authors <- results$data$author
  # reformat author list to keep necessary information
  author_list <- lapply(authors, function(author) {
    list(
      first_name = author$given,
      last_name = author$family
    )
  })
  
  # ----- clean title -----
  cleaned_title <- lapply(results$data$title, function(str) {
    str <- str_remove_all(str, "<([:alpha:]|\\/)+>")
    return(str)
  }) %>% unlist()
  
  # ----- clean abstract -----
  cleaned_abstract <- sapply(results$data$abstract, function(str) {
    str <- str %>%
      str_remove_all(regex("<jats:title>(abstract|summary|background)</jats:title>", ignore_case = TRUE)) %>%
      str_remove_all("<([:alpha:]|\\/|:)+>")
    str
  }) %>% unlist()
  # print(cleaned_abstract)
  
  # keep only necessary information in the returned dataset
  data.frame(
    title = cleaned_title,
    authors = as.array(author_list),
    issued_date = results$data$issued,
    abstract = cleaned_abstract,
    journal = results$data$container.title,
    publisher = results$data$publisher,
    doi = dois,
    referenced_by_count = results$data$is.referenced.by.count
  )
}

generate_pkg_info <- function(df, doi_col="doi"){
  dois <- df[[doi_col]]
  
  results <- cr_works(dois)
  
  # loop through author, which is a list of tibble
  authors <- sapply(results$data$author, \(item){
    item %>% 
      mutate(
        name = paste(given, family),
        name = str_replace_all(
          # bold my name
          name,
          regex("Anh Phan[A-Za-z ]*"), 
          glue::glue("**{name}**")
        )
      ) %>% 
      pull(name) %>% paste(collapse = ", ")
  })
  
  summary <- results$data$title
  
  df %>% 
    mutate(
      authors = authors,
      summary = summary
    )
}

generate_bib <- function(dois){
  doi_list <- paste0("https://www.doi.org/", dois)
  h <- new_handle()
  handle_setheaders(h, "accept" = "application/x-bibtex")
  
  walk(
    doi_list, ~ {
      link <- curl(., handle = h)
      # print(link)
      content <- try(readLines(link, warn = FALSE))
      # print(content)
      write(content, file = "./data-raw/publications.bib", append=TRUE)
    }
  )
  read_delim("./data-raw/publications.bib", delim = "\n")
}