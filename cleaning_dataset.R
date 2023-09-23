library(tidyverse)
library(janitor)
library(knitr)

languages_raw <- read_csv(
  "languages_dataset.csv", 
  # Some of the columns are very sparse, so have readr use everything for
  # guessing.
  guess_max = 4303
) |> 
  clean_names() |>
  # The semantic_scholar column is misformed for a handful of languages. It's ok
  # to introduce NAs here.
  mutate(
    semantic_scholar = as.integer(semantic_scholar)
  )


# Almost all columns are almost completely empty. Keep the columns that have
# more than 10% coverage.
good_lang_cols <- languages_raw |> 
  summarize(
    across(everything(), ~sum(!is.na(.x)))
  ) |>
  tidyr::pivot_longer(
    everything(),
    names_to = "column",
    values_to = "non_empty"
  ) |> 
  mutate(
    coverage = non_empty/nrow(languages_raw)
  ) |> 
  filter(coverage > 0.1) |> 
  pull(column)

languages <- languages_raw |> 
  select(!!!good_lang_cols) |> 
  # This column references a site that doesn't want to be used in projects.
  # select(-hopl) |> 
  # A couple columns are only relevant in the context of the mixed table with
  # non-languages included.
  # select(-number_of_repos, -rank, -paper_count) |> 
  # Some columns are internal metadata that is no longer true with this subset.
  # select(-fact_count, -example_count) |> 
  # I looked at R specifically, and the "country" column was inaccurate. Let's
  # not confuse people with known bad data.
  # select(-country) |> 
  # Organize the columns.
  select(
    pldb_id,
    title,
    description,
    type,
    appeared,
    creators,
    website,
    starts_with("domain_name"),
    reference,
    isbndb,
    book_count,
    semantic_scholar,
    language_rank,
    starts_with("github_"),
    starts_with("wikipedia"),
    starts_with("features_"),
    line_comment_token,
    everything()
  )

write_csv(
  languages, 
  "languages_dataset_cleaning.csv"
  )

# Use the online dictionary to help with the dictionary in the post.
dictionary_url <- "https://pldb.com/columns.csv"
dictionary <- read_csv(dictionary_url) |> 
  clean_names() |> 
  # I only need the column name and description for our dictionary.
  select(column, description) |> 
  # I cleaned the column names, so let's do the same here.
  mutate(
    column = make_clean_names(column)
  ) |> 
  # We don't need the extras.
  filter(
    column %in% colnames(languages)
  )
# Arrange dictionary to match the order of colnames(languages).
dictionary <- dictionary[match(colnames(languages), dictionary$column), ]

dictionary |> 
  mutate(
    class = map_chr(languages, typeof)
  ) |> 
  select(
    variable = column,
    class,
    description
  ) |> 
  knitr::kable()

head(languages)

