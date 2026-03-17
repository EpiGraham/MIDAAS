# Extracting Health Talk Experiences for Prostate Cancer as Prototype Test

library(httr)
library(rvest)
library(dplyr)
library(stringr)

url <- "https://healthtalk.org/experiences/prostate-cancer/finding-information-prostate-cancer-treatment/"

# Fetch the page
page <- GET(url, add_headers(
  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/122.0.0.0 Safari/537.36"
)) |>
  content(as = "text", encoding = "UTF-8") |>
  read_html()

# ── Helper: extract text safely ───────────────────────────────────────────────
get_text <- function(node, selector) {
  el <- html_element(node, selector)
  if (is.na(el)) NA_character_ else html_text2(el) |> trimws()
}

# ── Extract each interview block ──────────────────────────────────────────────
# Each interview is wrapped in an <h2> followed by metadata and a transcript
# We'll extract all h2 headings, metadata paragraphs, profile links, and transcripts

# Interview titles (h2 tags in the main content)
titles <- page |>
  html_elements("main h2, article h2, .entry-content h2, #main h2") |>
  html_text2() |>
  trimws()

# If selector above is empty, fallback to all h2s and filter out nav
if (length(titles) == 0) {
  titles <- page |> html_elements("h2") |> html_text2() |> trimws()
  titles <- titles[!str_detect(titles, "In this section|Share|sidebar")]
}

cat(sprintf("Found %d interview titles\n", length(titles)))
print(titles)

# ── Build structured data frame by parsing the full HTML ─────────────────────
raw_html  <- as.character(page)
full_text <- html_text2(page)

# Split content around h2 tags to isolate interview blocks
sections <- page |> html_elements("h2")

results <- lapply(sections, function(h2) {
  title <- html_text2(h2) |> trimws()
  
  # Skip non-interview h2s
  if (str_detect(title, regex("in this section|share|sidebar|want to make|related|quick links|more info", ignore_case = TRUE))) {
    return(NULL)
  }
  
  # Traverse siblings after this h2 to collect metadata and transcript
  # Use xpath to get the next sibling elements
  siblings <- tryCatch(
    html_elements(h2, xpath = "following-sibling::*[position() <= 10]"),
    error = function(e) NULL
  )
  
  sibling_text <- if (!is.null(siblings)) html_text2(siblings) else ""
  
  # Extract age at interview
  age_interview <- str_match(sibling_text, "Age at interview\\s*(\\d+)")[, 2] |>
    na.omit() |> head(1)
  
  # Extract gender
  gender <- str_match(sibling_text, "Gender\\s*(Male|Female|Non-binary)")[, 2] |>
    na.omit() |> head(1)
  
  # Extract age at diagnosis
  age_diagnosis <- str_match(sibling_text, "Age at diagnosis\\s*(\\d+)")[, 2] |>
    na.omit() |> head(1)
  
  # Extract profile URL
  profile_url <- siblings |>
    html_elements("a[href*='interviewees']") |>
    html_attr("href") |>
    head(1)
  
  # Transcript text: the longest paragraph-like block
  transcript <- siblings |>
    html_elements("p") |>
    html_text2() |>
    trimws() |>
    Filter(f = function(x) nchar(x) > 50) |>
    paste(collapse = "\n\n")
  
  tibble(
    title            = title,
    age_at_interview = ifelse(length(age_interview) > 0, as.integer(age_interview), NA_integer_),
    gender           = ifelse(length(gender) > 0, gender, NA_character_),
    age_at_diagnosis = ifelse(length(age_diagnosis) > 0, as.integer(age_diagnosis), NA_integer_),
    profile_url      = ifelse(length(profile_url) > 0, profile_url, NA_character_),
    transcript       = ifelse(nchar(transcript) > 0, transcript, NA_character_)
  )
})

interviews_df <- bind_rows(Filter(Negate(is.null), results))

cat(sprintf("\nExtracted %d interviews\n", nrow(interviews_df)))
print(interviews_df)

# ── Also extract the narrative/summary paragraphs between interviews ──────────
narrative_df <- page |>
  html_elements("p") |>
  html_text2() |>
  trimws() |>
  (\(x) x[nchar(x) > 100])() |>    # only substantive paragraphs
  (\(x) tibble(paragraph = seq_along(x), text = x))()

cat(sprintf("\nExtracted %d narrative paragraphs\n", nrow(narrative_df)))
print(narrative_df)

# ── Save to CSV ───────────────────────────────────────────────────────────────
write.csv(interviews_df,  "Documents/GitHub/MIDAAS/healthtalk_interviews.csv",  row.names = FALSE)
write.csv(narrative_df,   "Documents/GitHub/MIDAAS/healthtalk_narrative.csv",   row.names = FALSE)
cat("\nSaved: healthtalk_interviews.csv and healthtalk_narrative.csv\n")