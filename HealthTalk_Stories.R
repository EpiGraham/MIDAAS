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

# ── Extract all top-level content nodes (h2s and paragraphs) in document order ──
content_nodes <- page |> html_elements("h2, p")

# ── Walk through nodes, associating each h2 with the nearest preceding narrative ──
results      <- list()
last_narrative <- NA_character_

for (node in content_nodes) {
  tag  <- html_name(node)
  text <- html_text2(node) |> trimws()
  
  if (tag == "p" && nchar(text) > 100) {
    # Substantive paragraph — treat as narrative context
    last_narrative <- text
    
  } else if (tag == "h2") {
    # Skip non-interview headings
    if (str_detect(text, regex("in this section|share|sidebar|want to make|related|quick links|more info", ignore_case = TRUE))) {
      next
    }
    
    # Get siblings after this h2 for metadata
    siblings <- tryCatch(
      html_elements(node, xpath = "following-sibling::*[position() <= 10]"),
      error = function(e) NULL
    )
    
    sibling_text <- if (!is.null(siblings)) html_text2(siblings) else ""
    
    age_interview <- str_match(sibling_text, "Age at interview\\s*(\\d+)")[, 2] |> na.omit() |> head(1)
    gender        <- str_match(sibling_text, "Gender\\s*(Male|Female|Non-binary)")[, 2] |> na.omit() |> head(1)
    age_diagnosis <- str_match(sibling_text, "Age at diagnosis\\s*(\\d+)")[, 2] |> na.omit() |> head(1)
    profile_url   <- siblings |> html_elements("a[href*='interviewees']") |> html_attr("href") |> head(1)
    
    results <- append(results, list(tibble(
      title            = text,
      age_at_interview = ifelse(length(age_interview) > 0, as.integer(age_interview), NA_integer_),
      gender           = ifelse(length(gender) > 0, gender, NA_character_),
      age_at_diagnosis = ifelse(length(age_diagnosis) > 0, as.integer(age_diagnosis), NA_integer_),
      profile_url      = ifelse(length(profile_url) > 0, profile_url, NA_character_),
      narrative        = last_narrative   # <-- narrative instead of transcript
    )))
  }
}

interviews_df <- bind_rows(results)

cat(sprintf("Extracted %d interviews\n", nrow(interviews_df)))
print(interviews_df)

# ── Save to CSV ───────────────────────────────────────────────────────────────
write.csv(interviews_df,  "Documents/GitHub/MIDAAS/healthtalk_interviews.csv", row.names = FALSE)
cat("Saved: healthtalk_interviews.csv\n")