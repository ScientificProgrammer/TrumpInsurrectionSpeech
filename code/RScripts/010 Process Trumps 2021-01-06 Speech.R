library(here)
library(readr)
library(tm)
library(tibble)
library(stringr)
library(dplyr)
library(ggplot2)


transcriptFilename <- here::here("data/Transcript of Trumps Jan 6 2021 Speech.txt")

# cat(transcriptFilename, "\n\n")

# transcript <- readr::read_file(transcriptFilename)

speechCorpus01 <- VCorpus(VectorSource(readr::read_file(transcriptFilename)))

speechCorpus01 <- tm_map(speechCorpus01, content_transformer(tolower))

speechCorpus01 <- tm_map(speechCorpus01, removeWords, stopwords("english"))

speechCorpus01 <- tm_map(speechCorpus01, removePunctuation)

# speechCorpus01 <- tm_map(speechCorpus01, stemDocument)

speechCorpus01 <- tm_map(speechCorpus01, stripWhitespace)

speechCorpus01[[1]]$content

dtm1 <- TermDocumentMatrix(speechCorpus01)

speechSummary1 <- tibble::tibble(
  terms = dtm1$dimnames$Terms,
  freq_i = dtm1$i,
  freq_j = dtm1$j,
  freq_v = dtm1$v
)

sliceValueforN <- 25
speechSummary2 <- speechSummary1 %>%
  filter(stringr::str_detect(terms, "[^a-z]", negate = TRUE)) %>%
  arrange(desc(freq_v)) %>%
  slice_head(n = sliceValueforN)

ggplot2::ggplot(speechSummary2, aes(x = sliceValueforN:1, y = freq_v)) +
  geom_col(aes(fill = terms), color = "black", show.legend = FALSE) +
  scale_fill_manual(values = rainbow(sliceValueforN)) +
  geom_label(aes(label = terms), show.legend = FALSE, size = 5) +
  labs(
    title = "Word Frequencies from Trump's\nJan 6, 2021 Capitol Speech",
    x = "Word Frequency Rank",
    y = "Number of Word Occurences"
  ) +
  coord_flip() +
  theme(
    plot.margin = margin(20, 20, 20, 20),
    plot.title = element_text(size = 24, hjust = 0.5),
    axis.title = element_text(size = 20),
    axis.title.x = element_text(margin = margin(20, 0, 0, 0)),
    axis.title.y = element_text(margin = margin(0, 20, 0, 0)),
    axis.text = element_text(size = 14)
  )
