library(here)
library(readr)

transcriptFilename <- here::here("data/Transcript of Trumps Jan 6 2021 Speech.txt")

cat(transcriptFilename, "\n\n")

transcript <- readr::read_file(transcriptFilename)

cat(transcript)
