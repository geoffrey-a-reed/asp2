## @knitr load_analysis_packages
#
#
suppressPackageStartupMessages({
  library(dplyr)
  library(magrittr)
  library(readr)
  library(purrr)
  library(stringr)
  library(tibble)
  library(tidyr)
  library(tidytext)
  library(widyr)
})
# (End)


## @knitr load_stopwords
#
#
smart_stopwords <-
  get_stopwords(language = 'en', source = 'smart') %>%
  pull(word)
# (End)


## @knitr read_and_preprocess_text
#
#
texts <-
  data_frame(
    text = read_file('ASP2_Rockefeller_Random Reminiscences_1909.txt'),
    text_num = 1:length(text)
  ) %>%
  mutate(
    text = text %>%
      str_extract(regex('PREFACE.+and I feel so now\\.', dotall = TRUE))
  )
# (End)


## @knitr extract_chapters
#
#
chapters <-
  texts %>%
  unnest_tokens(
    chapter,
    text,
    token = 'regex',
    pattern = '(?=CHAPTER)',
    to_lower = FALSE
  ) %>%
  mutate(chapter_num = c(0, 1:(length(chapter) - 1)))
# (End)


## @knitr extract_sections
#
#
sections <-
  chapters %>%
  mutate(
    chapter = chapter %>% str_replace('^CHAPTER\\s+\\p{Lu}+\\s+', ''),
    chapter = chapter %>% str_replace('\\r\\n', '')
  ) %>%
  unnest_tokens(
    section,
    chapter,
    token = 'regex',
    pattern = '(?=(?:\\r\\n){3}\\p{Lu}{2})',
    to_lower = FALSE
  ) %>%
  mutate(
    section = section %>% str_trim(),
    section_num = c(1, -1, 2:(length(section) - 1))
  ) %>%
  filter(section_num > 0)
# (End)


## @knitr extract_paragraphs
#
#
paragraphs <-
  sections %>%
  unnest_tokens(
    paragraph,
    section,
    token = 'regex',
    pattern = '(?:\\r\\n\\r\\n)',
    to_lower = FALSE
  ) %>%
  filter(
    paragraph %>% str_detect('J\\.D\\.R\\.') %>% not(),
    paragraph %>% str_detect('_March_') %>% not(),
    paragraph %>% str_detect('November 13') %>% not(),
    paragraph %>% str_detect('DEAR MADAM:') %>% not(),
    paragraph %>% str_detect('Yours very truly') %>% not(),
    paragraph %>% str_detect('JOHN D\\. ROCKEFELLER') %>% not(),
    paragraph %>% str_detect('BOWLING GREEN') %>% not(),
    paragraph %>% str_detect('Yours truly,') %>% not(),
    paragraph %>% str_detect('H\\.M\\. BACKUS') %>% not()
  ) %>%
  mutate(
    paragraph = paragraph %>% str_squish(),
    paragraph_num = 1:length(paragraph)
  )
# (End)


## @knitr extract_sentences
#
#
sentences <-
  paragraphs %>%
  mutate(
    paragraph =
      paragraph %>%
      str_replace_all(
        c(
          '_' = '',
          'John D\\. Archbold' = 'John Dustin Archbold',
          'bbl\\.' = 'barrel',
          'Mr\\.' = 'Mister',
          'H\\.M\\. Flagler' = 'Henry Morrison Flagler',
          'St\\.' = 'Saint',
          'O\\.K\\.' = 'okay',
          'S\\.V\\. Harkness' = 'Stephen Vanderburgh Harkness',
          'J\\.D\\.R\\.' = 'John Davison Rockefeller',
          'J\\.D\\.' = 'John Davison',
          'John D\\. Rockefeller' = 'John Davison Rockefeller',
          '&' = 'and',
          'M\\.B\\. Clark' = 'Mister Clark',
          'T\\.P\\. Handy' = 'Truman Parmalee Handy',
          'Thomas W\\. Armitage' = 'Thomas Armitage',
          'Messrs\\.' = 'Misters',
          'F\\.M\\. Backus' = 'Mister Backus',
          'Mrs\\.' = 'Missus',
          'Charles H\\. Marr' = 'Charles Marr',
          'Peter S\\. Jennings' = 'Peter Jennings',
          'F\\.N\\. Backus' = 'Backus',
          'VS\\.' = 'VERSUS',
          'Frederick T\\. Gates' = 'Frederick Taylor Gates',
          'Starr J\\. Murphy' = 'Starr Jocelyn Murphy',
          'L\\.M\\. Bowers' = 'Lamont Montgomery Bowers',
          'Dr\\.' = 'Doctor',
          '1st\\.' = 'First:',
          '2nd\\.' = 'Second:',
          '3rd\\.' = 'Third:',
          '4th\\.' = 'Fourth:',
          '5th\\.' = 'Fifth:',
          '6th\\.' = 'Sixth:',
          'Robert C\\. Ogden' = 'Robert Curtis Ogden',
          'DR\\.' = 'DOCTOR',
          'WILLIAM R\\. HARPER' = 'WILLIAM RAINEY HARPER',
          'William R\\. Harper' = 'William Rainey Harper'
        )
      )
  ) %>%
  unnest_tokens(sentence, paragraph, token = 'sentences', to_lower = FALSE) %>%
  mutate(sentence_num = 1:length(sentence))
# (End)


## @knitr extract_tokens
#
#
words <-
  sentences %>%
  unnest_tokens(word, sentence, token = 'words') %>%
  filter(word %>% str_detect('\\d+') %>% not()) %>%
  mutate(word_num = 1:length(word))

words_nostop <-
  words %>%
  filter((word %in% smart_stopwords) %>% not())

bigrams <-
  sentences %>%
  unnest_tokens(bigram, sentence, token = 'ngrams', n = 2) %>%
  filter(
    bigram %>% is.na() %>% not(),
    bigram %>% str_detect('\\d+') %>% not()
  ) %>%
  mutate(bigram_num = 1:length(bigram))

bigrams_nostop <-
  bigrams %>%
  separate(bigram, c('bigram1', 'bigram2'), sep = '\\s+') %>%
  filter(
    (bigram1 %in% smart_stopwords) %>% not(),
    (bigram2 %in% smart_stopwords) %>% not()
  ) %>%
  unite(bigram, c('bigram1', 'bigram2'), sep = ' ')

trigrams <-
  sentences %>%
  unnest_tokens(trigram, sentence, token = 'ngrams', n = 3) %>%
  filter(
    trigram %>% is.na() %>% not(),
    trigram %>% str_detect('\\d+') %>% not()
  ) %>%
  mutate(trigram_num = 1:length(trigram))

trigrams_nostop <-
  trigrams %>%
  separate(trigram, c('trigram1', 'trigram2', 'trigram3'), sep = '\\s+') %>%
  filter(
    (trigram1 %in% smart_stopwords) %>% not(),
    (trigram2 %in% smart_stopwords) %>% not(),
    (trigram3 %in% smart_stopwords) %>% not()
  ) %>%
  unite(trigram, c('trigram1', 'trigram2', 'trigram3'), sep = ' ')
# (End)


## @knitr count_tokens
#
#
word_counts <-
  words %>%
  count(word) %>%
  rename(count = n) %>%
  arrange(count)

word_counts_nostop <-
  words_nostop %>%
  count(word) %>%
  rename(count = n) %>%
  arrange(count)

word_chapter_counts <-
  words %>%
  group_by(chapter_num, word) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(chapter_num, count)

word_section_counts <-
  words %>%
  group_by(section_num, word) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(section_num, count)

bigram_counts <-
  bigrams %>%
  count(bigram) %>%
  rename(count = n) %>%
  arrange(count)

bigram_counts_nostop <-
  bigrams_nostop %>%
  count(bigram) %>%
  rename(count = n) %>%
  arrange(count)

bigram_chapter_counts <-
  bigrams %>%
  group_by(chapter_num, bigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(chapter_num, count)

bigram_chapter_counts_nostop <-
  bigrams_nostop %>%
  group_by(chapter_num, bigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(chapter_num, count)

bigram_section_counts <-
  bigrams %>%
  group_by(section_num, bigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(section_num, count)

bigram_section_counts_nostop <-
  bigrams_nostop %>%
  group_by(section_num, bigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(section_num, count)

trigram_counts <-
  trigrams %>%
  count(trigram) %>%
  rename(count = n) %>%
  arrange(count)

trigram_counts_nostop <-
  trigrams_nostop %>%
  count(trigram) %>%
  rename(count = n) %>%
  arrange(count)

trigram_chapter_counts <-
  trigrams %>%
  group_by(chapter_num, trigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(chapter_num, count)

trigram_chapter_counts_nostop <-
  trigrams_nostop %>%
  group_by(chapter_num, trigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(chapter_num, count)

trigram_section_counts <-
  trigrams %>%
  group_by(section_num, trigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(section_num, count)

trigram_section_counts_nostop <-
  trigrams_nostop %>%
  group_by(section_num, trigram) %>%
  summarize(count = n()) %>%
  ungroup() %>%
  arrange(section_num, count)
# (End)


## @knitr extract_hapax
#
#
hapax_words <-
  word_counts %>%
  filter(count == 1) %>%
  pull(word)

dis_words <-
  word_counts %>%
  filter(count == 2) %>%
  pull(word)

tris_words <-
  word_counts %>%
  filter(count == 3) %>%
  pull(word)

tetrakis_words <-
  word_counts %>%
  filter(count == 4) %>%
  pull(word)

hapax_bigrams <-
  bigram_counts %>%
  filter(count == 1) %>%
  pull(bigram)

dis_bigrams <-
  bigram_counts %>%
  filter(count == 2) %>%
  pull(bigram)

tris_bigrams <-
  bigram_counts %>%
  filter(count == 3) %>%
  pull(bigram)

tetrakis_bigrams <-
  bigram_counts %>%
  filter(count == 4) %>%
  pull(bigram)

hapax_trigrams <-
  trigram_counts %>%
  filter(count == 1) %>%
  pull(trigram)

dis_trigrams <-
  trigram_counts %>%
  filter(count == 2) %>%
  pull(trigram)

tris_trigrams <-
  trigram_counts %>%
  filter(count == 3) %>%
  pull(trigram)

tetrakis_trigrams <-
  trigram_counts %>%
  filter(count == 4) %>%
  pull(trigram)
# (End)


## @knitr type_token_ratio
#
#
ttr_words_by_text <-
  words %>%
  group_by(text_num) %>%
  summarize(
    word_count = n(),
    distinct_word_count = n_distinct(word),
    ttr_word = distinct_word_count / word_count
  ) %>%
  ungroup()

ttr_words_by_chapter <-
  words %>%
  group_by(chapter_num) %>%
  summarize(
    word_count = n(),
    distinct_word_count = n_distinct(word),
    ttr_word = distinct_word_count / word_count
  ) %>%
  ungroup()

ttr_words_by_section <-
  words %>%
  group_by(section_num) %>%
  summarize(
    word_count = n(),
    distinct_word_count = n_distinct(word),
    ttr_word = distinct_word_count / word_count
  ) %>%
  ungroup()

ttr_bigrams_by_text <-
  bigrams %>%
  group_by(text_num) %>%
  summarize(
    bigram_count = n(),
    distinct_bigram_count = n_distinct(bigram),
    ttr_bigram = distinct_bigram_count / bigram_count
  ) %>%
  ungroup()

ttr_bigrams_by_chapter <-
  bigrams %>%
  group_by(chapter_num) %>%
  summarize(
    bigram_count = n(),
    distinct_bigram_count = n_distinct(bigram),
    ttr_bigram = distinct_bigram_count / bigram_count
  ) %>%
  ungroup()

ttr_bigrams_by_section <-
  bigrams %>%
  group_by(section_num) %>%
  summarize(
    bigram_count = n(),
    distinct_bigram_count = n_distinct(bigram),
    ttr_bigram = distinct_bigram_count / bigram_count
  ) %>%
  ungroup()

ttr_trigrams_by_text <-
  trigrams %>%
  group_by(text_num) %>%
  summarize(
    trigram_count = n(),
    distinct_trigram_count = n_distinct(trigram),
    ttr_trigram = distinct_trigram_count / trigram_count
  ) %>%
  ungroup()

ttr_trigrams_by_chapter <-
  trigrams %>%
  group_by(chapter_num) %>%
  summarize(
    trigram_count = n(),
    distinct_trigram_count = n_distinct(trigram),
    ttr_trigram = distinct_trigram_count / trigram_count
  ) %>%
  ungroup()

ttr_trigrams_by_section <-
  trigrams %>%
  group_by(section_num) %>%
  summarize(
    trigram_count = n(),
    distinct_trigram_count = n_distinct(trigram),
    ttr_trigram = distinct_trigram_count / trigram_count
  ) %>%
  ungroup()

ttr_text <-
  ttr_words_by_text %>%
  inner_join(ttr_bigrams_by_text, by = 'text_num') %>%
  inner_join(ttr_trigrams_by_text, by = 'text_num')

ttr_chapter <-
  ttr_words_by_chapter %>%
  inner_join(ttr_bigrams_by_chapter, by = 'chapter_num') %>%
  inner_join(ttr_trigrams_by_chapter, by = 'chapter_num')

ttr_section <-
  ttr_words_by_section %>%
  inner_join(ttr_bigrams_by_section, by = 'section_num') %>%
  inner_join(ttr_trigrams_by_section, by = 'section_num')
# (End)


## @knitr declare_tidykwic
#
#
tidykwic <- function(tbl, col, keywords, n = 5) {
  enquo_col <- enquo(col)
  ngrams <-
    tbl %>%
    unnest_tokens(kwic, !!enquo_col, token = 'ngrams', n = n)
  keywords %>%
    map_dfr(function(keyword) {
      pattern <-
        keyword %>%
        str_c('^(?:\\w+\\W+){', n %/% 2, '}\\b', ., '\\b') %>%
        regex(ignore_case = TRUE)
      ngrams %>%
        filter(
          kwic %>% is.na() %>% not(),
          kwic %>% str_detect(pattern)
        ) %>%
        mutate(keyword = keyword)
    })
}
# (End)


## @knitr kwic_hapax
#
#
hapax_words_kwic <-
  sentences %>%
  tidykwic(sentence, hapax_words)

dis_words_kwic <-
  sentences %>%
  tidykwic(sentence, dis_words)

tris_words_kwic <-
  sentences %>%
  tidykwic(sentence, tris_words)

tetrakis_words_kwic <-
  sentences %>%
  tidykwic(sentence, tetrakis_words)
# (End)


## @knitr load_graphics_packages
#
#
suppressPackageStartupMessages({
  library(cowplot)
  library(extrafont)
  library(ggplotify)
  library(ggplot2)
  library(scales)
  library(wordcloud)
})
# (End)


## @knitr declare_fonts
#
#
docx_fonts <- c(
  headings = 'Franklin Gothic Medium',
  body = 'Franklin Gothic Book'
)
# (End)


## @knitr load_and_check_system_fonts
#
#
if (docx_fonts %in% fonts() %>% all() %>% not()) {
  warning('Importing fonts... this will take some time!')
  font_import(prompt = FALSE)
} else if (docx_fonts %in% fonts() %>% all() %>% not()) {
  stop('Required fonts not found.')
}
# (End)


## @knitr load_and_check_windows_fonts
#
#
if (.Platform$OS.type != 'windows') {
  stop('Unsupported operating system.')
} else if (docx_fonts %in% windowsFonts() %>% all() %>% not()) {
  loadfonts(device = 'win')
}
# (End)


## @knitr declare_colors
#
#
docx_colors <- c(
  text_background_dark1 = rgb(0, 0, 0, maxColorValue = 255),
  text_background_light1 = rgb(255, 255, 255, maxColorValue = 255),
  text_background_dark2 = rgb(69, 69, 81, maxColorValue = 255),
  text_background_light2 = rgb(216, 217, 220, maxColorValue = 255),
  accent1 = rgb(227, 45, 145, maxColorValue = 255),
  accent2 = rgb(200, 48, 204, maxColorValue = 255),
  accent3 = rgb(78, 166, 220, maxColorValue = 255),
  accent4 = rgb(71, 117, 231, maxColorValue = 255),
  accent5 = rgb(137, 113, 225, maxColorValue = 255),
  accent6 = rgb(213, 71, 115, maxColorValue = 255),
  hyperlink = rgb(107, 159, 37, maxColorValue = 255),
  followed_hyperlink = rgb(140, 140, 140, maxColorValue = 255)
)
# (End)


## @knitr declare_desaturation_function
#
#
desaturate = function(colors, ds=0.4, dv=0.7) {
  colors = rgb2hsv(col2rgb(colors))
  colors["v", ] = colors["v", ] + dv * (1 - colors["v", ])
  colors["s", ] = ds * colors["s", ]
  apply(colors, 2, function(color) hsv(color[1], color[2], color[3]))
}
# (End)


