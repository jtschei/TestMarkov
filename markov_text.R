file <- "~/TestMarkov/data/alice29.txt"
raw_words <- scan(file, what = character())


clean_word <- function(txt) {
  txt <- tolower(txt)
  txt <- gsub("[^a-z]","",txt)
  txt
}

clean_words <- lapply(raw_words,clean_word)

asc <- function(x) { strtoi(charToRaw(x),16L) }

inc_matrix <- function(c1,c2,matrix) {
  matrix[c1,c2] <- matrix[c1,c2] + 1
  matrix
}

process_word <- function(word,alpha_matrix) {
  word_split <- strsplit(word, "")[[1]]
  space = " "
  prev_c = NA
  for (c in word_split) {
    if (!is.na(prev_c)) {
      alpha_matrix <- inc_matrix(prev_c,c,alpha_matrix)
    }
    prev_c = c
  }
  alpha_matrix <- inc_matrix(c,space,alpha_matrix)
  alpha_matrix
}

dims <- c(letters," ")
alpha_matrix <- matrix(rep(0,27*27),nrow=27,ncol=27,byrow=TRUE,dimname=list(dims,dims))

for (word in clean_words) {
  alpha_matrix <- process_word(word,alpha_matrix)
}

alpha_matrix <- alpha_matrix / sum(alpha_matrix)


