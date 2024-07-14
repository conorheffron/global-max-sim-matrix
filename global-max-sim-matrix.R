init_matrix_col1 <- function(sim_matrix, m) { 
  # Fill row values for first column
  for (i in 1:m)
    sim_matrix[i+1, 1] <- i * -2
  sim_matrix
}


init_matrix_row1 <- function(sim_matrix, n) { 
  # Fill column values for first row
  for (j in 1:n)
    sim_matrix[1, j+1] <- j * -2
  
  sim_matrix
}

populate_global_max_sim_matrix <- function(sim_matrix, seq1, seq2, d, m, n) { 
  # handle nested values
  for (j in 1:m+1) {
    for (i in 1:n+1) {
      i_char <-substr(seq1, i-1, i-1)
      j_char <- substr(seq2, j-1, j-1)
      # if (i)
      sc <- 0
      if (i_char == j_char) {
        sc <- d["match"]
      } else if (i_char != j_char) {
        sc <- d["mismatch"]
      }
      
      # sc <- d["match"]
      diag_score <- sim_matrix[i-1, j-1] + sc
      upper_score <- sim_matrix[i-1, j] + d["gap"]
      side_score <- sim_matrix[i, j-1] + d["gap"]
      
      
      score <- max(diag_score, upper_score, side_score)
      if (score == -Inf) {
        score <- 0
      }
      # print(paste(i, j, sim_matrix[i, j], i_char, j_char, score))
      sim_matrix[i, j] <- score
    }
  }
  sim_matrix
}