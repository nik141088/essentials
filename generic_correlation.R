# generic correlation function
# input data.frame/table with multiple column types: integers/numeric/character/factors
# output: correlation of all variables

# Correlation approaach
# consider integers/numeric as numbers while factors/characters as categories
# number-number correlation is easy: cor(x,y, use = "pairwise.complete.obs", method = "pearson")
# category-category correlation can be found using: rcompanion::cramerV(x,y)
# number-category can be done using a FE regression. Let x be number and y be category. Also, create a random variable r which is unrelated to everything else. The corr. can be found as: summary(lfe::felm(x ~ r | y)$r2 %>% sqrt

generic_correlation = function(dt, seed = 100, cat_count_cutoff = 1e5, print = F) {
  setDT(dt);
  set.seed(seed);
  cols = names(dt);
  N = length(cols);
  col_classes = sapply(dt, class);
  num_vars = which(col_classes %in% c("numeric", "integer"))
  cat_vars = which(col_classes %in% c("character", "factor"))
  
  cat_uniqueN = dt[, lapply(.SD, uniqueN), .SDcols = cat_vars]
  
  dt[, random__ := rnorm(.N)]
  C = matrix(NA_real_, nrow = N, ncol = N)
  
  for(i in 1:N) {
    for(j in 1:N) {
      
      if(print) {
        cat("Working on columns: ", cols[i], "and", cols[j], "\n");
      }
      
      if(i == j) {
        C[i,j] = 1
      }
      
      # find correlation for (i,j) and update for both (i,j) and (j,i)
      if(i < j) {

        if(i %in% num_vars && j %in% num_vars) {
          # number-number correlation
          
          if(print) {
            cat("Computing number-number correlation for i:", i, "and", "j:", j, "\n");
          }
          
          c = dt[, cor(get(cols[i]), get(cols[j]),
                       use = "pairwise.complete.obs", method = "pearson")]
          
          C[i,j] = C[j,i] = c
        }
        
        if(i %in% cat_vars && j %in% cat_vars) {
          # category-category correlation
          
          if(cat_uniqueN[[cols[i]]] > cat_count_cutoff || cat_uniqueN[[cols[j]]] > cat_count_cutoff) {
            cat(file = stderr(), "WARNING: Correlation for columns:", cols[i], "and", cols[j], 
                "can't be computed as they have more categories than we can handle.", 
                cols[i], "has", cat_uniqueN[[cols[i]]], "categories while", 
                cols[j], "has", cat_uniqueN[[cols[j]]], "categories. We can only handle",
                cat_count_cutoff, "categories. Try changing cat_count_cutoff argument.", "\n")
            next;
          }
          
          if(cat_uniqueN[[cols[i]]] == 1 || cat_uniqueN[[cols[j]]] == 1) {
            cat(file = stderr(), "WARNING: Correlation for columns:", cols[i], "and", cols[j], 
                "can't be computed as they have ONLY one category.", 
                cols[i], "has", cat_uniqueN[[cols[i]]], "categories while", 
                cols[j], "has", cat_uniqueN[[cols[j]]], "categories.",
                "We need atleast two categories to compute correlation.", "\n")
            next;
          }
          
          if(print) {
            cat("Computing category-category correlation for i:", i, "and", "j:", j, "\n");
          }
          
          c = dt[, rcompanion::cramerV(get(cols[i]), get(cols[j]))]
          
          C[i,j] = C[j,i] = c
        }
        
        if(i %in% num_vars && j %in% cat_vars) {
          # category-number correlation type-1
          
          num_col = cols[i]
          cat_col = cols[j]
          
          if(cat_uniqueN[[cat_col]] > cat_count_cutoff) {
            cat(file = stderr(), "WARNING: Correlation for columns:", cols[i], "and", cols[j], 
                "can't be computed since", cat_col, "has", cat_uniqueN[[cat_col]], 
                "categories but we can only handle", cat_count_cutoff, 
                "categories. Try changing cat_count_cutoff argument.", "\n")
            next;
          }
          
          if(cat_uniqueN[[cat_col]] == 1) {
            cat(file = stderr(), "WARNING: Correlation for columns:", cols[i], "and", cols[j], 
                "can't be computed since", cat_col, "has only 1 category.",
                "We need atleast two categories to compute correlation", "\n")
            next;
          }
          
          
          if(print) {
            cat("Computing number-category correlation for i:", i, "and", "j:", j, "\n");
          }
          
          f = as.formula(paste0("`", num_col, "`", " ~ random__ | ", "`", cat_col, "`"))
          
          c = summary(lfe::felm(f, data = dt))$r2 %>% sqrt
          
          C[i,j] = C[j,i] = c
        }
        
        if(i %in% cat_vars && j %in% num_vars) {
          # category-number correlation type-2
          
          cat_col = cols[i]
          num_col = cols[j]
          
          if(cat_uniqueN[[cat_col]] > cat_count_cutoff) {
            cat(file = stderr(), "WARNING: Correlation for columns:", cols[i], "and", cols[j], 
                "can't be computed since", cat_col, "has", cat_uniqueN[[cat_col]], 
                "categories but we can only handle", cat_count_cutoff, 
                "categories. Try changing cat_count_cutoff argument.", "\n")
            next;
          }
          
          if(print) {
            cat("Computing category-number correlation for i:", i, "and", "j:", j, "\n");
          }

          f = as.formula(paste0("`", num_col, "`", " ~ random__ | ", "`", cat_col, "`"))
          
          c = summary(lfe::felm(f, data = dt))$r2 %>% sqrt
          
          C[i,j] = C[j,i] = c
        }
      }
      
      if(j < i) {
        next;
      }
      
    }
  }
  
  dt[, random__ := NULL]
  
  colnames(C) = rownames(C) = cols
  
  return(C)
}
