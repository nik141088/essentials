# counts the number of NA entries for each column in a dataset
na_counts = function(dt, return = F, need_WRDS_var_names = F) {
  
  if(is.data.frame(dt) || data.table::is.data.table(dt) || tibble::is.tibble(dt)) {
    # all ok!
  } else {
    stop("The first argument: (dt = ) should be a data.frame, a data.table or a tibble");
  }
  
  df_NA = is.na(dt);
  
  ret = data.frame( colSums(!df_NA), nrow(dt) );
  names(ret) = c("V1", "V2");
  ret$V3 = round(100 * (ret$V1 / ret$V2), 2);
  if(need_WRDS_var_names) {
    ret$V4 = wrds_get_var_info(colnames(dt));
    names(ret) = c("Num non-NA", "tot rows", "perc. non-NA", "WRDS meaning");
  } else {
    names(ret) = c("Num non-NA", "tot rows", "perc. non-NA");
  }
  
  print(ret);
  # In below if all entries of each row is non-NA then only their sum will be 0
  num_all_non_NA_rows = length(which(rowSums(df_NA) == 0));
  print(paste0(num_all_non_NA_rows, " (",
               round(100*num_all_non_NA_rows/nrow(dt), 2),"%)",
               " rows out of ", nrow(dt), " have all their enteries as non-NA"));
  
  if(return) {
    return(ret);
  } else {
    return(NULL);
  }
}





# function aliases
len = length;
describe = psych::describe;
coalesce = data.table::fcoalesce;





# to source a file between two partcular line numbers
source2 = function(file, start = 1, end = R.utils::countLines(file)[[1]], echo = T, max.deparse.length = 10000, keep.source = T, ...) {
  file.lines <- scan(file, what=character(), skip=start-1, nlines=end-start+1, sep='\n')
  file.lines.collapsed <- paste(file.lines, collapse='\n')
  source(textConnection(file.lines.collapsed), echo = echo, max.deparse.length = max.deparse.length, keep.source = keep.source, ...)
}








# write data to excel.
# Use: write.excel(your_data)
# After executing this command, the data now resides in the clipboard.
# Try changing the clipboard file if it doesn't work. (Try: `clipboard = "clipboard"`)
write.excel = function(data, clipboard = "clipboard-16384") {
  write.table(data, file = clipboard, row.names = FALSE, sep = "\t");
}

# read data from excel
# Use: data = read.excel()
# The above command reads data from clipboard
read.excel = function(header=TRUE, clipboard = "clipboard-16384") {
  read.table(file = clipboard, sep="\t", header=header) %>% as.data.table
}


# finds minimum and maximum values across all arguments.
# It is typically used as a delimiter for plot axes.
# Note: minimum is 0.99 times the actual minimum while maximum is 1.01 the actual minimum
ylim_range = function(..., tolerance__ = 0.01) {
  dots <- list(...);
  
  minimum = Inf;
  maximum = -Inf;
  
  # Let's print them out.
  for (i in seq_along(dots)) {
    minimum = min(minimum, dots[[i]], na.rm = TRUE); 
    maximum = max(maximum, dots[[i]], na.rm = TRUE); 
  }
  
  t = tolerance__
  
  minimum = ifelse(minimum > 0, (1-t)*minimum, (1+t)*minimum)
  maximum = ifelse(maximum > 0, (1+t)*maximum, (1-t)*maximum)
  
  c(minimum, maximum)
}










# this function should be called for each id separately
na_locf_until = function(x, n) {
  # in time series data, fill in na's untill indicated n
  l <- cumsum(!is.na(x)); # Basically tells which are NA and which aren't
  c(NA, x[!is.na(x)])[replace(l, ave(l, l, FUN=seq_along) > (n+1), 0) + 1]
}



# text evaluation
EVL = function(text, print = F) {
  if(print) {
    cat("Executing --> ", text, "\n");
  }
  eval(parse(text = text), envir=.GlobalEnv);
}

# list all the functions in a R source file
is_function = function (expr) {

  is_assign = is.call(expr) && as.character(expr[[1]]) %in% c('=', '<-', 'assign');
  
  if (!is_assign) {
    return(FALSE);
  }
  value = expr[[3]];
  is.call(value) && as.character(value[[1]]) == 'function';
}

list_functions <- function(filename = paste0(base_dir, "essential_R_functions.R"),
                           sort = F) {
  
  function_name = function(expr) as.character(expr[[2]]);
  
  file_parsed = parse(filename);
  functions = Filter(is_function, file_parsed);
  function_names = unlist(Map(function_name, functions));
  if(sort == T) {
    return(sort(function_names));
  } else {
    return(function_names);
  }
}





get_chunks = function(n, c) {
  1:n %>% split(., ceiling(c * seq_along(.) / (length(.)))) %>% return;
}


# Run until pass (in try catch)

TRY_CATCH = gtools::defmacro(expression, print.attempts = T, max.attempts = 10, ret_val = NULL, sleep = 0, expr = {
  passed__ = F;
  ret__ = ret_val;
  attempt__ = 0;
  while(passed__ == F & attempt__ < max.attempts) {
    Sys.sleep(sleep*1e-3); # in milli seconds
    attempt__ = attempt__ + 1;
    if(print.attempts == T) {
      cat("Attempt: ", attempt__, "\n");
    }
    tryCatch({
      ret__ = eval(expression);
      passed__ = T;
    }, error = function(e){
    }, finally = {})
  }
  remove(passed__, attempt__);
  return(ret__);
})


# proper way to call exists
EXISTS = function(x, ...) {
  exists(as.character(substitute(x)), ...);
}


# The below is a macro (short-hand) for the most used case of merge
MRG = gtools::defmacro(dt_1, dt_2, by = c("id", "date"), expr = {
  cols_1 = names(dt_1);
  cols_2 = names(dt_2);
  by_cols = by;
  mrg_cols = cols_2[which(!(cols_2 %in% cols_1))];
  if(len(mrg_cols) == 0) {
    # no need to merge
    dt_1 = dt_1;
  } else {
    mrg_cols = c(by_cols, mrg_cols);
    dt_1 = merge(dt_1, dt_2[, mrg_cols, with = F], by = by, all.x = T);
  }
})



# the function lag is present both in dplyr and data.table. We need to use lag from dplyr
LAG = function(x, n = 1L, default = NA, order_by = NULL, ...) {
  dplyr::lag(x, n, default, order_by, ...);
}

# the function lag is present both in dplyr and data.table. We need to use lag from dplyr
LEAD = function(x, n = 1L, default = NA, order_by = NULL, ...) {
  dplyr::lead(x, n, default, order_by, ...);
}

DELTA = function(x) {
  x - dplyr::lag(x);
}

# AVG(x) is same as (x_t + x_{t-1}) / 2
AVG = function(x, times = 2, lag = 0) {
  shift(x, seq(lag, lag + times - 1), type = "lag") %>% Reduce(`+`, .) / times;
}

# AVG1(x) is same as (x_{t-1} + x_{t-2}) / 2
AVG1 = function(x, times = 2) {
  AVG(x, times = times, lag = 1);
}


# Cumulative mean/sum etc with NAs
cumsum = function(x, na.rm = F) {
  if(na.rm == F) {
    base::cumsum(x);
  } else {
    "[<-"(x, !is.na(x), base::cumsum(na.omit(x)));
  }
}

cumprod = function(x, na.rm = F) {
  if(na.rm == F) {
    base::cumprod(x);
  } else {
    "[<-"(x, !is.na(x), base::cumprod(na.omit(x)));
  }
}

cummin = function(x, na.rm = F) {
  if(na.rm == F) {
    base::cummin(x);
  } else {
    "[<-"(x, !is.na(x), base::cummin(na.omit(x)));
  }
}

cummax = function(x, na.rm = F) {
  if(na.rm == F) {
    base::cummax(x);
  } else {
    "[<-"(x, !is.na(x), base::cummax(na.omit(x)));
  }
}

cummean = function(x, na.rm = F) {
  if(na.rm == F) {
    dplyr::cummean(x);
  } else {
    "[<-"(x, !is.na(x), dplyr::cummean(na.omit(x)));
  }
}






# CL0(x,y) is a short-hand for coalesce(x,0) + coalesce(y,0) with the additional requirement that both x and y are not NA.
CL0 = function(...) {
  x = 0;
  for(i in list(...)) {
    x = x + coalesce(i, as(0, typeof(i)));
  }
  
  # make sure all are not NAs
  y = 0;
  for(i in list(...)) {
    y = y | !is.na(i);
  }
  y[y == 0] = NA;
  
  x = x*y;
  
  return(x);
}


# CL0_prod(x,y) is a short-hand for coalesce(x,1) * coalesce(y,1) with the additional requirement that both x and y are not NA.
CL0_prod = function(...) {
  x = 1;
  for(i in list(...)) {
    x = x * coalesce(i, as(1, typeof(i)));
  }
  
  # make sure all are not NAs
  y = 0;
  for(i in list(...)) {
    y = y | !is.na(i);
  }
  y[y == 0] = NA;
  
  x = x*y;
  
  return(x);
}


COR = function(x, y = NULL, use = "pairwise.complete.obs", method = c("pearson", "kendall", "spearman"), round_digits = 2, ts_avg = NULL, ts_exclude_NA = T) {
  
  if(is.null(ts_avg)) {
    C = cor(x, y, use, method);
  } else {
    if(is.na(match(ts_avg, names(x)))) {
      stop("Please make sure ", ts_avg, " is part of your data.frame!")
    }
    if(!is.null(y)) {
      stop("Please provide entire data.frame as single argument!");
    }
    # find C for each ts_avg then average it
    cols = setdiff(names(x), ts_avg);
    unq_ts = unique(x[, ts_avg, with = F]) %>% unlist %>% sort %>% na.omit;
    C = matrix(0, nrow = len(cols), ncol = len(cols));
    C_zeros = C + diag(len(cols));
    rownames(C) = colnames(C) = cols;
    for(u in unq_ts) {
      C__ = cor(x[get(ts_avg) == u, cols, with = F], y, use, method);
      if(ts_exclude_NA == T) {
        C__ = coalesce(C__, C_zeros);
      }
      C = C + C__;
    }
    C = C / len(unq_ts);
  }
  
  if(is.null(round_digits) || round_digits == 0) {
    # leave C as it is
  } else {
    C = round(C, round_digits);
  }
  
  return(C);
}





COR_2 = function(x, y = NULL, method = c("pearson", "spearman"), round_digits = 2) {
  
  if(is.null(y)) {
    C = Hmisc::rcorr(x %>% as.matrix, type = method);
  } else {
    C = Hmisc::rcorr(x %>% as.matrix, y %>% as.matrix, type = method);
  }
  
  if(is.null(round_digits) || round_digits == 0) {
    # leave C as it is
  } else {
    C$r = round(C$r, round_digits);
  }
  
  return(C);
}



# both correlations combined
COR_both = function(dt, cols = NULL, round_digits = 2) {
  
  if(is.null(cols)) {
    cols = names(dt);
  }
  
  N = length(cols);
  
  C1 = COR(dt, method = "pearson", round_digits = round_digits);
  C2 = COR(dt, method = "spearman", round_digits = round_digits);
  
  # Need to take lower.tri of C and upper.tri of C2 and add them!
  lw = lower.tri(matrix(N*N, N, N), diag = F);
  up = upper.tri(matrix(N*N, N, N), diag = F);
  
  C = lw*C1 + up*C2;
  diag(C) = NA;
  
  return(C);

}




COV = function(x, y = NULL, use = "pairwise.complete.obs", method = c("pearson", "kendall", "spearman")) {
  cov(x, y, use, method)
}

VAR = function(x, y = NULL, use = "pairwise.complete.obs", ...) {
  var(x, y, use, method, ...)
}


is.POS = function(x) {
  return(x > 0)
}

is.NEG = function(x) {
  return(x < 0)
}

is.ZERO = function(x) {
  return(x == 0)
}


# for R multi-plot
optimal_layout = function(w, h, N, alpha = 0.5) {
  
  # forces alpha to be between 0 and 1
  alpha = max(0, min(alpha, 1));
  
  best_obj = Inf;
  best_n1 = NA_integer_;
  best_n2 = NA_integer_;
  
  for(n1 in 1:N) {
    for(n2 in 1:N) {
      
      if(n1 * n2 < N) {
        next;
      }
      
      obj = alpha*((n1/n2)/(h/w) - 1)^2 + (1-alpha)*(n1*n2/N - 1)^2;
      
      if(obj < best_obj) {
        best_obj = obj;
        best_n1 = n1;
        best_n2 = n2;
      }
      
    }
  }
  
  return(c(best_n1, best_n2));
  
}





# useful for plotting huge data columns
PLT = function(x, y = NULL, layout = NULL, layout_tuning = 0.5, gui_plot = F, width = 7.17, height = 10, pdf_file = "del.pdf",
               grid = F, pts = NULL, figure_margin = NULL, title = NULL, title_size = 2,
               h_line = NA, v_line = NA, h_line_col = "grey25", v_line_col = "grey75",
               type = "l", lwd = 2, col = "blue",  xlim = NULL, ylim = NULL,
               pre_expr = NA, post_expr = NA, pre_loop_expr = NULL, post_loop_expr = NULL,
               log = "", main = NULL, sub = NULL, xlab = NULL, ylab = NULL, lty = NULL,
               ann = par("ann"), axes = TRUE, frame.plot = axes,
               panel.first = NULL, panel.last = NULL, asp = NA, ...) {
  
  if(is.data.frame(x) == F) {
    if(is.null(y)) {
      # only x provided. Assume the other as counting from 1 to nrow(x)
      x = data.frame(x = 1:length(x), y = x);
    } else {
      x = data.frame(x = x, y = y);
    }
    print(x);
  }

  if(is.null(pts)) {
    pts = ifelse(is.data.frame(x), nrow(x), length(x));
  } else if(pts == "all") {
    pts = min(ifelse(is.data.frame(x), nrow(x), length(x)), 1000);
  } else if(is.numeric(pts) | is.integer(pts)) {
    pts = pts;
  } else {
    stop("pts must either be: 1) number/integer, 2) all OR 3) NULL")
  }
  
  pts = min(ifelse(is.data.frame(x), nrow(x), length(x)), ifelse(is.null(pts), 1000, pts));
  
  probabilities = seq(from = 0, to = 1, length.out = pts);
  
  is_DT = is.data.table(x);
  

  if(is.data.frame(x)) {
    
    if(ncol(x) < 2) {
      stop("data frame must have at-least 2 columns");
    }
    
    setDF(x);

    xx = quantile(x[,1], probs = probabilities, na.rm = T, names = F, type = 1);
    
    yy = x[match(xx, x[,1] %>% unlist), 2:ncol(x)];
    
    if(is.data.frame(yy) == F) {
      yy = data.frame(yy);
    }
    
    if(is_DT) {
      setDT(x);
    }
    
    if(is.null(xlab)) {
      xlab =  colnames(x[0,1]);
    } else if(length(xlab) == 1) {
      xlab = rep(xlab, ncol(x)-1);
    }
    
    if(is.null(ylab)) {
      ylab =  colnames(x[0, 2:ncol(x)]);
    } else if(length(ylab) == 1) {
      ylab = rep(ylab, ncol(x)-1);
    }
    
    # vectorize the below args
    if(!is.null(main) & length(main) == 1) { main = rep(main, ncol(x) - 1) }
    if(!is.null(sub) & length(sub) == 1) { sub = rep(sub, ncol(x) - 1) }
    if(!is.null(log) & length(log) == 1) { log = rep(log, ncol(x) - 1) }
    if(!is.null(type) & length(type) == 1) { type = rep(type, ncol(x) - 1) }
    if(!is.null(lty) & length(lty) == 1) { lty = rep(lty, ncol(x) - 1) }
    if(!is.null(col) & length(col) == 1) { col = rep(col, ncol(x) - 1) }
    if(!is.null(lwd) & length(lwd) == 1) { lwd = rep(lwd, ncol(x) - 1) }
    
    if(length(h_line) == 1) {
      h_line = rep(h_line, ncol(x) - 1);
    }
    
    if(length(v_line) == 1) {
      v_line = rep(v_line, ncol(x) - 1);
    }
    
    if(length(pre_expr) == 1) {
      pre_expr = rep(pre_expr, ncol(x) - 1);
    }
    
    if(length(post_expr) == 1) {
      post_expr = rep(post_expr, ncol(x) - 1);
    }
    
  }
  
  if(is.null(pdf_file)) {
    pdf_file = paste0(Sys.time() %>% gsub(" |:", "-", .), ".pdf");
  }
  
  
  if(gui_plot == F) {
    pdf(pdf_file, width, height);
  }
  
  # multi_plot(xx, yy, layout, type, xlim, ylim, log, main, sub, xlab, ylab, ann, axes, frame.plot, panel.first, panel.last, asp, ...);
  
  org_mfrow = par("mfrow");
  org_mar = par("mar");
  
  if(!is.null(figure_margin)) {
    par(mar = figure_margin);
  }
  
  if(is.null(layout)) {
    par(mfrow = optimal_layout(width, height, length(yy), layout_tuning)); # set the layout to be optimal. It mayn't be perfect
  } else {
    if(layout[1]*layout[2] < length(yy)) {
      stop("Wrong layout provided!");
    }
    par(mfrow = layout);
  }
  
  if(length(grid) == 1) {
    grid = rep(grid, length(yy));
  }

  if(!is.null(pre_loop_expr)) {
    for(p in 1:length(pre_loop_expr)) {
      eval(parse(text = pre_loop_expr[p]))
    }
  }
  
  for(i in 1:length(yy)) {

    if(!is.na(pre_expr[[i]][1])) {
      for(p in 1:length(pre_expr[[i]])) {
        eval(parse(text = pre_expr[[i]][p]))
      }
    }
    
    plot(xx, yy[, i] %>% unlist,
         type = type[i], xlim = xlim, ylim = ylim, log = log[i], main = main[i], sub = sub[i],
         xlab = xlab[i], ylab = ylab[i], lwd = lwd[i], col = col[i], lty = lty[i],
         ann = ann, axes = axes, frame.plot = frame.plot, panel.first = panel.first, panel.last = panel.last, asp = asp, ...);
    
    if(!is.na(post_expr[[i]][1])) {
      for(p in 1:length(post_expr[[i]])) {
        eval(parse(text = post_expr[[i]][p]))
      }
    }
    
    # h_line and v_line are list of vectors. One list element for each line respectively.
    if(!is.na(h_line[[i]][1])) {
      abline(h = h_line[[i]], lwd = 2, col = h_line_col, lty = 4);
    }
    
    if(!is.na(v_line[[i]][1])) {
      abline(v = v_line[[i]], lwd = 2, col = v_line_col, lty = 4);
    }
    
    if(grid[i] == T) {
      grid(NULL, NULL, lty = 1, col = "cornsilk2");
    }
  }
  
  if(!is.null(post_loop_expr)) {
    for(p in 1:length(post_loop_expr)) {
      eval(parse(text = post_loop_expr[p]))
    }
  }
  
  if(!is.null(title)) {
    title(title, line = -2, outer = T, cex.main = title_size);
  }
  
  if(gui_plot == F) {
    dev.off();
  }
  
  par(mar = org_mar);
  par(mfrow = org_mfrow);
  
}





# epsilon range
EPS = function(x = NULL, n = 1) {
  if(is.null(x)) {
    return( .Machine$double.eps * n );
  } else {
    return(c(x - .Machine$double.eps*n, x + .Machine$double.eps*n));
  }
}





# Winsorize "cols" within data.table dt at the level of "level". Do it separately for each every_col variable. If exclude == T, then extreme data points are removed from the table rather than winsoring it
# NOTE: since dt is being modified here, call by reference will not work! So the best way to use winsorization would be to apply it on a small set of variables before a regression.
# In case the winsorization needs to be done on entire panel use every_col = "ones" where ones is a column of all 1's
winsorize = function(dt, cols, level = 0.01, every_col = "date", exclude = F, quantile_type = 7) {
  
  if(is.na(match(every_col, colnames(dt)))) {
    stop(paste0("The data.table doesn't have ", every_col, " and id vars!"));
  }
  
  pct_lo = dt[, lapply(.SD, quantile, p = c(level), na.rm = T, type = quantile_type), .SDcols = cols, by = every_col];
  pct_hi = dt[, lapply(.SD, quantile, p = c(1-level), na.rm = T, type = quantile_type), .SDcols = cols, by = every_col];
  
  # Rename column names for differentiation
  colnames(pct_lo) = paste0("pct_lo.", colnames(pct_lo));
  colnames(pct_hi) = paste0("pct_hi.", colnames(pct_hi));
  # Don't change name of every_col variable
  setnames(pct_lo, paste0("pct_lo.", every_col), every_col);
  setnames(pct_hi, paste0("pct_hi.", every_col), every_col);
  
  dt = merge(dt, pct_lo, by = every_col, all.x = T, allow.cartesian = T);
  dt = merge(dt, pct_hi, by = every_col, all.x = T, allow.cartesian = T);
  
  for(c in cols) {
    col_class = dt[, class(get(c))];
    c_lo = paste0("pct_lo.", c);
    c_hi = paste0("pct_hi.", c);
    if(exclude == T) {
      # exclude at "level"
      # setting NA is equivalent to dropping it (they won't be used in rgeressions)
      dt = dt[get(c) < get(c_lo), eval(c) := as(NA, col_class)];
      dt = dt[get(c) > get(c_hi), eval(c) := as(NA, col_class)];
    } else {
      # winsorize at "level"
      dt[get(c) < get(c_lo), eval(c) := as(get(c_lo), col_class)];
      dt[get(c) > get(c_hi), eval(c) := as(get(c_hi), col_class)];
    }
  }
  
  # remove pct_lo and pct_hi vars
  del_cols = c(colnames(pct_lo), colnames(pct_hi)) %>% setdiff(every_col);
  dt[, (del_cols) := NULL];

  remove(pct_lo, pct_hi, c_lo, c_hi);
  
  return(dt);
}






# Finding ranks and then normalizing it to fall between 0 and 1
find_ranks = function(dt, vars, by_col = NULL, print = F) {
  
  options(warn = 2); # Need errors so that TRY_CATCH can return NA
  
  for(v in vars) {
    
    new_var_name = paste0(v, "_", paste0(by_col, collapse = "_"), if(!is.null(by_col)) "_", "RANK");
    # Do nothing if the variable already exists
    if(is.element(names(dt), new_var_name) %>% sum > 0) {
      cat("variable: ", new_var_name, " already exists!", "\n", sep = "");
      next;
    }
    
    if(print) {
      print(v);
    }
    
    # ranks start from 1
    
    dt[, new_var__ := frank(get(v), na.last = "keep", ties.method = "dense"), by = by_col];
    
    dt[, temp_min := TRY_CATCH(min(new_var__, na.rm = T), F, 1, NA_integer_), by = by_col];
    dt[, temp_max := TRY_CATCH(max(new_var__, na.rm = T), F, 1, NA_integer_), by = by_col];
    
    # normalize to lie betweeon 0 and 1    
    dt[, new_var__ := (new_var__ - temp_min) / (temp_max - temp_min)];
    
    dt[, temp_min := NULL];
    dt[, temp_max := NULL];
    
    setnames(dt, "new_var__", paste0(v, "_", paste0(by_col, collapse = "_"), if(!is.null(by_col)) "_", "RANK"));
    
  }
  
  options(warn = 0);
  
}





# Once ranks are known finding quintiles is extremely easy and fast
find_quintiles_from_ranks = function(dt, vars, boundaries = 10*(1:9), var_suffix = "_decile",
                                     quintile_names = NULL, by_col = NULL, print = F, custom_rank_vars = NULL) {
  
  pct = c(0, boundaries, 100) / 100;
  
  n = length(pct) - 1; # total number of quintiles
  
  if(is.null(quintile_names)) {
    quintile_names = paste0("QUINTILE_", 1:n);
  }
  
  cnt = 0;
  for(v in vars) {
    cnt = cnt + 1;
    
    if(is.null(custom_rank_vars)) {
      rank_var = paste0(v, "_", paste0(by_col, collapse = "_"), if(!is.null(by_col)) "_", "RANK");
    } else {
      rank_var = custom_rank_vars[cnt];
    }
    
    
    if(!is.element(rank_var, colnames(dt))) {
      stop(paste0("Var: ", rank_var, " not present in data.table! ",
                  "Run find_ranks(dt, \"", v, "\", ", ifelse(is.null(by_col), "NULL", paste0("\"", by_col, "\"")), ") first!"));
    }
    
    if(print) {
      print(v);
    }
    
    final_var = paste0(v, var_suffix);
    for(i in 1:n) {
      dt[pct[i] <= get(rank_var) & get(rank_var) < pct[i+1], eval(final_var) := quintile_names[i]];
    }
    
  }
  
}







# PLEASE consider using find_ranks() followed by find_quintiles_from_ranks() for speed!
# Find quintiles at boundaries for variables: vars. There will be length(boundaries) + 1 quintiles. Default is equally placed deciles
find_quintiles = function(dt, vars, boundaries = 10*(1:9), var_suffix = "_decile", quintile_names = NULL, by_col = NULL, print = T) {
  
  pct = c(0, boundaries, 100);
  
  n = length(pct) - 1; # total number of quintiles
  
  if(is.null(quintile_names)) {
    quintile_names = paste0("QUINTILE_", 1:n);
  }
  
  for(d in vars) {
    
    if(print) {
      print(d);
    }
    
    dt[, eval(paste0(d, ".pct.", 0)) := -Inf];
    for(b in boundaries) {
      var_name = paste0(d, ".pct.", b);
      if(is.null(by_col)) {
        dt[, eval(var_name) := quantile(get(d), p = b/100, na.rm = T)];
      } else {
        dt[, eval(var_name) := quantile(get(d), p = b/100, na.rm = T), by = by_col];
      }
    }
    dt[, eval(paste0(d, ".pct.", 100)) := Inf];
    
    final_var = paste0(d, var_suffix);
    for(i in 1:n) {
      lt_pct = paste0(d, ".pct.", pct[i]);
      rt_pct = paste0(d, ".pct.", pct[i+1]);
      dt[get(d) >= get(lt_pct) & get(d) < get(rt_pct), eval(final_var) := quintile_names[i]];
    }
    
    for(i in 1:length(pct)) {
      dt[, eval(paste0(d, ".pct.", pct[i])) := NULL];
    }
  }
  
}











# wrapper function for stargazer
REG = function(..., type = "text", out = NULL, omit_ser = T,
               title = "", style = "default", summary = NULL, out.header = FALSE, column.labels = NULL, column.separate = NULL,
               covariate.labels = NULL, dep.var.caption = NULL, dep.var.labels = NULL, dep.var.labels.include = TRUE, align = FALSE,
               coef = NULL, se = NULL, t = NULL, p = NULL, t.auto = TRUE, p.auto = TRUE, ci = FALSE, ci.custom = NULL, ci.level = 0.95,
               ci.separator = NULL, add.lines = NULL, apply.coef = NULL, apply.se = NULL, apply.t = NULL, apply.p = NULL, apply.ci = NULL,
               colnames = NULL, column.sep.width = "5pt", decimal.mark = NULL, df = TRUE, digit.separate = NULL, digit.separator = NULL, 
               digits = NULL, digits.extra = NULL, flip = FALSE, float = TRUE, float.env = "table", font.size = NULL, header = TRUE, 
               initial.zero = NULL, intercept.bottom = TRUE, intercept.top = FALSE, keep = NULL, keep.stat = NULL, label = "", model.names = NULL, 
               model.numbers = NULL, multicolumn = TRUE, no.space = NULL, notes = NULL, notes.align = NULL, notes.append = TRUE, notes.label = NULL, 
               object.names = FALSE, omit = NULL, omit.labels = NULL, omit.stat = NULL, omit.summary.stat = NULL, omit.table.layout = NULL, omit.yes.no = c("Yes", "No"),
               order = NULL, ord.intercepts = FALSE, perl = FALSE, report = NULL, rownames = NULL, rq.se = "nid", selection.equation = FALSE, 
               single.row = FALSE, star.char = NULL, star.cutoffs = NULL, suppress.errors = FALSE, table.layout = NULL, table.placement = "!htbp", 
               zero.component = FALSE, summary.logical = TRUE, summary.stat = NULL, nobs = TRUE, mean.sd = TRUE, min.max = TRUE, median = FALSE, iqr = FALSE) {
  
  
  
  if(is.null(out)) {
    if(type == "text") {
      # ok! file name not mandatory for text printing
      if(is.null(omit.stat) & omit_ser) {
        omit.stat = "ser";
      }
    } else {
      stop("Please provide output file name!")
    }
  } else {
    # Do both html and latex
    out_html  = paste0(out, ".html");
    out_latex = paste0(out, ".tex");
  }
  
  
  final = list();
  for(f in list(...)) {
    if(class(f) != "list") {
      f = list(f);
    }
    f = f[which(!sapply(f, is.null))];
    final = append(final, f);
  }
  
  
  if(type == "both") {
    
    stargazer::stargazer(final, type = "html", out = out_html,
                         title = title, style = style, summary = summary, out.header = out.header, column.labels = column.labels, column.separate = column.separate,
                         covariate.labels = covariate.labels, dep.var.caption = dep.var.caption, dep.var.labels = dep.var.labels, dep.var.labels.include = dep.var.labels.include, align = align,
                         coef = coef, se = se, t = t, p = p, t.auto = t.auto, p.auto = p.auto, ci = ci, ci.custom = ci.custom, ci.level = ci.level,
                         ci.separator = ci.separator, add.lines = add.lines, apply.coef = apply.coef, apply.se = apply.se, apply.t = apply.t, apply.p = apply.p, apply.ci = apply.ci,
                         colnames = colnames, column.sep.width = column.sep.width, decimal.mark = decimal.mark, df = df, digit.separate = digit.separate, digit.separator = digit.separator,
                         digits = digits, digits.extra = digits.extra, flip = flip, float = float, float.env = float.env, font.size = font.size, header = header,
                         initial.zero = initial.zero, intercept.bottom = intercept.bottom, intercept.top = intercept.top, keep = keep, keep.stat = keep.stat, label = label, model.names = model.names,
                         model.numbers = model.numbers, multicolumn = multicolumn, no.space = no.space, notes = notes, notes.align = notes.align, notes.append = notes.append, notes.label = notes.label,
                         object.names = object.names, omit = omit, omit.labels = omit.labels, omit.stat = omit.stat, omit.summary.stat = omit.summary.stat, omit.table.layout = omit.table.layout, omit.yes.no = omit.yes.no,
                         order = order, ord.intercepts = ord.intercepts, perl = perl, report = report, rownames = rownames, rq.se = rq.se, selection.equation = selection.equation,
                         single.row = single.row, star.char = star.char, star.cutoffs = star.cutoffs, suppress.errors = suppress.errors, table.layout = table.layout, table.placement = table.placement,
                         zero.component = zero.component, summary.logical = summary.logical, summary.stat = summary.stat, nobs = nobs, mean.sd = mean.sd, min.max = min.max, median = median, iqr = iqr);

    
    stargazer::stargazer(final, type = "latex", out = out_latex,
                         title = title, style = style, summary = summary, out.header = out.header, column.labels = column.labels, column.separate = column.separate,
                         covariate.labels = covariate.labels, dep.var.caption = dep.var.caption, dep.var.labels = dep.var.labels, dep.var.labels.include = dep.var.labels.include, align = align,
                         coef = coef, se = se, t = t, p = p, t.auto = t.auto, p.auto = p.auto, ci = ci, ci.custom = ci.custom, ci.level = ci.level,
                         ci.separator = ci.separator, add.lines = add.lines, apply.coef = apply.coef, apply.se = apply.se, apply.t = apply.t, apply.p = apply.p, apply.ci = apply.ci,
                         colnames = colnames, column.sep.width = column.sep.width, decimal.mark = decimal.mark, df = df, digit.separate = digit.separate, digit.separator = digit.separator,
                         digits = digits, digits.extra = digits.extra, flip = flip, float = float, float.env = float.env, font.size = font.size, header = header,
                         initial.zero = initial.zero, intercept.bottom = intercept.bottom, intercept.top = intercept.top, keep = keep, keep.stat = keep.stat, label = label, model.names = model.names,
                         model.numbers = model.numbers, multicolumn = multicolumn, no.space = no.space, notes = notes, notes.align = notes.align, notes.append = notes.append, notes.label = notes.label,
                         object.names = object.names, omit = omit, omit.labels = omit.labels, omit.stat = omit.stat, omit.summary.stat = omit.summary.stat, omit.table.layout = omit.table.layout, omit.yes.no = omit.yes.no,
                         order = order, ord.intercepts = ord.intercepts, perl = perl, report = report, rownames = rownames, rq.se = rq.se, selection.equation = selection.equation,
                         single.row = single.row, star.char = star.char, star.cutoffs = star.cutoffs, suppress.errors = suppress.errors, table.layout = table.layout, table.placement = table.placement,
                         zero.component = zero.component, summary.logical = summary.logical, summary.stat = summary.stat, nobs = nobs, mean.sd = mean.sd, min.max = min.max, median = median, iqr = iqr);
    
  } else {
    
    stargazer::stargazer(final, type = type, out = out,
                         title = title, style = style, summary = summary, out.header = out.header, column.labels = column.labels, column.separate = column.separate,
                         covariate.labels = covariate.labels, dep.var.caption = dep.var.caption, dep.var.labels = dep.var.labels, dep.var.labels.include = dep.var.labels.include, align = align,
                         coef = coef, se = se, t = t, p = p, t.auto = t.auto, p.auto = p.auto, ci = ci, ci.custom = ci.custom, ci.level = ci.level,
                         ci.separator = ci.separator, add.lines = add.lines, apply.coef = apply.coef, apply.se = apply.se, apply.t = apply.t, apply.p = apply.p, apply.ci = apply.ci,
                         colnames = colnames, column.sep.width = column.sep.width, decimal.mark = decimal.mark, df = df, digit.separate = digit.separate, digit.separator = digit.separator,
                         digits = digits, digits.extra = digits.extra, flip = flip, float = float, float.env = float.env, font.size = font.size, header = header,
                         initial.zero = initial.zero, intercept.bottom = intercept.bottom, intercept.top = intercept.top, keep = keep, keep.stat = keep.stat, label = label, model.names = model.names,
                         model.numbers = model.numbers, multicolumn = multicolumn, no.space = no.space, notes = notes, notes.align = notes.align, notes.append = notes.append, notes.label = notes.label,
                         object.names = object.names, omit = omit, omit.labels = omit.labels, omit.stat = omit.stat, omit.summary.stat = omit.summary.stat, omit.table.layout = omit.table.layout, omit.yes.no = omit.yes.no,
                         order = order, ord.intercepts = ord.intercepts, perl = perl, report = report, rownames = rownames, rq.se = rq.se, selection.equation = selection.equation,
                         single.row = single.row, star.char = star.char, star.cutoffs = star.cutoffs, suppress.errors = suppress.errors, table.layout = table.layout, table.placement = table.placement,
                         zero.component = zero.component, summary.logical = summary.logical, summary.stat = summary.stat, nobs = nobs, mean.sd = mean.sd, min.max = min.max, median = median, iqr = iqr);
    
  }
  
  
  
}




# wrapper function for grep
GRP = function(x, pattern, ignore.case = F, perl = F, value = T, fixed = F, useBytes = F, invert = F, non_char = F) {
  if(!non_char & !("character" %in% class(x))) {
    stop("You're trying to grep a non-character object. To proceed, please set non_char to TRUE.");
  }
  
  return( grep(pattern = pattern, x = x, ignore.case = ignore.case, perl = perl, value = value, fixed = fixed, useBytes = useBytes, invert = invert) );
}







# Make a decile view with col_1 going top to bottom and col_2 going left to right. fun is applied on the var
decile_view = function(dt, col_1, col_2, grid = c(10, 10), small_calc = T, var = "log_turn", fun = mean, print = F, dim_names = T, switch_dimensions = F, ...) {
  
  if(switch_dimensions) {
    temp__ = col_1;
    col_1 = col_2;
    col_2 = temp__;
    remove(temp__);
  }
  
  var_names =  var %>%
    strsplit("+", fixed = T) %>% unlist %>% trimws %>%
    strsplit("-", fixed = T) %>% unlist %>% trimws %>%
    strsplit("*", fixed = T) %>% unlist %>% trimws %>%
    strsplit("/", fixed = T) %>% unlist %>% trimws %>%
    strsplit("(", fixed = T) %>% unlist %>% trimws %>%
    strsplit(")", fixed = T) %>% unlist %>% trimws %>%
    strsplit("\\^\\d+", fixed = F) %>% unlist %>% trimws;
  var_names = stringr::str_replace_all(var_names, "^\\d+$", "");
  var_names = var_names[nchar(var_names) > 0];
  
  temp = dt[, lapply(.SD, fun, ...), .SDcols = var_names, by = c(col_1, col_2)] %>% copy;
  temp = temp[!is.na(get(col_1)) & !is.na(get(col_2)) & get(col_1) != "" & get(col_2) != ""];

  eval(parse(text = paste0("temp[, final := ", var, "]")));

  # temp = temp[, c(col_1, col_2, "final"), with = F];
  temp[, (var_names) := NULL];
  
  temp = temp %>% dcast(get(col_1) ~ get(col_2), value.var = "final");
  row_names = temp[,1] %>% unlist;
  temp = temp[, -1]; # remove the names column
  
  if(is.null(grid)) {
    grid = dim(temp);
  }
  
  setDF(temp);
  max_diff = (temp[grid[1], grid[2]] - temp[1,1]) %>% unlist;
  if(print) {
    cat("Top-Bottom: ", col_1, " and Left-Right: ", col_2, " -- max. diff: ", max_diff, "\n");
  }
  
  if(small_calc == T) {
    rownames(temp) = row_names;
    
    if(dim_names == F) {
      colnames(temp) = rownames(temp) = NULL;
    }
    
    return(temp);
  }
  
  temp[, paste0(grid[2], "_minus_", 1)]  = temp[, grid[2]] - temp[, 1];
  temp[, paste0(grid[2], "_by_", 1)]     = temp[, grid[2]] / temp[, 1];
  temp[grid[1]+1, ]                      = temp[grid[1], ] - temp[1, ]; # [paste0(grid[1], "_minus_", 1), 1]
  temp[grid[1]+2, ]                      = temp[grid[1], ] / temp[1, ];
  setDT(temp);
  
  temp = cbind(c(row_names, paste0(grid[1], c("_minus_", "_by_"), 1)), temp);
  colnames(temp)[1] = "";
  
  if(dim_names == F) {
    colnames(temp) = rownames(temp) = NULL;
  }
  
  return(temp);
}






# latex lag vars
latex_lag_vars = function(cols, include = NULL, exclude = NULL) {
  
  if(!is.null(include) & !is.null(exclude)) {
    stop("Only include or exclude should be included!");
  }
  
  ret_val = c();
  
  if(!is.null(include) & length(include) > 0) {
    
    for(i in 1:len(cols)) {
      if(i %in% include) {
        ret_val = c(ret_val, paste0("$", cols[i], "_{t-1}$"));
      } else {
        ret_val = c(ret_val, paste0("$", cols[i], "$"));
      }
    }
    
  } else if(!is.null(exclude) & length(exclude) > 0) {
    
    for(i in 1:len(cols)) {
      if(i %in% exclude) {
        ret_val = c(ret_val, paste0("$", cols[i], "$"));
      } else {
        ret_val = c(ret_val, paste0("$", cols[i], "_{t-1}$"));
      }
    }
    
  } else {
    
    ret_val = paste0("$", cols, "_{t-1}$");
    
  }
  
  return(ret_val);
}

  






decimal_align = function(dt, sig_digits = 3) {
  
  fmt = paste0("%0.", sig_digits, "f");
  
  max_char = dt %>% sprintf(fmt, .) %>% nchar %>% max;
  
  pad = rep(" ", max_char) %>% paste0(collapse = "");
  
  dt %>% sprintf(paste0(pad, fmt), .) %>% substr(., nchar(.) - max_char + 1, nchar(.)) %>% gsub("NA", "  ", .);
  
}








# This function finds residuals from a regression specified in formula, f. The data is available in dt. residuals are assigned to new_var. If by_var is specified then regressions are run separately for each by_var. Additionally it is required that each regression is ran where valid_N non-NA data points are available
find_residuals = function(dt, f, new_var, by_var = "id", valid_N = 20) {
  
  if(is.null(valid_N)) {
    valid_N = 0;
  }
  
  dep_var = as.character(f)[2];
  
  ind_vars = as.character(f)[3];
  ind_vars = strsplit(ind_vars, "\\+")[[1]] %>% gsub(" ", "", .);
  # interaction terms?
  unique_ind_vars = ind_vars;
  interaction_idx = which(str_detect(ind_vars, ":"));
  if(len(interaction_idx) > 0) {
    for(i in interaction_idx) {
      unique_ind_vars = setdiff(unique_ind_vars, ind_vars[i]);
      unique_ind_vars = c(unique_ind_vars, strsplit(ind_vars[i], "\\:")[[1]] %>% gsub(" ", "", .)); 
    }
  }
  unique_ind_vars = unique(unique_ind_vars);
  
  zero_idx = which(ind_vars == "0");
  if(length(zero_idx) > 0) {
    ind_vars = ind_vars[-zero_idx];
  }
  
  reg_vars = paste0("slp__", 1:length(ind_vars));
  if(length(zero_idx) == 0) {
    reg_vars = c("int__", reg_vars);
  }
  
  expr = paste0("!is.na(", dep_var, ") & !is.na(", paste0(unique_ind_vars, collapse = ") & !is.na("), ")") %>% parse(text = .);
  
  # it may very well happen that there are no valid data to run lm. To counter that use TRY_CATCH
  expr_reg = paste0("(reg_vars) := TRY_CATCH(lm(", deparse(f), ")$coef, print.attempts = F, max.attempts = 1, ret_val = rep(NA_real_, len(reg_vars))) %>% as.list") %>% parse(text = .);
  # expr_reg = paste0("(reg_vars) := lm(", deparse(f), ")$coef %>% as.list") %>% parse(text = .);
  
  if(is.null(by_var)) {
    
    dt[eval(expr), eval(expr_reg)];
    
  } else {
    
    valid_by_vars = dt[eval(expr), .N, by = by_var][N >= valid_N][, by_var, with = F];
    
    setkeyv(dt, by_var);
    setkeyv(valid_by_vars, by_var);
    
    dt[valid_by_vars, eval(expr_reg), by = by_var];
    
  }
  
  # take care of interactions
  expr_ind_vars = str_replace(ind_vars, ":", "*");
  
  if(length(zero_idx) == 0) {
    expr2 = paste0(new_var, " := ", dep_var, " - (", paste0(reg_vars, "*", c(1, expr_ind_vars), collapse = " + "), ")") %>% parse(text = .);
  } else {
    expr2 = paste0(new_var, " := ", dep_var, " - (", paste0(reg_vars, "*", c(expr_ind_vars), collapse = " + "), ")") %>% parse(text = .);
  }
  
  dt[, eval(expr2)];
  dt[, (reg_vars) := NULL];
  
}








# the first gsub chnages existing "\\\\" to "\\". The second gsub changes all "\\" (incl. the existing and new ones) to "\\\\"
text_spec2 = function(x, format = "latex", bold = FALSE, italic = FALSE, monospace = FALSE, 
                      underline = FALSE, strikeout = FALSE, color = NULL, background = NULL, 
                      align = NULL, font_size = NULL, angle = NULL, tooltip = NULL, 
                      popover = NULL, link = NULL, extra_css = NULL, escape = F, 
                      background_as_tile = TRUE, latex_background_in_cell = FALSE) {
  
  kableExtra::text_spec(x = x, format = format, bold = bold, italic = italic, monospace = monospace, 
            underline = underline, strikeout = strikeout, color = color, background = background, 
            align = align, font_size = font_size, angle = angle, tooltip = tooltip, 
            popover = popover, link = link, extra_css = extra_css, escape = escape, 
            background_as_tile = background_as_tile, latex_background_in_cell = latex_background_in_cell) %>%
    gsub("\\\\\\\\", "\\\\", .) %>%
    gsub("\\\\", "\\\\\\\\", .);
  
}









# fill missing dates by id
fill_missing_dates = function(dt, date_var = "date", firm_var = "id", vars = NULL) {
  
  # change the column name to something unique
  setnames(dt, firm_var, "id__")
  setnames(dt, date_var, "date__")
  
  setorder(dt, id__, date__);
  
  
  # keep only those entries which have a valid id and date__
  dt = dt[!is.na(id__) & !is.na(date__)];
  
  date.bookends = merge(dt[, date__[1], by = id__], # first date of each id__
                        dt[, date__[.N], by = id__], # last date of each id__
                        by = "id__");
  
  colnames(date.bookends) = c("id__", "first", "last");
  setorder(date.bookends, id__);
  
  
  unq_trading_dates = sort( dt[, unique(date__)] );
  unq_trading_dates = data.table(date__ = unq_trading_dates, start_date_num = 1:length(unq_trading_dates));
  unq_trading_dates[, end_date_num := start_date_num];
  
  date.bookends = merge(date.bookends, unq_trading_dates[, .(date__, start_date_num)], by.x = "first", by.y = "date__", all.x = T);
  date.bookends = merge(date.bookends, unq_trading_dates[, .(date__, end_date_num)], by.x = "last", by.y = "date__", all.x = T);
  
  setorder(date.bookends, id__);
  
  # takes 30 sec max. final is ~ 1.3 GB
  final = lapply(1:nrow(date.bookends), function(i) ( data.table(id__ = date.bookends$id__[i], date__ = unq_trading_dates$date__[ date.bookends$start_date_num[i] : date.bookends$end_date_num[i] ]) ) ) %>% rbindlist; # don't call rbindlist in a loop. It slows it down considerably (O(n^2))
  
  remove(date.bookends, unq_trading_dates);
  
  # Now merge dt on final on (id__, date__) pair. Many of the entries in final data.table will be NAs. Takes ~ 1 min.
  if(is.null(vars)) {
    dt = merge(final, dt, by = c("id__", "date__"), all.x = T);
  } else {
    vars = union(c("id__", "date__"), vars);
    dt = merge(final, dt[, vars, with = F], by = c("id__", "date__"), all.x = T);
  }
  remove(final);
  
  # change the column name to back to original
  setnames(dt, "date__", date_var)
  setnames(dt, "id__", firm_var)
  
  return(dt);
}











# fill missing generally
# year quarter usage: fill_missing_general(dt, first_var = "fyearq", second_var = "fqtr", FACTOR = 4)
# year month usage: fill_missing_general(dt, first_var = "year", second_var = "month", FACTOR = 12)
fill_missing_general = function(dt, first_var, second_var, FACTOR = NULL, firm_var = "id", vars = NULL) {
  
  dt[, new_date_tmp__ := FACTOR*get(first_var) + get(second_var) - 1];
  
  # now call fill_missing_dates()
  dt = fill_missing_dates(dt = dt, date_var = "new_date_tmp__", firm_var = firm_var, vars = vars);
  
  # convert new_date_tmp__ to first_var and second_var
  dt[, eval(first_var)  := as.integer( new_date_tmp__ / FACTOR )];
  dt[, eval(second_var) := new_date_tmp__ - FACTOR*get(first_var) + 1];
  
  # remove new_date_tmp__
  dt[, new_date_tmp__ := NULL];  
  
  return(dt);

}















# find summary of a lfe::felm type object. It's useful for storing!
lfe_summ = function(f, prcnt_explained = NULL) {
  
  n = sum(!sapply(f, is.null));
  
  if(!is.null(prcnt_explained)) {
    if(len(prcnt_explained) != n) {
      stop("length of prcnt_explained doesn't match with the number of regressions supplied in f.")
    }
  }

  if(sapply(f[1:n], class) %>% unique != "felm") {
    stop("lfe_summ can only accept objects of type: lfe::felm");
  }
  
  summ = vector("list", n);
  
  for(i in 1:n) {
    s = summary(f[[i]]);
    summ[[i]] = list(call = s$call,
                     coefficients = as.data.frame(s$coefficients),
                     r.squared = s$r.squared,
                     adj.r.squared = s$adj.r.squared,
                     P.r.squared = s$P.r.squared,
                     P.adj.r.squared = s$P.adj.r.squared,
                     N = s$N,
                     prcnt_explained = prcnt_explained[i]);
  }
  
  return(summ);
}











# force variable to lie within a range
force_range = function(x, x_min = 0, x_max = 1, ret_expr = F, round_digits = 3) {
  
  # new range
  A_1 = x_min;
  A_n = x_max;
  R = A_n - A_1;
  
  if(!(A_n > A_1)) {
    stop("Invalid new range provided!")
  }
  
  a_1 = min(x, na.rm = T);
  a_n = max(x, na.rm = T);
  
  if(is.infinite(a_1) | is.infinite(a_n)) {
    stop("The input has no valid entries!");
  }
  
  if(!(a_n > a_1)) {
    stop("The input vector has no variation!")
  }
  
  r = a_n - a_1; # r > 0 as well as R > 0
  
  # set range to [0,1]
  y = (x - a_1) / r;
  
  # now force range to [A_1, A_n]
  z = y*R + A_1;
  
  # z = y*R + A_1 
  #   = R*(x - a_1)/r + A_1
  #   = (R/r)*x - (R/r)*a_1 + A_1
  #   = (A_1 - (R/r)*a_1) + (R/r)*x
  if(ret_expr == T) {
    int = (A_1 - (R/r)*a_1);
    slp = (R/r);
    sig_str = paste0("%0.", round_digits, "f + %0.", round_digits, "f * x");
    return(sprintf(sig_str, int, slp));
  }
  
  return(z);
  
}








# stars in stastical significance. E.g.
# ifelse(p.val < .0001, "****", ifelse(p.val < .001, "*** ", ifelse(p.val < .01, "**  ", ifelse(p.val < .05, "*   ", "    "))));
# p.val.cutoff must be a vector such that the first entry is the cut-off between 0-star and 1-star; 2nd value is the cut-off btwn 1-star and 2-star; ... ; n-th value is the cut-off between (n-1)-star and n-star
stars = function(x, p.val.cutoff = c(0.05, 0.01, 0.001, 0.0001), symbol = "*", pad = "", tnote = F, tnote_beg_text = "\\tnote{", tnote_end_text = "}") {
  n = len(p.val.cutoff);
  
  if(n < 1) {
    stop("You must provide at-least one valid entry in p.val.cutoff");
  }
  
  p.val.cutoff = c(1, p.val.cutoff, 0);
  
  stars = rep("", len(x));
  
  for(i in 0:n) {
    if(p.val.cutoff[i+1] <= p.val.cutoff[i+2]) {
      stop("p.val.cutoff must be strictly decreasing and lie between 0 and 1 (exclusive).")
    }
    idx = which(x %between% c(p.val.cutoff[i+2], p.val.cutoff[i+1]));
    if(len(idx) > 0) {
      stars[idx] = c(rep(symbol, i), rep(pad, n-i)) %>% paste0(collapse = "");
      if(tnote == T) {
        stars[idx] = paste0(tnote_beg_text, stars[idx], tnote_end_text);
      }
    }
  }
  
  if(is.matrix(x)) {
    stars = matrix(stars, nrow = nrow(x), ncol = ncol(x));
  }
  
  return(stars);
}


















# The below function performs a overlapping merge between two tables: dt (main table with one date variable) AND names (secondary table with start and end dates). The required variables (oher than id and date vars) from names table should be specified in extra_names_var.
# nomatch argument controls whether non-matching names rows are returned as NA (nomatch = NA) or the corresponding dt rows are skipped altogether (nomatch = 0). Note that nomatch = 0 will change the composition of dt.
overlapping_merge = function(dt, names,
                             names_id = "id", names_date_vars = c("start_date", "end_date"), 
                             dt_id = "id", dt_date_var = "date",
                             extra_names_var = c(), nomatch = NA) {
  
  other_dt_vars = setdiff(names(dt), c(dt_id, dt_date_var));
    
  setkeyv(names, c(names_id, names_date_vars));
  # copy date var
  dt_date_var2 = "date2__";
  dt[, eval(dt_date_var2) := get(dt_date_var)];
  # overlapping merge
  dt = foverlaps(dt, names[, c(names_id, names_date_vars, extra_names_var), with = F],
                 by.x = c(dt_id, dt_date_var, dt_date_var2), nomatch = nomatch);
  # remove id and date vars of names
  dt[, c(names_date_vars, dt_date_var2) := NULL];
  # re-order column names
  cols = c(dt_id, dt_date_var, other_dt_vars, extra_names_var);
  dt = dt[, cols, with = F];
  
  return(dt);
  
}







# memory usage of R objects
mem_use = function(n = 10, obj_list = ls(envir = parent.env(environment())), excl_fn = T) {
  ans = rep(NA, len(obj_list));
  for(i in 1:len(obj_list)) {
    if(excl_fn & is.function(get(obj_list[i]))) {
      next;
    }
    ans[i] = round(object.size(get(obj_list[i]))/2^20, 2)
  }
  names(ans) = obj_list;
  ans = ans[!is.na(ans)];
  ans = sort(ans, decreasing = T);
  if(len(obj_list) < n) {
    return(ans);
  } else {
    return(ans[1:n]);
  }
}






# the below give all the packages used in a shiny app
libs_used_in_shiny_app = function(shiny_app_dir, file_type = "*", regex_str = "^\\s*library|^\\s*require") {
  
  all_files = list.files(path = shiny_app_dir,
                         pattern = file_type,
                         full.names = T, recursive = T);
  
  libs = c();
  file = c();
  for(f in all_files) {
    txt = readLines(f);
    idx = str_detect(txt, regex_str) %>% which;
    libs = c(libs, trimws(txt[idx]));
    file = c(file, rep(f, len(idx)));
  }
  
  return(data.table(file = file, libs = libs));
  
}
