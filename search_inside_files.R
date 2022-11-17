# this is a purely R based pattern detection utility to search through files. Mostly useful for searching inside R/py files. It doesn't depend on any external tool. Required packages are `stringr` and `crayon`. To disable coloring use `identity` as the color function.
search_inside_files = function(target_dir = getwd(),
                               pattern,
                               recursive = FALSE,
                               which_files = "*\\.[R|r]$",
                               skip_comment_lines = TRUE,
                               show_all_lines = FALSE,
                               show_nonmatching_filenames = TRUE,
                               show_line_num = TRUE,
                               file_out = stdout(),
                               return_dataframe = F,
                               comment_line_color_fun = crayon::bgBlue,
                               matched_color_fun = crayon::bgGreen,
                               line_color_fun = crayon::bgRed,
                               filename_color_fun = crayon::cyan) {
  
  if(length(pattern) > 1) {
    pattern = paste0(pattern, collapse = "|")
  }
  
  # list all R files
  if(dir.exists(target_dir)) {
    files = list.files(target_dir, pattern = which_files, full.names = T, recursive = recursive, all.files = T)
  } else {
    warning(paste0(target_dir, " doesn't exists! Trying to use it as a filename instead!"))
    if(file.exists(target_dir)) {
      files = target_dir
    } else {
      stop(paste0("Check whether ", target_dir, " is correct!"))
    }
  }
  
  if(length(files) == 0) {
    cat("No matching files found!", file = stderr())
  } else {
    # search for pattern
    # read all files
    txt = lapply(files, readLines, warn = F)
    txt_n = sapply(txt, length)
    max_nchar_txt_n = max(nchar(txt_n))
    
    if(return_dataframe) {
      ret_df = NULL
    }
    
    for(f in seq_along(txt)) {
      
      # print contents of the file
      # paste0(txt[[f]], collapse = "\n") %>% cat
      empty_lines = str_detect(txt[[f]], "^\\s+$")
      comment_lines = str_detect(txt[[f]], "^\\s*#")
      # search pattern
      pattern_lines = str_detect(txt[[f]], pattern)

      if(show_nonmatching_filenames == TRUE) {
        str = paste0("File: ", files[f]) %>% filename_color_fun
        cat(file = file_out, sep = "", str, "\n")
      } else {
        # some match in pattern_lines which are not in comment_lines, OR
        # some match in pattern_lines as well as comment_lines
        if((any(pattern_lines) & !any(pattern_lines & comment_lines)) |
           (any(pattern_lines & comment_lines) & !skip_comment_lines)) {
          str = paste0("File: ", files[f]) %>% filename_color_fun
          cat(file = file_out, sep = "", str, "\n")
        }
      }

      color_text = data.table(linenum = txt_n[f],
                              org = txt[[f]],
                              comment = FALSE,
                              empty = FALSE,
                              pattern = FALSE,
                              len = nchar(txt[[f]]),
                              col = NA_character_)
      # update color_text
      color_text[, col := org]
      color_text[empty_lines, empty := TRUE]
      color_text[comment_lines, comment := TRUE]
      color_text[pattern_lines, pattern := TRUE]
      # color matched text
      for(p in 1:nrow(color_text)) {
        # only include pattern_lines
        if(color_text$pattern[p] == FALSE) {
          next
        }
        # locate all pattern matches
        l = str_locate_all(color_text$org[p], pattern)[[1]] %>% as.data.frame
        # create keep/replace string start/ends
        l$replace = 1
        r = 1
        while(r <= nrow(l)) {
          # beginning
          if(r == 1 && l$start[r] > 1) {
            # cat("start r:", r, "\n")
            l = rbind(c(1, l$start[r]-1, 0),
                      l)
            r = 1
            next
          }
          # end
          if(r == nrow(l) && l$end[r] < color_text$len[p]) {
            # cat("end r:", r, "\n")
            l = rbind(l,
                      c(l$end[r]+1, color_text$len[p], 0))
            r = 1
            next
          }
          # in-between
          cond = (l$start[r] > l$end[r-1]+1)
          if(length(cond) > 0 && cond == TRUE) {
            # cat("in-between r:", r, "\n")
            l = rbind(l[1:(r-1),],
                      c(l$end[r-1]+1, l$start[r]-1, 0),
                      l[r:nrow(l),])
            r = 1
            next
          }
          # inc loop counter
          r = r + 1
        }
        # replace text as needed
        l$org = sapply(1:nrow(l), function(m) substr(color_text$org[p], l$start[m], l$end[m]))
        setDT(l)
        l[, col := org]
        l[replace == 1, col := col %>% matched_color_fun]
        if(color_text$comment[p] == TRUE) {
          l[replace == 0, col := col %>% comment_line_color_fun]
        }
        color_text$col[p] = paste0(l$col, collapse = "")
      }
      
      # print to output
      for(p in 1:nrow(color_text)) {
        # show all lines?
        if(show_all_lines == FALSE && color_text$pattern[p] == FALSE) {
          next
        }
        # show comment lines?
        if(skip_comment_lines == TRUE && color_text$comment[p] == TRUE) {
          next
        }
        
        cat(file = file_out, sep = "",
            if(show_line_num)
              sprintf(paste0("%0.", max_nchar_txt_n, "d: "), p) %>% line_color_fun,
            color_text$col[p], "\n")
      }
      
      if(return_dataframe) {
        ret_df = rbind(ret_df, cbind(file =  files[f], color_text))
      }
      
    }
    
    if(return_dataframe) {
      return(ret_df)
    }
    
  }
  
}

# # usage
# search_inside_files(target_dir = "C:/Users/nikhi/Documents/app/",
#                     pattern = "amount",
#                     recursive = T)

# # Usage #2 (show all library load commands)
# pkg_naming_rule = "[a-zA-Z][a-zA-Z0-9\\.]*[a-zA-Z0-9]"
# search_inside_files(target_dir = "C:/Users/nikhi/Documents/app/",
#                     recursive = T,
#                     pattern = paste0("\\s*library\\(", pkg_naming_rule, "\\)"))




# List all the packages (in order) that are called/loaded/used in a directory
packages_within_a_folder = function(dir, include_RProfile = T, file_out = "nul") {
  # letters (small and capital), numbers and dots. Start with a letter. End with a letter or a number. Have atleast 2 characters!
  r_pkg_name_pattern = "^[a-zA-Z][a-zA-Z0-9\\.]*[a-zA-Z0-9]$"
  # check on your machine: which(str_detect(list.files(.libPaths()[1]), r_pkg_name_pattern) == F)
  
  stripped_pkg_pattern = str_replace_all(r_pkg_name_pattern, "\\$$|^\\^", "")
  lib_patterns = c(paste0("^\\s*library\\(", stripped_pkg_pattern, "\\)"),
                   paste0("^\\s*require\\(", stripped_pkg_pattern, "\\)"),
                   paste0("\\b", stripped_pkg_pattern, "\\:\\:"))
  
  if(include_RProfile == TRUE && file.exists("~/.RProfile")) {
    # First search within ~/.Rprofile
    tmp = search_inside_files(target_dir = "~",
                              which_files = "^\\.RProfile$",
                              pattern = lib_patterns,
                              recursive = F,
                              show_nonmatching_filenames = F,
                              return_dataframe = T,
                              file_out = file_out)
    tmp = tmp[pattern == TRUE & comment == FALSE & empty == FALSE]
  } else {
    tmp = NULL
  }
  
  # Now search in the requested directory
  df = search_inside_files(target_dir = dir,
                           pattern = lib_patterns,
                           recursive = T,
                           show_nonmatching_filenames = F,
                           return_dataframe = T,
                           file_out = file_out)
  df = df[pattern == TRUE & comment == FALSE & empty == FALSE]
  
  # combine
  df = rbind(tmp, df)
  
  df = df[, .(file, pkg = org)]
  df[, pkg_extract := str_extract(pkg, paste0(lib_patterns, collapse = "|"))]
  # remote irrelevant text
  df[, pkg_extract := str_replace_all(pkg_extract, "^\\s*library\\(", "")]
  df[, pkg_extract := str_replace_all(pkg_extract, "^\\s*require\\(", "")]
  df[, pkg_extract := str_replace_all(pkg_extract, "\\)$", "")]
  df[, pkg_extract := str_replace_all(pkg_extract, "\\:\\:$", "")]
  
  pkgs = unique(df$pkg_extract)
  
  return(pkgs)
  
}

# # Usage
# pkgs = packages_within_a_folder(dir = "C:/Users/nikhi/Documents/app/")



# R-code to install missing packages wrt a folder
missing_package_R_code = function(pkgs, print = F, return_text = F) {
  
  if(print == F & return_text == F) {
    print = T
  }
  
  text = paste0("# required packages:", "\n",
                "required_packages = c(\"", paste0(pkgs, collapse = "\", \""), "\")", "\n",
                "# installed packages:", "\n",
                "installed_packages = list.files(.libPaths()[1])", "\n",
                "# new packages to be installed:", "\n",
                "new_packages = setdiff(required_packages, installed_packages)", "\n",
                "while(length(new_packages) > 0) {", "\n",
                "  p = new_packages[1]", "\n",
                "  install.packages(p)", "\n",
                "  installed_packages = list.files(.libPaths()[1])", "\n",
                "  new_packages = setdiff(required_packages, installed_packages)", "\n",
                "}", "\n")
  
  if(print) {
    crayon::bgGreen("# Put the below in your app:\n") %>% cat
    crayon::bgGreen("# -----------------------------\n\n") %>% cat
    crayon::bgYellow(text) %>% cat
    crayon::bgGreen("\n# -----------------------------\n") %>% cat
  }
  
  if(return_text) {
    return(text)
  }
  
}

# # Usage
# pkgs = packages_within_a_folder(dir = "C:/Users/nikhi/Documents/app/")
# missing_package_R_code(pkgs)
