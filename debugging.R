# set the below to 0 to stop all logging. set to 1 for printing only DBG_CAT(). set to 2 for printing both DBG_CAT() and DBG_PRINT()
DEBUG_PRINT_LEVEL = 1;

# the below will print the current function, line number and filename for easier debugging!
# By default it prints the third entry in function trace. You may need to play around with `n` to figure out the best usage!
DBG_CAT = gtools::defmacro(func,
                           n = 3,
                           level = DEBUG_PRINT_LEVEL,
                           file = stderr(),
                           header_text = '\n### Special Printing! ###\n',
                           footer_text = '\n\n',
                           systime_format = '%d-%m-%Y %H:%M:%S',
                           expr = {
                             if(level >= 1) {
                               cat(file = file,
                                   header_text,
                                   'Time: ', format(Sys.time(), systime_format),
                                   'Function: ', deparse(sys.calls()[[sys.nframe() - n]]),
                                   ' called in line ', getSrcLocation(func, 'line'),
                                   ' of file: ', getSrcFilename(func),
                                   ' in directory: ', getSrcDirectory(func),
                                   footer_text)
                             }
                           })


# the below will print the entire data.frame/table. Be cautious!
DBG_PRINT = function(..., level = DEBUG_PRINT_LEVEL, file = stderr()) {
  if(level >= 2) {
    vars = list(...)
    if(length(vars) > 1) {
      for(v in vars) {
        print(v);
      }
    } else {
      cat(file = file, paste0("Printing ", as.character(substitute(...)), ":\n"));
      print(...);
    }
    # vars = as.character(sys.call(which = sys.nframe())[-1])
    # if(length(vars) > 0) {
    #   for(v in vars) {
    #     cat(file = stderr(), paste0("Printing ", v, ":\n"));
    #     print(EVL(v));
    #   }
    # }
  }
}
