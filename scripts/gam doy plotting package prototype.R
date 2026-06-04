
## Function that takes an mgcv model and identifies the response variable and predictor variables, distinguishing
## predictors that are numeric and predictors that are factors
parse_terms = function(model){
  all.terms = names(model$var.summary)
  response = as.character(model$formula)[2]
  dat.model = model$model |> 
    select(-any_of(response))
  
  predictors = setdiff(all.terms, response)
  predictors.factor = names(dat.model)[sapply(dat.model, function(x){is.factor(x) | is.character(x)})]
  predictors.numeric = setdiff(predictors, predictors.factor)
  return(list(response = response, 
              predictors.factor = predictors.factor,
              predictors.numeric = predictors.numeric))
}

## Function that 
predict_gam = function(model, plot_across = "doy",
                       across_increment = 1,
                       quant_trimming = 0.01){ ## Defines quantile-based trimming before predicting the `plot_across` variable. 
  ##                    For example, if this is 0.01 and plot_across is "doy", predictions will range between the 0.01 and 0.99 quantiles of observed doy within each combination of factor predictors
  dat.model = model$model |> 
    select(-any_of(response))
  
  ## parse terms
  terms.ls = parse_terms(model)
  predictors.factor = terms.ls$predictors.factor
  predictors.numeric = setdiff(terms.ls$predictors.numeric, plot_across)
  response = terms.ls$response
  
  helper_sequencer = function(dat){expand_grid(seq(quantile(dat[[plot_across]], quant_trimming),
                                                   quantile(dat[[plot_across]], 1 - quant_trimming),
                                                   by = across_increment))}
  
  pred.df = dat.model |> 
    select(any_of(c(predictors.factor, plot_across))) |> 
    nest(.by = any_of(predictors.factor)) |> 
    mutate(pred_mats = purrr::map(data, helper_sequencer)) |> 
    select(-data) |> 
    unnest(pred_mats)
  names(pred.df)[ncol(pred.df)] = plot_across
  
  predictors.list = list()
  
  cli::cli_alert("Using median values of {predictors.numeric} across ALL factor predictors.")
  for(i in 1:length(predictors.numeric)){
    predictors.list[[predictors.numeric[i]]] = median(dat.model[[predictors.numeric[i]]])
  }
  pred.df.numerics = do.call(expand_grid, predictors.list)
  
  pred.df = expand_grid(pred.df, pred.df.numerics)
  
  ##just in case of factor response
  ## handle discrete predictors
  ## handle non-across continuous predictors
  ## then handle across predictors
  pred_raw = predict(model, newdata = pred.df, type = "link", se.fit = TRUE)
  pred.df$prediction = model$family$linkinv( pred_raw$fit )
  pred.df$ci_low = as.numeric(model$family$linkinv( pred_raw$fit - 1.96 * pred_raw$se.fit ))
  pred.df$ci_high = as.numeric(model$family$linkinv( pred_raw$fit + 1.96 * pred_raw$se.fit ))
  return(pred.df)
}



plot_seasonal_gam_panels = function(model, ## fitted gam model
                               col_across = NULL, #name of variable to color terms by
                               plot_across = "doy", #name of variable to use as the x axis
                               across_increment = 1,
                               factor_subset = NULL, ## provide a list of named vectors, with name of factor predictor and value(s) of acceptable factor values to plot
                               quant_trimming = 0.01,
                               # plot_observations = FALSE, # If TRUE, add data points to plot
                               plot_coverage = FALSE){ ## If TRUE, add small histogram of data coverage along bottom of each panel
                                
  terms.ls = parse_terms(model)
  predictors.factor = terms.ls$predictors.factor
  predictors.numeric = setdiff(terms.ls$predictors.numeric, plot_across)
  response = terms.ls$response
  
  if(is.null(col_across)){
    col_across = predictors.factor[1]
    cli::cli_alert("`col_across` not provided. Defaulting to coloring by \"{col_across}\".")
    cli::cli_alert("Options for `col_across`: {predictors.factor}.")
  }
  
  dat.model = model$model
  
  dat.pred = predict_gam(model, plot_across = plot_across,
                         across_increment = across_increment,
                         quant_trimming = quant_trimming) 
  
  if(!is.null(factor_subset)){
    if(!is.list(factor_subset)){cli::cli_abort("`factor_subset` must be `NULL` or a list of named vectors")}
    if(length(setdiff(names(factor_subset), predictors.factor))){
      cli::cli_abort("Vectors in `factor_subset` must have names of predictor variables, which are {predictors.factor}")
    }
    for(i in 1:length(factor_subset)){
      
      dat.pred = dat.pred |> 
        filter(.data[[names(factor_subset)[i]]] %in% factor_subset[[i]])
      
      dat.model = dat.model |> 
        filter(.data[[names(factor_subset)[i]]] %in% factor_subset[[i]])
      
    }
  }
  
  # 
  predictors.panel = setdiff(predictors.factor, col_across)
  
  panel_combinations = dat.pred |> 
    select(all_of(predictors.panel)) |> 
    distinct()
  panel_combinations$plot = as.list(rep(NA, nrow(panel_combinations)))
  
  for(i in 1:nrow(panel_combinations)){
    dat.panel = left_join(panel_combinations[i, ],
                          dat.pred, 
                          by = predictors.panel)
    panel_details = panel_combinations[i, ] |> 
      select(-plot) |> 
      mutate(across(everything(), ~ as.character(.x))) |> 
      unlist()
    
    gp = dat.panel |> 
      ggplot(aes(x = .data[[plot_across]], y = prediction, col = .data[[col_across]], fill = .data[[col_across]])) +
      geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.1) +
      geom_path(linewidth = 0.8)+ 
      scale_x_continuous(labels = doy_2md)+ ## FRAGILE -- can't handle non-date axis
      labs(y = response,
           x = "",
           title = paste(paste(names(panel_details), ": ", panel_details), collapse = "\n")) 
    
    
    ## add histogram under-panel if desired
    if(plot_coverage){
      dat.rugs = left_join(panel_combinations[i, ],
                           dat.model,
                           by = predictors.panel) |> 
        group_by(across(all_of(c(predictors.panel, plot_across)))) |> 
        summarize(n = n(), .groups = "drop") |> 
        ungroup()
      
      hist.gp = dat.rugs |> 
        # filter(.data[[facet_by]] == cur.panel$data[[facet_by]][1]) |> 
        ggplot(aes(x = doy, y = n))+
        scale_y_continuous(n.breaks = 2)+
        geom_col(na.rm = TRUE)+
        scale_x_continuous(limits = layer_scales(gp)$x$range$range,
                           labels = doy_2md)+ ## FRAGILE -- can't handle non-date axis
        labs(x = "")
      gp = gp / hist.gp  + 
        plot_layout(heights = c(10,1), axis_title = "collect", axes = "collect_x")
    }
    
    panel_combinations$plot[[i]] = gp
    
  }
  
  return(panel_combinations)
}

relabel_panel_y = function(object, ## patchwork object made of ggplot objects or patchwork objects that need relabeling
                           new_label, ## new y axis label
                           subplot = 0){ ## if the items of `object` are patchwork objects, provide which subplot(s) 
  ## of each item are to have their y axis changed. 
  ## For example, `plot_seasonal_gam_panels` with `plot_coverage = TRUE`
  ## has subplots that are a combination of the gam predictions [[1]]
  ## and a histogram of coverage [[2]].
  ## In that case, changing gam prediction y axis label
  ## would require `subplot = 1`
  for(i in 1:length(object)){
    if(subplot == 0){
      object[[i]] = object[[i]] + ylab(new_label)
    } else {
      for(i.subplot in subplot){
        object[[i]][[i.subplot]] = object[[i]][[i.subplot]] + ylab(new_label)
      }
    }
  }
  return(object)
}

## as relabel_panel_y
relabel_panel_x = function(object, new_label, subplot = 0){
  for(i in 1:length(object)){
    if(subplot == 0){
      object[[i]] = object[[i]] + ylab(new_label)
    } else{
      for(i.subplot in subplot){
        object[[i]][[i.subplot]] = object[[i]][[i.subplot]] + xlab(new_label)
      }
    }
  }
  return(object)
}

## wrap_plots() wrapper with extra content
## See grid_wrap_panels for 
wrap_panels = function(panel_df, ## dataframe from plot_seasonal_gam_panels
                       ncol = NULL, ## number of columns to enforce
                       nrow = NULL){ ## number of rows to enforce
  
  terms_constant = names(panel_df)[apply(panel_df, 2, function(x){length(unique(x))})==1]
  
  title.terms = panel_df |> 
    select(-plot) |> 
    select(-any_of(terms_constant))
  
  annotation.text = panels.df |> 
    select(all_of(terms_constant)) |> 
    distinct()|> 
    mutate(across(everything(), ~as.character(.x))) |> 
    unlist()
  annotation.text = paste0(paste0(names(annotation.text), ": ", annotation.text), collapse = " | ")
  
  for(i in 1:nrow(panel_df)){
    title.text = title.terms[i, ] |> 
      mutate(across(everything(), ~as.character(.x))) |> 
      unlist()
    
    title.text = paste0(paste0(names(title.text), ": ", title.text), collapse = " | ")
    
    panel_df$plot[[i]][[1]] = panel_df$plot[[i]][[1]] + ggtitle(title.text)
  }
  
  res = wrap_plots(panel_df$plot, ncol = ncol, nrow = nrow)
  
  if(length(terms_constant) > 0){
    res = res + plot_annotation(subtitle = annotation.text)
  }
  
  return(res)
}


## facet_grid but for our sweet gam fits
##  will also handle 
##    (a) missing panels (e.g., we had not data for a given combination of factor predictors)
##    (b) subpanels: multiple terms in the panels_df that match a single combination of column + row variables.
grid_panels = function(
    panels_df, ## version of plot_seasonal_gam_panels with 1 row of column x var combo
    column_var, row_var, #variables to determine the columns and rows of grid. Should be factors or chars
    panel_titles = FALSE, ## if TRUE, includes all panel titles even if redundant with row/col info 
    col_title_ratio = 30, ## how much bigger should content be than title?
    row_title_ratio = 30, ## how much bigger should content be than title?
    font_size = 18, ## what size should axis label text be?
    new_y_label = NULL, ## optional: replace y axis label of primary plots with new term
    new_x_label = NULL, ## optiona: replace x axis label with new term
    title_size = 8 ## size of panel titles; default of 10. Change for readability
    
){
  
  
  row_vals = sort(unique(panels_df[[row_var]]))
  column_vals = sort(unique(panels_df[[column_var]]))
  
  list_figs = rep(list(ggplot()+theme_void()), length(row_vals) * length(column_vals))
  
  # panels_matrix = wrap_plots(list_figs,
  #                            ncol = length(column_vals), nrow = length(row_vals), byrow = TRUE)
  
  ## 
  for(i.row in 1:length(row_vals)){
    for(i.column in 1:length(column_vals)){
      ind = (i.row - 1) * length(column_vals) + i.column
      
      cur.plot = panels_df |> 
        filter(.data[[column_var]] == as.character(column_vals[i.column]),
               .data[[row_var]] == as.character(row_vals[i.row]))
      
      if(nrow(cur.plot) == 1 & !panel_titles){
        ## overwrite lone wolf titles
        ## index depth depends on if individual plots are patchworks with histogram panel or not
        if("patchwork" %in% class(cur.plot$plot[[1]])){
          cur.plot$plot[[1]][[1]] = cur.plot$plot[[1]][[1]] + ggtitle("")
        } else {
          cur.plot$plot[[1]] = cur.plot$plot[[1]] + ggtitle("")
        }
      }
      
      
      ## updating x and y labels
      if(any(!is.null(new_x_label) | !is.null(new_y_label))){
        for(i.subplot in 1:length(plot_labels)){
          if(!is.null(new_y_label)){
            if("patchwork" %in% class(cur.plot$plot[[1]])){
              cur.plot$plot[[i.subplot]][[1]] = cur.plot$plot[[i.subplot]][[1]] + ylab(new_y_label)
            } else {
              cur.plot$plot[[i.subplot]] = cur.plot$plot[[i.subplot]] + ylab(new_y_label)
            }
          }
          
          if(!is.null(new_x_label)){
            if("patchwork" %in% class(cur.plot$plot[[1]])){
              cur.plot$plot[[i.subplot]][[1]] = cur.plot$plot[[i.subplot]][[1]] + xlab(new_x_label)
              cur.plot$plot[[i.subplot]][[2]] = cur.plot$plot[[i.subplot]][[2]] + xlab(new_x_label)
            } else {
              cur.plot$plot[[i.subplot]] = cur.plot$plot[[i.subplot]] + xlab(new_x_label)
            }
          }
        }
      }
      
      if(nrow(cur.plot) > 1 & !panel_titles){
        # rework multi-subpanel titles
        ## new titles
        vars_subplot = setdiff(names(cur.plot), c(column_var, row_var))
        vars_subplot = setdiff(vars_subplot, "plot")
        plot_labels = cur.plot |> 
          select(any_of(vars_subplot)) |> 
          mutate(across(everything(), ~as.character(.x)))
        for(col_cur in names(plot_labels)){
          plot_labels[[col_cur]] = paste0(col_cur, ": ", plot_labels[[col_cur]])
        }
        plot_labels = apply(plot_labels, 1, paste0, collapse = "\n")
        
        ## replace the titles
        for(i.subplot in 1:length(plot_labels)){
          if("patchwork" %in% class(cur.plot$plot[[1]])){
            cur.plot$plot[[i.subplot]][[1]] = cur.plot$plot[[i.subplot]][[1]] + ggtitle(plot_labels[i.subplot])
          } else {
            cur.plot$plot[[i.subplot]] = cur.plot$plot[[i.subplot]] + ggtitle(plot_labels[i.subplot])
          }
        }
      } 
      
      if(nrow(cur.plot) >0){
        list_figs[[ind]] = cur.plot |> 
          pull(plot) |> 
          wrap_plots()
      }
      ## janky title handling. Consider complex story with multiple miniplots
      # if(nrow(cur.plot) == 1 & !panel_titles){
      #   ## overwrite lone wolf titles
      #   ## structure of subpanels is tricky: could be patchwork with histogram on bottom,
      #   ## or just ggplot
      #   ##
      #   if("patchwork" %in% class(list_figs[[1]][[1]])){
      #     list_figs[[ind]][[1]][[1]] = list_figs[[ind]][[1]][[1]] + ggtitle("")
      #   } else {
      #     list_figs[[ind]][[1]] = list_figs[[ind]][[1]] + ggtitle("")
      #   }
      # }
      
    }
  }
  
  panels_matrix = wrap_plots(list_figs,
                             ncol = length(column_vals), nrow = length(row_vals), byrow = TRUE)+
    plot_layout(guides = "collect") &
    theme(plot.title = element_text(size = title_size))
  
  grobs_colnames = list()
  for(i in 1:(length(column_vals))){
    grobs_colnames[[i]] = wrap_elements(panel = textGrob(
      column_vals[i], rot = 0,
      gp = gpar(col = "black", fontsize = font_size))
    )
  }
  
  grobs_rownames = list()
  for(i in 1:(length(row_vals))){
    grobs_rownames[[i]] = wrap_elements(panel = textGrob(
      row_vals[i], rot = 90,
      gp = gpar(col = "black", fontsize = font_size))
    )
  }
  
  column_name_strip = wrap_plots(grobs_colnames, nrow = 1)
  
  final_plot = (column_name_strip / panels_matrix)+
    plot_layout(heights = c(1, col_title_ratio))
  
  row_name_strip = wrap_plots(grobs_rownames, ncol = 1)
  row_name_strip = ((ggplot() + theme_void()) / row_name_strip) + 
    plot_layout(heights = c(1, col_title_ratio))
  
  final_plot = (row_name_strip | final_plot) + 
    plot_layout(widths = c(1, row_title_ratio))
  
  return(final_plot)
}


#  Example uses
# out = gam(fish_count ~ fin_mark*life_stage +
#             s(doy, by = mark_stage, k = 20) + 
#             yearfac + 
#             boat_used + angler_minutes,
#           method = "REML",
#           family = "nb",
#           data = dat.fit)
# 
# panels_df = plot_seasonal_gam_panels(out, col_across = "boat_used",
#                                 plot_coverage = TRUE)
# 
# panels_df[1:6,] |> 
#   wrap_panels()
# 
# grid_panels(panels_df, column_var = "yearfac", row_var = "fin_mark",
#             panel_titles = FALSE, title_size = 8)
# 
# 
# 
# grid_panels(panels_df, column_var = "yearfac", row_var = "fin_mark",
#             panel_titles = FALSE, new_y_label = "fish count", new_x_label = "date")
