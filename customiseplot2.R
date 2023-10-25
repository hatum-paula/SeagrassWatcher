#' @title dbnplot2
#' @description Plot weighted mean or state beliefs or both for specified DBN nodes. Handles both 
#' the case where posterior is missing $node and $scenario (as part of batch plotting - see 
#' visbatchdbn) or the normal posterior with $node and $scenario
#' @param sellabel - either 1) name of node to plot, or 2) ddf key (node+scn)
#' @param posterior - data frame formatted as per dbn$posterior (see DBNSMILER-package 
#' documentation) WITH/WITHOUT $node and $scenario; -$node,$state,$t,$p,-$scenario,$pctile,$xbar 
#' @param plottype - 'statebeliefs' (state probability plots) OR 'wmean' (weighted mean) OR 'both'
#' @param dateformat - char specifying x-axis date format, e.g. \%m-\%Y for months-year
#' \%d for day, \%H for hour, \%M for minute, \%S for second. Currently only handles dates.
#' @param customiseplots - if 1, looks for function customiseplotswrapper to customise plot colours
#' @return a ggplot object
#' @export
#' @examples none
#' @name dbnplot2

dbnplot2 = function(sellabel, posterior, plottype, dateformat, customiseplots, datarange = NULL) {
  out2 = NULL
  
  # Check if posterior contains node and scenario
  if('node' %in% colnames(posterior) && 'scenario' %in% colnames(posterior)) {
    # Yes - so sellabel is actually the node name
    z = posterior[posterior$node %in% sellabel,]
  } else {
    # No - so sellabel
    z = cbind(posterior, node = sellabel)
  }
  
  # Add plottype column based on if p is NA (wmean) or not (statebelief)
  colplottype = vector('character', length = nrow(z))
  colplottype[is.na(z$p)] = 'Weighted_Mean'
  colplottype[!is.na(z$p)] = 'State_Probability'
  z = cbind(z, plottype = colplottype)
  
  if (plottype == 'both') {
    out2 = ggplot() +
      geom_line(data = subset(z, plottype == 'State_Probability'), aes(as.Date(t), p, group = state, colour = state)) +
      geom_line(data = subset(z, plottype == 'Weighted_Mean'),
                aes(as.Date(t), xbar, group = pctile, alpha = factor(pctile))) +
      facet_grid(plottype ~ node, scales = "free_y") +
      theme_bw() + 
      xlab('Date') + 
      ylab('') +
      scale_x_date(date_labels = "%Y", breaks = "1 year", limits = datarange) +
      guides(
        colour = guide_legend(title = "State", order = 1),
        alpha = guide_legend(title = "Percentile", order = 2)) +
      #guides(alpha = guide_legend(title = "percentile")) +
      theme(text = element_text(size = 16),
            axis.text = element_text(size = 16),
            axis.title = element_text(size = 18),
            title = element_text(size = 20),
            legend.text = element_text(size = 14),
            legend.title = element_text(size = 16),
            strip.text = element_text(size = 18)) 
  } else if (plottype == 'state_Probability') {
    out2 = ggplot() +
      geom_line(data = subset(z, plottype == 'State_Probability'), aes(as.Date(t), p, group = state, colour = state)) +
      facet_grid(plottype ~ node, scales = "free_y") +
      theme_bw() + 
      xlab('Date') + 
      ylab('') +
      scale_x_date(limits = datarange)
  } else if (plottype == 'Weighted_Mean') {
    out2 = ggplot() +
      geom_line(data = subset(z, plottype == 'Weighted_Mean'),
                aes(as.Date(t), xbar, group = pctile, colour = factor(pctile))) + 
      facet_grid(plottype ~ node, scales = "free_y") +
      theme_bw() + 
      xlab('Date') + 
      ylab('') +
      scale_x_date(limits = datarange)
  }
  
  if (customiseplots) {
    out2 = tryCatch({
      if (!exists('dp')) {
        dp = NULL
      }
      customiseplotswrapper(out2, sellabel, dp)
    }, error = function(cond) {
      print(cond)
      out2
    })
  }
  
  return(out2)
}