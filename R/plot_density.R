#' Plot results of distance sampling
#'
#' @param obj Model output from \code{fit_distance_models}
#' @param type Type of output to plot, either \code{D} for density estimates or \code{N} for abundance estimates. Default is \code{D}.
#' @param year Defaults to \code{FALSE}. Change to \code{TRUE} if strata include estimates by year and you wish to plot trends.
#'
#' @return Returns a ggplot2 object for further customization.
#'
#' @author Kristen Dybala, \email{kdybala@@pointblue.org}
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' fdat = format_data(data=dat, strata='group')
#' results = fit_distance_models(fdat, 'speciesname', bins=c(0,10,20,30,40,50,75,100))
#' plot_density(results, 'D', year=T) + theme_classic() + scale_color_manual(values=c('#682C7','#64AD34'))
#' }
#'
plot_density = function(obj, type='D', year=F) {
  if (type %in% names(obj)) {
    dat = obj[[type]]
  } else {
    stop('Requires output of fit_distance_models and type D or N')
  }
  if (year==F) {
    ggplot2::ggplot(dat, aes(x=Label, y=Estimate, ymin=lcl, ymax=ucl)) + ggplot2::geom_errorbar(width=0.1) +
      ggplot2::geom_point()
  } else if (year==T) {
    dat$year = as.numeric(gsub('.*_', '', dat$Label))
    dat$group = as.factor(gsub('_.*', '', dat$Label))
    dat = dat[-which(is.na(dat$year)),]
    ggplot2::ggplot(dat, aes(x=year, y=Estimate, ymin=lcl, ymax=ucl, color=group, shape=group)) +
      ggplot2::geom_line() + ggplot2::geom_errorbar(width=0.1) + ggplot2::geom_point() + xlab(NULL)
  }
}
