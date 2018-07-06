#' Benchmark and save data
#'
#' Benchmark `partition()` and `kmeans_icc()` and save the results to the
#' internal `benchmarks` data.frame.
#'
#' @param ... arguments to [benchmark_all()]
#'
#' @return invisibly returns a `data.frame`
#' @export
benchmark_and_save <- function(...) {
  .df <- benchmark_all(...)
  benchmarks <- rbind(benchmarks, .df)
  usethis::use_data(benchmarks, overwrite = TRUE)
  invisible(.df)
}

#' Benchmark all partition functions
#'
#' Benchmark `partition()` and `kmeans_icc()` using multiple sample sizes and
#' methods
#'
#' @param n a vector of sample sizes to simulate for benchmarking. Default is to
#'   test 10, 100, and 1000.
#' @param times number of benchmarks to run in `microbenchmark`. Default is 100.
#'
#' @return a `data.frame`
#' @export
benchmark_all <- function(n = c(10, 100, 1000), times = 100L) {
  partition_bench <- purrr::map_df(c("ICC", "MI", "minR2", "PC1"),
                ~benchmark_partition(n = n, method = .x, times = times))
  kmeans_bench <- benchmark_kmeans(n = n, times = times)

  rbind(partition_bench, kmeans_bench)
}

#' Benchmark specific partition functions
#'
#' Benchmark `partition()` and `kmeans_icc()` using multiple sample sizes and
#' methods
#'
#' @param n a vector of sample sizes to simulate for benchmarking. Default is to
#'   test 10, 100, and 1000.
#' @param pct.var see [partition::partition]
#' @param method see [partition::partition]
#' @param dist.type see [partition::partition]
#' @param threshold.icc see [partition::kmeans_icc]
#' @param unit what unit of time? Default is "ms" for milliseconds. See
#'   [microbenchmark::microbenchmark]
#' @param times number of benchmarks to run in `microbenchmark`. Default is 100.
#'
#' @return a `data.frame`
#' @export
benchmark_partition <- function(n = c(10, 100, 1000), pct.var = .8, method = "ICC", dist.type = "p", unit = "ms", times = 100L) {
  .df <- purrr::map_df(n, function(.n) {
      .data <- partition::sim_blk_diag_mvn(2:20, c.lb = .2, c.ub = .4, n = .n)
     benchmark <- microbenchmark::microbenchmark(
          partition::partition(.data, pct.var = pct.var, method = method, dist.type = dist.type),
          unit = unit, times = times
        )
     benchmark <- as.data.frame(benchmark)
     benchmark$n <- .n
     benchmark$call <- "partition()"
     benchmark$method <- method
     benchmark$expr <- NULL
     benchmark
  })

 .df$date <- Sys.Date()

 .df
}

#' @export
#' @rdname benchmark_partition
benchmark_kmeans <- function(n = c(10, 100, 1000), threshold.icc = .4, ..., unit = "ms", times = 100L) {
  .df <- purrr::map_df(n, function(.n) {
     .data <- partition::sim_blk_diag_mvn(2:20, c.lb = .2, c.ub = .4, n = .n)
     benchmark <- microbenchmark::microbenchmark(
        partition::kmeans_icc(.data, threshold.icc = threshold.icc),
        unit = unit, times = times
      )
     benchmark <- as.data.frame(benchmark)
     benchmark$n <- .n
     benchmark$call <- "kmeans_icc()"
     benchmark$method <- "ICC"
     benchmark$expr <- NULL
     benchmark
  })

 .df$date <- Sys.Date()

 .df
}


#' Plot stored benchmarks
#'
#' `plot_benchmarks()` uses `ggplot2` and `ggridges` to plot densities of
#' benchmarked times in the stored `benchmarks` data or new data.
#'
#' @param what What should be plotted? Either "kmeans_icc()" or "partition()"
#' @param .df a `data.frame` with benchmarks. Default is `NULL`, which plots
#'   benchmarks in the internal `benchmarks` data.
#'
#' @return a `ggplot`
#' @export
plot_benchmarks <- function(what = "all", .df = NULL) {

  if (is.null(.df)) {
    if (what != "all") {
      benchmarks_data <- benchmarks[benchmarks$call %in% what, ]
    } else {
      benchmarks_data <- benchmarks
    }
  } else {
    benchmarks_data <- .df
  }

  #  colors from the colorblindr package
  clrs <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442",
            "#0072B2", "#D55E00", "#CC79A7", "#999999")
   p <- ggplot2::ggplot(benchmarks_data, ggplot2::aes(time, forcats::fct_rev(as.character(date)),
                                            fill = factor(n),
                                            color = factor(n)
                                            )) +
    ggridges::geom_density_ridges() +
    ggridges::theme_ridges() +
    ggplot2::theme(strip.background = ggplot2::element_rect(fill = "white"),
                   strip.text = ggplot2::element_text(size = 16,
                                    margin = ggplot2::margin(2, 0, 2, 0, "mm")
                                   )) +
    ggplot2::scale_color_manual(name = "n", values = clrs) +
    ggplot2::scale_fill_manual(name = "n", values = paste0(clrs, "90")) +
    ggplot2::scale_x_continuous(breaks = scales::pretty_breaks(3)) +
    ggplot2::facet_grid(method ~ call, scales = "free_x") +
    ggplot2::labs(x = "time (milliseconds)", y = "date")

   if (what == "all") p <- p + ggplot2::labs(caption = "Note: x-axis differs by function")

   p
}
