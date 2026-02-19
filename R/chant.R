#' Get bundled Lang Nghiem file path
#'
#' Returns the absolute path to the packaged text file.
#'
#' @return A character(1) path.
#' @export
cln_file <- function() {
  system.file(
    "extdata", "lang_nghiem_chi.md",
    package = "chulangnghiem", mustWork = TRUE
  )
}

# --- internal helpers ---
.cln_trim <- function(x) gsub("^\\s+|\\s+$", "", x)

.cln_parse_line <- function(raw) {
  # input: "01. 🙏 ... # 如來"
  main <- raw
  han  <- ""
  if (grepl("#", raw, fixed = TRUE)) {
    parts <- strsplit(raw, "#", fixed = TRUE)[[1]]
    main <- parts[1]
    han  <- paste(parts[-1], collapse = "#")
  }
  main <- sub("^\\s*[0-9]+\\s*[.)]\\s*", "", main)
  list(main = .cln_trim(main), han = .cln_trim(han))
}

.cln_total_lines <- function(file) {
  length(readLines(file, warn = FALSE, encoding = "UTF-8"))
}

.cln_read_range <- function(file, start, end) {
  x <- readLines(file, warn = FALSE, encoding = "UTF-8")
  n <- length(x)
  start <- max(1L, min(as.integer(start), n))
  end   <- max(1L, min(as.integer(end), n))
  if (end < start) { tmp <- start; start <- end; end <- tmp }
  x[start:end]
}

.cln_block_end <- function(start) {
  # end = nearest multiple of 12 >= start
  (((start - 1L) %/% 12L) + 1L) * 12L
}

# ---------- ANSI colors (terminal) ----------
.cln_ansi <- function() {
  list(
    reset  = "\033[0m",
    bold   = "\033[1m",
    gray   = "\033[90m",
    white  = "\033[37m",
    red    = "\033[31m",
    green  = "\033[32m",
    yellow = "\033[33m"
  )
}

.cln_supports_ansi <- function() {
  # RStudio Console + terminal thường OK; nếu fail thì tự tắt.
  # Bạn có thể ép bật bằng options(chulangnghiem.ansi=TRUE)
  opt <- getOption("chulangnghiem.ansi")
  if (isTRUE(opt)) return(TRUE)
  if (isFALSE(opt)) return(FALSE)

  # Windows: đa số RStudio mới / Windows Terminal OK.
  # Nếu ai dùng console cổ không hỗ trợ thì họ set options(...=FALSE).
  TRUE
}

# Phiên âm: 12 câu / vòng -> 3 đỏ, 3 xanh, 3 trắng, 3 vàng
.cln_color_main <- function(line_no) {
  r <- (as.integer(line_no) - 1L) %% 12L
  if (r < 3L) "red" else if (r < 6L) "green" else if (r < 9L) "white" else "yellow"
}

# Hán: 12 câu / vòng -> 3 trắng, 3 vàng, 3 đỏ, 3 xanh
.cln_color_han <- function(line_no) {
  r <- (as.integer(line_no) - 1L) %% 12L
  if (r < 3L) "white" else if (r < 6L) "yellow" else if (r < 9L) "red" else "green"
}

.cln_fmt_line <- function(line_no, main, han = "", use_ansi = TRUE) {
  idx <- sprintf("%03d.", as.integer(line_no))

  if (!use_ansi) {
    if (nzchar(han)) return(paste0(idx, " ", main, "  # ", han))
    return(paste0(idx, " ", main))
  }

  C <- .cln_ansi()
  cm <- C[[.cln_color_main(line_no)]]
  ch <- C[[.cln_color_han(line_no)]]

  out <- paste0(
    C$gray, idx, C$reset, " ",
    C$bold, cm, main, C$reset
  )

  if (nzchar(han)) {
    out <- paste0(
      out, "  ",
      C$gray, "#", C$reset, " ",
      C$bold, ch, han, C$reset
    )
  }
  out
}

#' Chant by line number / block
#'
#' Like your bash `ln`:
#' - `cln(13)` chants from 13 to end of its 12-line block (24)
#' - `cln(13, 27)` chants 13..27
#' - `cln("0*")` chants block 0 => 1..12; `cln("1*")` => 13..24
#' - `cln("0*:2*")` chants blocks 0..2 => 1..36
#'
#' Modes:
#' - Interactive (default): press **Enter** to go next line; type `q` then Enter to quit.
#' - 🚀 Auto mode: set `auto = TRUE` to auto-advance every `delay` seconds (no Enter needed).
#'
#' Colors (like your bash):
#' - Main: 3 red, 3 green, 3 white, 3 yellow (repeat every 12 lines)
#' - Han:  3 white, 3 yellow, 3 red, 3 green (repeat every 12 lines)
#'
#' @param start Integer line number OR character like "0*" or "0*:2*".
#' @param end Optional integer line number.
#' @param file Optional path. Default is bundled file inside the package.
#' @param auto Logical. If TRUE, auto-advance without Enter.
#' @param delay Number of seconds between lines in auto mode (default 1.0).
#' @param color Logical. If TRUE, use ANSI colors in console.
#' @export
cln <- function(start = 1L, end = NULL, file = cln_file(),
                auto = FALSE, delay = 1.0, color = TRUE) {
  if (!file.exists(file)) stop("File not found: ", file)
  if (!is.logical(auto) || length(auto) != 1) auto <- FALSE
  delay <- suppressWarnings(as.numeric(delay))
  if (!is.finite(delay) || delay < 0) delay <- 1.0

  use_ansi <- isTRUE(color) && .cln_supports_ansi()

  # resolve ranges
  ranges <- list()

  if (is.character(start) && length(start) == 1) {
    s <- .cln_trim(start)

    # "K*:M*"
    if (grepl("^[0-9]+\\*:[0-9]+\\*$", s)) {
      s2 <- sub("\\*$", "", s)
      b1 <- as.integer(sub("\\*:.*$", "", s2))
      b2 <- as.integer(sub("^.*:\\s*", "", s2))
      if (is.na(b1) || is.na(b2)) stop("start không hợp lệ. Ví dụ: '0*:2*'")

      if (b2 < b1) { tmp <- b1; b1 <- b2; b2 <- tmp }
      rs <- b1 * 12L + 1L
      re <- (b2 + 1L) * 12L
      ranges[[1]] <- c(rs, re)

    } else if (grepl("^[0-9]+\\*$", s)) {
      b <- as.integer(sub("\\*$", "", s))
      if (is.na(b)) stop("start không hợp lệ. Ví dụ: '0*' hoặc '1*'")
      rs <- b * 12L + 1L
      re <- rs + 11L
      ranges[[1]] <- c(rs, re)

    } else {
      stop("start không hợp lệ. Dùng số (13), '0*', hoặc '0*:2*'.")
    }

  } else {
    start <- as.integer(start)
    if (is.na(start) || start < 1L) start <- 1L

    if (is.null(end)) end <- .cln_block_end(start)
    end <- as.integer(end)
    if (is.na(end) || end < 1L) end <- .cln_block_end(start)

    ranges[[1]] <- c(start, end)
  }

  # clamp by file length
  total <- .cln_total_lines(file)
  ranges <- lapply(ranges, function(r) c(max(1L, r[1]), min(total, r[2])))
  ranges <- Filter(function(r) r[2] >= r[1], ranges)
  if (!length(ranges)) stop("Không có đoạn hợp lệ để tụng.")

  # header
  cat("\n📿 CHÚ LĂNG NGHIÊM (chulangnghiem)\n")
  cat("File: ", file, "\n", sep = "")
  for (r in ranges) cat("Range: ", r[1], " → ", r[2], "\n", sep = "")
  if (isTRUE(auto)) {
    cat("🚀 AUTO: tự chạy mỗi ", delay, " giây | gõ Ctrl+C để dừng\n", sep = "")
  } else {
    cat("Enter: câu kế | gõ q rồi Enter: thoát\n")
  }
  cat("----------------------------------------\n\n")

  quit <- FALSE
  for (r in ranges) {
    lines <- .cln_read_range(file, r[1], r[2])
    idx <- seq.int(r[1], r[2])

    for (i in seq_along(lines)) {
      p <- .cln_parse_line(lines[i])
      cat(.cln_fmt_line(idx[i], p$main, p$han, use_ansi = use_ansi), "\n", sep = "")

      if (isTRUE(auto)) {
        Sys.sleep(delay)
      } else {
        k <- readline("")
        k <- .cln_trim(k)
        if (nzchar(k) && tolower(k) == "q") {
          quit <- TRUE
          break
        }
      }
    }
    if (quit) break
  }

  cat("\n🙏 Hết đoạn. Nam Mô A Di Đà Phật.\n")
  invisible(TRUE)
}

#' Find by keyword and chant from the picked line to end of its block
#'
#' Like bash `lnk "tát đát"`:
#' - prints matched lines
#' - you choose a line number (Enter = first match)
#' - chants from that line to end of its 12-line block
#'
#' @param keyword Character keyword to search (case-insensitive).
#' @param file Optional path. Default is bundled file inside the package.
#' @param auto Logical. If TRUE, auto-advance without Enter in chanting.
#' @param delay Number of seconds between lines in auto mode (default 1.0).
#' @param color Logical. If TRUE, use ANSI colors in console.
#' @export
clnk <- function(keyword, file = cln_file(), auto = FALSE, delay = 1.0, color = TRUE) {
  if (!file.exists(file)) stop("File not found: ", file)

  kw <- .cln_trim(paste(keyword, collapse = " "))
  if (!nzchar(kw)) stop('Nhập keyword. Ví dụ: clnk("tát đát")')

  x <- readLines(file, warn = FALSE, encoding = "UTF-8")
  hit <- grep(kw, x, ignore.case = TRUE)

  if (!length(hit)) {
    message("❌ Không tìm thấy: ", kw)
    return(invisible(NULL))
  }

  cat("\n🔎 Match keyword: \"", kw, "\"\n", sep = "")
  cat("----------------------------------------\n")
  show <- head(hit, 200)
  for (i in show) {
    p <- .cln_parse_line(x[i])
    cat(sprintf("%03d  %s\n", i, p$main))
  }
  cat("----------------------------------------\n")

  pick <- readline("Nhập số câu muốn tụng (Enter = câu đầu tiên, q = thoát): ")
  pick <- .cln_trim(pick)
  if (nzchar(pick) && tolower(pick) == "q") return(invisible(NULL))

  start <- if (!nzchar(pick)) show[1] else as.integer(pick)
  if (is.na(start) || start < 1L) stop("Phải nhập số dòng hợp lệ.")

  cln(start, file = file, auto = auto, delay = delay, color = color)
}
