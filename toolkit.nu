# Prints input string with highlighted span
#
# Useful for debugging parser output
export def span [span_start: int, span_end: int]: string -> nothing {
    let s = $in | encode utf8

    let pre = $s | bytes at ..$span_start | decode
    let highlighted = $s | bytes at $span_start..$span_end | decode
    let post = $s | bytes at $span_end.. | decode

    print ($pre + $'(ansi ru)($highlighted)(ansi reset)' + $post)
}
