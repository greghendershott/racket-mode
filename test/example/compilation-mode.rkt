#lang racket/base

"[{\"type\":\"NumericLiteral\",\"value\":1},{\"type\":\"NumericLiteral\",\"value\":1},{\"type\":\"NumericLiteral\",\"value\":1}]" ;issue #616
(displayln "[{\"type\":\"NumericLiteral\",\"value\":1},{\"type\":\"NumericLiteral\",\"value\":1},{\"type\":\"NumericLiteral\",\"value\":1}]")
(displayln "/path/to/file.rkt:2:10")
(displayln " /path/to/file.rkt:2:10")
(displayln "#<syntax:/path/to/file.rkt:2:10>")
(displayln " #<syntax:/path/to/file.rkt:2:10>")
(displayln "...truncated:2:1")
(displayln " ...truncated:2:1")
(displayln "#<syntax:....truncated:2:1")
'done
