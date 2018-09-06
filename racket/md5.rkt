#lang racket/base

(require file/md5)

(provide file->md5)

(define (file->md5 file)
  (call-with-input-file* file (compose bytes->string/utf-8 md5)))
