# Racket mode for GNU Emacs

[![CI](https://github.com/greghendershott/racket-mode/workflows/CI/badge.svg)](https://github.com/greghendershott/racket-mode/actions)
[![MELPA](https://melpa.org/packages/racket-mode-badge.svg)](https://melpa.org/#/racket-mode)
[![Documentation](https://img.shields.io/badge/Docs-Documentation-blue.svg)](https://www.racket-mode.com/)

This provides a major mode to edit [Racket] source files, as well as a
major mode for a Racket REPL. The edit/run experience is similar to
[DrRacket].

[Racket]: https://www.racket-lang.org/
[DrRacket]: https://docs.racket-lang.org/drracket/

Compatible with **Emacs 25.1+** and **Racket 6.9+**.

## Documentation

See the [Guide and Reference](https://www.racket-mode.com/).

## Contributing

Pull requests are welcome; please [read this](CONTRIBUTING.md).

[Acknowledgments](THANKS.md).

## Alternatives

- Emacs' built-in `scheme-mode` major mode plus the minor modes [Quack]
  and/or [Geiser].

[Quack]: https://www.neilvandyke.org/quack/
[Geiser]: https://www.nongnu.org/geiser/

## Complementary packages

- Racket Mode includes support for indent while editing, which
  preserves your line breaks. If you want to use an auto-reformatter
  --- an expressive pretty printer that chooses line breaks while
  computing an optimal layout --- the Racket package
  [fmt](https://docs.racket-lang.org/fmt/) is supported by the Emacs
  package
  [emacs-format-all-the-code](https://github.com/lassik/emacs-format-all-the-code).
