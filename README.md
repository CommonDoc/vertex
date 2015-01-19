# VerTeX

[![Build Status](https://travis-ci.org/CommonDoc/vertex.svg?branch=master)](https://travis-ci.org/CommonDoc/vertex)

A markup language with TeX syntax.

# Syntax

## Basic

VerTeX syntax, as the name implies, is basically TeX syntax. Blocks look like
this:

* `\some-tag`
* `\b{bold text}`
* `\link[uri='https://www.google.com/']{Google}`

The syntax, as a sort of regular expression, is like this:

```
\<tag name>([(<attribute>=<value>)*])?({<body>})?
```

## Markup

### `p`

A paragraph.

```tex
\p{A paragraph.}

\{Another paragraph.}
```

### `b`

Bold text.

```tex
This is \b{bold text}.
```

### `i`

Italicized text.

```tex
This text is in \i{italics}.
```

### `u`

Underlined text.

```tex
This text is \u{underlined}.
```

### `strike`

Struck-through text.

```tex
This text is \strike{struck through}.
```

### `sup` and `sub`

Superscript and subscript.

```tex
The value of the vacuum permittivity, ε\sub{0}, is 8.8x10\sup{-12}
```

## Code

### `c`

Inline code.

```tex
The function \c{find} takes as arguments...
```

### `code`

A block of code.

```tex
\code[lang='lisp']{
  (let ((x 1))
    (incf x))
}
```

## Quotes

### `q`

An inline quote.

### `quote`

A block quote.

# License

Copyright (c) 2014 Fernando Borretti

Licensed under the MIT License.
