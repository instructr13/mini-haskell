#import "@preview/equate:0.3.3": equate
#import "@preview/beautiframe:0.4.5": *

#show: body => {
  set text(font: "New Computer Modern", size: 10.5pt)
  show heading: set text(font: "Noto Sans CJK JP")
  show raw: set text(font: "Maple Mono", ligatures: false, features: (
    "calt": 0,
    "clig": 0,
    "liga": 0,
  ))

  show figure.caption: set text(weight: "bold")

  show link: set text(fill: blue)
  show link: underline

  show figure.where(kind: table): content => {
    set figure.caption(position: top)
    set block(breakable: true)

    content
  }

  show figure.where(kind: raw): content => {
    set figure.caption(position: top)
    set block(breakable: true)

    content
  }

  show figure: content => {
    v(.8em)
    content
    v(.8em)
  }

  set par(
    first-line-indent: (amount: 1em, all: true),
    justification-limits: (tracking: (min: -0.01em, max: 0.02em)),
  )

  show: equate.with(sub-numbering: true, number-mode: "label")

  set math.equation(numbering: "(1.1)")
  show math.equation.where(block: false): set math.frac(style: "horizontal")
  show math.equation: it => {
    set block(breakable: true)
    set text(font: "New Computer Modern Math")
    show math.text: set text(font: "New Computer Modern")

    it
  }

  body
}

#show title: body => {
  set text(font: "Noto Sans CJK JP", weight: "bold", fill: blue)
  set block(breakable: true)
  set align(center)

  body
}

#set document(title: "Lesson 5 Homework", description: "2026 Function Programming Internship", author: "Mido")
#set page(paper: "a4", numbering: "1")

#beautiframe-setup(
  style: "modern",
)

#let question = (label: none, level: 1, title, body) => {
  let heading-tag(num) = box(
    inset: (x: 0.4em),
    outset: (y: 0.5em),
    radius: 2pt,
    stroke: maroon + 0.7pt,
    text(fill: maroon, num),
  )

  set heading(numbering: "1", supplement: "Question")

  show heading: it => {
    set text(size: 11pt, weight: "regular")
    set par(first-line-indent: 0pt, hanging-indent: 0pt)

    let num = counter(heading).display(it.numbering)

    block(above: 1.2em, below: 0.7em, grid(
      columns: (auto, 1fr),
      column-gutter: 0.5em,
      heading-tag(num), it.body,
    ))
  }

  [
    #heading(level: level, title)#label

    #body
  ]
}

#let concat = math.class("binary", "++")

#let fn = (name, ..args) => $mono(text(fill: #std.blue, name)) #args.pos().join(" ")$

#let xs = $x s$
#let ys = $y s$
#let zs = $z s$

#title()

#theorem(number: none)[
  $
    A_1 & : [] concat y s              &                        = y s \
    A_2 & : (x : xs) concat y s        &         = x : (xs concat ys) \
    R_1 & : fn("rev", [])              &                         = [] \
    R_2 & : fn("rev", (x : xs))        &   = fn("rev", xs) concat [x] \
    S_1 & : fn("revapp", [], ys)       &                         = ys \
    S_2 & : fn("revapp", (x : xs), ys) & = fn("revapp", xs, (x : ys))
  $
]

#question([Compute $C[t]$ or $t sigma$.])[

]
