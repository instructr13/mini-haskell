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

#set document(title: "Lesson 4 Homework", description: "2026 Function Programming Internship", author: "Mido")
#set page(paper: "a4", numbering: "1")

#beautiframe-setup(
  style: "modern",
)

#let question = (label: none, title, body) => {
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
    #heading(level: 1, title)#label

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

#question(label: <q:1>, [Show that $xs concat [] = xs$ holds for all lists $xs$.])[
  #theorem(number: none)[
    $xs concat [] = xs$ for all lists $xs$
  ]

  #proof[
    We show the claim by *structual induction* on $xs$.

    - If $xs = []$ then
      $
        xs concat [] & = [] concat [] \
                     & = []           && wide "by " A_1
      $

    - If $xs = x : xs prime$ for some $x$ and $xs prime$ then
      $
        xs concat [] & = (x : xs prime) concat [] \
                     & = x : (xs prime concat []) && wide "by " A_2 \
                     & = x : xs prime             && wide "I.H."
      $
  ]
]

#question(label: <q:2>, [
  Show that
  $
    xs concat (ys concat zs) = (xs concat ys) concat zs
  $
  holds for all lists $xs, ys, zs$.
])[
  #theorem(number: none)[
    $
      xs concat (ys concat zs) = (xs concat ys) concat zs
    $
    for all lists $xs, ys, zs$
  ]

  #proof[
    We show the claim by *structual induction* on $xs$.

    - If $xs = []$ then
      $
        [] concat (ys concat zs) & = ys concat zs             && wide "by " A_1 \
                                 & = ([] concat ys) concat zs && wide "by " A_1
      $
    - If $xs = x : xs prime$ then
      $
        (x : xs prime) concat (ys concat zs) & = x : (xs prime concat (ys concat zs)) && wide "by " A_2 \
                                             & = x : ((xs prime concat ys) concat zs) && wide "I.H." \
                                             & = (x : (xs prime concat ys) concat zs) && wide "by " A_2 \
                                             & = ((x : xs prime) concat ys) concat zs && wide "by " A_2
      $
  ]
]

#question([Show $fn("rev", xs) = fn("revapp", xs, [])$ for all lists $xs$.])[
  #theorem(number: none)[
    $fn("rev", xs) = fn("revapp", xs, [])$ for all lists $xs$
  ]

  #lemma(number: none)[
    $fn("revapp", xs, ys) = fn("revapp", xs, []) concat ys$ for all lists $xs, ys$

    #proof[
      We show the claim by *structual induction* on $xs$.

      - If $xs = []$ then
        $
          fn("revapp", [], ys) & = ys                             && wide "by " S_1 \
                               & = [] concat ys                   && wide "by " A_1 \
                               & = fn("revapp", [], []) concat ys && wide "by " S_1
        $

      - If $xs = x : xs prime$ then
        $
          fn("revapp", (x : xs prime), ys) & = fn("revapp", xs prime, (x : ys))                       && wide "by " S_2 \
                                           & = fn("revapp", xs prime, []) concat (x : ys)             && wide "I.H." \
                                           & = fn("revapp", xs prime, []) concat (x : ([] concat ys)) && wide "by " A_1 \
                                           & = fn("revapp", xs prime, []) concat ((x : []) concat ys) && wide "by " A_2 \
                                           & = fn("revapp", xs prime, []) concat ([x] concat ys) \
                                           & = (fn("revapp", xs prime, []) concat [x]) concat ys      && wide "by " #[@q:2] \
                                           & = fn("revapp", xs prime, [x]) concat ys                  && wide "I.H." \
                                           & = fn("revapp", (x : xs prime), []) concat ys             && wide "by " S_2
        $
    ]
  ]

  #pagebreak()

  #proof[
    We show the claim by *structual induction* on $xs$.

    - If $xs = []$ then
      $
        fn("rev", []) & = []                   && wide "by " R_1 \
                      & = fn("revapp", [], []) && wide "by " S_1
      $

    - If $xs = x : xs prime$ then
      $
        fn("rev", (x : xs prime)) & = fn("rev", xs prime) concat [x]        && wide "by " R_2 \
                                  & = fn("revapp", xs prime, []) concat [x] && wide "I.H." \
                                  & = fn("revapp", xs prime, [x])           && wide "by Lemma" \
                                  & = fn("revapp", (x : xs prime), [])      && wide "by " S_2
      $
  ]
]
