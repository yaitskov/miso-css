{-# LANGUAGE TypeFamilies #-}
module Miso.Css.Tags where

import Miso.Css.Prelude ( Maybe(Nothing), Proxy(Proxy) )
import Miso.Css.Style ( E(NilE), ElementStructure(Composite), EmptyKids )

type family Help model action tag where
  Help model action tag = E model action tag Composite Nothing Nothing '[] EmptyKids '[] '[] '[] '[] '[]

div_ :: Help model action "div"
div_ = NilE (Proxy @"div")

span_ :: Help model action "span"
span_ = NilE (Proxy @"span")

p_ :: Help model action "p"
p_ = NilE (Proxy @"p")

b_ :: Help model action "b"
b_ = NilE (Proxy @"b")

i_ :: Help model action "i"
i_ = NilE (Proxy @"i")

th_ :: Help model action "th"
th_ = NilE (Proxy @"th")

td_ :: Help model action "td"
td_ = NilE (Proxy @"td")

table_ :: Help model action "table"
table_ = NilE (Proxy @"table")

thead_ :: Help model action "thead"
thead_ = NilE (Proxy @"thead")

tbody_ :: Help model action "tbody"
tbody_ = NilE (Proxy @"tbody")

tr_ :: Help model action "tr"
tr_ = NilE (Proxy @"tr")

h1_ :: Help model action "h1"
h1_ = NilE (Proxy @"h1")

img_ :: Help model action "img"
img_ = NilE (Proxy @"img")

br_ :: Help model action "br"
br_ = NilE (Proxy @"br")

hr_ :: Help model action "hr"
hr_ = NilE (Proxy @"hr")

ul_ :: Help model action "ul"
ul_ = NilE (Proxy @"ul")

ol_ :: Help model action "ol"
ol_ = NilE (Proxy @"ol")

li_ :: Help model action "li"
li_ = NilE (Proxy @"li")

a_ :: Help model action "a"
a_ = NilE (Proxy @"a")

article_ :: Help model action "article"
article_ = NilE (Proxy @"article")

section_ :: Help model action "section"
section_ = NilE (Proxy @"section")

h2_ :: Help model action "h2"
h2_ = NilE (Proxy @"h2")

h3_ :: Help model action "h3"
h3_ = NilE (Proxy @"h3")

h4_ :: Help model action "h4"
h4_ = NilE (Proxy @"h4")

h5_ :: Help model action "h5"
h5_ = NilE (Proxy @"h5")

h6_ :: Help model action "h6"
h6_ = NilE (Proxy @"h6")

form_ :: Help model action "form"
form_ = NilE (Proxy @"form")

input_ :: Help model action "input"
input_ = NilE (Proxy @"input")

button_ :: Help model action "button"
button_ = NilE (Proxy @"button")

label_ :: Help model action "label"
label_ = NilE (Proxy @"label")
