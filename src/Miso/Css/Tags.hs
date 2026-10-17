{-# LANGUAGE TypeFamilies #-}
module Miso.Css.Tags where

import Miso.Css.Prelude ( Maybe(Nothing), Proxy(Proxy) )
import Miso.Css.Style ( E(NilE), ElementStructure(Composite), EmptyKids )

type family Help ctx props model action tag where
  Help ctx props model action tag =
    E ctx props model action tag Composite Nothing Nothing '[] EmptyKids '[] '[] '[] '[] '[] '[]

div_ :: Help ctx props model action "div"
div_ = NilE (Proxy @"div")

span_ :: Help ctx props model action "span"
span_ = NilE (Proxy @"span")

p_ :: Help ctx props model action "p"
p_ = NilE (Proxy @"p")

b_ :: Help ctx props model action "b"
b_ = NilE (Proxy @"b")

i_ :: Help ctx props model action "i"
i_ = NilE (Proxy @"i")

th_ :: Help ctx props model action "th"
th_ = NilE (Proxy @"th")

td_ :: Help ctx props model action "td"
td_ = NilE (Proxy @"td")

table_ :: Help ctx props model action "table"
table_ = NilE (Proxy @"table")

thead_ :: Help ctx props model action "thead"
thead_ = NilE (Proxy @"thead")

tbody_ :: Help ctx props model action "tbody"
tbody_ = NilE (Proxy @"tbody")

tr_ :: Help ctx props model action "tr"
tr_ = NilE (Proxy @"tr")

h1_ :: Help ctx props model action "h1"
h1_ = NilE (Proxy @"h1")

img_ :: Help ctx props model action "img"
img_ = NilE (Proxy @"img")

br_ :: Help ctx props model action "br"
br_ = NilE (Proxy @"br")

hr_ :: Help ctx props model action "hr"
hr_ = NilE (Proxy @"hr")

ul_ :: Help ctx props model action "ul"
ul_ = NilE (Proxy @"ul")

ol_ :: Help ctx props model action "ol"
ol_ = NilE (Proxy @"ol")

li_ :: Help ctx props model action "li"
li_ = NilE (Proxy @"li")

a_ :: Help ctx props model action "a"
a_ = NilE (Proxy @"a")

article_ :: Help ctx props model action "article"
article_ = NilE (Proxy @"article")

section_ :: Help ctx props model action "section"
section_ = NilE (Proxy @"section")

h2_ :: Help ctx props model action "h2"
h2_ = NilE (Proxy @"h2")

h3_ :: Help ctx props model action "h3"
h3_ = NilE (Proxy @"h3")

h4_ :: Help ctx props model action "h4"
h4_ = NilE (Proxy @"h4")

h5_ :: Help ctx props model action "h5"
h5_ = NilE (Proxy @"h5")

h6_ :: Help ctx props model action "h6"
h6_ = NilE (Proxy @"h6")

form_ :: Help ctx props model action "form"
form_ = NilE (Proxy @"form")

input_ :: Help ctx props model action "input"
input_ = NilE (Proxy @"input")

button_ :: Help ctx props model action "button"
button_ = NilE (Proxy @"button")

label_ :: Help ctx props model action "label"
label_ = NilE (Proxy @"label")
