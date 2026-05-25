module Miso.Css.Tags where

import Data.Proxy ( Proxy(Proxy) )
import Miso.Css.Style ( E(NilE), ElementStructure(Composite), EmptyKids )
import Miso.Css.Prelude ( Maybe(Nothing) )


div_ :: E model action "div" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
div_ = NilE (Proxy @"div")

span_ :: E model action "span" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
span_ = NilE (Proxy @"span")

p_ :: E model action "p" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
p_ = NilE (Proxy @"p")

b_ :: E model action "b" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
b_ = NilE (Proxy @"b")

i_ :: E model action "i" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
i_ = NilE (Proxy @"i")

th_ :: E model action "th" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
th_ = NilE (Proxy @"th")

td_ :: E model action "td" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
td_ = NilE (Proxy @"td")

table_ :: E model action "table" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
table_ = NilE (Proxy @"table")

tr_ :: E model action "tr" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
tr_ = NilE (Proxy @"tr")

h1_ :: E model action "h1" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
h1_ = NilE (Proxy @"h1")

img_ :: E model action "img" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
img_ = NilE (Proxy @"img")

br_ :: E model action "br" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
br_ = NilE (Proxy @"br")

hr_ :: E model action "hr" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
hr_ = NilE (Proxy @"hr")

ul_ :: E model action "ul" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
ul_ = NilE (Proxy @"ul")

ol_ :: E model action "ol" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
ol_ = NilE (Proxy @"ol")

li_ :: E model action "li" Composite Nothing Nothing '[] EmptyKids '[] '[] '[]
li_ = NilE (Proxy @"li")
