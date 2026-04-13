module Miso.Css.Tags where

import Data.Proxy ( Proxy(Proxy) )
import Miso.Css.Style ( E(NilE), ElementStructure(Composite) )
import Miso.Css.Prelude ( Maybe(Nothing) )


div_ :: E model action "div" Composite Nothing Nothing '[] '[] '[] '[]
div_ = NilE (Proxy @"div")

span_ :: E model action "span" Composite Nothing Nothing '[] '[] '[] '[]
span_ = NilE (Proxy @"span")

p_ :: E model action "p" Composite Nothing Nothing '[]'[] '[] '[]
p_ = NilE (Proxy @"p")

b_ :: E model action "b" Composite Nothing Nothing '[]'[] '[] '[]
b_ = NilE (Proxy @"b")

i_ :: E model action "i" Composite Nothing Nothing '[]'[] '[] '[]
i_ = NilE (Proxy @"i")

th_ :: E model action "th" Composite Nothing Nothing '[]'[] '[] '[]
th_ = NilE (Proxy @"th")

td_ :: E model action "td" Composite Nothing Nothing '[]'[] '[] '[]
td_ = NilE (Proxy @"td")

table_ :: E model action "table" Composite Nothing Nothing '[]'[] '[] '[]
table_ = NilE (Proxy @"table")

tr_ :: E model action "tr" Composite Nothing Nothing '[]'[] '[] '[]
tr_ = NilE (Proxy @"tr")

h1_ :: E model action "h1" Composite Nothing Nothing '[]'[] '[] '[]
h1_ = NilE (Proxy @"h1")

img_ :: E model action "img" Composite Nothing Nothing '[]'[] '[] '[]
img_ = NilE (Proxy @"img")

br_ :: E model action "br" Composite Nothing Nothing '[]'[] '[] '[]
br_ = NilE (Proxy @"br")

hr_ :: E model action "hr" Composite Nothing Nothing '[]'[] '[] '[]
hr_ = NilE (Proxy @"hr")

ul_ :: E model action "ul" Composite Nothing Nothing '[]'[] '[] '[]
ul_ = NilE (Proxy @"ul")

ol_ :: E model action "ol" Composite Nothing Nothing '[]'[] '[] '[]
ol_ = NilE (Proxy @"ol")

li_ :: E model action "li" Composite Nothing Nothing '[]'[] '[] '[]
li_ = NilE (Proxy @"li")
