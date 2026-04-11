module Miso.Css.Tags where

import Data.Proxy ( Proxy(Proxy) )
import Miso.Css.Style
import Miso.Css.Prelude


div_ :: E "div" Composite Nothing '[] '[] '[]
div_ = NilE (Proxy @"div")

p_ :: E "p" Composite Nothing '[]'[] '[]
p_ = NilE (Proxy @"p")

b_ :: E "b" Composite Nothing '[]'[] '[]
b_ = NilE (Proxy @"b")

i_ :: E "i" Composite Nothing '[]'[] '[]
i_ = NilE (Proxy @"i")

th_ :: E "th" Composite Nothing '[]'[] '[]
th_ = NilE (Proxy @"th")

td_ :: E "td" Composite Nothing '[]'[] '[]
td_ = NilE (Proxy @"td")

table_ :: E "table" Composite Nothing '[]'[] '[]
table_ = NilE (Proxy @"table")

tr_ :: E "tr" Composite Nothing '[]'[] '[]
tr_ = NilE (Proxy @"tr")

h1_ :: E "h1" Composite Nothing '[]'[] '[]
h1_ = NilE (Proxy @"h1")

img_ :: E "img" Composite Nothing '[]'[] '[]
img_ = NilE (Proxy @"img")

br_ :: E "br" Composite Nothing '[]'[] '[]
br_ = NilE (Proxy @"br")

hr_ :: E "hr" Composite Nothing '[]'[] '[]
hr_ = NilE (Proxy @"hr")

ul_ :: E "ul" Composite Nothing '[]'[] '[]
ul_ = NilE (Proxy @"ul")

ol_ :: E "ol" Composite Nothing '[]'[] '[]
ol_ = NilE (Proxy @"ol")

li_ :: E "li" Composite Nothing '[]'[] '[]
li_ = NilE (Proxy @"li")
