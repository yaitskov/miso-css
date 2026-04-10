module Miso.Css.Tags where

import Data.Proxy ( Proxy(Proxy) )
import Miso.Css.Style ( E(NilE), ElementStructure(Composite) )


div_ :: E "div" Composite '[] '[]
div_ = NilE (Proxy @"div")

p_ :: E "p" Composite '[] '[]
p_ = NilE (Proxy @"p")

b_ :: E "b" Composite '[] '[]
b_ = NilE (Proxy @"b")

i_ :: E "i" Composite '[] '[]
i_ = NilE (Proxy @"i")

th_ :: E "th" Composite '[] '[]
th_ = NilE (Proxy @"th")

td_ :: E "td" Composite '[] '[]
td_ = NilE (Proxy @"td")

table_ :: E "table" Composite '[] '[]
table_ = NilE (Proxy @"table")

tr_ :: E "tr" Composite '[] '[]
tr_ = NilE (Proxy @"tr")

h1_ :: E "h1" Composite '[] '[]
h1_ = NilE (Proxy @"h1")

img_ :: E "img" Composite '[] '[]
img_ = NilE (Proxy @"img")

br_ :: E "br" Composite '[] '[]
br_ = NilE (Proxy @"br")

hr_ :: E "hr" Composite '[] '[]
hr_ = NilE (Proxy @"hr")

ul_ :: E "ul" Composite '[] '[]
ul_ = NilE (Proxy @"ul")

li_ :: E "li" Composite '[] '[]
li_ = NilE (Proxy @"li")
