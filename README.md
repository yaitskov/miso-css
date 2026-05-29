# miso-css

## Motivation

**miso-css** is an evolutionary step ahead from
[css-class-bindings](https://github.com/yaitskov/css-class-bindings).

CSS class of an atomic selector can be applied to any DOM element, but
that is not true for classes used in composite selectors.  Rules with
partially matched selectors are silently ignored by browser and this
open a door for introducing bugs during consequent changes.
Css-class-binding just cannot cope with such problem and miso-css
uses dependent types to track what CSS classes can applied to HTML
elements.

## Usage

miso-css runs a css parser to extract CSS selectors and generates
Haskell constants for every CSS class with correspondent name that is
found in the input.  A type of such constant describes all possible
ways the class can used in DOM.

Besides that the library ships **E** type represting an HTML element
and a set of operators for constructing tags and combining them in DOM tree.
E is miso **VNode** type protected with a few type parameters.

### Composing tags

Before jumping straight to style application lets get familiar with
syntax for tag composition because it is different in vanilla miso.

#### Appending a child
```haskell
div_ </ p_
```

``` html
<div>
  <p></p>
</div>
```

#### Adding a sibling
```haskell
ul_ </ li_ </ li_
```

``` html
<ul>
  <li></li>
  <li></li>
</ul>
```

#### Appending a child to child
```haskell
body_ </ (section_ </ p_)
```

``` html
<body>
  <section>
    <p></p>
  </section>
</body>
```

#### Adding CDATA
```haskell
a_ <@ "click"
```

``` html
<a>click</a>
```

#### Adding a raw miso DOM chunk
```haskell
import Miso.Html qualified as MH
import Miso.Html.Property qualified as MH

go = div_ =< MH.p_ [] [ "h" ]
```

``` html
<div><p>h</p></div>
```

#### Adding tag attribute
```haskell
a_ =<| atr @"url" "http://link.com"
```

``` html
<a url="http://link.com"></a>
```

#### Adding tag ID
```haskell
div_ =# (Proxy @"footer")
```

``` html
<div id="footer"></div>
```

#### Applying CSS class
```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
[css|.red { color: red; }|]

div_ =. red
```

``` html
<div class="red"></div>
```

#### Mix all at once
```haskell
{-# LANGUAGE QuasiQuotes #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
[css|.form .red { color: red; }|]

div_ =. form =# Proxy @"footer"
  </ (a_ =. red =<| atr @"url" "/click.php?x=1"
      </ (span_ <@ "Click me"))
```

``` html
<div class="form" id="footer>
  <a class="red" url="/click.php?x=1">
    <span>Click me</span>
  </a>
</div>
```

### Breaking rules

Until now all above samples must be valid and should type
check.  This section enumerates HTML snippets with ill-applied
classes, expected errors and comments.

#### There can be only one

An element ID can be used once in a HTML document.

```haskell
div_ =# (Proxy @"Duncan MacLeod")
  </ div_ =# (Proxy @"Duncan MacLeod")
```

```
Couldn't match type: '[DuplicatedId "Duncan MacLeod"]
               with: '[]
```

#### Parent class is missing

```haskell
[css|.a .b {}|]

div_ =. b
```

The error message is a list of triples where first element is a list of not
applied classes, ids (hashes), tag names, attribute name.

```haskell
[([C "a"], [], [])]
```

Class **a** and **b** are missing:

```haskell
[css|.a .b .c {}|]

div_ =. c
```

```haskell
[ ([C "b"], [], [])
, ([C "a"], [], [])
]
```

#### B element

When selector with child relation is partially applied the triple
contains B element.  It is a synthetic element preventing the failed
rule from matching latery somewhere upper in DOM by an accident.

```haskell
[css|.a > .b {}|]

div_ </ div_ =. b
```

```
[([B, C "a"], [], [])]
```

#### One of classes is missing

Second element of triple is a list of applied classes.  It helps to
understand what worket out and what didn't in a composite selector.

```haskell
[css|.a.b > .c {}|]

div_ =. a </ div_ =. c
```

```haskell
[([B, C "b"], [C "a"], [])]
```

##### Sibling is missing

The third element of triple explains sibling errors.

```haskell
[css|.a + .b {}|]

div_ </ div_ =. b
```

Class **a** is not applied:

```haskell
[([B], [], [[ [B], [C "a"]]])]
```

### E type

```haskell
data E
     model
     action
     (en :: Symbol)
     (es :: ElementStructure)
     (re :: Maybe Root)
     (ei :: Maybe Symbol)
     (atrs :: [Symbol])
     (knownIds :: KnownIDS)
     (cls :: [Symbol])
     (l :: [[[Seg]]])
     (children :: [[SubSeg]])
```

First two parameters **model** and **action** are forwarded to miso VNode type.

#### en - tag name

In ghci session:

```
:t div_
div_
  :: E model
       action
       "div"
...
```

#### es - element structure

Most often its value is **Composite** which means that the element could have
children.  *Es* parameter of *CDATA* element is **Atomic**.

#### re - root indicator

It is the root tag indicator. A root tag cannot be adopted.

```haskell
[css|:root > .a {}|]
```
#### ei - tag hash

```haskell
div_ =# Proxy @"Duncan"
```

#### atrs - names of tag attributes

```
:t a_ =# Proxy @"x" =<| atr @"url" "/click.php?x=1"
...
       ["url", "id"]
...
```
#### knownIds - hashes used in tag descendants

```
:t div_ =# Proxy @"x" </ div_ =# Proxy @"y" </ div_ =# Proxy @"z"
...
       (KnownIds '[] ["x", "y", "z"])
...
```

#### cls - classes applied to tag

Classes applied to children and descedants are not included.
```
:t div_ =. a =. b </ div_ =. c
...
       ["b", "a"]
...
```

#### l - ancestor constraints

The parameter describes requirements to be satisfied in ancestor of
the tag.

```haskell
[css|.a .b {}|]
```

```
:t div_ =. b
...
       '[ '[['(AutoClean, '[], '[], '[]),
             '(NowOrLater, '[C "a"], '[], '[])]]]
...
```

#### children

List of lists of children subselectors in reverse order.

```haskell
[css|.a {} .b {}|]
```

```
:t div_ </ ul_ =. a =. b </ ol_ =# Proxy @"x"
...
       [[I "x", T "ol"], [T "ul", C "b", C "a"]]
...
```

### Hello World

``` haskell
module Main where

import Miso
import Miso.Css.Miso

-- default name is "cssAsLiteralText"
renameCssTextConst "cssFromQq"

[css|
.c .b .a {
  color: #fc2c2c;
}
|]

-- instead of quasi-quoted CSS
-- the whole CSS file can be included with:
--   includeCss "assets/style.css"

app :: App Model Action
app = (component emptyModel updateModel viewModel)
  { styles = [ Style cssFromQq ] }

viewModel :: Model -> View Model Action
viewModel _ =
  toView $
    html_
      </ (body_
        </ (div_ =. c
          </ (div_ =. b
            </ (button_ =. a
              <@ "Submit"))))
```

## Development environment

HLS should be available inside the default dev shell.

```shell
$ nix develop
$ emacs src/*/*/Qq.hs &
$ cabal build
$ cabal test --test-option=--hide-successes
```
