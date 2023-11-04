{-# OPTIONS_GHC -Wno-missing-export-lists #-}

module Concur.Replica.Spa.Html where

import Concur.Replica.DOM qualified as DOM
import Concur.Replica.DOM.Props (Props, key)
import Concur.Replica.Spa.Widget (Widget)
import Concur.Replica.Spa.Widget qualified as Widget
import Relude hiding (div)

node :: Text -> [Props a] -> [Widget a] -> Widget a
node tag props children = do
  env <- ask
  Widget.new $ DOM.el tag props (Widget.runWidget env <$> children)

text :: Text -> Widget a
text = Widget.new . DOM.text

nothing :: Widget a
nothing = text ""

maybeWidget :: (t -> Widget a) -> Maybe t -> Widget a
maybeWidget _ Nothing = text ""
maybeWidget f (Just x) = f x

div :: [Props a] -> [Widget a] -> Widget a
div = node "div"

button :: [Props a] -> [Widget a] -> Widget a
button = node "button"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/table
table :: [Props a] -> [Widget a] -> Widget a
table = node "table"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/thead
thead :: [Props a] -> [Widget a] -> Widget a
thead = node "thead"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tbody
tbody :: [Props a] -> [Widget a] -> Widget a
tbody = node "tbody"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tr
tr :: [Props a] -> [Widget a] -> Widget a
tr = node "tr"

-- | Contains `Key`, inteded to be used for child replacement patch
-- - <https://developer.mozilla.org/en-US/docs//EleWidgetent/tr>
trKeyed :: Text -> [Props a] -> [Widget a] -> Widget a
trKeyed k props = node "tr" (key k : props)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/th
th :: [Props a] -> [Widget a] -> Widget a
th = node "th"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/td
td :: [Props a] -> [Widget a] -> Widget a
td = node "td"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/tfoot
tfoot :: [Props a] -> [Widget a] -> Widget a
tfoot = node "tfoot"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/section
section :: [Props a] -> [Widget a] -> Widget a
section = node "section"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/header
header :: [Props a] -> [Widget a] -> Widget a
header = node "header"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/footer
footer :: [Props a] -> [Widget a] -> Widget a
footer = node "footer"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/form
form :: [Props a] -> [Widget a] -> Widget a
form = node "form"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/p
p :: [Props a] -> [Widget a] -> Widget a
p = node "p"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/s
s :: [Props a] -> [Widget a] -> Widget a
s = node "s"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ul
ul :: [Props a] -> [Widget a] -> Widget a
ul = node "ul"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/span
span :: [Props a] -> [Widget a] -> Widget a
span = node "span"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/strong
strong :: [Props a] -> [Widget a] -> Widget a
strong = node "strong"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li
li :: [Props a] -> [Widget a] -> Widget a
li = node "li"

---- | Contains `Key`, inteded to be used for child replacement patch
-- <https://developer.mozilla.org/en-US/docs/Web/HTML/Element/li>
liKeyed :: Text -> [Props a] -> [Widget a] -> Widget a
liKeyed k props = node "li" (key k : props)

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h1
h1 :: [Props a] -> [Widget a] -> Widget a
h1 = node "h1"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h2
h2 :: [Props a] -> [Widget a] -> Widget a
h2 = node "h2"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h3
h3 :: [Props a] -> [Widget a] -> Widget a
h3 = node "h3"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h4
h4 :: [Props a] -> [Widget a] -> Widget a
h4 = node "h4"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h5
h5 :: [Props a] -> [Widget a] -> Widget a
h5 = node "h5"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/h6
h6 :: [Props a] -> [Widget a] -> Widget a
h6 = node "h6"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/hr
hr :: [Props a] -> Widget a
hr = flip (node "hr") []

-- | https://developer.Widgetozilla.org/en-US/docs/Web/HTML/node/pre
pre :: [Props a] -> [Widget a] -> Widget a
pre = node "pre"

-- | https://developer.mozilla.org/en-US/docs//EleWidgetent/input
input :: [Props a] -> Widget a
input = flip (node "input") []

-- | https://developer.Widgetozilla.org/en-US/docs/Web/HTML/node/label
label :: [Props a] -> [Widget a] -> Widget a
label = node "label"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/a
a :: [Props a] -> [Widget a] -> Widget a
a = node "a"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/mark
mark :: [Props a] -> [Widget a] -> Widget a
mark = node "mark"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ruby
ruby :: [Props a] -> [Widget a] -> Widget a
ruby = node "ruby"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rt
rt :: [Props a] -> [Widget a] -> Widget a
rt = node "rt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/rp
rp :: [Props a] -> [Widget a] -> Widget a
rp = node "rp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdi
bdi :: [Props a] -> [Widget a] -> Widget a
bdi = node "bdi"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/bdo
bdo :: [Props a] -> [Widget a] -> Widget a
bdo = node "bdo"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/wbr
wbr :: [Props a] -> Widget a
wbr = flip (node "wbr") []

-- | https://developer.Widgetozilla.org/en-US/docs/Web/HTML/node/details
details :: [Props a] -> [Widget a] -> Widget a
details = node "details"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/summary
summary :: [Props a] -> [Widget a] -> Widget a
summary = node "summary"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menuitem
menuitem :: [Props a] -> [Widget a] -> Widget a
menuitem = node "menuitem"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/menu
menu :: [Props a] -> [Widget a] -> Widget a
menu = node "menu"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/fieldset
fieldset :: [Props a] -> [Widget a] -> Widget a
fieldset = node "fieldset"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/legend
legend :: [Props a] -> [Widget a] -> Widget a
legend = node "legend"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/datalist
datalist :: [Props a] -> [Widget a] -> Widget a
datalist = node "datalist"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/optgroup
optgroup :: [Props a] -> [Widget a] -> Widget a
optgroup = node "optgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/keygen
keygen :: [Props a] -> [Widget a] -> Widget a
keygen = node "keygen"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/output
output :: [Props a] -> [Widget a] -> Widget a
output = node "output"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/progress
progress :: [Props a] -> [Widget a] -> Widget a
progress = node "progress"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/meter
meter :: [Props a] -> [Widget a] -> Widget a
meter = node "meter"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/center
center :: [Props a] -> [Widget a] -> Widget a
center = node "center"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/audio
audio :: [Props a] -> [Widget a] -> Widget a
audio = node "audio"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/video
video :: [Props a] -> [Widget a] -> Widget a
video = node "video"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/source
source :: [Props a] -> Widget a
source = flip (node "source") []

-- | https://developer.Widgetozilla.org/en-US/docs/Web/HTML/node/track
track :: [Props a] -> Widget a
track = flip (node "track") []

-- | https://developer.Widgetozilla.org/en-US/docs//node/eWidgetbed
embed :: [Props a] -> Widget a
embed = flip (node "embed") []

-- | https://devnode.mozilla.org/en-US/docs/Web/HTML/Element/object
object :: [Props a] -> [Widget a] -> Widget a
object = node "object"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/param
param :: [Props a] -> Widget a
param = flip (node "param") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/ins
ins :: [Props a] -> [Widget a] -> Widget a
ins = node "ins"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/del
del :: [Props a] -> [Widget a] -> Widget a
del = node "del"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/small
small :: [Props a] -> [Widget a] -> Widget a
small = node "small"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/cite
cite :: [Props a] -> [Widget a] -> Widget a
cite = node "cite"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dfn
dfn :: [Props a] -> [Widget a] -> Widget a
dfn = node "dfn"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/abbr
abbr :: [Props a] -> [Widget a] -> Widget a
abbr = node "abbr"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/time
time :: [Props a] -> [Widget a] -> Widget a
time = node "time"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/var
var :: [Props a] -> [Widget a] -> Widget a
var = node "var"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/samp
samp :: [Props a] -> [Widget a] -> Widget a
samp = node "samp"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/kbd
kbd :: [Props a] -> [Widget a] -> Widget a
kbd = node "kbd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/caption
caption :: [Props a] -> [Widget a] -> Widget a
caption = node "caption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/colgroup
colgroup :: [Props a] -> [Widget a] -> Widget a
colgroup = node "colgroup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/col
col :: [Props a] -> Widget a
col = flip (node "col") []

-- | https://developer.Widgetozilla.org/en-US/docs/Web/HTML/node/nav
nav :: [Props a] -> [Widget a] -> Widget a
nav = node "nav"

-- | https://developer.mozilla.org/en-US/docs//EleWidgetent/article
article :: [Props a] -> [Widget a] -> Widget a
article = node "article"

-- | https://devnode.mozilla.org/en-US/docs/Web/HTML/Element/aside
aside :: [Props a] -> [Widget a] -> Widget a
aside = node "aside"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/address
address :: [Props a] -> [Widget a] -> Widget a
address = node "address"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/main
main_ :: [Props a] -> [Widget a] -> Widget a
main_ = node "main"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/body
body :: [Props a] -> [Widget a] -> Widget a
body = node "body"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figure
figure :: [Props a] -> [Widget a] -> Widget a
figure = node "figure"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/figcaption
figcaption :: [Props a] -> [Widget a] -> Widget a
figcaption = node "figcaption"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dl
dl :: [Props a] -> [Widget a] -> Widget a
dl = node "dl"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dt
dt :: [Props a] -> [Widget a] -> Widget a
dt = node "dt"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/dd
dd :: [Props a] -> [Widget a] -> Widget a
dd = node "dd"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/img
img :: [Props a] -> Widget a
img = flip (node "img") []

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/iframe
iframe :: [Props a] -> [Widget a] -> Widget a
iframe = node "iframe"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/canvas
canvas :: [Props a] -> [Widget a] -> Widget a
canvas = node "canvas"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/math
math :: [Props a] -> [Widget a] -> Widget a
math = node "math"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/select
select :: [Props a] -> [Widget a] -> Widget a
select = node "select"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/option
option :: [Props a] -> [Widget a] -> Widget a
option = node "option"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/textarea
textarea :: [Props a] -> [Widget a] -> Widget a
textarea = node "textarea"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sub
sub :: [Props a] -> [Widget a] -> Widget a
sub = node "sub"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/sup
sup :: [Props a] -> [Widget a] -> Widget a
sup = node "sup"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/br
br :: [Props a] -> Widget a
br = flip (node "br") []

-- | https://developer.Widgetozilla.org/en-US/docs/Web/HTML/node/ol
ol :: [Props a] -> [Widget a] -> Widget a
ol = node "ol"

-- | https://developer.mozilla.org/en-US/docs//EleWidgetent/blockquote
blockquote :: [Props a] -> [Widget a] -> Widget a
blockquote = node "blockquote"

-- | https://devnode.mozilla.org/en-US/docs/Web/HTML/Element/code
code :: [Props a] -> [Widget a] -> Widget a
code = node "code"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/em
em :: [Props a] -> [Widget a] -> Widget a
em = node "em"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/i
i :: [Props a] -> [Widget a] -> Widget a
i = node "i"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/b
b :: [Props a] -> [Widget a] -> Widget a
b = node "b"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/u
u :: [Props a] -> [Widget a] -> Widget a
u = node "u"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/q
q :: [Props a] -> [Widget a] -> Widget a
q = node "q"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/script
-- script :: [Props a] -> [Widget a] -> Widget a
-- script = node "script"

-- | https://developer.mozilla.org/en-US/docs/Web/HTML/Element/link
-- link :: [Props a] -> Widget a
-- link = flip (node "link") []
