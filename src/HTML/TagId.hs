{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
module HTML.TagId
  ( TagId(..)
  , tagIdFromText
  , tagIdFromBS
  , tagIdToText
  , tagIdIsSpecial
  , tagIdIsFormatting
  , tagIdIsHeading
  , tagIdIsImpliedEndTag
  , tagIdIsDefaultScopeTerminator
  , tagIdIsButtonScopeTerminator
  , tagIdIsListItemScopeTerminator
  , tagIdIsTableScopeTerminator
  , tagIdIsForeignBreakout
  , tagIdIsVoid
  , tagIdIsRawText
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.ByteString (ByteString)
import qualified Data.HashMap.Strict as HM

data TagId
  = TagA | TagAbbr | TagAddress | TagApplet | TagArea | TagArticle | TagAside
  | TagB | TagBase | TagBasefont | TagBgsound | TagBig | TagBlockquote | TagBody
  | TagBr | TagButton
  | TagCaption | TagCenter | TagCode | TagCol | TagColgroup
  | TagDd | TagDetails | TagDialog | TagDir | TagDiv | TagDl | TagDt
  | TagEm | TagEmbed
  | TagFieldset | TagFigcaption | TagFigure | TagFont | TagFooter | TagForm
  | TagFrame | TagFrameset
  | TagH1 | TagH2 | TagH3 | TagH4 | TagH5 | TagH6
  | TagHead | TagHeader | TagHgroup | TagHr | TagHtml
  | TagI | TagIframe | TagImage | TagImg | TagInput
  | TagKeygen
  | TagLi | TagLink | TagListing
  | TagMain | TagMarquee | TagMenu | TagMenuitem | TagMeta
  | TagNav | TagNobr | TagNoembed | TagNoframes | TagNoscript
  | TagObject | TagOl | TagOptgroup | TagOption
  | TagP | TagParam | TagPlaintext | TagPre
  | TagRb | TagRp | TagRt | TagRtc | TagRuby
  | TagS | TagScript | TagSearch | TagSection | TagSelect | TagSmall
  | TagSource | TagSpan | TagStrike | TagStrong | TagStyle | TagSub
  | TagSummary | TagSup
  | TagSvg | TagMath
  | TagTable | TagTbody | TagTd | TagTemplate | TagTextarea | TagTfoot
  | TagTh | TagThead | TagTitle | TagTr | TagTrack | TagTt
  | TagU | TagUl
  | TagVar | TagWbr
  | TagXmp
  | TagUnknown
  deriving (Eq, Ord, Enum, Bounded, Show)

{-# NOINLINE tagMap #-}
tagMap :: HM.HashMap Text TagId
tagMap = HM.fromList
  [ ("a", TagA), ("abbr", TagAbbr), ("address", TagAddress), ("applet", TagApplet)
  , ("area", TagArea), ("article", TagArticle), ("aside", TagAside)
  , ("b", TagB), ("base", TagBase), ("basefont", TagBasefont), ("bgsound", TagBgsound)
  , ("big", TagBig), ("blockquote", TagBlockquote), ("body", TagBody)
  , ("br", TagBr), ("button", TagButton)
  , ("caption", TagCaption), ("center", TagCenter), ("code", TagCode)
  , ("col", TagCol), ("colgroup", TagColgroup)
  , ("dd", TagDd), ("details", TagDetails), ("dialog", TagDialog)
  , ("dir", TagDir), ("div", TagDiv), ("dl", TagDl), ("dt", TagDt)
  , ("em", TagEm), ("embed", TagEmbed)
  , ("fieldset", TagFieldset), ("figcaption", TagFigcaption), ("figure", TagFigure)
  , ("font", TagFont), ("footer", TagFooter), ("form", TagForm)
  , ("frame", TagFrame), ("frameset", TagFrameset)
  , ("h1", TagH1), ("h2", TagH2), ("h3", TagH3), ("h4", TagH4)
  , ("h5", TagH5), ("h6", TagH6)
  , ("head", TagHead), ("header", TagHeader), ("hgroup", TagHgroup)
  , ("hr", TagHr), ("html", TagHtml)
  , ("i", TagI), ("iframe", TagIframe), ("image", TagImage), ("img", TagImg)
  , ("input", TagInput)
  , ("keygen", TagKeygen)
  , ("li", TagLi), ("link", TagLink), ("listing", TagListing)
  , ("main", TagMain), ("marquee", TagMarquee), ("menu", TagMenu)
  , ("menuitem", TagMenuitem), ("meta", TagMeta)
  , ("nav", TagNav), ("nobr", TagNobr), ("noembed", TagNoembed)
  , ("noframes", TagNoframes), ("noscript", TagNoscript)
  , ("object", TagObject), ("ol", TagOl), ("optgroup", TagOptgroup)
  , ("option", TagOption)
  , ("p", TagP), ("param", TagParam), ("plaintext", TagPlaintext), ("pre", TagPre)
  , ("rb", TagRb), ("rp", TagRp), ("rt", TagRt), ("rtc", TagRtc), ("ruby", TagRuby)
  , ("s", TagS), ("script", TagScript), ("search", TagSearch)
  , ("section", TagSection), ("select", TagSelect), ("small", TagSmall)
  , ("source", TagSource), ("span", TagSpan), ("strike", TagStrike)
  , ("strong", TagStrong), ("style", TagStyle), ("sub", TagSub)
  , ("summary", TagSummary), ("sup", TagSup)
  , ("svg", TagSvg), ("math", TagMath)
  , ("table", TagTable), ("tbody", TagTbody), ("td", TagTd)
  , ("template", TagTemplate), ("textarea", TagTextarea), ("tfoot", TagTfoot)
  , ("th", TagTh), ("thead", TagThead), ("title", TagTitle), ("tr", TagTr)
  , ("track", TagTrack), ("tt", TagTt)
  , ("u", TagU), ("ul", TagUl)
  , ("var", TagVar), ("wbr", TagWbr)
  , ("xmp", TagXmp)
  ]

{-# INLINE tagIdFromText #-}
tagIdFromText :: Text -> TagId
tagIdFromText !t = case HM.lookup t tagMap of
  Just tid -> tid
  Nothing  -> TagUnknown

{-# INLINE tagIdFromBS #-}
tagIdFromBS :: ByteString -> TagId
tagIdFromBS = tagIdFromText . TE.decodeUtf8Lenient

tagIdToText :: TagId -> Text
tagIdToText TagA = "a"
tagIdToText TagAbbr = "abbr"
tagIdToText TagAddress = "address"
tagIdToText TagApplet = "applet"
tagIdToText TagArea = "area"
tagIdToText TagArticle = "article"
tagIdToText TagAside = "aside"
tagIdToText TagB = "b"
tagIdToText TagBase = "base"
tagIdToText TagBasefont = "basefont"
tagIdToText TagBgsound = "bgsound"
tagIdToText TagBig = "big"
tagIdToText TagBlockquote = "blockquote"
tagIdToText TagBody = "body"
tagIdToText TagBr = "br"
tagIdToText TagButton = "button"
tagIdToText TagCaption = "caption"
tagIdToText TagCenter = "center"
tagIdToText TagCode = "code"
tagIdToText TagCol = "col"
tagIdToText TagColgroup = "colgroup"
tagIdToText TagDd = "dd"
tagIdToText TagDetails = "details"
tagIdToText TagDialog = "dialog"
tagIdToText TagDir = "dir"
tagIdToText TagDiv = "div"
tagIdToText TagDl = "dl"
tagIdToText TagDt = "dt"
tagIdToText TagEm = "em"
tagIdToText TagEmbed = "embed"
tagIdToText TagFieldset = "fieldset"
tagIdToText TagFigcaption = "figcaption"
tagIdToText TagFigure = "figure"
tagIdToText TagFont = "font"
tagIdToText TagFooter = "footer"
tagIdToText TagForm = "form"
tagIdToText TagFrame = "frame"
tagIdToText TagFrameset = "frameset"
tagIdToText TagH1 = "h1"
tagIdToText TagH2 = "h2"
tagIdToText TagH3 = "h3"
tagIdToText TagH4 = "h4"
tagIdToText TagH5 = "h5"
tagIdToText TagH6 = "h6"
tagIdToText TagHead = "head"
tagIdToText TagHeader = "header"
tagIdToText TagHgroup = "hgroup"
tagIdToText TagHr = "hr"
tagIdToText TagHtml = "html"
tagIdToText TagI = "i"
tagIdToText TagIframe = "iframe"
tagIdToText TagImage = "image"
tagIdToText TagImg = "img"
tagIdToText TagInput = "input"
tagIdToText TagKeygen = "keygen"
tagIdToText TagLi = "li"
tagIdToText TagLink = "link"
tagIdToText TagListing = "listing"
tagIdToText TagMain = "main"
tagIdToText TagMarquee = "marquee"
tagIdToText TagMenu = "menu"
tagIdToText TagMenuitem = "menuitem"
tagIdToText TagMeta = "meta"
tagIdToText TagNav = "nav"
tagIdToText TagNobr = "nobr"
tagIdToText TagNoembed = "noembed"
tagIdToText TagNoframes = "noframes"
tagIdToText TagNoscript = "noscript"
tagIdToText TagObject = "object"
tagIdToText TagOl = "ol"
tagIdToText TagOptgroup = "optgroup"
tagIdToText TagOption = "option"
tagIdToText TagP = "p"
tagIdToText TagParam = "param"
tagIdToText TagPlaintext = "plaintext"
tagIdToText TagPre = "pre"
tagIdToText TagRb = "rb"
tagIdToText TagRp = "rp"
tagIdToText TagRt = "rt"
tagIdToText TagRtc = "rtc"
tagIdToText TagRuby = "ruby"
tagIdToText TagS = "s"
tagIdToText TagScript = "script"
tagIdToText TagSearch = "search"
tagIdToText TagSection = "section"
tagIdToText TagSelect = "select"
tagIdToText TagSmall = "small"
tagIdToText TagSource = "source"
tagIdToText TagSpan = "span"
tagIdToText TagStrike = "strike"
tagIdToText TagStrong = "strong"
tagIdToText TagStyle = "style"
tagIdToText TagSub = "sub"
tagIdToText TagSummary = "summary"
tagIdToText TagSup = "sup"
tagIdToText TagSvg = "svg"
tagIdToText TagMath = "math"
tagIdToText TagTable = "table"
tagIdToText TagTbody = "tbody"
tagIdToText TagTd = "td"
tagIdToText TagTemplate = "template"
tagIdToText TagTextarea = "textarea"
tagIdToText TagTfoot = "tfoot"
tagIdToText TagTh = "th"
tagIdToText TagThead = "thead"
tagIdToText TagTitle = "title"
tagIdToText TagTr = "tr"
tagIdToText TagTrack = "track"
tagIdToText TagTt = "tt"
tagIdToText TagU = "u"
tagIdToText TagUl = "ul"
tagIdToText TagVar = "var"
tagIdToText TagWbr = "wbr"
tagIdToText TagXmp = "xmp"
tagIdToText TagUnknown = ""

{-# INLINE tagIdIsSpecial #-}
tagIdIsSpecial :: TagId -> Bool
tagIdIsSpecial !tid = case tid of
  TagAddress -> True; TagApplet -> True; TagArea -> True; TagArticle -> True
  TagAside -> True; TagBase -> True; TagBasefont -> True; TagBgsound -> True
  TagBlockquote -> True; TagBody -> True; TagBr -> True; TagButton -> True
  TagCaption -> True; TagCenter -> True; TagCol -> True; TagColgroup -> True
  TagDd -> True; TagDetails -> True; TagDialog -> True; TagDir -> True
  TagDiv -> True; TagDl -> True; TagDt -> True; TagEmbed -> True
  TagFieldset -> True; TagFigcaption -> True; TagFigure -> True
  TagFooter -> True; TagForm -> True; TagFrame -> True; TagFrameset -> True
  TagH1 -> True; TagH2 -> True; TagH3 -> True; TagH4 -> True
  TagH5 -> True; TagH6 -> True; TagHead -> True; TagHeader -> True
  TagHgroup -> True; TagHr -> True; TagHtml -> True
  TagIframe -> True; TagImg -> True; TagInput -> True; TagKeygen -> True
  TagLi -> True; TagLink -> True; TagListing -> True; TagMain -> True
  TagMarquee -> True; TagMenu -> True; TagMenuitem -> True; TagMeta -> True
  TagNav -> True; TagNoembed -> True; TagNoframes -> True; TagNoscript -> True
  TagObject -> True; TagOl -> True; TagP -> True; TagParam -> True
  TagPlaintext -> True; TagPre -> True; TagScript -> True; TagSearch -> True
  TagSection -> True; TagSelect -> True; TagSource -> True; TagStyle -> True
  TagSummary -> True; TagTable -> True; TagTbody -> True; TagTd -> True
  TagTemplate -> True; TagTextarea -> True; TagTfoot -> True; TagTh -> True
  TagThead -> True; TagTitle -> True; TagTr -> True; TagTrack -> True
  TagUl -> True; TagWbr -> True
  _ -> False

{-# INLINE tagIdIsFormatting #-}
tagIdIsFormatting :: TagId -> Bool
tagIdIsFormatting !tid = case tid of
  TagA -> True; TagB -> True; TagBig -> True; TagCode -> True
  TagEm -> True; TagFont -> True; TagI -> True; TagNobr -> True
  TagS -> True; TagSmall -> True; TagStrike -> True; TagStrong -> True
  TagTt -> True; TagU -> True
  _ -> False

{-# INLINE tagIdIsHeading #-}
tagIdIsHeading :: TagId -> Bool
tagIdIsHeading !tid = case tid of
  TagH1 -> True; TagH2 -> True; TagH3 -> True
  TagH4 -> True; TagH5 -> True; TagH6 -> True
  _ -> False

{-# INLINE tagIdIsImpliedEndTag #-}
tagIdIsImpliedEndTag :: TagId -> Bool
tagIdIsImpliedEndTag !tid = case tid of
  TagDd -> True; TagDt -> True; TagLi -> True; TagOption -> True
  TagOptgroup -> True; TagP -> True; TagRb -> True; TagRp -> True
  TagRt -> True; TagRtc -> True
  _ -> False

{-# INLINE tagIdIsDefaultScopeTerminator #-}
tagIdIsDefaultScopeTerminator :: TagId -> Bool
tagIdIsDefaultScopeTerminator !tid = case tid of
  TagApplet -> True; TagCaption -> True; TagHtml -> True; TagTable -> True
  TagTd -> True; TagTh -> True; TagMarquee -> True; TagObject -> True
  TagTemplate -> True
  _ -> False

{-# INLINE tagIdIsButtonScopeTerminator #-}
tagIdIsButtonScopeTerminator :: TagId -> Bool
tagIdIsButtonScopeTerminator !tid =
  tid == TagButton || tagIdIsDefaultScopeTerminator tid

{-# INLINE tagIdIsListItemScopeTerminator #-}
tagIdIsListItemScopeTerminator :: TagId -> Bool
tagIdIsListItemScopeTerminator !tid = case tid of
  TagOl -> True; TagUl -> True
  _ -> tagIdIsDefaultScopeTerminator tid

{-# INLINE tagIdIsTableScopeTerminator #-}
tagIdIsTableScopeTerminator :: TagId -> Bool
tagIdIsTableScopeTerminator !tid = case tid of
  TagHtml -> True; TagTable -> True; TagTemplate -> True
  _ -> False

{-# INLINE tagIdIsForeignBreakout #-}
tagIdIsForeignBreakout :: TagId -> Bool
tagIdIsForeignBreakout !tid = case tid of
  TagB -> True; TagBig -> True; TagBlockquote -> True; TagBody -> True
  TagBr -> True; TagCenter -> True; TagCode -> True; TagDd -> True
  TagDiv -> True; TagDl -> True; TagDt -> True; TagEm -> True
  TagEmbed -> True; TagH1 -> True; TagH2 -> True; TagH3 -> True
  TagH4 -> True; TagH5 -> True; TagH6 -> True; TagHead -> True
  TagHr -> True; TagI -> True; TagImg -> True; TagLi -> True
  TagListing -> True; TagMenu -> True; TagMeta -> True; TagNobr -> True
  TagOl -> True; TagP -> True; TagPre -> True; TagRuby -> True
  TagS -> True; TagSmall -> True; TagSpan -> True; TagStrong -> True
  TagStrike -> True; TagSub -> True; TagSup -> True; TagTable -> True
  TagTt -> True; TagU -> True; TagUl -> True; TagVar -> True
  _ -> False

{-# INLINE tagIdIsVoid #-}
tagIdIsVoid :: TagId -> Bool
tagIdIsVoid !tid = case tid of
  TagArea -> True; TagBase -> True; TagBr -> True; TagCol -> True
  TagEmbed -> True; TagHr -> True; TagImg -> True; TagInput -> True
  TagLink -> True; TagMeta -> True; TagSource -> True; TagTrack -> True
  TagWbr -> True
  _ -> False

{-# INLINE tagIdIsRawText #-}
tagIdIsRawText :: TagId -> Bool
tagIdIsRawText !tid = tid == TagScript || tid == TagStyle
