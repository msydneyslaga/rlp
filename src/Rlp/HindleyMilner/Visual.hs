{-# LANGUAGE LexicalNegation #-}
{-# LANGUAGE QuasiQuotes #-}
module Rlp.HindleyMilner.Visual
    (
    )
    where
--------------------------------------------------------------------------------
import Control.Monad
import System.IO
import Data.Text                        qualified as T
import Data.Text.IO                     qualified as T
import Data.Pretty                      hiding (annotate)
import Data.String                      (IsString(..))
import Data.Foldable
import Misc.CofreeF
import Control.Exception

import Data.Functor.Foldable

import Text.Blaze.Html5                 as H
import Text.Blaze.Html5.Attributes      as A
import Text.Blaze.Html.Renderer.String
import Clay                             (Css, (?), px, pct, (**), (|>), (|+)
                                        , (|~))
import Clay                             qualified as C
import Clay.Render                      qualified as C
import Language.Javascript.JMacro
import Language.Javascript.JQuery       qualified as JQuery

import Core.Syntax                      as Core
import Rlp.AltSyntax                    as Rlp
import Rlp.HindleyMilner

import Prelude hiding ((**))
--------------------------------------------------------------------------------

type AnnExpr = Cofree (RlpExprF PsName)

tooltips :: Css
tooltips = do
    ".has-type" ? do
        C.position     C.relative
        C.display      C.inlineBlock
        C.borderBottom (px 1) C.dotted C.black
    ".has-type.hovering" ? do
        C.background C.green
    ".has-type.hovering" |> ".type-text" ? do
        C.display      C.block
        C.opacity      1
        C.position     C.fixed
        C.overflowWrap C.breakWord
    ".has-type" |> ".type-text" ? do
        C.display      C.none
        C.overflowWrap C.breakWord
        C.maxWidth     (pct 50)

stylesheet :: Css
stylesheet = tooltips

numbers :: Int -> Html
numbers n = docTypeHtml do
    H.head do
        H.title "naturals"
        H.style "doge"
    body do
        pre "a list of nats"
        ul $ forM_ [1..n] (li . toHtml)

withTooltip :: Html -> Html -> Html
withTooltip normal hover =
    H.div ! class_ "has-type"
        $ normal *> (H.div ! class_ "type-text" $ hover)

-- withTooltip :: Html -> Html -> Html
-- withTooltip normal hover =
--     H.div ! class_ "has-type"
--           -- ! onload "installHoverListener(this)"
--         $ normal

annExpr :: (a -> Doc ann) -> AnnExpr a -> Html
annExpr sf = code . cata \case
    t :<$ InL (LitF l)   -> withTooltip (rout l) (sf' t)
    t :<$ InL (VarF n)   -> withTooltip (rout n) (sf' t)
    t :<$ InL (AppF f x) -> withTooltip (f *> " " *> x) (sf' t)
    t :<$ InL (LamF bs e) -> withTooltip ("λ" *> bs' *> " -> " *> e) (sf' t)
        where
            bs' = fromString . show . hsep $ outPrec appPrec1 <$> bs
  where
    sf' = fromString . show . sf

rootScript :: JStat
rootScript = [jmacro|
    $(".has-type").on("mouseover mouseout", \e {
        e.stopPropagation();
        $(this).toggleClass("hovering", e.type === "mouseover");
        var o = $(this).children(".type-text")[0];
        var x = e.clientX;
        var y = e.clientY;
        o.style.top = (y + 20) + 'px';
        o.style.left = (x + 20) + 'px';
    });
|]

jsScript :: (IsString s, JsToDoc w, JMacro w) => w -> s
jsScript = fromString . show . renderJs

rootPage :: Html -> Html
rootPage html = docTypeHtml do
    H.head do
        H.title "naturals"
        H.style (toHtml . C.render $ stylesheet)
    H.body do
        html
        script ! src (fromString $ "https:" ++ JQuery.url) $ ""
        script ! src "https://code.jquery.com/ui/1.13.2/jquery-ui.min.js" $ ""
        script (fromString . show . renderJs $ rootScript)

renderTmp :: Html -> IO ()
renderTmp = writeFile "/tmp/t.html" . renderHtml

renderTmp' :: Html -> IO ()
renderTmp' = writeFile "/tmp/t.html" . renderHtml . rootPage

renderExpr :: RlpExpr PsName -> IO ()
renderExpr e = case runHM' . annotate $ e of
    Left es -> error (show es)
    Right e' -> renderTmp' . annExpr (fromString . show) $ e'

renderExpr' :: RlpExpr PsName -> IO ()
renderExpr' e = case runHM' . solve $ e of
    Left es -> error (show es)
    Right e' -> renderTmp' . annExpr out $ e'

