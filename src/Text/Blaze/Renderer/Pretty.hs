-- | A renderer that produces pretty HTML, mostly meant for debugging purposes.
--
module Text.Blaze.Renderer.Pretty
    ( renderMarkup
    , renderHtml
    ) where

import Text.Blaze.Internal
import Text.Blaze.Renderer.String (fromChoiceString)

-- | Render some 'Markup' to an appending 'String'.
--
renderString :: Markup  -- ^ Markup to render
             -> String  -- ^ String to append
             -> String  -- ^ Resulting String
renderString = go 0 Nothing
  where
    go :: Int -> Maybe (String -> String) -> MarkupM b -> String -> String
    go i Nothing (Parent _ open close content) =
        ind i . getString open . (">\n" ++) . go (inc i) Nothing content
              . ind i . getString close .  ('\n' :)
    go i (Just attrs) (Parent _ open close content) =
        ind i . getString open . attrs . (">\n" ++) . go (inc i) Nothing content
              . ind i . getString close .  ('\n' :)
    go i Nothing (CustomParent tag content) =
        ind i . ('<' :) . fromChoiceString tag . (">\n" ++) .
        go (inc i) Nothing content . ind i . ("</" ++) . fromChoiceString tag .
        (">\n" ++)
    go i (Just attrs) (CustomParent tag content) =
        ind i . ('<' :) . fromChoiceString tag . attrs . (">\n" ++) .
        go (inc i) Nothing content . ind i . ("</" ++) . fromChoiceString tag .
        (">\n" ++)
    go i Nothing (Leaf _ begin end) =
        ind i . getString begin . getString end . ('\n' :)
    go i (Just attrs) (Leaf _ begin end) =
        ind i . getString begin . attrs . getString end . ('\n' :)
    go i Nothing (CustomLeaf tag close) =
        ind i . ('<' :) . fromChoiceString tag .
        ((if close then " />\n" else ">\n") ++)
    go i (Just attrs) (CustomLeaf tag close) =
        ind i . ('<' :) . fromChoiceString tag . attrs .
        ((if close then " />\n" else ">\n") ++)
    go i Nothing (AddAttribute _ key value h) = flip (go i) h $
        Just $ getString key . fromChoiceString value . ('"' :)
    go i (Just attrs) (AddAttribute _ key value h) = flip (go i) h $
        Just $ getString key . fromChoiceString value . ('"' :) . attrs
    go i Nothing (AddCustomAttribute key value h) = flip (go i) h $
        Just $ (' ' : ) . fromChoiceString key . ("=\"" ++) . fromChoiceString value .
        ('"' :)
    go i (Just attrs) (AddCustomAttribute key value h) = flip (go i) h $
        Just $ (' ' : ) . fromChoiceString key . ("=\"" ++) . fromChoiceString value .
        ('"' :) .  attrs
    go i _ (Content content) = ind i . fromChoiceString content . ('\n' :)
    go i _ (Comment comment) = ind i .
        ("<!-- " ++) . fromChoiceString comment . (" -->\n" ++)
    go i attrs (Append h1 h2) = go i attrs h1 . go i attrs h2
    go _ _ Empty = id
    {-# NOINLINE go #-}

    -- Increase the indentation
    inc = (+) 4

    -- Produce appending indentation
    ind i = (replicate i ' ' ++)
{-# INLINE renderString #-}

-- | Render markup to a lazy 'String'. The result is prettified.
--
renderMarkup :: Markup -> String
renderMarkup html = renderString html ""
{-# INLINE renderMarkup #-}

renderHtml :: Markup -> String
renderHtml = renderMarkup
{-# INLINE renderHtml #-}
{-# DEPRECATED renderHtml
    "Use renderHtml from Text.Blaze.Html.Renderer.Pretty instead" #-}
