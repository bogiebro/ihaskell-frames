{-# LANGUAGE ConstraintKinds, DataKinds, FlexibleContexts,
    GADTs, PatternSynonyms, ScopedTypeVariables, TypeOperators #-}
module IHaskell.Display.Frames (toTblP, toTbl, tblFold) where
import Frames
import qualified Pipes as P
import qualified Pipes.Prelude as P
import qualified Control.Foldl as L
import qualified Text.Blaze.Html4.Strict as H
import Data.Monoid
import IHaskell.Display
import IHaskell.Display.Blaze
import Data.Vinyl
import Data.Vinyl.Functor
import Data.Vinyl.TypeLevel
import Data.Proxy

instance H.ToMarkup a => H.ToMarkup (Identity a) where
  toMarkup (Identity a) = H.toMarkup a

toTblP :: (Monad m, ColumnHeaders rs, AsVinyl rs,
         RecAll Identity (UnColumn rs) H.ToMarkup) =>
         P.Producer (Record rs) m () -> m H.Html
toTblP = L.purely P.fold tblFold

toTbl :: (Foldable t, ColumnHeaders rs, AsVinyl rs,
         RecAll Identity (UnColumn rs) H.ToMarkup) => t (Record rs) -> H.Html
toTbl = L.fold tblFold

tblFold :: (ColumnHeaders rs, AsVinyl rs,
            RecAll Identity (UnColumn rs) H.ToMarkup) => L.Fold (Record rs) H.Html
tblFold = tblFold' Proxy

tblFold' :: (ColumnHeaders rs, AsVinyl rs,
            RecAll Identity (UnColumn rs) H.ToMarkup) =>
            Proxy (Record rs) -> L.Fold (Record rs) H.Html
tblFold' p = L.foldMap toRow (H.table . mappend hs) where
  hs = mconcat . map (H.th . H.string) $ columnHeaders p
  toRow (xs :: Record rs) = H.tr . mconcat . recordToList . 
             rmap (\(Compose (Dict x))-> Const $ H.td (H.toHtml x)) .
             reifyConstraint (Proxy :: Proxy H.ToMarkup) $ toVinyl xs
