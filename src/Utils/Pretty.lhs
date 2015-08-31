Definition of a pretty-print type class
========================

> module Utils.Pretty(module Text.PrettyPrint.HughesPJ, PPrint(..)) where

> import Text.PrettyPrint.HughesPJ

> class PPrint a where
>    pprint :: a -> Doc


