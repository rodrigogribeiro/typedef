A parsec based parser for constraints
========================

> module Parser.Constr where

> import Text.Parser
> import Text.Language


Constraint language def

> constrDef :: LanguageDef
> constrDef = emptyDef {
>               reservedOpNames = [",", ".", ":", "=", "->"]
>               reservedNames = ["exists", "def", "isdef"]
>             }             
