{-# LANGUAGE OverloadedStrings     #-}


module Bridge where


import qualified Data.ByteString            as BS
import           Data.ByteString            (ByteString)
import           Data.List                  (intersperse)
import           Data.String                (fromString)
import           PlutusTx                   (ToData, Data (..), toBuiltinData, builtinDataToData)


numToBS :: Integer -> ByteString
numToBS = fromString . show


concatWithComma :: [ByteString] -> ByteString
concatWithComma = BS.concat . intersperse ","


fromData :: ByteString -> Data -> ([ByteString], ByteString)
fromData fnName dat =
  -- {{{
  let
    addPostfix :: ByteString -> ByteString
    addPostfix post = fnName <> "_" <> post

    applyIndeces :: ByteString -> ByteString -> Integer -> ByteString
    applyIndeces elemName fn maxInd =
      -- {{{
      let
        args =
          concatWithComma $
            map (\ind -> elemName <> "[" <> numToBS ind <> "]") [0 .. maxInd]
      in
      fn <> "(" <> args <> ")"
      -- }}}
  in
  case dat of
    Constr index dats   ->
      -- {{{
      let
        foldFn :: Data
               -> (Integer, [(ByteString, [ByteString])], ByteString)
               -> (Integer, [(ByteString, [ByteString])], ByteString)
        foldFn dat' (count, fnParams, acc) =
          -- {{{
          let
            fnName'                 = addPostfix $ numToBS count
            (innerParams, innerDef) = fromData fnName' dat'
          in
          (count + 1, (fnName', innerParams) : fnParams, innerDef <> acc)
          -- }}}
        varsFold :: (ByteString, [ByteString])
                 -> (Integer, [ByteString], ByteString, [ByteString])
                 -> (Integer, [ByteString], ByteString, [ByteString])
        varsFold (fnName', fnParams) (count, varsList, vars, inputParams) =
          -- {{{
          let
            varName        = "_" <> numToBS count
            commaSeparated = concatWithComma fnParams
            def            =
                 "let " <> varName <> "=" <> fnName' <> "("
              <>   commaSeparated
              <> ");"
          in
          ( count + 1
          , varName : varsList
          , def <> vars
          , fnParams ++ inputParams
          )
          -- }}}
        (_, allParams, fnDefs)                   =
          -- {{{
          foldr foldFn   (0, [], BS.empty) dats
          -- }}}
        (_, listOfVars, varDefs, allInputParams) =
          -- {{{
          foldr varsFold (0, [], BS.empty, []) allParams
          -- }}}
        finalParams = concatWithComma allInputParams
        finalVars   = "[" <> concatWithComma listOfVars <> "]"
      in
      ( allInputParams
      ,    "function " <> fnName <> "(" <> finalParams <> "){"
        <>   fnDefs
        <>   varDefs
        <>   "return data_constr(" <> (0x22 `BS.cons` (numToBS index `BS.snoc` 0x22)) <> "," <> finalVars <> ");"
        <> "}"
      )
      -- }}}
    Map []              ->
      -- {{{
      ( []
      ,    "function " <> fnName <> "(){"
        <>   "return data_map([]);"
        <> "}"
      )
      -- }}}
    Map ((k, v) : _ )   -> -- assumes shared type between elements.
      -- {{{
      let
        pluralPN          = addPostfix "kvs"
        pluralPNParam     = pluralPN <> "_param"
        singularPN        = addPostfix "kv"
        kFnName           = "key_" <> fnName
        vFnName           = "val_" <> fnName
        (kParams, kFnDef) = fromData kFnName k
        (vParams, vFnDef) = fromData vFnName v
        maxKInd           = fromIntegral $ length kParams - 1
        maxVInd           = fromIntegral $ length vParams - 1
      in
      ( [pluralPN]
      ,    "function " <> fnName <> "(" <> pluralPNParam <> "){"
        <>   kFnDef
        <>   vFnDef
        <>   pluralPNParam <> ".map("
        <>     singularPN <> " => "
        <>       "[" <> applyIndeces (singularPN <> "[0]") kFnName maxKInd
        <>       "," <> applyIndeces (singularPN <> "[1]") vFnName maxVInd
        <>       "]"
        <>   ");"
        <>   "return data_map("<> pluralPNParam <> ");"
        <> "}"
      )
      -- }}}
    List []             ->
      -- {{{
      ( []
      ,    "function " <> fnName <> "(){"
        <>   "return data_list([]);"
        <> "}"
      )
      -- }}}
    List (dat' : _ )    -> -- assumes shared type between elements.
      -- {{{
      let
        pluralPN                  = addPostfix "xs"
        pluralPNParam             = pluralPN <> "_param"
        singularPN                = addPostfix "x"
        innerFnName               = "inner_" <> fnName
        (innerParams, innerFnDef) = fromData innerFnName dat'
        innerMaxIndex             = fromIntegral $ length innerParams - 1
      in
      ( [pluralPN]
      ,    "function " <> fnName <> "(" <> pluralPNParam <> "){"
        <>   innerFnDef
        <>   pluralPNParam <> ".map(" <> singularPN <> " => "
        <>     applyIndeces singularPN innerFnName innerMaxIndex
        <>   ");"
        <>   "return data_list(" <> pluralPNParam <> ");"
        <> "}"
      )
      -- }}}
    I _                 ->
      -- {{{
      let
        paramName = addPostfix "a"
      in
      ( [paramName]
      ,    "function " <> fnName <> "(" <> paramName <> "_param){"
        <>   "return data_integer(" <> paramName <> "_param);"
        <> "}"
      )
      -- }}}
    B _                 ->
      -- {{{
      let
        paramName = addPostfix "a"
      in
      ( [paramName]
      ,    "function " <> fnName <> "(" <> paramName <> "_param){"
        <>   "return data_byteString(" <> paramName <> "_param);"
        <> "}"
      )
      -- }}}
  -- }}}


writeFile :: ToData a => FilePath -> ByteString -> a -> IO ()
writeFile file fnName customData =
  -- {{{
  let
    data' = builtinDataToData $ toBuiltinData customData
    (_, jsFunction) = fromData fnName data'
  in
  BS.writeFile file jsFunction
  -- }}}


