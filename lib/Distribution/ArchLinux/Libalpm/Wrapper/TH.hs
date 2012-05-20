module Distribution.ArchLinux.Libalpm.Wrapper.TH (
  generateUpdaters,
  generateEmptyRecord
) where

import Debug.Trace
import Control.Applicative
import Data.Char
import Language.Haskell.TH

-- | Generate update functions for a certain record. The transformation of field names
-- goes as follows:
--
-- * @_fieldName@ to @fieldName@ ;
--
-- * @fieldName@ to @setFieldName@ .
--
-- So for the record
--
-- > data Record = Record { _x :: Int, y :: Double }
--
-- the following updaters will be generated:
--
-- > x :: Int -> Record -> Record
-- > setY :: Double -> Record -> Record
--
generateUpdaters :: Name -> Q [Dec]
generateUpdaters name = do
  TyConI (DataD _ _ _ ctors _) <- reify name
  concat <$> mapM generateUpdatersForCtor ctors

generateUpdatersForCtor :: Con -> Q [Dec]
generateUpdatersForCtor (RecC _ vars) = mapM genUpdater vars
  where
    genUpdater (fname, _, _) = do
      vname <- newName "value"
      rname <- newName "record"
      let nname = transformName fname
          updater_body = RecUpdE (VarE rname) [(fname, VarE vname)]
          updater_clause = Clause [VarP vname, VarP rname] (NormalB updater_body) []
          updater_decl = FunD nname [updater_clause]
      return updater_decl

transformName :: Name -> Name
transformName name = mkName $ case nameBase name of
  ('_':rest) -> rest
  sname      -> "set" ++ capitalize sname

capitalize :: String -> String
capitalize []     = []
capitalize (c:cs) = toUpper c : cs

-- | Generate a record value containing 'Nothing' values in all its fields given record type name.
generateEmptyRecord :: String -- ^ A name for the record value
                    -> Name   -- ^ A name of record type
                    -> Name   -- ^ A name of record type constructor
                    -> Q [Dec]
generateEmptyRecord (mkName -> declname) recname conname = do
  TyConI (DataD _ _ _ ctors _) <- reify recname
  let (ctor:_) = filter ((== conname) . ctorName) ctors
  return $ [generateRecordEmptyDecl declname ctor]
  where
    ctorName (RecC name _) = name

generateRecordEmptyDecl :: Name -> Con -> Dec
generateRecordEmptyDecl declname (RecC conname vars) =
  let varBindings = map (\(name, _, _) -> (name, ConE (mkName "Nothing"))) vars
      val_body = RecConE conname varBindings
      val_clause = Clause [] (NormalB val_body) []
      val_decl = FunD declname [val_clause]
  in val_decl

