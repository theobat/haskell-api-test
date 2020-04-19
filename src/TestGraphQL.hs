{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoImplicitPrelude #-}

module TestGraphQL
  ( rootResolver,
  )
where
import qualified Data.ByteString.Lazy.Char8 as B
import Data.Morpheus (interpreter)
import Data.Morpheus.Document (importGQLDocumentWithNamespace)
import Data.Morpheus.Types (GQLScalar(parseValue, serialize), GQLRootResolver (..), GQLType(description), IORes, Undefined (..), ResolveQ,ResolveM, Resolver, QUERY, MUTATION, KIND)
import Data.Morpheus.Kind (INPUT, SCALAR)
import Protolude

rootResolver :: GQLRootResolver IO () Query Mutation Undefined
rootResolver =
  GQLRootResolver
    { queryResolver = Query {deity = resolveDeity},
      mutationResolver = Mutation {plantInsert = const (pure testDeity)},
      subscriptionResolver = Undefined
    }



-- queryDeity :: DeityArgs -> Resolver QUERY () IO Deity
-- queryDeity DeityArgs {name} =
--     pure
--     Deity
--         { fullName = name,
--         power = (Just "Shapeshifting"),
--         puppetList = []
--         }
-- testMut :: Mutation m
-- testMut = Mutation {plantInsert = const plantInsert}
--   where
--     plantInsert :: ResolveM () IO Deity
--     plantInsert = undefined
data MutationInput = MutationInput{
      plantInsertList :: Maybe (PlantInputDBContext)
    }
  deriving (Generic, GQLType)
data Mutation m
  = Mutation
      { plantInsert :: MutationInput -> m Deity
      }
  deriving (Generic, GQLType)
data Query m
  = Query
      { deity :: DeityArgs -> m Deity
      }
  deriving (Generic, GQLType)

resolveDeity :: (Applicative m) => DeityArgs -> m Deity
resolveDeity DeityArgs {name = name} = pure $ testDeity {fullName = name}

testDeity :: Deity    
testDeity = Deity
        { fullName = "kokok",
        power = (Just "Shapeshifting"),
        puppetList = []
        }

data Deity
  = Deity
      { fullName :: Text, -- Non-Nullable Field
        power :: Maybe Text, -- Nullable Field
        puppetList :: [Int]
      }
  deriving (Generic)

instance GQLType Deity where
  description = const $ Just ("A supernatural being considered divine and sacred" :: Text)

data Puppet
  = Puppet
      { coreName :: Text,
        coreNumber :: Int
      }
  deriving (Generic)
instance GQLType Puppet where
  type KIND Puppet = INPUT
  description = const $ Just ("qsdqsdd" :: Text)

data OkOk
  = OkOk
      { qsdqsd :: Text,
        dd :: Int
      }
  deriving (Generic)
instance GQLType OkOk where
  type KIND OkOk = INPUT
  description = const $ Just ("qsdqsdd" :: Text)

data DeityArgs
  = DeityArgs
      { name :: Text, -- Required Argument
        mythology :: Maybe Text -- Optional Argument
      }
  deriving (Generic, GQLType)

data DBContext containerType typeContext = DBContext {
  plant :: Apply typeContext (containerType Puppet),
  truck :: Apply typeContext (containerType Puppet),
  ok :: Apply typeContext (containerType OkOk)
} deriving (Generic)


type family Apply token someType

data PlantInputListToken
type instance Apply PlantInputListToken someType = PlantInputList someType

type family PlantInputList someType where
  PlantInputList (containerType Puppet) = containerType Puppet
  PlantInputList _ = Maybe Void

instance GQLType (DBContext Maybe PlantInputListToken) where
  type KIND (DBContext Maybe PlantInputListToken) = INPUT
  description = const $ Just ("qsdqsdd" :: Text)
newtype PlantInputDBContext = PlantInputDBContext (DBContext Maybe PlantInputListToken)

deriving instance Generic PlantInputDBContext
instance GQLType PlantInputDBContext where
  type KIND PlantInputDBContext = INPUT
  description = const $ Just ("qsdqsdd" :: Text)

-- instance GQLType PlantInputDBContext where
--   
instance GQLType Void where
  type KIND Void = SCALAR
  description = const $ Just ("qsdqsdd" :: Text)

instance GQLScalar Void where
  parseValue _ = pure $ undefined
  serialize _ = undefined
