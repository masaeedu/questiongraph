{-# LANGUAGE
    KindSignatures
  , DataKinds
  , RankNTypes
  , GADTs
  , TypeFamilies
  , TypeFamilyDependencies
  , ScopedTypeVariables
  , TypeApplications
  , ViewPatterns
  , DeriveGeneric
  , DeriveFunctor
  , StandaloneDeriving
  , FlexibleContexts
  , AllowAmbiguousTypes
  , TemplateHaskell
#-}

module Main where

import Prelude hiding (Enum)

import Data.Proxy
import GHC.Generics (Generic)
import Control.Monad (ap, when)
import Text.Read (readMaybe)
import Control.Monad.Free
import Data.Vec.Lazy (Vec(..))
import Data.Fin (Fin, fromNat)
import Data.Fin.Enum (to, from, Enum, EnumSize)
import Data.Type.Nat (Nat(..), fromNatural, SNatI)
import Control.Lens (itraverse)
import Control.Monad.Free
import Control.Monad.Free.TH

-- Types
data QuestionType where
  Text   :: QuestionType
  Choice :: Nat -> QuestionType

data Question (qt :: QuestionType) where
  TextQuestion   :: String -> Question Text
  ChoiceQuestion :: SNatI n => String -> Vec n String -> Question (Choice n)

type family Answer (qt :: QuestionType) = (a :: *) | a -> qt where
  Answer Text = String
  Answer (Choice n) = Fin n

data QuestionnaireF a = forall q. Ask (Question q) (Answer q -> a)

-- Free monad instance
instance Functor QuestionnaireF where
  fmap f (Ask q c) = Ask q $ f . c

$(makeFree ''QuestionnaireF)

type Questionnaire = Free QuestionnaireF

-- An interactive, CLI interpretation of what it means to run a questionnaire
promptChoice :: forall n. SNatI n => IO (Fin n)
promptChoice = do
  l <- getLine
  let mi = fromNat =<< fromNatural <$> readMaybe l
  case mi of
    Just i  -> pure i
    Nothing -> do
      print "Sorry, couldn't parse your input. Please select one of the options above."
      promptChoice

interpretF :: QuestionnaireF a -> IO a
interpretF (Ask q c) = case q of
  TextQuestion q -> do
    putStrLn $ "Q: " ++ q
    putStrLn "Please enter your answer below"
    ans <- getLine
    pure $ c ans

  ChoiceQuestion q v -> do
    putStrLn $ "Q: " ++ q
    putStrLn "Please select one of the following options"
    itraverse (\i a -> putStrLn $ show i ++ ". " ++ a) v
    i <- promptChoice
    pure $ c i

runQuestionnaire :: Questionnaire a -> IO a
runQuestionnaire = foldFree interpretF

-- Class for associating a prompt with each chooseable datatype
class Enum a => Prompt a where
  prompt :: Proxy a -> Vec (EnumSize a) String

instance Prompt Bool where
  prompt _ = "no" ::: "yes" ::: VNil

-- Some useful functions for building questionnaires
choose :: forall a. (Enum a, Prompt a, SNatI (EnumSize a)) => String -> Questionnaire a
choose s = do
  i <- ask $ ChoiceQuestion s (prompt (Proxy :: Proxy a))
  pure $ to i

choose_ :: forall a. (Enum a, Prompt a, SNatI (EnumSize a)) => String -> Questionnaire ()
choose_ s = () <$ choose @a s

yesno :: String -> Questionnaire Bool
yesno = choose

yesno_ s = () <$ yesno s

freeform :: String -> Questionnaire String
freeform = ask . TextQuestion

freeform_ s = () <$ freeform s

-- Let's build a questionnaire
data Activity = Gym | Circuit | Cycling
  deriving (Generic, Eq)
instance Enum Activity
instance Prompt Activity where
  prompt _ = "gym" ::: "circuit training" ::: "cycling" ::: VNil

data Stretching = NotAtAll | ABit | Much
  deriving (Generic)
instance Enum Stretching
instance Prompt Stretching where
  prompt _ = "not at all" ::: "a bit" ::: "much" ::: VNil

data BodyPart = Upper | Lower
  deriving (Generic)
instance Enum BodyPart
instance Prompt BodyPart where
  prompt _ = "upper" ::: "lower" ::: VNil

test :: Questionnaire ()
test = do
  hard <- yesno "Did you do something particularly hard or outside your comfort zone?"
  when hard $
    freeform_ "What was the hard or uncomfortable thing you did?"
  exercise <- yesno "Did you exercise today?"
  when exercise $ do
    activity <- choose "What exercise did you do"
    when (activity == Gym) $
      choose_ @BodyPart "Did you work out your upper or lower body at the gym"
    freeform_ "How long did you exercise for?"
  choose_ @Stretching "Did you stretch out?"

main :: IO ()
main = runQuestionnaire test >>= print
