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
  , FlexibleContexts
  , AllowAmbiguousTypes
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
import Data.Type.Nat (Nat(..), Nat3, fromNatural, SNatI)
import Control.Lens (itraverse)

data QuestionType where
  Text   :: QuestionType
  Choice :: Nat -> QuestionType

data Question (qt :: QuestionType) where
  TextQuestion   :: String -> Question Text
  ChoiceQuestion :: SNatI n => String -> Vec n String -> Question (Choice n)

type family Answer (qt :: QuestionType) = (a :: *) | a -> qt where
  Answer Text = String
  Answer (Choice n) = Fin n

data Questionnaire a where
  Result :: a -> Questionnaire a
  Ask    :: Question q -> (Answer q -> Questionnaire a) -> Questionnaire a

instance Functor Questionnaire where
  fmap f fa = pure f <*> fa

instance Applicative Questionnaire where
  pure = return
  (<*>) = ap

instance Monad Questionnaire where
  return = Result

  Result a  >>= amb = amb a
  (Ask q c) >>= amb = Ask q $ \ans -> (c ans) >>= amb

ask :: Question q -> Questionnaire (Answer q)
ask q = Ask q Result

select :: forall n. SNatI n => String -> Vec n String -> Questionnaire (Fin n)
select s v = do
  i <- ask $ ChoiceQuestion s v
  pure $ i

select_ s v = () <$ select s v

choose :: forall a. (Enum a, Prompt a, SNatI (EnumSize a)) => String -> Questionnaire a
choose s = to <$> select s (prompt (Proxy :: Proxy a))

choose_ :: forall a. (Enum a, Prompt a, SNatI (EnumSize a)) => String -> Questionnaire ()
choose_ s = () <$ choose @a s

yesno :: String -> Questionnaire Bool
yesno = choose

yesno_ s = () <$ yesno s

freeform :: String -> Questionnaire String
freeform = ask . TextQuestion

freeform_ s = () <$ freeform s

instance Enum (Fin n) where
  type EnumSize (Fin n) = n
  to = id
  from = id

class Enum a => Prompt a where
  prompt :: Proxy a -> Vec (EnumSize a) String

instance Prompt Bool where
  prompt _ = "no" ::: "yes" ::: VNil

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

promptChoice :: forall n. SNatI n => IO (Fin n)
promptChoice = do
  l <- getLine
  let mi = fromNat =<< fromNatural <$> readMaybe l
  case mi of
    Just i  -> pure i
    Nothing -> do
      print "Sorry, couldn't parse your input. Please select one of the options above."
      promptChoice

run :: Questionnaire a -> IO a
run (Result a) = pure a
run (Ask q c) = case q of
  TextQuestion q -> do
    putStrLn $ "Q: " ++ q
    putStrLn "Please enter your answer below"
    ans <- getLine
    run $ c ans

  ChoiceQuestion q v -> do
    putStrLn $ "Q: " ++ q
    putStrLn "Please select one of the following options"
    itraverse (\i a -> putStrLn $ show i ++ ". " ++ a) v
    i <- promptChoice
    run $ c i

main :: IO ()
main = run test >>= print
