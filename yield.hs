{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE TemplateHaskell       #-}
{-# LANGUAGE RecordWildCards #-}

import Data.Csv
import qualified Data.ByteString.Lazy as BL
import qualified Data.Vector as V
import qualified Control.Monad.Trans.State.Lazy as S
import Control.Lens hiding ((.=))
import qualified Data.Text as T
import Control.Monad.IO.Class


data Tx = Tx
  { _date :: T.Text
  , _apr  :: Double
  } deriving (Show, Eq)

makeLenses ''Tx

data Vtx = Vtx
  { _date2     :: T.Text
  , _apr2      :: Double
  , _dpr2      :: Double
  , _balance2  :: Double
  } deriving (Show, Eq)

makeLenses ''Vtx

instance FromNamedRecord Tx where
  parseNamedRecord r = Tx
    <$> r .: "Date"
    <*> r .: "APR"

instance DefaultOrdered Vtx where
  headerOrder _ =
    header [ "Date"
           , "APR"
           , "DPR"
           , "Balance"
           ]

instance ToNamedRecord Vtx where
  toNamedRecord Vtx{..} =
    namedRecord
    [ "Date"    .= _date2
    , "APR"     .= _apr2
    , "DPR"     .= _dpr2
    , "Balance" .= _balance2
    ]

type Vstack = [Vtx]

decodeTx :: BL.ByteString -> Either String (V.Vector Tx)
decodeTx = fmap snd . decodeByName

bVtx :: Tx -> Vtx
bVtx x  = Vtx
  (getD   x)
  (getApr x)
  (getApr x / 365)
  1000

getD :: Tx -> T.Text
getD =  view date

getApr :: Tx -> Double
getApr = view apr

getvDate :: Vtx -> T.Text
getvDate = view date2

getvApr :: Vtx -> Double
getvApr = view apr2

getvDpr :: Vtx -> Double
getvDpr = view dpr2

getvBalance :: Vtx -> Double
getvBalance = view balance2

round2dp :: Double -> Double
round2dp x = fromIntegral (round $ x * 1e2) / 1e2

parseVtx :: IO Vstack
parseVtx = do
  csv <- BL.readFile "apr.csv"
  let ptx = decodeTx csv
  let Right listTx = fmap V.toList ptx
  let a = fmap getApr listTx
  let b = fmap bVtx listTx

  return b

popTx :: S.StateT Vstack IO ()
popTx = do
  tx     <- popVtx
  stack  <- S.get

  if lastTx tx stack then do
    if length stack <= 6 then io $ print "" else do
      tx2 <- popVtx
      tx3 <- popVtx
      tx4 <- popVtx
      tx5 <- popVtx
      tx6 <- popVtx
      tx7 <- popVtx

      let a = getvDate tx `T.append` " - " `T.append` getvDate tx7
      let b = apr7Days tx tx2 tx3 tx4 tx5 tx6 tx7
      let c = dpr7Days tx tx2 tx3 tx4 tx5 tx6 tx7
      let d = getvBalance tx / 100 * c + getvBalance tx
      let e = set balance2 d tx7
      let k = setVtx a b c d tx7

      io $ print "Last tx"
      io $ print k
      io $ writeData k

    else do
    io $ print "-----"
    io $ print tx
    tx2 <- popVtx
    tx3 <- popVtx
    tx4 <- popVtx
    tx5 <- popVtx
    tx6 <- popVtx
    tx7 <- popVtx
    tx8 <- popVtx

    let a = getvDate tx `T.append` " - " `T.append` getvDate tx7
    let b = apr7Days tx tx2 tx3 tx4 tx5 tx6 tx7
    let c = dpr7Days tx tx2 tx3 tx4 tx5 tx6 tx7
    let d = getvBalance tx / 100 * c + getvBalance tx
    let e = set balance2 d tx8
    let k = setVtx a b c d tx8
    push e

    io $ print k
    io $ writeData k
    popTx

popTx2 :: S.StateT Vstack IO ()
popTx2 = do
  tx     <- popVtx
  stack  <- S.get

  if lastTx2 tx stack then do
    let a = getvBalance tx / 100 * getvDpr tx + getvBalance tx
    let b = set balance2 a tx

    io $ print tx
    io $ writeData2 b

    else do
    tx2 <- popVtx
    let a = getvBalance tx / 100 * getvDpr tx + getvBalance tx
    let b = set balance2 a tx2
    let c = set balance2 a tx
    push b

    io $ print tx
    io $ writeData2 c
    popTx2

setVtx ::
  T.Text
  -> Double
  -> Double
  -> Double
  -> Vtx
  -> Vtx
setVtx x y z a b =
  set date2 x
  (set apr2 y
   (set dpr2 z
    (set balance2 a b)))

dpr7Days
  :: Vtx
  -> Vtx
  -> Vtx
  -> Vtx
  -> Vtx
  -> Vtx
  -> Vtx
  -> Double
dpr7Days x y z a b c d
  = getvDpr x
  + getvDpr y
  + getvDpr z
  + getvDpr a
  + getvDpr b
  + getvDpr c
  + getvDpr d

apr7Days
  :: Vtx
  -> Vtx
  -> Vtx
  -> Vtx
  -> Vtx
  -> Vtx
  -> Vtx
  -> Double
apr7Days x y z a b c d
  = ( getvApr x
  +   getvApr y
  +   getvApr z
  +   getvApr a
  +   getvApr b
  +   getvApr c
  +   getvApr d) / 7

lastTx :: Vtx -> Vstack -> Bool
lastTx x y
  | null y = True
  | x == last y = True
  | length y <= 6 = True
  | otherwise = False

lastTx2 :: Vtx -> Vstack -> Bool
lastTx2 x y
  | null y = True
  | x == last y = True
  | otherwise = False

run :: Vstack -> IO ((), Vstack)
run = S.runStateT popTx

run2 :: Vstack -> IO ((), Vstack)
run2 = S.runStateT popTx2

push :: Vtx -> S.StateT Vstack IO ()
push x = S.state $ \xs -> ((),x:xs)

popVtx :: S.StateT Vstack IO Vtx
popVtx = do
  (x:xs) <- S.get
  S.put xs
  return x

io :: IO a -> S.StateT Vstack IO a
io = liftIO

wHeader :: IO ()
wHeader = BL.writeFile "yield.csv"
  "Date,APR,DPR,Balance\n"

wHeader2 :: IO ()
wHeader2 = BL.writeFile "yield2.csv"
  "Date,APR,DPR,Balance\n"

writeData :: Vtx -> IO ()
writeData x = BL.appendFile "yield.csv" (bytes x)

writeData2 :: Vtx -> IO ()
writeData2 x = BL.appendFile "yield2.csv" (bytes x)

bytes :: Vtx -> BL.ByteString
bytes x = BL.drop 22 (encodeDefaultOrderedByName [x])

main :: IO ()
main = do
  wHeader
  txs <- parseVtx
  run txs
  print "done"

main2 :: IO ()
main2 = do
  wHeader2
  txs <- parseVtx
  run2 txs
  print "done"
