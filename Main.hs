import Control.Concurrent
import Control.Concurrent.MVar
import Control.Monad
import qualified Data.Map as M
import System.IO
import Text.Printf

type Name=String

-- synchronous, polyadic
-- arity of a channel can change or not.
-- here the latter is chosen to ease implementation.
data Proc
    =New Name Proc
    |Recv Name [Name] Proc
    |Send Name [Name] Proc
    |Par Proc Proc
    |Many Proc
    |Null
    -- this is hidden from user. only 'bind' knows about it.
    |Prim String
    deriving(Show)

-- naive mapping to Haskell threads, without garbage process collection
bind :: Proc -> IO ()
bind p=do
    ctx<-newDefaultContext
    void $ forkIO $ bindWithContext ctx (return ()) pRTS
    where pRTS=Par p $ Many (Prim "print")


newDefaultContext :: IO Context
newDefaultContext=do
    glb<-newMVar M.empty
    return $ Context glb []

type Notify=IO ()
data Context=Context (MVar (M.Map Name Channel)) [(Name,Channel)]
data Channel=Channel Name (MVar [Channel])



-- lazy evaluation of 'Many' preserves pi semantics yet manages with finite
-- amount of processes. But serious process collector will be needed
-- in real application. (it's possible that absence of process collector significantly
-- impact expressiveness, even theoretically.)
--
-- generally thinking, process collector can be though as mapping of
-- pi process to Haskell RTS thread, OS process, CPU core, network node,
-- or whatever machinery.
--
-- garbage collection can also be treated with this kind of generalization.
-- (mapping of mutable map to RAM)
bindWithContext :: Context -> Notify -> Proc -> IO ()
bindWithContext ctx notify (Send nch nvs cont)=do
    (Channel _ mv)<-lookupContext ctx nch
    vs<-mapM (lookupContext ctx) nvs
    putMVar mv vs
    notify
    bindWithContext ctx (return ()) cont

bindWithContext ctx notify (Recv nch ns cont)=do
    (Channel _ mv)<-lookupContext ctx nch
    cs<-takeMVar mv
    notify
    let ctx'=foldl extendContext ctx (zip ns cs)
    if (length ns/=length cs)
        then putStrLn "arity mismatch"
        else  bindWithContext ctx' (return ()) cont

bindWithContext ctx notify (New n cont)=do
    ch<-newChannel n
    bindWithContext (extendContext ctx (n,ch)) notify cont

bindWithContext ctx notify (Par c0 c1)=do
    bindWithContext ctx notify c0
    void $ forkIO $ bindWithContext ctx notify c1

bindWithContext ctx notify (Many c)=do
    req<-newEmptyMVar
    let notify=putMVar req ()
    
    -- this is really bad. needs serious process collector.
    forever $ do
        forkIO $ bindWithContext ctx notify c
        takeMVar req

bindWithContext ctx notify (Prim "print")=do
    (Channel _ mv)<-lookupContext ctx "print"
    cs<-takeMVar mv
    notify
    
    case cs of
        [Channel ('*':str) _,Channel _ contmv] -> putStr str >> putMVar contmv []
        _ -> putStrLn "error in print"

bindWithContext ctx notify (Prim n)=do
    printf "tried to bind unknown primitive %s\n" n

bindWithContext _ _ Null=return ()

newChannel :: Name -> IO Channel
newChannel n=do
    mv<-newEmptyMVar
    return $ Channel n mv

extendContext :: Context -> (Name,Channel) -> Context
extendContext (Context global cs) p=Context global (p:cs)

lookupContext :: Context -> Name -> IO Channel
lookupContext (Context global cs) n=case lookup n cs of
    Just c -> return c
    Nothing -> do
        gm<-takeMVar global
        case M.lookup n gm of
            Just c -> putMVar global gm >> return c
            Nothing -> do
                c<-newChannel n
                putMVar global $ M.insert n c gm
                return c

