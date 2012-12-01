\documentclass[svgnames]{beamer}

%include polycode.fmt
%include lhs2TeX.fmt

%format <|>     = "{\color{blue}\texttt{ <|> }}"
%format <+>     = "{\color{blue}\texttt{ <+> }}"
%format <?>     = "{\color{blue}\texttt{ <?> }}"
%format <*>     = "{\color{blue}\texttt{ <*> }}"
%format eagerly = "{\color{blue}eagerly}"
%format cut     = "{\color{blue}cut}"
%format stop    = "{\color{blue}stop}"
%format return  = "{\color{blue}return}"
%format liftIO  = "{\color{blue}liftIO}"
%format >>      = "{\color{blue}\texttt{ >> }}"
%format >>=     = "{\color{blue}\texttt{ >>= }}"
%format forkIO  = "{\color{red}forkIO}"
%format block   = "{\color{red}block}"
%format unblock = "{\color{red}unblock}"
%format killThread = "{\color{red}killThread}"
%format newGroup = "{\color{Brown}newGroup}"
%format close = "{\color{Brown}close}"
%format inGroup = "{\color{Brown}inGroup}"
%format local = "{\color{Brown}local}"
%format newPrimGroup = "{\color{Brown}newPrimGroup}"
%format isZero = "{\color{Brown}isZero}"
%format HIO = "{\color{Brown}HIO}"
%format Orc = "{\color{blue}Orc}"
%format <\>  = "{\texttt{ </> }}"
%format forall = "\forall"
%format -    = "$\texttt{-}$"

\usetheme{Warsaw}
\usepackage[english]{babel}
\usepackage[absolute,overlay]{textpos}
\usepackage{verbatim}
\usepackage{color}
\mode<presentation>
\title[{\makebox[.45\paperwidth]{Concurrent Orchestration in Haskell\hfill%
       \insertframenumber/\inserttotalframenumber}}]{Concurrent Orchestration in Haskell}
\subtitle[Errors]{John Launchbury and Trevor Elliott, Haskell '10, Galois Inc.}
\author[Ruben de Gooijer]{Colloquium presentation by Ruben de Gooijer}
\institute[Test]{
  Department of Information and Computing Sciences\\
  University of Utrecht, the Netherlands
}
\date{November 3, 2011}

\newenvironment{reference}[2]{%
  \begin{textblock*}{\textwidth}(#1,#2)
      \footnotesize\it\bgroup\color{red!50!black}}{\egroup\end{textblock*}}

\renewcommand\hscodestyle{\large}

\begin{document}

\begin{frame}[plain]
   \titlepage
\end{frame}

\begin{frame}

\begin{code}
either :: IO a -> IO b -> IO (Either a b)
either p q = do
   m <- newEmptyMVar 
   block $ do 
      pId <- forkIO (unblock $ p >>= putMVar m . Left)
      qId <- forkIO (unblock $ q >>= putMVar m . Right)
      r <- takeMVar m
      killThread pId
      killThread qId
      return r
\end{code}

What's wrong with plain Haskell? Can we do better?

\end{frame}

\begin{frame}[fragile]
Yes we can!
\newline\newline
The authors have implemented the Orc language \newline as a Haskell library.
\newline\newline
Orc is a \emph{concurrent scripting} DSL which allows concurrency problems
to be expressed at a conceptually higher level using:
\begin{itemize}
   \item primitive combinators
   \item and automated thread management
\end{itemize}

\vspace{4mm}
\end{frame}

\begin{frame}[fragile]{Code Example}

\begin{definition}
\emph{Concurrent scripting} or \emph{orchestration}: the act of coordinating
several external actions whose timing and interleaving is unpredictable.
\end{definition}

For example:

\begin{code}
search = do
   term <- prompt "Enter a search term: " 
   cut (bing term <|> google term)
\end{code}

\begin{itemize}
   \item External actions: prompt, bing, google
   \item Nondeterminism
\end{itemize}

\end{frame}

\begin{frame}{Why Haskell?}

So why choose Haskell as host language?
\newline\newline
\textbf{Concurrency is hard.} \newline
\emph{\small Race conditions, deadlocks, composability, ...}
\newline\newline
\textbf{Haskell and its libraries already provide a solid foundation for concurrent programming.} \newline
\emph{\small Immutability by default, side-effect tracking, Concurrent Haskell, STM, ...}

\end{frame}

\begin{frame}{Either Revisited}

\begin{code}
either p q =
   cut ((p >>= return . Left) <|> (q >>= return . Right))
\end{code}
\vspace*{3mm}

The programmer doesn't need to worry about:
\begin{itemize}
   \item Locks
   \item Thread management
   \item Asynchronous exceptions
\end{itemize}

\end{frame}


\begin{frame}{Overview}

Presentation outline.

\begin{itemize}
   \item The EDSL
   \item The Implementation
   \item Conclusion
\end{itemize}

\end{frame}

\begin{frame}
\huge The EDSL
\end{frame}

\begin{frame}[fragile]{The EDSL}

From a bird's-eye-view Orc is a combination of three things:
\begin{itemize}
   \vspace{4mm}
   \item \textbf{Many-valued concurrency} \newline
   \emph{Concurrent computations may produce zero or more results.}
   \vspace{4mm}
   \item \textbf{External actions (effects)} \newline
   \emph{Calling webservices, making phone calls, launching missiles...}   
   \vspace{4mm}
   \item \textbf{Managed resources} \newline
   \emph{Threads are accounted for.}
\end{itemize}

\end{frame}

\begin{frame}{The EDSL}

In Orc \emph{external actions} are represented by \emph{sites}.
\vspace{4mm}
\begin{definition}
Sites are external actions that may produce zero or more results, or halt.
\end{definition}
\vspace{4mm}
If |p| is a site then \emph{halt} is defined as:
\begin{itemize}
   \item All sites called by \texttt{p} have either responded or halted.
   \item |p| will never call any more sites.
   \item |p| will never publish any more values.
\end{itemize}

\end{frame}

\begin{frame}[fragile]{The EDSL}

To represent both \emph{sites} and Orc computations a new type constructor is introduced:

\begin{code}
newtype Orc a = Orc { ... }
\end{code}

A value of type |Orc a| \emph{may} produce \emph{zero or more results} of type \texttt{a}, 
and \emph{may} have \emph{many} effects.

\end{frame}

\begin{frame}[fragile]{The EDSL}

To orchestrate \emph{sites} Orc defines a small set of primitive combinators:
\begin{itemize}
   \item sequential composition
   \item parallel composition
   \item pruning
   \item otherwise
\end{itemize}
\vspace{4mm}

The authors of Orc have shown that these primitives are sufficiently expressive to solve many concurrency problems.$^{1}$
\newline\newline
We'll now see how these primitives translate to Haskell...
\vspace*{4mm}

\begin{reference}{4mm}{80mm}
$^{1}$Workflow Patterns in Orc, Cook, et al, Proceedings of the 8th International Conference, COORDINATION 2008, Coordination Models and Languages
\end{reference}

\end{frame}


\begin{frame}[fragile]{The EDSL}

Sequential composition.

\begin{code}
(>>=) :: Orc a -> (a -> Orc b) -> Orc b
\end{code}

\begin{itemize}
   \item Orc is made an instance of the Monad class
   \item The monad laws hold
\end{itemize}

We may use {do}-notation to express \emph{sequential composition}.

\end{frame}

\begin{frame}[fragile]{The EDSL}

For example:
\begin{code}
do  x <- p
    y <- q x
    h x y
\end{code}

The expression reads as nested iteration:\newline\newline 
\emph{
foreach \texttt{x} produced by \texttt{p} \newline
   \hspace*{5mm} foreach \texttt{y} produced by \texttt{(q x)} \newline 
   \hspace*{10mm} produce all values of \texttt{(h x y)}
}
\newline\newline
Nested iteration is similar to the List monad. But now:
\begin{itemize}
   \item the ordering of results is nondeterministic
   \item all iterations are run concurrently
\end{itemize}

\end{frame}

\begin{frame}[fragile]{The EDSL}

We can use \verb stop  to end the local thread of execution.

\begin{code}
stop :: Orc a
\end{code}

For example:
\begin{code}
do  x  <-  p
    y  <-  stop
    h x y
\end{code}

which is equivalent to:

> p >> stop

\end{frame}

\begin{frame}[fragile]{The EDSL}

Parallel composition.

\begin{code}
<|> :: Orc a -> Orc a -> Orc a
\end{code}

For example:

\begin{code}
p <|> q
\end{code}

\begin{itemize}
   \item Executes \texttt{p} and \texttt{q} in parallel and returns all the results produced by both as
         the become available.
   \item Results of \texttt{p} and \texttt{q} may be arbitrarily interleaved.
\end{itemize}

\end{frame}

\begin{comment}
\begin{frame}[fragile]{The EDSL}

Some interesting properties of \verb <|> . 

\begin{itemize}
   \item Left-right identity. 
>   stop <|> p == p <|> stop == p 

   \item Commutativity: switching arguments does not affect outcome.
>   p <|> q == q <|> p
   
   \item Associativity: evaluation order does not matter.
>   p <|> (q <|> z) == (p <|> q) <|> z
   
   \item Bind is right-distributive over \verb <|>  .
>   (p <|> q) >>= k == (p >>= k) <|> (q >>= k) 
\end{itemize}

\end{frame}

\end{comment}

\begin{frame}[fragile]{The EDSL - Code Example}

A simple program that combines parallel and sequential composition to 
be of a constant nuisance to John.

> emailJohn :: Orc ()
> emailJohn = email "john.doe@undefined.com"
>                 <|> (delay 2 >> emailJohn)

\end{frame}

\begin{frame}[fragile]{The EDSL}

Managing concurrency.

\begin{code}
eagerly :: Orc a -> Orc (Orc a)
\end{code}

For example:

\begin{code}
eagerly p
\end{code}

\begin{itemize}
   \item Execute \texttt{p} in a separate thread and return a handle to its first result.
   \item Once the first result is produced the redundant concurrency gets pruned.
\end{itemize}

> cut p  =   join . eagerly
>        ==  do  x <- eagerly p
>                x

\end{frame}

\begin{frame}[fragile]{The EDSL - Code Example}

Fork-join:
\begin{code}
sync :: (a -> b -> c) -> Orc a -> Orc b -> Orc c
sync f p q = do
   po <- eagerly p
   qo <- eagerly q
   pure f <*> po <*> qo
\end{code}

\pause

> notBefore:: Orc a -> Float -> Orc a
> p `notBefore` w = sync const p (delay w)


\end{frame}

\begin{frame}[fragile]{The EDSL}

In Orc \emph{sites} are lifted by the compiler or implemented externally.

> return  ::  a      ->  Orc a
> liftIO  ::  IO a   ->  Orc a

In Haskell \emph{sites} are simply liftings of either:
\begin{itemize}
   \item pure (|return|) or 
   \item |IO| computations (|liftIO|). 
\end{itemize}
\vspace{4mm}
For example:
> google term = liftIO $ httpGet ("google.com?s=" ++ t)

\end{frame}

\begin{frame}{The EDSL}
The canonical way to run Orc computations is using \verb runOrc .

> runOrc  ::  Orc a  ->  IO ()

For example:

> runOrc $ google "But what does it mean?"

\emph{Note:} the unit type of IO. 


\end{frame}

\begin{frame}[fragile]{The EDSL - Code Example}

All combinators thus far inherit non-termination from their arguments.
\newline\newline
As a remedy the Orc library provides a |timeout| combinator.

> timeout :: Float -> a -> Orc a -> Orc a
> timeout n a p = cut (p <|> (delay n >> return a))

> > timeout 3 "No results." (google "Haskell")

\end{frame}

\begin{frame}
\huge The Implementation
\end{frame}

\begin{frame}{The Implementation}

Orc is implemented by stacking monads on top of each other.
\newline\newline
The implementation stack (bottom to top).
\newline
\begin{itemize}
   \item \textbf{The IO monad}
   \newline
   \emph{External actions (effects)}
   \newline
   \item \textbf{The hierarchical IO monad (HIO)}
   \newline
   \emph{Managed resources, i.e. automated thread management}
   \newline
   \item \textbf{The Orc monad}
   \newline
   \emph{Multiple results}
\end{itemize}

\vspace{4mm}

All layers are made instances of |MonadIO|. \newline
To move up a layer use |liftIO|.

\end{frame}

\begin{frame}[fragile]{The Implementation - HIO}

\begin{itemize}
\item \textbf{The goal} \newline
Implement automated thread management for Orc computations.
\newline

\pause

\item \textbf{The problem} \newline
The concurrency introduced inside the |IO| monad is not accounted for. 
\newline\newline
We are left without a handle to kill redundant threads. 
\newline

\pause

\item \textbf{The solution} \newline
Extend the |IO| monad with an environment that can be used to do the bookkeeping of running threads.
\end{itemize}

\end{frame}

\begin{frame}[fragile]{The Implementation - HIO}

Computations in the HIO monad are represented by:

\begin{code}
newtype HIO a = HIO { inGroup :: Group -> IO a }
\end{code}

isomorphic to |ReaderT Group IO a|.

\begin{code}
type Group        =  (TVar NumberOfActiveThreads, 
                     TVar Inhabitants)

data Inhabitants  =  ...

data Entry        =  Thread ThreadId
                  |  Group Group
\end{code}


\end{frame}

\begin{frame}[fragile]{The Implementation - HIO}

\begin{code}
instance Monad HIO where
  return x = HIO $ \w -> return x
  m >>= k  = HIO $ \w -> do 
                  x <- m `inGroup` w
                  k x `inGroup` w

\end{code}

Think Monad Reader.

\end{frame}

\begin{frame}[fragile]{The Implementation - HIO}

We use an overloaded version of |forkIO| such that we may do some bookkeeping at the spawning
and killing of threads.

\begin{code}
instance HasFork HIO where
  fork hio = HIO $ \w -> block $ do
                         increment w
    fork  (block (do  tid       <-  myThreadId
                      register      (Thread tid) w
                      unblock       (hio `inGroup` w))
          `finally` 
            decrement w)
\end{code}

\tiny\emph{Note:} block is deprecated, replaced by mask.

\end{frame}

\begin{frame}[fragile]{The Implementation - HIO}

HIO computations can be run inside the IO monad.

\begin{code}
runHIO :: HIO b -> IO ()
runHIO hio = do
    w <- newPrimGroup
    r <- hio `inGroup` w
    isZero w
    return ()
\end{code}

isZero: wait until all threads in the |Group| have \emph{halted}.

\end{frame}

\begin{frame}[fragile]{The Implementation - Orc Monad}

\begin{itemize}
\item \textbf{The goal} \newline
Allow the definition of Orc computations that produce zero or more results.
For example:

> ((p <|> q) >>= k) :: Orc a

\item \textbf{The problem} \newline
As the results of \texttt{p} and \texttt{q} become available we need to somehow pass them along to the rest of the program |k|.
\newline

\pause

\item \textbf{The solution} \newline
Write functions that produce results in \emph{continuation-passing-style} (CPS).
\newline\newline
Enables functions to pass their results to the future of the program.

\end{itemize}

\end{frame}

\begin{frame}[fragile]{The Implementation - Orc Monad}
Short detour into CPS...

\begin{code}
multiply :: Int -> Int -> Int
multiply x y = x * y
\end{code}

\pause 

Written in \emph{continuation-passing-style}:

\begin{code}
multiply :: Int -> Int -> (Int -> r) -> r
multiply x y k = k (x * y)
\end{code}

\pause

Usage:

\begin{code}
> multiply 3 4 (putStrLn . show)
> 12
\end{code}

\pause

The \textbf{current} computation |multiply 3 4| decides what it does with the \textbf{future}
of the computation |putStrLn . show| !

\end{frame}

\begin{frame}[fragile]{The Implementation - Orc Monad}

The implementation of the Orc type constructor. 

\vspace{4mm}
> newtype Orc a = Orc { withCont :: (a -> HIO ()) -> HIO () }

\begin{itemize}
   \item Newtype abstracts from explicit continuation passing
   \item The Orc constructor is not exposed. Only primitive combinators are allowed access to the current continuation
   \item The representation of the future of the program is not polymorphic. 
         In the end Orc computations should always run inside |HIO|.
   \item Isomorphic to |ContT () HIO a|.
\end{itemize}

\end{frame}

\begin{frame}[fragile]{The Implementation - Orc Monad}

Hiding the \emph{current continuation} inside the Orc type.
\newline\newline
From explicit CPS:

> multiply        :: Int -> Int -> (Int -> r) -> r
> multiply x y k  = k (x * y)

to implicit CPS using Orc:

> multiply        :: Int -> Int -> Orc Int
> multiply x y    = Orc $ \k -> k (x * y)

\end{frame}

\begin{frame}[fragile]{The Implementation - Orc Monad}

The implementation of the primitive combinators.

\large
> return x  =  \k -> k x

> p >>= h   =  \k -> p (\x -> h x k)

> p <|> q   =  \k -> fork (p k) >> q k

> stop      =  \k -> return ()

> runOrc p  = runHIO $ p (\x -> return ())

\end{frame}

\begin{frame}

Equational reasoning.

\def\commentbegin{\quad\{\ }
\def\commentend{\}}

>     return "Hello" >>= const stop
>
> == {- definition of |>>=| -}
>
>     \k -> return "Hello" (\x -> (const stop) x k)
>
> == {- definition of |return| -}
>
>     \k -> (\k -> k "Hello") (\x -> (const stop) x k)
>
> == {- beta-reduction -}   
>
>     \k -> (\x -> (const stop) x k) "Hello"
>
> == {- beta-reduction -}
>
>     \k -> (const stop) "Hello" k
>
> == {- definition of |const| -}
>
>     \k -> stop k


\end{frame}

\begin{frame}[fragile]{The Implementation - Orc Monad}

\begin{code}
eagerly          ::  Orc a -> Orc (Orc a)
eagerly p        =   Orc $ \k -> do
   result   <-  newEmptyMVar
   w        <-  newGroup
   local w  $   fork (p `saveOnce` (result,w))
   k (liftIO $ readMVar result)
\end{code}

\pause

\begin{code}
saveOnce            ::  Orc a -> (MVar a,Group) -> HIO ()
p `saveOnce` (r,w)  =   do 
   ticket <- newMVar ()
   p # \x -> 
     (takeMVar ticket >> putMVar r x >> close w)
\end{code}

\small readMVar: is atomic only if there are \textbf{no} other \newline producers for the |result| MVar.

\end{frame}

\begin{frame}[fragile]{The Implementation - Orc Monad}

In Orc |eagerly| is lazy in the binding of its results.

> eagerlyLazy :: Orc a -> Orc a
> eagerlyLazy p = Orc $ \k -> do
>  ...
>  k (unsafePerformIO $ readMVar res)

\begin{itemize}
   \item Good: we may use lazy values directly.
   \item Bad: the programmer needs to be careful about evaluation order.
\end{itemize}
\vspace{3mm}
The authors deemed the non-lazy |eagerly| to be more inline with a core Haskell philosophy:
\emph{the programmer should not be concerned with the evaluation order of expressions.}

\end{frame}

\begin{frame}{Conclusion}

\begin{itemize}
   \item Hiding thread management and orchestration inside a concurrent scripting 
         language makes it easier to write concurrent programs.
   \item Orc naturally exists within a general purpose language. \newline
   \emph{\small core combinators are a small part of the overall program.}
   \item Haskell proofs to be a good choice for embedding Orc. \newline
   \emph{\small higher-order functions, laziness, powerful type system, monads, concurrency}
   \item A non-lazy semantics for |eagerly| fits better in Haskell.
\end{itemize}

\end{frame}

\begin{frame}{Conclusion}

Future.
\begin{itemize}
   \item Implement more advanced thread management.
   \item The authors have made significant progress on proving the algebriac laws of Orc and identified the
         laws required from Concurrent Haskell. But there still is work to be done.
   \item Investigate splitting up |eagerly| into a part that limits work and an eager memo part which
         returns a reusable handle to all results.
   \item It would be interesting to see how Orc relates to the other concurrency approaches out there!
\end{itemize}

\end{frame}

\begin{frame}[fragile]{Related Work}

\begin{itemize}
   \item \emph{Reo: a channel-based coordination model for component composition.}
   \item \emph{Coordination Models Orc and Reo Compared}
   \item \emph{Translating Orc features into Petri nets and the Join calculus.}
   \item $\pi$-calculus / Pict language
   \item Actor Model

\end{itemize}

\end{frame}

\begin{frame}{End}

\begin{center}
Thank you. Questions?
\newline\newline
The library is available on hackage: 
\url{http://hackage.haskell.org/package/orc}
\newline \newline
\emph{more combinators more fun}
\end{center}

\end{frame}

\begin{frame}{The EDSL - Code Example}

Analogous to the list |scanl| function. However, now the order in which the combining function
is applied is nondeterministic.

> scan :: (a -> s -> s) -> s -> Orc a -> Orc s
> scan f s p = do
>  accum <- newTVar s
>  x <- p
>  (w,w') <- modifyTVar accum (f x)
>  return w'

\end{frame}



\begin{frame}[fragile]{Usage Examples}

Parallel or.

\begin{code}
parallelOr :: Orc Bool -> Orc Bool -> Orc Bool
parallelOr p q = do
   ox <- eagerly p
   oy <- eagerly q
   cut  (   (ox >>= guard >> return True)
       <|>  (oy >>= guard >> return False)
       <|>  (pure (||) <*> ox <*> oy))
\end{code}

\end{frame}

\end{document}
