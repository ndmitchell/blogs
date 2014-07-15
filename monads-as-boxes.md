# Monads as Boxes

Everyone has a Monad tutorial, and I’m no exception with my IO without Monads tutorial. However, part of the rite of passage for a Haskell programmer is to write a Monad tutorial, so here is mine. I first understood Monads by reading Cale Gibbards “Monads as containers” tutorial, so any resemblance to that is purely because it was excellent.

You can think of monads as boxes, inside which you can put values. A monadic value is usally written:

m a

Where m has a Monad constraint on it. Here m is the box, and a is the value inside the box:

<<! box containing a red circle !>>

Now we can talk about the three basic monad operations:

THE BASIC MONAD OPERATIONS

fmap

In Haskell this is technically in the functor class, but all monads should be functors. The idea of fmap, as I said in the last blog, is to replace the contents of the container:

<<! box containing a red circle, a red->blue arrow, box containing a blue circle !>>

A functor applies a function to the value inside the box, and returns the new value in a new box.

return

Return simply takes a value and wraps it in a box.

<<! a red circle, then a box containing a red circle !>>

join

The final operation takes a box containing a box and removes one level of structure.

<<! a box containing a box with a red circle, a box with a red circle !>>

INSTANTIATING THESE MONADS

We can now give monad instances for Maybe, List and IO. Note that a monad gives us no way to construct interesting values other than return, and no way to get a value out of a monad. For that we have to use values specific to our type of monad.

We can think of IO as a sequence of actions to perform at runtime along with a value to return (it’s a bit more complex in real life.) Then fmap is just change the value without changing the list of actions, return is create an empty list of actions, and join adds together the actions. More concretely:

data Actions a = Actions [Action] a

join (Actions xs (Action ys a)) = Actions (xs++ys) a


DERIVED OPERATIONS

You may notice that Monad actually has >>= and >>. These can also be expressed simply:

(>>=)

<<! a red circle in a box, a red circle to blue box, and then a blue box. !>>

This can easily be described as an fmap/join combination. Haskell chooses to provide this as a direct primitive, but it’s often conceptually simpler to explain fmap/join.

(>>)

This is even more derived, and relies on the box changing during a join.

MONAD LAWS

The first law expresses that applying an operation to a monadic value gives you the same.

    return a >>= k  ==  k a
    
    
    m >>= return  ==  m
    m >>= (\x -> k x >>= h)  ==  (m >>= k) >>= h
    
