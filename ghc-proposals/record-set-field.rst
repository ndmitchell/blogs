.. proposal-number:: Leave blank. This will be filled in when the proposal is
                     accepted.

.. trac-ticket:: Leave blank. This will eventually be filled with the Trac
                 ticket number which will track the progress of the
                 implementation of the feature.

.. implemented:: Leave blank. This will be filled in with the first GHC version which
                 implements the described feature.

Add ``setField`` to ``HasField``
================================

This is a proposal to add the function `setField` to the built-in typeclass
``HasField``, allowing type-based resolution of field names in record update functions.
It *does not* introduce any new extensions.

Motivation
----------

A serious limitation of the Haskell record system is the inability to
overload field names in record types: for example, if the data types

.. code-block:: haskell

  data Person  = Person  { personId :: Int, name :: String }
  data Address = Address { personId :: Int, address :: String }

are in scope in the same module, there is no way to determine which
type an occurrence of the ``personId`` record selector refers to.
The ``HasField`` extension defined in the already-implemented
`Overloaded Record Fields proposal <https://github.com/ghc-proposals/ghc-proposals/blob/master/proposals/0002-overloaded-record-fields.rst>`_
introduce ``HasField``, to allow type-based resolution of field names and
polymorphism over record selectors. The class ``HasField`` is currently defined as:

.. code-block:: haskell

  -- | Constraint representing the fact that the field @x@ belongs to
  -- the record type @r@ and has field type @a@.  This will be solved
  -- automatically, but manual instances may be provided as well.
  class HasField (x :: k) r a | x r -> a where
    -- | Selector function to extract the field from the record.
    getField :: r -> a

While this class provides a way to get a field, it provides no way to set a field.
To quote:

  In the interests of simplicity, this proposal does not include a class
  to provide polymorphism over record updates

Such a proposal to deal with record updates would clearly be desirable.

Proposed Change
---------------

We propose to extend the ``HasField`` class with an additional method ``setField``

.. code-block:: haskell

  -- | Update function to set a field in the record.
  setField :: a -> r -> r

Using this addition function it would be possible to write a function:

.. code-block:: haskell

  mkLens :: HasField lbl r a => Lens' r a

And thus allow generating lenses from the ``HasField`` class. The function
``setField`` is also useful in its own right, complementing the ``getField``
method and providing the ability to modify records polymorphically.

Drawbacks
---------

The ``HasField`` type class will now be implemented by a pair rather than a ``newtype`` which,
if not optimised away, may give a small performance penalty. However, given the optimiser, that
seems unlikely.

Alternatives
------------

Polymorphic ``setField``
~~~~~~~~~~~~~~~~~~~~~~~~

A *type-changing update* is one where the type ``r`` is higher-kinded and the field
``x`` is the only member of that type. As an example, given a value of type ``(Int, Bool)``,
the selector pointing to the first component, and a new value of type ``Double`` we can
produce ``(Int, Bool)``. The design space for type-changing updates is large, and almost
certainly requires an additional type class. In contrast, the design space for type-preserving
updates is small and it can easily be incorporated into the existing type class. The addition
of type-preserving updates in no way constraints the design space for future type-changing
updates, but is useful in its own right.

Adding ``updateField``
~~~~~~~~~~~~~~~~~~~~~~

An alternative to ``setField`` is:

.. code-block:: haskell

  -- | Update function to set a field in the record.
  updateField :: (a -> a) -> r -> r

The function ``updateField`` can be recovered using ``setField`` and ``getField``, but
``setField`` is simpler, so we prefer it.

Implementation Plan
-------------------

Adam Gundry has offered to implement this feature.
